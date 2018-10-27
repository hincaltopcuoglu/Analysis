## PROPENSITY MODEL ##
#How many of the customers has more than 1 policy
#Define the profile for the customers who has more than 1 policy
#What are the main profile of the customer who can buy Casco with us
#If possible can you please work on a model to calculate a score for potential of buying Casco
#Create a structured campaign for propensity
library(readxl)
library(rJava)
library(dplyr)
library(data.table)
library(rlang)
library(corrplot)
library(ggplot2)
library(Boruta)
library(caret)
library(pROC)
library(e1071)


#read data
dataset<-readxl::read_excel('Propensity_Model.xlsx')

#look at data
head(dataset)

#turn into dataframe
dataset<-data.frame(dataset)

#look at the variable structure
str(dataset)

#turn character values into category
for (i in 1:ncol(dataset)){
  if ((is.character(dataset[,i]))==TRUE) {dataset[,i]<-as.factor(dataset[,i])}
}

#turn num variables to integers
for (i in 1:ncol(dataset)){
  if ((is.numeric(dataset[,i]))==TRUE) {dataset[,i]<-as.integer(dataset[,i])}
}


#make some data manipulations
dataset$First.Policy.Start.Date<-as.Date(dataset$First.Policy.Start.Date)
dataset$Become.A.Customer.Date<-as.Date(dataset$Become.A.Customer.Date)

dataset$Days_Cnt<-dataset$First.Policy.Start.Date-dataset$Become.A.Customer.Date
dataset$Days_Cnt<-as.numeric(dataset$Days_Cnt)

str(dataset)
head(dataset)
##################################################
#How many of the customers has more than 1 policy
dataset$policy_cnts <- rowSums(dataset[,7:18])

dataset%>%
  select(Status.Type,policy_cnts)%>%
  filter(policy_cnts>1)%>%
  summarise(cnt=n())

#13637 customers has more than 1 policy

#################################################

#Define the profile for the customers who has more than 1 policy
df2<-dataset%>%filter(policy_cnts>1)
df2<-as.data.frame(df2)
str(df2)
nrow(df2)
###

#customer tipine gore daðýlým
df2%>%group_by(Status.Type)%>%count()

#Gender tipine gore daðýlým
df2%>%group_by(Gender)%>%count()

#musteri ve gender tipine gore ortalama poliçe sayýlarýnýn daðýlýmý
df2%>%group_by(Status.Type,Gender)%>%summarise(mean=mean(policy_cnts))

#sehir dagilimi
df2%>%group_by(City)%>%count()%>%arrange(desc(n))

#musteri tipi ve cinsiyet bazýnda yas ortalamalari
#kirli data var gibi duruyor.
df2%>%group_by(Status.Type,Gender)%>%summarise(Min_Age=min(Age),
                                               Q1_Age=quantile(Age,.25),
                                               Mean_Age=mean(Age),
                                               Q3_Age=quantile(Age,.75),
                                               Max_Age=max(Age),
                                               cnt=n())
#modelleme kýsmýnda bu veriyi temizleyeceðim.


#yaþ gruplarýný kategorilere ayýralým
df2$Age_Cat<-ifelse(df2$Age<=35,'Genc',
              ifelse(df2$Age>35 & df2$Age<=50,'Orta_Yas','Yasli'))


#cinsiyet ve yas gruplarýna bakalým
#genç erkeklerin hatýrý sayýlý bir oraný var.
df2%>%group_by(Age_Cat,Gender)%>%summarise(cnt=n())%>%arrange(desc(cnt))


#yas ve cinsiyet bazýnda ürün kulaným sayýlarý  
df2 %>% group_by(Age_Cat,Gender) %>% 
  summarise_at(.vars = names(.)[7:18],
               .funs = c(sum="sum"))%>%melt()%>%arrange(desc(value))%>%
  filter(value>100)%>%
  ggplot(aes(x = variable, y = value, fill = Age_Cat)) +
  geom_bar(stat = "identity")+theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
#orta yas kitlesi genelde en çok sattýðýmýz iki ürünü kullanýyor.bunun yanýnda genç kitlenin de
#diðer ürünlere ilgisi var gibi duruyor

               
#musteri tipi ve cinsiyet bazýnda ürün kulaným sayýlarý  
df2 %>% group_by(Status.Type,Gender) %>% 
  summarise_at(.vars = names(.)[7:18],
               .funs = c(sum="sum"))%>%melt()%>%arrange(desc(value))%>%
  filter(value>600)%>%
  ggplot(aes(x = variable, y = value, fill = Gender)) +
  geom_bar(stat = "identity")+theme(axis.text.x = element_text(angle = 90, vjust = 0.5))



#musteri tipi bazýnda ürün kullanýmlarý
df2 %>% group_by(Status.Type) %>% 
  summarise_at(.vars = names(.)[7:18],
               .funs = c(sum="sum"))%>%melt()%>%arrange(desc(value))%>%
  filter(value>600)%>%
  ggplot(aes(x = variable, y = value, fill = Status.Type)) +
  geom_bar(stat = "identity")+theme(axis.text.x = element_text(angle = 90, vjust = 0.5))


#correlations between police sayýlari
df3<-df2[,7:18]

M<-cor(df3)

corrplot(M, method="circle",tl.cex = 0.5)
########

#birlikte en çok satýlan ürünleri bulalým

df3<-filter_all(df2[,7:18],any_vars(.>=1))
df3<-as.data.frame(df3)

df4<-data.frame()
for (i in colnames(df3)){
  for (t in colnames(df3)){
    xx<-df3%>%group_by(noquote(i),noquote(t))%>%
      filter_at(vars(noquote(i), noquote(t)), all_vars(.>=1))%>%
      summarise(cnt=n())%>%data.frame()
      df4<-rbind(df4,xx)
  } 
}

df5<-subset(df4, noquote.i.!=noquote.t.)
df6<-df5%>%group_by(cnt) %>% slice(1)%>%arrange(desc(cnt))%>%data.frame()

print(df6)

########

#kasko ürünüe ve özelliklerine odaklanalým.
#kasko hedefini kodlayalým
dataset$target_casco<-ifelse(dataset$CASCO.Auto.Count>=1,1,0)

#kasko olma durumuna göre daðýlým.yarý yarýya bir daðýlým söz konusu.
table(dataset$target_casco)

#hedef degiskeni factor yapalým
dataset$target_casco<-as.factor(dataset$target_casco)


#checking nulls
as.data.frame(apply(dataset,2,function(x) sum(is.na(x))))

#function to change nulls with means if type of variable is numeric
myFun_to_nulls <- function(x) {
  if (is.numeric(x)) {
    x[is.na(x)] <- mean(x, na.rm = TRUE)
    x
  } else {
    x[is.na(x)] <- names(which.max(table(x)))
    x
  }
}

dataset2<-as.data.table(dataset)

#null fonksiyonunu uygulayalým
dataset2<-dataset2[, lapply(.SD, myFun_to_nulls)]

#checking nulls again
if(sum(as.data.frame(apply(dataset2,2,function(x) sum(is.na(x)))))==0) {cat("First Null processing is done correctly")}


#outlier function
remove_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
  H <- IQR(x, na.rm = na.rm)
  y <- x
  y[x < qnt[1]-1.5*H]<-NA
  y[x > qnt[2]+1.5*H]<-NA
  y
  
}

dataset3<-as.data.frame(dataset2)


#replace outliers with NA
for (i in 3:ncol(dataset3)){
  if ((is.numeric(dataset3[,i]))==TRUE) {dataset3[,i]<-remove_outliers(dataset3[,i])}
}

#checking nulls
as.data.frame(apply(dataset3,2,function(x) sum(is.na(x))))

dataset3<-as.data.table(dataset3)

#outlierlarý temizleyelim
dataset3<-dataset3[, lapply(.SD, myFun_to_nulls)]

#checking nulls again
if(sum(as.data.frame(apply(dataset3,2,function(x) sum(is.na(x)))))==0) {cat("Second Null processing is done correctly and Outliers cleaned")}

#outlier temizliðinden sonra yaþ deðiþkenin daðýlýmý.fena gözükmüyor.
summary(dataset3$Age)

#bu degiskenlere gerek yok cikartýyorum
dataset3$First.Policy.Start.Date<-NULL
dataset3$Become.A.Customer.Date<-NULL

#########################  end of 1   ######################### 

#########################2-) Feature Engineering   ######################### 

#split data
dt = sort(sample(nrow(dataset3), nrow(dataset3)*.8))
train<-dataset3[dt,]
test<-dataset3[-dt,]



# Decide if a variable is important or not using Boruta
boruta_output <- Boruta(target_casco ~ ., data=train[,3:19], doTrace=2)  # perform Boruta search


boruta_signif <- names(boruta_output$finalDecision[boruta_output$finalDecision %in% c("Confirmed", "Tentative")])  # collect Confirmed and Tentative variables

print(boruta_signif)  # significant variables


plot(boruta_output, cex.axis=.9, las=2, xlab="", main="Variable Importance")  # plot variabl

attStats(boruta_output)

################# model #################

model<-glm(target_casco ~  Age  + Gender + MTPL.Auto.Count + Days_Cnt + policy_cnts ,family=binomial(link='logit'),data=train) 

summary(model)

#model katsayýlarýný yorumlayalým
#age
exp(0.002915)
#yaþta 1 birimlik artýþ casco satýn almaya %2 fayda saðlýyor.

#Gender Male
exp(0.174428)
#erkek musterilerin artmasý casco satýn almayý %19 arttýrýyor

#MTPL.Auto.Count
exp(-2.418902)
#bu ürüne sahip olunmasý kasko satýn almasýný %8 azaltýyor.bu enteresan bir bilgi çünkü kasko ile bu ürün
#birlikte en çok satýlan ürünler.bu urun ozelinde araþtýrma yapýlmlý.

#Days_Cnt
exp(0.049979)
#ilk müþteri olma zamanýndan ilk poliçe sahibi olma süresice geçen zaman arrtýkça kasko alma %5 artýyor
#musteri baþka ürünlerle þirketi ilk etapta deniyor olabilir.

#policy_cnts
exp(3.356338)
#en çok etki eden deðiþken,ne kadar çok farklý poliçe satarsak kasko satma olasýlýðýmýz o kadar artýyor %28


anova(model, test="Chisq")

fitted.results <- predict(model,newdata=test,type='response')

results<-ifelse(fitted.results>=0.5,1,0)

results<-as.factor(results)

#model baþarýsý %82 gayet iyi bir oran
confusionMatrix(test$target_casco, results)

results<-as.numeric(results)

roc_obj <- roc(test$target_casco, results)
auc(roc_obj)
#%80 model açýklayýcýlýðý var

plot((roc_obj))


