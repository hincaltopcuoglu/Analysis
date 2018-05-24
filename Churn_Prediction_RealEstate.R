library(RJDBC)
library(data.table)
library(dplyr)
library(ggplot2)
library(xlsx)
library(outliers)
library(smbinning)
library(sqldf)

#churn scripts

options(scipen=999)

drv <- JDBC("com.microsoft.sqlserver.jdbc.SQLServerDriver",
            "C:/sqljdbc_3.0/enu/sqljdbc4.jar")


conn <- dbConnect(drv, "jdbc:sqlserver://xxxx.rds.amazonaws.com", "EmlakDataScientist", "")


df1 <- dbGetQuery(conn, "SELECT * FROM [xx].dbo.FINAL_TABLE_CHURN_29122017")


df2<-as.data.frame(df1)


df2$Have_Joker_6_Months<-as.factor(df2$Have_Joker_6_Months)

#convert target variable to factor  
df2$Is_Churned_On_Month<-as.factor(df2$Is_Churned_On_Month)


#checking nulls
as.data.frame(apply(df2,2,function(x) sum(is.na(x))))


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

df2<-as.data.table(df2)

df2<-df2[, lapply(.SD, myFun_to_nulls)]

#checking nulls again
if(sum(as.data.frame(apply(df2,2,function(x) sum(is.na(x)))))==0) {cat("First Null processing is done correctly")}



#outlier function
remove_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
  H <- IQR(x, na.rm = na.rm)
  y <- x
  y[x < qnt[1]-1.5*H]<-NA
  y[x > qnt[2]+1.5*H]<-NA
  y
  
}


df3<-as.data.frame(df2)


#replace outliers with NA
for (i in 6:ncol(df3)){
  if ((is.numeric(df3[,i]))==TRUE) {df3[,i]<-remove_outliers(df3[,i])}
}



#checking nulls
as.data.frame(apply(df3,2,function(x) sum(is.na(x))))



df3<-as.data.table(df3)

df3<-df3[, lapply(.SD, myFun_to_nulls)]

#checking nulls again
if(sum(as.data.frame(apply(df3,2,function(x) sum(is.na(x)))))==0) {cat("Second Null processing is done correctly and Outliers cleaned")}

#####################################################################  end of 1 ##################################################################### 



#################################################################### 2-) Feature Engineering #################################################################### 


train<-df3%>%filter(MonthX>=1 & MonthX<11)

test<-df3%>%filter(MonthX>=11)

#train$Joker_Timing[1]<-">12"


#######################################################


library(Boruta)
# Decide if a variable is important or not using Boruta
boruta_output <- Boruta(Is_Churned_On_Month ~ ., data=train[,3:11], doTrace=2)  # perform Boruta search


boruta_signif <- names(boruta_output$finalDecision[boruta_output$finalDecision %in% c("Confirmed", "Tentative")])  # collect Confirmed and Tentative variables

print(boruta_signif)  # significant variables


plot(boruta_output, cex.axis=.9, las=2, xlab="", main="Variable Importance")  # plot variabl

attStats(boruta_output)


############################################################## modelling ##################################################################################


modelxx<-glm(Is_Churned_On_Month ~  Have_Joker_6_Months + LoginDayBeforeChurn + Tenure  ,family=binomial(link='logit'),data=train) 

summary(modelxx)

anova(modelxx, test="Chisq")

fitted.results <- predict(modelxx,newdata=test,type='response')

results<-ifelse(fitted.results>=0.5,1,0)

library(caret)
confusionMatrix(test$Is_Churned_On_Month, results)


###########################################################################################################################################################


library(pROC)
roc_obj <- roc(test$Is_Churned_On_Month, results)
auc(roc_obj)

plot((roc_obj))


##############################################################

df_To_score <- dbGetQuery(conn, "SELECT * FROM [xx].dbo.FINAL_TABLE_CHURN_FOR_SCORING")

df_To_score$Have_Joker_6_Months<-as.factor(df_To_score$Have_Joker_6_Months)

table(df_To_score$LoginDayBeforeChurn)

scoring<-predict(modelxx,newdata=df_To_score,type='response')

scoring<-format(round(scoring,2),nsmall = 2)

results_scoring<-ifelse(scoring>=0.5,1,0)


df_scored<-cbind(df_To_score,scoring)

colnames(df_scored)[11]<-"Churn_Score"

df_scored$Scoring_Time<-'201801_Scores'


table(df_To_score$MonthX,results_scoring)



df_scored%>%arrange(desc(Churn_Score))



dbRemoveTable(conn,"[xx].[dbo].[Churn_Scores]")


conn%>%
  dbWriteTable("[xx].[dbo].[Churn_Scores]",df_scored%>%select(FirmID,MemberID,YearX,MonthX,Churn_Score,Scoring_Time))



###########################################################################################

##### analysis ####

#Joker and Tenure
ggplot(train, aes(tenure_x, ..count..)) + geom_bar(aes(fill = Is_Churned_On_Month), position = "dodge") + theme(axis.text.x = element_text(angle = 90, hjust = 1))       

options(scipen=999)

train$tenure_x<-cut(train$Tenure, seq(0,5000,500),dig.lab = 5)


#LoginDayBeforeChurn

min(train$LoginDayBeforeChurn)


options(scipen=999)

train$LoginDayBeforeChurn_x<-cut(train$LoginDayBeforeChurn, seq(-400,300,50),dig.lab = 5)


ggplot(train, aes(LoginDayBeforeChurn_x, ..count..)) + geom_bar(aes(fill = Is_Churned_On_Month), position = "dodge") + theme(axis.text.x = element_text(angle = 90, hjust = 1))     

