options(java.parameters = "- Xmx2048m")
options(scipen=999)
##############################################
library(RJDBC)
library(data.table)
library(dplyr)
library(ggplot2)
library(outliers)
library(sqldf)
library(stringr)
library(tidyr)
library(splitstackshape)
library(DMwR)
library(ca)
library(VIM)
library(randomForest)
library(caTools)
library(purrr)
library(broom)
##############################################
#connect to database and get the data
drv <- JDBC("com.microsoft.sqlserver.jdbc.SQLServerDriver",
            "C:/sqljdbc_3.0/enu/sqljdbc4.jar")


conn <- dbConnect(drv, "jdbc:sqlserver://rds.amazonaws.com" )


df1 <- dbGetQuery(conn, "SELECT * FROM dbo.House_Sale_Prediction")


df2<-as.data.frame(df1)

df2$AdvertID<-NULL

##############################################

#filter data to the extent Price and M2
df3<-df2%>%
  filter(CalculatedStartingPrice>=50000 & CalculatedStartingPrice<=1600000)%>%
  filter(M2AdvertPropertyValue>=45 & M2AdvertPropertyValue<=300)

##############################################
 
# take dummied lokasyon variables together them with price
m1<-cbind(df3$CalculatedStartingPrice,df3[,18:28])

#turn into factor
for(i in 2:12){
  m1[,i]<-as.factor(as.character(m1[,i]))
  
}

# name them as target columns
target_columns<-colnames(m1[2:12])

# melt data frame
m2<-melt(m1,id=target_columns)


# this scores are produced with regard to analysis of each dummy variable with price variable like observing distributions of box plots,taking mean of each variable regard to price
# then by taking each mean price ,conducting 0-1 normalization
lokasyon_df<-data.frame(deger=c(0.209806583,
                                0.236359009,
                                1,
                                0.234899898,
                                0.470240923,
                                0.369706481,
                                0.47587377,
                                0.833975229,
                                0,
                                0.805581948,
                                0.28564642
                        )
)

#make it data frame
m2<-as.data.frame(m2)

#turn dummy variables into numeric
for(i in 1:11){
  m2[,i]<-as.numeric(as.character(m2[,i]))
  
}

# multiply each variable with normalized scores which found above
for(i in 1:11){
  m2[,i]<-m2[,i]*lokasyon_df[i,1]
  
}

# sum them by row
m2$Row_Sum_Score<-rowSums(m2[,1:11])

# add last found score to main data frame.Now it is correlated wih dependent price variable
df3$Lokasyon_Score<-m2$Row_Sum_Score

############################################

# take dummied isinma variables together them with price
i1<-cbind(df3$CalculatedStartingPrice,df3[,29:42])

#turn into factor
for(i in 2:15){
  i1[,i]<-as.factor(as.character(i1[,i]))
  
}

colnames(i1)[3]<-"i_yok"

# name them as target columns
target_columns<-colnames(i1[2:15])

# melt data frame
i2<-melt(i1,id=target_columns)



# this scores are produced with regard to analysis of each dummy variable with price variable like observing distributions of box plots,taking mean of each variable regard to price
# then by taking each mean price ,conducting 0-1 normalization
isinma_df<-data.frame(deger=c(0.185761869,
                              0.188622834,
                              1,
                              0.624088186,
                              0.404847346,
                              0.294777887,
                              0.450259003,
                              0.317718655,
                              0.458641061,
                              0.499513068,
                              0.846144519,
                              0.155958575,
                              0,
                              0.742721592))

#make it data frame
i2<-as.data.frame(i2)

#turn dummy variables into numeric
for(i in 1:14){
  i2[,i]<-as.numeric(as.character(i2[,i]))
  
}

# multiply each variable with normalized scores which found above
for(i in 1:14){
  i2[,i]<-i2[,i]*isinma_df[i,1]
  
}

# sum them by row
i2$Row_Sum_Score<-rowSums(i2[,1:14])

# add last found score to main data frame.Now it is correlated wih dependent price variable
df3$Isinma_Score<-i2$Row_Sum_Score

############################################

# take dummied cevre variables together them with price
c1<-cbind(df3$CalculatedStartingPrice,df3[,43:67])

#turn into factor
for(i in 2:26){
  c1[,i]<-as.factor(as.character(c1[,i]))
  
}

# name them as target columns
target_columns<-colnames(c1[2:26])

# melt data frame
c2<-melt(c1,id=target_columns)

# for (i in 1:26){
#   c2%>%group_by(c2[,i])%>%summarise(mean(value))%>%print()
#   print(i)
# }



# this scores are produced with regard to analysis of each dummy variable with price variable like observing distributions of box plots,taking mean of each variable regard to price
# then by taking each mean price ,conducting 0-1 normalization
cevre_df<-data.frame(deger=c(0.93850416,
                             0.144186894,
                             0.323852525,
                             0.344165804,
                             0,
                             0.266708846,
                             1,
                             0.413503202,
                             0.089382262,
                             0.502406151,
                             0.164912382,
                             0.805303117,
                             0.245868323,
                             0.091644618,
                             0.074686529,
                             0.082959469,
                             0.093389317,
                             0.461482419,
                             0.628868055,
                             0.347444304,
                             0.600090111,
                             0.344319184,
                             0.946029372,
                             0.218796733,
                             0.45489666))

#make it data frame
c2<-as.data.frame(c2)

#turn dummy variables into numeric
for(i in 1:25){
  c2[,i]<-as.numeric(as.character(c2[,i]))
  
}

# multiply each variable with normalized scores which found above
for(i in 1:25){
  c2[,i]<-c2[,i]*cevre_df[i,1]
  
}

# sum them by row
c2$Row_Sum_Score<-rowSums(c2[,1:25])

# add last found score to main data frame.Now it is correlated wih dependent price variable
df3$Cevre_Score<-c2$Row_Sum_Score


#################################################################################################

# now calculate mean price for each district
df3<-as.data.frame(df3)

final.filtered<-df3%>%
  select(CityID,TownID,DistrictID,CalculatedStartingPrice)%>%
  group_by(CityID,TownID,DistrictID)%>%
  summarise(ort_fiyat=mean(CalculatedStartingPrice,na.rm=TRUE))

#merge new variable
merged_data_frame <- inner_join(df3,final.filtered,by=c("CityID","TownID","DistrictID"))


### cluster ort_fiyat ###

set.seed(123)

cluster_ort_fiyat<-kmeans(merged_data_frame$ort_fiyat,9,nstart = 20)

clustered_ort_fiyat<-cbind(merged_data_frame,cluster_ort_fiyat$cluster)

colnames(clustered_ort_fiyat)[87]<-"Ort_Fiyat_Cluster"

clustered_ort_fiyat$Ort_Fiyat_Cluster<-as.factor(clustered_ort_fiyat$Ort_Fiyat_Cluster)

####################################

#cluster price and distance

df4<-clustered_ort_fiyat

r1<-as.data.frame(cbind(df4$CalculatedStartingPrice,df4$House_Town_Distance))

colnames(r1)<-c("CalculatedStartingPrice","House_Town_Distance")

r1 <- na.omit(r1)

set.seed(123)
cluster_distance<-kmeans(r1,8,nstart = 20)

clustered_distance<-cbind(df4,cluster_distance$cluster)

colnames(clustered_distance)[88]<-"Distance_Cluster"

clustered_distance$Distance_Cluster<-as.factor(clustered_distance$Distance_Cluster)

clustered_distance<-clustered_distance%>%mutate(RowNumbers=rownames(.))

###############
# name it to df5
df5<-clustered_distance

#function to null values
myFun_to_nulls <- function(x) {
  if (is.numeric(x)) {
    x[is.na(x)] <- mean(x, na.rm = TRUE)
    x
  } else {
    x[is.na(x)] <- names(which.max(table(x)))
    x
  }
}


# before applying function turn data frame to data.table
df5<-as.data.table(df5)
# apply null function
df5<-df5[, lapply(.SD, myFun_to_nulls)]
#turn into data.frame again
df5<-as.data.frame(df5)


##########################################################################
#split into train and test
set.seed(123) 
sample = sample.split(df5$CalculatedStartingPrice, SplitRatio = .8)
train = subset(df5, sample == TRUE)
test  = subset(df5, sample == FALSE)

##########################################################################
#train model with Random Forest
forest_result<-randomForest(log(CalculatedStartingPrice) ~ M2AdvertPropertyValue + Banyo_Sayisi + Bina_Yasi + Ort_Fiyat_Cluster +
                              Binadaki_Kat_Sayisi + Bulundugu_Kat +  Oda_Sayisi + Lokasyon_Score + Isinma_Score + Cevre_Score + Distance_Cluster , data=train,ntree=20)


#predict on test set
pred3 <- predict(forest_result, newdata = test)
#calculate rmse
rmse <- sqrt(sum((exp(pred3) - test$CalculatedStartingPrice)^2)/length(test$CalculatedStartingPrice))
#calculate average r square
print(mean(forest_result$rsq))
#mean rsquare 0.94 with deviance 24976


#########################################################################



production_function<-
  function(City,
           Town,
           District,
           M2AdvertPropertyValue,
           Banyo_Sayisi,
           Bina_Yasi,
           Binadaki_Kat_Sayisi,
           Bulundugu_Kat,
           Oda_Sayisi,
           Is_On_Street=0,
           Is_Near_To_Street=0,
           Is_Near_To_Sea=0,
           Is_Cephe_To_E5=0,
           Is_Near_To_E5=0,
           Is_Near_To_Airport=0,
           Is_Near_To_Metrobus=0,
           Is_Near_To_Metro=0,
           Is_Near_To_Otogar=0,
           Is_Near_To_Sahil=0,
           Is_Near_To_TT=0,
           Is_Gunes=0,
           i_yok=0,
           Is_Jeotermal=0,
           Is_Kalorifer_A=0,
           Is_Kalorifer_D=0,
           Is_Kalorifer_K=0,
           Is_KKA=0,
           Is_Klima=0,
           Is_Kombi_D=0,
           Is_Kombi_E=0,
           Is_Merkezi=0,
           Is_Soba_D=0,
           Is_Soba_K=0,
           Is_Yerden=0,
           Is_SwimPool=0,
           Is_ShoppingMall=0,
           Is_Garden=0,
           Is_BogazManzarasi=0,
           Is_Camii=0,
           Is_MountainView=0,
           Is_SeaView=0,
           Is_in_Nature=0,
           Is_Pharmacy=0,
           Is_LakeView=0,
           Is_Hospital=0,
           Is_ClosedSwimPool=0,
           Is_Kres=0,
           Is_Market=0,
           Is_School=0,
           Is_Park=0,
           Is_Police=0,
           Is_Cinema=0,
           Is_in_Site=0,
           Is_SportSaloon=0,
           Is_Sport=0,
           Is_CityView=0,
           Is_Tennis=0,
           Is_University=0,
           Is_Vadi=0,
           distance
           ){
    
    ####
    location_df<-data.frame(cbind(Is_On_Street,
                                  Is_Near_To_Street,
                                  Is_Near_To_Sea,
                                  Is_Cephe_To_E5,
                                  Is_Near_To_E5,
                                  Is_Near_To_Airport,
                                  Is_Near_To_Metrobus,
                                  Is_Near_To_Metro,
                                  Is_Near_To_Otogar,
                                  Is_Near_To_Sahil,
                                  Is_Near_To_TT))
    
  
   for(i in 1:11){
     location_df[,i]<-location_df[,i]*lokasyon_df[i,1]
    }
    
   location_df$Lokasyon_Score<-rowSums(location_df[,1:11])
    
  ####
   isinmax_df<-data.frame(cbind(Is_Gunes,
                                i_yok,
                                Is_Jeotermal,
                                Is_Kalorifer_A,
                                Is_Kalorifer_D,
                                Is_Kalorifer_K,
                                Is_KKA,
                                Is_Klima,
                                Is_Kombi_D,
                                Is_Kombi_E,
                                Is_Merkezi,
                                Is_Soba_D,
                                Is_Soba_K,
                                Is_Yerden))
    
   for(i in 1:14){
     isinmax_df[,i]<-isinmax_df[,i]*isinma_df[i,1]
     
   }
   
   
   isinmax_df$Isinma_Score<-rowSums(isinmax_df[,1:14])
   
   ####
   
   cevrex_df<-data.frame(cbind(Is_SwimPool,
                               Is_ShoppingMall,
                               Is_Garden,
                               Is_BogazManzarasi,
                               Is_Camii,
                               Is_MountainView,
                               Is_SeaView,
                               Is_in_Nature,
                               Is_Pharmacy,
                               Is_LakeView,
                               Is_Hospital,
                               Is_ClosedSwimPool,
                               Is_Kres,
                               Is_Market,
                               Is_School,
                               Is_Park,
                               Is_Police,
                               Is_Cinema,
                               Is_in_Site,
                               Is_SportSaloon,
                               Is_Sport,
                               Is_CityView,
                               Is_Tennis,
                               Is_University,
                               Is_Vadi))
                               
   for(i in 1:25){
     cevrex_df[,i]<-cevrex_df[,i]*cevre_df[i,1]
     
   }
   
   cevrex_df$Cevre_Score<-rowSums(cevrex_df[,1:25])
   ####
   
   
   new_data<-data.frame(cbind(M2AdvertPropertyValue,
                              Banyo_Sayisi,
                                      Bina_Yasi,
                                      Binadaki_Kat_Sayisi,
                                      Bulundugu_Kat,
                                      Oda_Sayisi,
                                      location_df$Lokasyon_Score,
                                      isinmax_df$Isinma_Score,
                                      cevrex_df$Cevre_Score
                                      ))
   
   
   colnames(new_data)[7]<-"Lokasyon_Score"
   colnames(new_data)[8]<-"Isinma_Score"
   colnames(new_data)[9]<-"Cevre_Score"
   
   
   Ort_Fiyat_Cluster<-df5%>%filter(CityID==City)%>%filter(TownID==Town)%>%filter(DistrictID==District)%>%select(Ort_Fiyat_Cluster)%>%distinct()
   
   Distance_Cluster<-clustered_distance%>%
     filter(CityID==City & TownID==Town & Banyo_Sayisi==Banyo_Sayisi & Oda_Sayisi==Oda_Sayisi)%>%
     group_by(RowNumbers)%>%
     mutate(euc=sqrt(sum(House_Town_Distance-distance)^2))%>%
     ungroup()%>%
     filter(euc==min(euc))%>%
     as.data.frame()%>%
     select(Distance_Cluster)
   
   new_data2<-cbind(new_data,Ort_Fiyat_Cluster,Distance_Cluster)
   
  
   #return(new_data2)
   x<-predict(forest_result, newdata = new_data2)
   return(exp(x))
    
}

production_function(City=2,Town=36,District=70981,M2AdvertPropertyValue=145,Banyo_Sayisi=1,Bina_Yasi=18,Binadaki_Kat_Sayisi=6,Bulundugu_Kat=3,Oda_Sayisi=3,
                    1,
                    1,
                    0,
                    0,
                    0,
                    0,
                    0,
                    0,
                    0,
                    0,
                    1,
                    0,
                    0,
                    0,
                    0,
                    0,
                    0,
                    0,
                    0,
                    0,
                    0,
                    1,
                    0,
                    0,
                    0,
                    0,
                    0,
                    1,
                    0,
                    0,
                    0,
                    0,
                    1,
                    0,
                    0,
                    1,
                    1,
                    1,
                    1,
                    1,
                    0,
                    0,
                    1,
                    1,
                    0,
                    1,
                    1,
                    1,
                    0,
                    distance = 20.47)





