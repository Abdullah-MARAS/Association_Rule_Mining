#Getting Data From csv file
getwd() #GET DIRECTORY
setwd("C:/Users/TEST/Desktop/Staj/basket analyse data") #SET DIRECTORY
options(max.print=10000000) 
CustData<-read.csv("RFMData.csv",sep=";",header=TRUE,stringsAsFactors = FALSE,na.strings='NULL') #READ DATA BY SEMICOLON
Category<-read.csv("products2.csv",sep=";",header=TRUE,stringsAsFactors = FALSE) #READ DATA BY SEMICOLON
SubCategory<-read.csv("Subcategories.csv",sep=";",header=TRUE,stringsAsFactors = FALSE) #READ DATA BY SEMICOLON
ListSubCategory<-read.csv("listofsubcategories.csv",sep=";",header=TRUE,stringsAsFactors = FALSE) #READ DATA BY SEMICOLON

#Delete empty Columns
ListSubCategory$X<-NULL
SubCategory$X<-NULL
Category$X<-NULL

#Cleaning Null Values and create a new variable CustData2
CustData1 <- data.frame(CustData$CustomerID,CustData$OfferID)
names(CustData1) <- c("CustomerID", "OfferID")
names(Category)<-c("OfferID","CATEGORY")
CustData2<-CustData1[!is.na(CustData1$CustomerID), ]
CustData2<-CustData2[!is.na(CustData2$OfferID), ]

#Combining SubCategory and List SubCategory/When you combine two data there are sum NULL Value
install.packages("dplyr")
library(dplyr)
CSubCategory<-left_join(SubCategory, ListSubCategory, by = c("S_ID"="ID"))

#Create new Data without SUbcategory ID

CSubCategory2 <-data.frame(CSubCategory$D_ID,CSubCategory$NAME)

#Finding Labels Name

LabelDefine<-left_join(CustData, CSubCategory2, by = c("OfferID"="CSubCategory.D_ID"))

#Cleaning Data

CleanLabelDefine<-subset(LabelDefine, CSubCategory.NAME!= "")

# Combine products in One Line 

CleanLabelDefine2<-aggregate(CSubCategory.NAME ~ CustomerID, data = CleanLabelDefine, paste, collapse = ",")
CleanLabelDefine2<-CleanLabelDefine2$CSubCategory.NAME
CleanLabelDefine2<-CleanLabelDefine2[-1]

write.csv(CleanLabelDefine2, file = "CleanLabelDefine2new.csv",row.names=FALSE,quote=FALSE)

#Data Analyse(Association Rule Mining)

library(arules)

#Read Data

Groc<-read.transactions("CleanLabelDefine2new.csv",sep=",")

summary(Groc)
inspect(Groc[15:20])
itemFrequency(Groc[,1:14])
itemFrequencyPlot(Groc,support=0.10)
itemFrequencyPlot(Groc,topN=3)
#m1<-apriori(Groc)
#m1
m2<-apriori(Groc, parameter = list(support=0.001,confidence=0.7,minlen=2))
m2

summary(m2)
inspect(m2)
#Lift'e göre yukarýdan aþaðýya sýralama yaptýk
inspect(sort(m2,by="lift"))
sel <- plot(m2, measure=c("support", "lift"), shading="confidence", interactive=TRUE)

plot(m2, method="graph")









