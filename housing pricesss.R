# house prediction

dataset1<-read.csv('train.csv',stringsAsFactors = F,na.strings = c(""))
dataset2<-read.csv('test.csv',stringsAsFactors = F,na.strings = c(""))
dataset2$SalePrice <-NA

dataset1$IsTrainSet<-TRUE
dataset2$IsTrainSet<-FALSE
cd<-rbind(dataset1,dataset2)
str(cd)
names(cd)
#converrt char to factor excluding na's
for(col in colnames(cd)){
  if(typeof(cd[,col])=="character"){
    newcol=cd[,col]
    cd[col]=factor(newcol,exclude = "NA")
  }
  
}

str(cd)
summary(cd)
levels(cd$Exterior1st)
#check for duplicate rows
#concatinate and print
cat("Duplicate rows are", nrow(cd)-nrow(unique(cd))) #check for duplicate rows


str(cd)
#finding the missing values for all the variables
colSums(sapply(cd,is.na))




#check for outliers in numeric variables
for(col in colnames(cd)){
  if(is.numeric(cd[,col])){
    boxplot(cd[,col],las=2,cex.length=0.7,xlab=col)
  }
}
#density plot to determine skweness of the numeric variables
#for(col in colnames(cd)){
 # if(is.numeric(cd[,col])){
#    plot(density(cd[,col]),main = col)
 # }
#}
#handling skewed data
#determining sew of each numerical variable
library(e1071)
classes <- lapply(cd,function(x) class(x))
numeric_feats <- names(classes[classes=="integer" | classes=="numeric"])
skew<-sapply(numeric_feats,function(x){skewness(cd[[x]],na.rm=T)})
#determining threshhold skewness and transform all variables above threshhold
skew<-skew[skew>0.75]

# transform excessively skewed featrues with loh(x+1)
for(x in names(skew)){
  
  cd[[x]]<-log(cd[[x]]+1)
}
#handling numeric missing data
#using mice

install.packages('mice')
install.packages('VIM')
library(mice)
library(VIM)
cat_var <- names(cd)[which(sapply(cd, is.numeric))]
dataset2<-subset(cd,select=c("Id", "MSSubClass" , "LotArea"   ,  "OverallQual"  , "OverallCond" ,"YearBuilt"  , "YearRemodAdd"  ,"X1stFlrSF" ,
             "X2ndFlrSF"    , "LowQualFinSF",  "GrLivArea"  ,   "FullBath"    ,  "HalfBath"   ,   "BedroomAbvGr"  ,"KitchenAbvGr",  "TotRmsAbvGrd", 
             "Fireplaces" ,   "WoodDeckSF"  ,  "OpenPorchSF"  , "EnclosedPorch" ,"X3SsnPorch" ,   "ScreenPorch" ,  "PoolArea"   ,   "MiscVal" ,     
             "MoSold"   ,     "YrSold"    ,    "SalePrice"  ))

impute<-mice(dataset2[,1:26],m=5,seed=123)
print(impute)

colSums(sapply(dataset2,is.na))
#turns out there is no missing values in numeric variables

#handling missing values in catagorical vairable
catagorical_names<- names(cd)[which(sapply(cd,is.factor))]
catagorical_names
dataset3<- subset(cd,select = catagorical_names)
#missing values in catagorical variables
colSums(sapply(dataset3,is.na))
str(dataset3)
summary(dataset3)



#note<- for high categorical variables we will use count/frequency encoding, for nominal witl low levles we will use one hot encoding and
#for ordinal  we will rank
# convert factor to numeric
#ordinal encoding i.e rank wise encding
select_train$ExterCond2 <- as.numeric(factor(select_train$ExterCond, 
                                             levels = c("Ex", "Fa","Gd", "TA","Po"),
                                             labels = c(5,2,4,3,1) ,ordered = TRUE))
select_train$HeatingQC2 <- as.numeric(factor(select_train$HeatingQC, 
                                             levels = c("Ex", "Fa","Gd", "TA","Po"),
                                             labels = c(5,2,4,3,1) ,ordered = TRUE))
select_train$CentralAir2 <- as.numeric(factor(select_train$CentralAir, 
                                              levels = c("N", "Y"),
                                              labels = c(0,1) ,ordered = TRUE))
#for missing values we can use mice or predicting with each

# using mice to determine the missing values of catagorical variables/one hot encoding and frequency encoding\
#mszoning-Label Encoding


summary(dataset3)
dataset3<-dataset3[-c(4,52,51,50)]
colSums(sapply(dataset3, is.na))
str(dataset3)

#converting factors with more than 50 levels to integers
dataset3$LotFrontage=as.numeric(dataset3$LotFrontage)
dataset3$MasVnrArea=as.numeric(dataset3$MasVnrArea)
dataset3$BsmtFinSF1=as.numeric(dataset3$BsmtFinSF1)
dataset3$BsmtFinSF2=as.numeric(dataset3$BsmtFinSF2)
dataset3$BsmtUnfSF=as.numeric(dataset3$BsmtUnfSF)
dataset3$TotalBsmtSF=as.numeric(dataset3$TotalBsmtSF)
dataset3$BsmtUnfSF=as.numeric(dataset3$BsmtUnfSF)
dataset3$GarageArea=as.numeric(dataset3$GarageArea)
dataset3$GarageYrBlt=as.numeric(dataset3$GarageYrBlt)
imputee<-mice(dataset3,m=3,maxit=3,ridge=0.1,threshold=10,nnet.MaxNWts = 4000)
dataset3$loggedEvents
ncol(dataset3)
print(imputee)
#############################################################################################3
#subsetting dataset3 for mice computation to work faster

#not recommended as it leads to logged error which is the collinearity error
dataset31<-subset(dataset3,select = c(1:25))
impute<-mice(dataset31,m=3,maxit = 3,remove.collinear=FALSE,ridge=0.1,threshold=10)
print(impute)
impute$imp$LotFrontage
colSums(sapply(dataset31,is.na))
colSums(sapply(dataset32, is.na))
summary(dataset3)
dataset3[65,]
dataset32<-subset(dataset3,select = c(11:30))
impute2<-mice(dataset32,m=5,maxit=5)

dataset33<-subset(dataset3,select=c(31:40))
impute3<-mice(dataset33,m=5,maxit=5)

dataset34<-subset(dataset3,select=c(41:50))
impute4<-mice(dataset34,m=5,mamaxit =3,ridge=0.001,threshold=1)
impute4$loggedEvents
colSums(sapply(dataset34, is.na))
colSums(sapply(dataset3, is.na))
#########################################################################################333

#using randowm forest for training and testing
rf.cv.4<-train(Label~.,data=dataset3,method="rf",
               trControl=cv.control,tuneLength=7,
               importance=T)




#drill down the results
confusionMatrix(train.svd$Label,rf.cv.3$finalModel$predicted)

install.packages("knitr")
library(knitr)
install.packages("rmarkdown")
library(rmarkdown)



rmarkdown::render("housing pricesss.R")
























