#removing all objects stored
rm(list=ls())
#set working directory
setwd("C:/Users/Nitish Rohilla/Desktop/other/Employee Absentism")
#installing all the necessary libraries
x=c("ggplot2","ggplot","DMwR","rpart","xlsx","gbm","corrgram","caret","mlbench","pROC","mlr","dummies","unbalanced","MASS","DataCombine","ROSE","Information","randomForest","e1071")
install.packages(x)
lapply(x,require,character.only =TRUE)
#clearing the space
rm(x)
#Reading the data from directory
dt=read.xlsx('Absenteeism_at_work_Project.xls', sheetIndex=1)
#+++++++++++++++++++++++++++++++++++++++++++++performing exploratory analysis
#Knowing the structure of the data
str(dt)
#we can see that some of reason of absence variable contains zero values, we are putting them equal to category 25(i.e. unjustified absence).
dt$Reason.for.absence[dt$Reason.for.absence==0] = 25
#lookin at the bottom of mothn.of.absence variable contains zero 
#imputing values using the all observation present in same variable, in relation with season
#imputing months that might fall in relation with season
dt[740,3]=5
dt[738,3]=7
dt[739,3]=3
#As we can see from structure there are two kinds of variables present
#categorising the variables from the above EDA
#continious Variables
var_cont=c('Distance.from.Residence.to.Work','Service.time','Age','Work.load.Average.day.','Transportation.expense','Hit.target','Weight','Height','Body.mass.index','Absenteeism.time.in.hours')
#categorical Variables
var_categorical=c('ID','Reason.for.absence','Month.of.absence','Day.of.the.week','seasons','Disciplinary.failure','Education','Social.drinker','Social.smoker','Son','Pet')
#t=dt
#---------------Pre-processing----------------------------------------------
#---------------------Missing Value Analysis--------------------------------
#Missing values count in each variable
colSums(is.na(dt))
#dataframe creation for missing values percentage for analysis
missed_val=data.frame(apply(dt,2, function(x){sum(is.na(x))}))
missed_val$Variables=row.names(missed_val)
names(missed_val)[1]="Missed_val_percentage"
#Missing value percentage calculation
missed_val$Missed_val_percentage=(missed_val$Missed_val_percentage/nrow(dt))*100
#sorting missed_val in descending order
missed_val=missed_val[order(-missed_val$Missed_val_percentage),]
#making dataframe more logical and re arranging columns
row.names(missed_val)=NULL
missed_val=missed_val[,c(2,1)]
#plotting bar graph for visually analysing the missing percentage
ggplot(data=missed_val[1:18,], aes(x=reorder(Variables,-Missed_val_percentage),y=Missed_val_percentage))+ geom_bar(stat="identity",fill="grey")+xlab("Data vriables")+ggtitle("missed value percentage per variable")+theme_bw()
#*******************************selecting the best  suited method for imputaion************************
#----Manually testing each of them---------
#dt$Age[2]=NA
#actual Value = 50
#mean=36.4
#median=37
#KNN=50 (closest so, this method suits the best)
#---Mean Method
#dt$Age[is.na(df$Age)]=mean(df$Age,na.rm=T)
#--Median Method
#dt$Age[is.na(df$Age)]=median(df$Age,na.rm=T)
#---Knn Method
dt=knnImputation(dt, k=3)
#Check if all missing value imputed
colSums(is.na(dt))
#****************************OUTLIER ANALYSIS********************************
#outlier check for the continious variables in the data set
for(i in 1:length(var_cont))
{
  assign(paste0("gn",i),ggplot(aes_string(y=(var_cont[i]),x="Absenteeism.time.in.hours"),data=subset(dt))+
           stat_boxplot(geom="errorbar",width=0.5)+
           geom_boxplot(outlier.colour="blue",fill="grey",outlier.shape=18,outlier.size=2,notch=FALSE)+
    theme(legend.position="bottom")+
      labs(y=var_cont[i],x="Absenteesim time in hours")+
      ggtitle(paste("Boxplot for Absenteeism hours for",var_cont[i])))
  
}
#plotting together all the plots generated
gridExtra::grid.arrange(gn1,gn2,gn3,ncol=3)
gridExtra::grid.arrange(gn4,gn5,gn6,ncol=3)
gridExtra::grid.arrange(gn7,gn8,gn9,gn10,ncol=4)
#since we have a less number of observation, inspite of deleting the ouliers we are taking an alternate solution
#Replacing all the outliers with NA and then performing the imputation
for (i in var_cont) {
  obs=dt[,i][dt[,i]%in%boxplot.stats(dt[,i])$out]
  dt[,i][dt[,i]%in% obs]=NA
}
#Now performing the imputation
dt=knnImputation(dt,k=3)
#***********************************************FEATURE SELECTION***********************
#Analyzing continious variables through the Correlation Plot
corrgram(dt[,var_cont],order=F,upper.panel=panel.pie, text.panel=panel.txt,main="correlation plot")
cor(dt[,1:21])
#Performing ANOVA Test for all the categorical variables for analyzing the selection
summary(aov(formula=Absenteeism.time.in.hours~ID,data=dt))
summary(aov(formula=Absenteeism.time.in.hours~Reason.for.absence,data=dt))
summary(aov(formula=Absenteeism.time.in.hours~Month.of.absence,data=dt))
summary(aov(formula=Absenteeism.time.in.hours~Day.of.the.week,data=dt))
summary(aov(formula=Absenteeism.time.in.hours~Seasons,data=dt))
summary(aov(formula=Absenteeism.time.in.hours~Disciplinary.failure,data=dt))
summary(aov(formula=Absenteeism.time.in.hours~Education,data=dt))
summary(aov(formula=Absenteeism.time.in.hours~Social.drinker,data=dt))
summary(aov(formula=Absenteeism.time.in.hours~Social.smoker,data=dt))
summary(aov(formula=Absenteeism.time.in.hours~Son,data=dt))
summary(aov(formula=Absenteeism.time.in.hours~Pet,data=dt))

#As we can see the p-value of many variables is more than 0.05, so these variables are independent of Absenteeism.time.in.hours variable
#Reduction od dimension
dt=subset(dt,select=-c(Weight,Month.of.absence,Day.of.the.week,Pet,Social.smoker,Seasons,Education))
#Updating the variables
#continious Variables
var_cont=c('Distance.from.Residence.to.Work','Service.time','Age','Work.load.Average.day.','Transportation.expense','Hit.target','Height','Body.mass.index','Absenteeism.time.in.hours')
#categorical Variables
var_categorical=c('ID','Reason.for.absence','Disciplinary.failure','Social.drinker','Son')

#********************************************FEATURE SCALING*******************************
#Performing the normality check of variable height
hist(dt$Height)
#Performing normalization
for(i in var_cont)
{
  print(i)
  dt[,i]=(dt[,i]-min(dt[,i]))/(max(dt[,i])-min(dt[,i]))
}
#Creation of dummy variables for categorical variables to trick the regression algorithms into correctly analyzing attributes variables
#as regression analysis treats all independent variables in the analysis as numerical
dt=dummy.data.frame(dt,var_categorical)
#***************************************DEVELOPING MODEL********************************
#Cleaning the environment,as RAM is limited
rmExcept("dt")
set.seed(123)
#Sampling to divide data into test and train
tr.index= sample(1:nrow(dt),0.8*nrow(dt))
Train=dt[tr.index,]
Test=dt[-tr.index,]
#++++++++++++Performing the Principal Component Analysis to reduce the dimension of the data+++++++++++++++++++
#As creating dummies for the categorical variables has increased the numbers of variables on the data set, We need to extract low dimensional set of variables
#which can give as much information
#to achieve this, we need to perform PCA
#analyzing each component
an_cmp=prcomp(Train)
#calculation of SD and Var of each component
sd_cmp=an_cmp$sdev
var_cmp=sd_cmp^2
#visually analyzing the cumulative proportion of varinace with principal components
p_var_cmp=var_cmp/sum(var_cmp)
plot(cumsum(p_var_cmp),xlab="principal components",ylab = "Explained cumulative proportion of variance",type="b")
#Addition of Principal Components with training data
upd_train=data.frame(Absenteeism.time.in.hours=Train$Absenteeism.time.in.hours,an_cmp$x)
#looking at the plot above of variance, we can conclude that the 60 components out of all could explain more than 92% of data varience
#selection 60 components
upd_train=upd_train[,1:60]
#now converting the test data into pca and selecting same number of varaibes
upd_test=predict(an_cmp,newdata=Test)
upd_test=as.data.frame(upd_test)
upd_test=upd_test[,1:60] 
#********************Linear Regression***********************
#Developing model on the training data
lr_fit=lm(Absenteeism.time.in.hours~.,data = upd_train)
#prdicting for the test data
pr_LR=predict(lr_fit,upd_test)
#Analyzing the model performance
print(postResample(pred=pr_LR,obs=Test$Absenteeism.time.in.hours))
#       RMSE    Rsquared         MAE 
#0.002772912 0.999773858 0.001070085 
#as this model is more accurate,we can form dataframe of predicted values
#new_predicted=as.data.frame(tt[593:740,3])
#new_predicted$absentism.hours.predicted=pr_LR
#colnames(new_predicted)[1]="months"
#write.xlsx(h,"predicted_test_values.xls",row.names = F)
#***************************************************decision Tree*************************
#developing model on the training data
#using method-anova as it is a regression model/dependent is continious
dt_fit=rpart(Absenteeism.time.in.hours~.,data=upd_train,method = "anova")
#----------Predicting----------------
pr_DT=predict(dt_fit,upd_test)
#Analyzing the model performance
print(postResample(pred=pr_DT,obs=Test$Absenteeism.time.in.hours))
# RMSE  Rsquared       MAE 
#0.1469967 0.4646631 0.0964528 
#****************************************RANDOM FOREST*************************
rf_fit=randomForest(Absenteeism.time.in.hours~.,data=upd_train)
#----------Predicting----------------
pr_RF=predict(rf_fit,upd_test)
#Analyzing the model performance
print(postResample(pred=pr_RF,obs=Test$Absenteeism.time.in.hours))
# RMSE   Rsquared        MAE 
#0.08414573 0.81593701 0.05601834 
#********************************K Nearest Neighbour***************
#first creating trainControl method to controll the nuances of the train() method created by caret package
tracontrol=trainControl(method='repeatedcv',
                        number=10,
                        repeats=3)
#using train() method of caret package for training the data for knn algorithms
model=caret::train(Absenteeism.time.in.hours~.,data=upd_train,method="knn",tuneGrid=expand.grid(k=1:70),metric="Rsquared",trControl=tracontrol)
#Predicting
pr=predict(model,newdata=upd_test)
#analyzing the performance of the model
RMSE(pr,Test$Absenteeism.time.in.hours)
#RMSE- 0.1237135
