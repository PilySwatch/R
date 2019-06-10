# This task requires : CUSTOMER BRAND PREFERENCES
# the attachments:
# - Complete Responses: 
# This csv file contains the 10,000 completed survey responses - you'll use this file to build trained and predictive models using the caret package.
# - Survey Key: 
# This file also includes a tab that explains the survey questions and the numeric code for each of the possible answers in the survey. 
# - Survey_Incomplete: 
# This is the data set you will use to test your model. It includes 5,000 observations, but no brand df - this is what you'll be predicting.

## THE DATA ####
library(caret)
library(mlbench)

CompleteResponses <- read.csv("~/Desktop/Code/Task 1./CompleteResponses.csv")
df <- CompleteResponses

#--------------------------------------------------------------------------------------------------------#
# DATA EXPLORATION ####
str(df)
summary(df)

# Changing type 
df$brand<-as.factor(df$brand)
df$elevel<-as.factor(df$elevel)
df$car<- as.factor(df$car)
df$zipcode <- as.factor(df$zipcode)

# library(psych)
# library(PerformanceAnalytics)
# # Correlation Matrix: Check which variables are correlated to one another
# cor(df[,unlist(lapply(df, is.numeric))])
# 
# # Visualisations
# Cor_df <-pairs.panels(df)
# Cor_df_1 <-chart.Correlation(df)

# DATA TRAINING ####
# Creating data partition
set.seed(107)
inTrain <- createDataPartition(y = df$brand,p = .75,list = FALSE)
# str(inTrain)
train_set <- df[ inTrain,]
test_set  <- df[-inTrain,]

# (!!!)
# #To see which variables are the most important for our prediction
# myFit<-Pi_rfFit$finalModel
# importance(myFit)
# varImp(myFit)

#--------------------------------------------------------------------------------------------------------#
# KNN Classifier (K-Nearest Neighbors) ####
ctrl_KNN <- trainControl(method = "repeatedcv", number= 10, repeats = 2)
#Remember always that the "ctrl" might be different from method to method. So check always the names.

# tgrid <- data.frame(k= c(1, 2, 3, 4)) 
# Tuning parameter

KNN_Fit <- train(
  brand ~ age+salary,
  data = train_set,
  method = "knn",
  preProc = c("center", "scale"),
  trControl = ctrl_KNN,
  #tuneGrid = tgrid
  tuneLength = 10)
KNN_Fit

# Renaming levels of a variable (df$brand)
levels(df$brand)
levels(df$brand) <- c("acer", "sony")

# Plot: KNN Model (k= 17)
ggplot(KNN_Fit)

# Prediction:
# For classification models, the default behavior is to calculated the predicted class. 
# Using the option type = "prob" can be used to compute class probabilities from the model.
KNN_classes <-predict(KNN_Fit, newdata = test_set)
str(KNN_classes)

KNN_probs <- predict(KNN_Fit, newdata = test_set, type = "prob")
head(KNN_probs)

# Confusion Matrix and Associated Statistics for the Model fit:
confusionMatrix(data = KNN_classes, test_set$brand) # Accuracy : 92.3% - Class 0

#--------------------------------------------------------------------------------------------------------#
# DECISION TREE Classifier (DT) ####
library(rpart)
library(rpart.plot)
str(df)

DT_Fit <- rpart(
  brand ~ age+salary,
  data = train_set,
  method="class")

printcp(DT_Fit)
# Classification tree:
#   rpart(formula = brand ~ ., data = train_set, method = "class")
# 
# Variables actually used in tree construction:
#   [1] age    salary
# 
# Root node error: 2808/7424 = 0.37823
# 
# n= 7424 
# 
# CP nsplit rel error  xerror      xstd
# 1 0.096747      0   1.00000 1.00000 0.0148804
# 2 0.094373      3   0.70976 0.78953 0.0140430
# 3 0.071403      4   0.61538 0.63889 0.0131356
# 4 0.050570      6   0.47258 0.49858 0.0120030
# 5 0.028846     10   0.27030 0.29879 0.0097150
# 6 0.021724     11   0.24145 0.26745 0.0092526
# 7 0.017094     12   0.21973 0.26104 0.0091534
# 8 0.010000     13   0.20264 0.24217 0.0088511

# Plot
rpart.plot(DT_Fit, extra = 106)
plotcp(DT_Fit) 
 
plot(DT_Fit, uniform=TRUE, 
     main="Decision Tree Graph")
text(DT_Fit, use.n=TRUE, all=TRUE, cex=.8)

# Make Prediction
DT_prediction <-predict(DT_Fit, test_set, type = 'class')
table_mat <- table(test_set$brand, DT_prediction)
table_mat
# DT_prediction
#      0    1
# 0  824  112
# 1   88 1450

# The model correctly predicted 824 brand = 0 but classified only 112. 
# By analogy, the model misclassified 88 brand = 1 while they turned out to be 0. 

accuracy_Test <- sum(diag(table_mat)) / sum(table_mat) # 0.9191593 

#--------------------------------------------------------------------------------------------------------#
# RANDOM FOREST ####
# mtry :: tuning parameter
library(randomForest)
ctrl_RF <- trainControl(method = "repeatedcv", repeats = 2)

RF_Fit<-train(brand ~ salary + age,
              method="rf", 
              data=train_set, 
              importance=TRUE,
              trControl = ctrl_RF) 
RF_Fit

# Make prediction
RF_Pred <- predict(RF_Fit, newdata = test_set)

postResample(pred = RF_Pred, obs = test_set$brand)
# Accuracy     Kappa 
# 0.9090542 0.8061338 

# C50 Classifier ####
library(C50)
library(plyr)
# trials :: tuning parameter

ctrl_C50 <- trainControl(method = "repeatedcv",number= 10, repeats = 2)

C50_Fit <-train(brand ~ salary + age,
              method="C5.0", 
              data=train_set, 
              importance=TRUE,
              trControl = ctrl_C50) 
C50_Fit


C50_classes <-predict(C50_Fit, newdata = test_set)
str(C50_classes)

C50_probs <- predict(C50_Fit, newdata = test_set, type = "prob")
head(C50_probs)

# Confusion Matrix and Associated Statistics for the Model fit:
confusionMatrix(data = C50_classes, test_set$brand) # Accuracy : 92% - Class 0

#--------------------------------------------------------------------------------------------------------#
# APPLY THE MODEL INTO THE NEW DATA SET::: KNN ####
SurveyIncomplete <- read.csv("~/Desktop/Code/Task 1./SurveyIncomplete.csv")
df_final <- SurveyIncomplete

df_final$brand<-as.factor(df_final$brand)
df_final$elevel<-as.factor(df_final$elevel)
df_final$car<- as.factor(df_final$car)
df_final$zipcode <- as.factor(df_final$zipcode)

str(df_final)

Pred_Brand <- predict(KNN_Fit, newdata = df_final) 
head(Pred_Brand)

df_final$brand <- Pred_Brand
df_final

# Brand Prediction Plot
plot(KNN_classes, test_set$brand)

# Barplot
levels(df_final$brand) <- c("acer", "sony")
ggplot(data=df_final)+geom_bar(aes(brand, fill=brand))

# Boxplot
ggplot(data= df_final)+ geom_boxplot(aes(x=brand, y=salary))

# Scatter plots: x, y, color
library(viridis)
ggplot()+geom_point(data=df_final, aes(x=age, y=salary, color=brand))
ggplot()+geom_point(data=df_final, aes(x=age, y=salary, color=brand)) + scale_color_manual(values = c("pink", "green"))

#ggplot()+geom_point(data=df_final, aes(x=age, y=salary, color=brand)) + viridis::scale_color_viridis("brand")
#ggplot()+geom_point(data=df_final, aes(x=age, y=salary, color=brand)) + scale_color_gradient(colors=rainbow(2))

