library(caret)
cars <- read.csv("~/Downloads/Code/R Tutorial Data Sets/cars.csv")
df_working <- cars

## DATA EXPLORATION ####
str(df_working)
summary(df_working)
attributes(df_working)
head(df_working)
names(df_working) <-c("car_model" ,"speed","distance") # Rename variable's names

is.na(df_working) 
# Check if there's NA in the data
is.na(df_working) 
# na.omit()
#Drops any rows with missing values and omits them forever.
# na.exclude()
#Drops any rows with missing values, but keeps track of where they were.


# Computing Mean (AVG)
distance_mean <- mean(df_working$distance) # mean = 42.98
speed_mean <- mean(df_working$speed) # mean = 15.4

# Computing Median
distance_median <- median(df_working$distance) # median = 36
speed_median <- median(df_working$speed) # median = 15

# Correlation between the speed and the distance (Overlapped)
cor(df_working$speed, df_working$distance)  
#> [1] 0.963559



## VISUALISATIONS ####
# Histogram visualisation (data must be numeric)
hist(df_working$distance)
hist(df_working$speed) 

# Scatterplot: Visualize any linear relationships between the dependent 
# (response) variable and independent (predictor) variables
scatter.smooth(x=cars$speed, y=cars$distancce, main="Dist ~ Speed") 

# Boxplot: Check the outliers
par(mfrow=c(1, 2))  # divide graph area in 2 columns
# box plot for 'speed' 
boxplot(df_working$speed, main="Speed", sub=paste("Outlier rows: ", boxplot.stats(df_working$speed)$out)) 
# box plot for 'distance'
boxplot(df_working$distance, main="Distance", sub=paste("Outlier rows: ", boxplot.stats(df_working$distance)$out)) 

# Density plot – Check if the response variable is close to normality
library(e1071)
par(mfrow=c(1, 2))  # divide graph area in 2 columns
# density plot for 'speed'
plot(density(df_working$speed), main="Density Plot: Speed", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(df_working$speed), 2))) 
polygon(density(df_working$speed), col="red")
# density plot for 'distance'
plot(density(df_working$dist), main="Density Plot: Distance", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(df_working$dist), 2)))  
polygon(density(df_working$distance), col="red")



## APPLYING MODELS IN THE WHOLE DATA SET ####
# Linear  Model
Linear_Model <- lm(distance ~ speed, data=df_working)  
print(Linear_Model)
# Coefficients:
# (Intercept)        speed  
# -29.337            4.696 

# distance = Intercept + (β ∗ speed)
# distance = -29.337 + 4.696*speed

summary(Linear_Model)
# Call:
#   lm(formula = distance ~ speed, data = df_working)
# 
# Residuals:
#   Min     1Q Median     3Q    Max 
# -8.493 -4.472 -1.102  1.929 31.939 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -29.3371     3.0606  -9.586 9.94e-13 ***
#   speed         4.6959     0.1882  24.956  < 2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 6.965 on 48 degrees of freedom
# Multiple R-squared:  0.9284,	Adjusted R-squared:  0.927 
# F-statistic: 622.8 on 1 and 48 DF,  p-value: < 2.2e-16

# NOTE(!)
# when p Value is less than significance level (< 0.05),
# we can safely reject the null hypothesis that the co-efficient β of the predictor is zero.
# In our case, linearMod, both these p-Values are well below the 0.05 threshold,
# so we can conclude our model is indeed statistically significant.


# Computing t- Value: 
# A larger t-value indicates that it is less likely that the coefficient is not equal to zero purely by chance. 
# So, higher the t-value, the better.
model_summary <- summary(Linear_Model)  # capture model summary as an object
model_coeffs <- model_summary$coefficients  # model coefficients
beta.estimate <- model_coeffs["speed", "Estimate"]  # get beta estimate for speed
std.error <- model_coeffs["speed", "Std. Error"]  # get std.error for speed
t_value <- beta.estimate/std.error  # calc t statistic
p_value <- 2*pt(-abs(t_value), df=nrow(df_working)-ncol(df_working))  # calc p Value
f_statistic <- Linear_Model$fstatistic[1]  # fstatistic
f <- summary(Linear_Model)$fstatistic  # parameters for model p-value calc
model_p <- pf(f[1], f[2], f[3], lower=FALSE)


## PREDICTING LINEAR MODELS/ REGRESSION: DATA TRAINING ####
set.seed(123)

# Setting up my data partition: 
# trainingRowIndex <- sample(1:nrow(cars), 0.8*nrow(cars))  # row indices for training data
# trainingData <- cars[trainingRowIndex, ]  # model training data
# testData  <- cars[-trainingRowIndex, ]   # test data

train_size <-round(nrow(df_working)*0.7) # size = 35
test_size <-nrow(df_working)-train_size # size = 15

training_indices <-sample(seq_len(nrow(df_working)),size =train_size)

train_set <-df_working[training_indices,]
test_set <-df_working[-training_indices,] 

# LINEAR MODEL
LM <-lm(distance~ speed, train_set) # Model
# oefficients:
# (Intercept)    speed  
# -25.696        4.445

distPred <-predict(LM,test_set) # predict distance
summary(LM)
# Residuals:
#   Min     1Q Median     3Q    Max 
# -6.974 -4.307 -1.640  3.968 12.023 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -25.6964     2.8442  -9.035 1.93e-10 ***
#   speed         4.4447     0.1754  25.342  < 2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 5.534 on 33 degrees of freedom
# Multiple R-squared:  0.9511,	Adjusted R-squared:  0.9496 
# F-statistic: 642.2 on 1 and 33 DF,  p-value: < 2.2e-16

AIC(LM) # 223.0305

# Calculate prediction accuracy and error rates
actuals_preds <- data.frame(cbind(actuals=test_set$distance, predicteds=distPred))  # make actuals_predicteds dataframe.
correlation_accuracy <- cor(actuals_preds)  # 95.3%
head(actuals_preds)

min_max_accuracy <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max))  
# 0.8843985 => 88.4%, min_max accuracy
mape <- mean(abs((actuals_preds$predicteds - actuals_preds$actuals))/actuals_preds$actuals)  
# 0.1254156 => 12.5%, mean absolute percentage deviation


# k - Fold Cross Validation
library(DAAG)
cvResults <- suppressWarnings(CVlm(df_working, form.lm=distance ~ speed, m=5, dots=FALSE, seed=29, legend.pos="topleft",  printit=FALSE, main="Small symbols are predicted values while bigger ones are actuals."));  # performs the CV
attr(cvResults, 'ms')  # => 54.04669 mean squared error
dict(Linear_Regression,test_set)

