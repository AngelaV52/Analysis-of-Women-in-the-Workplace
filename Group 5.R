###### GRAPHS #####
rm(list = ls())
jobs_gender <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-03-05/jobs_gender.csv",)
jobs_gender <- jobs_gender[complete.cases(jobs_gender), ]
numerical_jg <- jobs_gender[c(1,5:12)]

# Describing the Data 
# year:	(integer)	Year
# occupation:	(character)	Specific job/career
# major_category:	(character)	Broad category of occupation
# minor_category:	(character)	Fine category of occupation
# total_workers:	(double)	Total estimated full-time workers > 16 years old
# workers_male:	(double)	Estimated MALE full-time workers > 16 years old
# workers_female:	(double)	Estimated FEMALE full-time workers > 16 years old
# percent_female:	(double)	The percent of females for specific occupation
# total_earnings:	(double)	Total estimated median earnings for full-time workers > 16 years old
# total_earnings_male:	(double)	Estimated MALE median earnings for full-time workers > 16 years old
# total_earnings_female:	(double)	Estimated FEMALE median earnings for full-time workers > 16 years old
# wage_percent_of_male:	(double)	Female wages as percent of male wages 

# Summary tables of means, max, mins, and standard deviations
summary(numerical_jg)
sapply(numerical_jg, sd, na.rm = TRUE)

# find all of categories
categories_major <- unique(jobs_gender[3]) 
categories_minor <- unique(jobs_gender[4]) 
print(categories_major)
print(categories_minor)

library(stringr)
clean_jg <- jobs_gender
clean_jg$year <- as.factor(clean_jg$year)
# changing the name of majors 
clean_jg$major_category <- (gsub(",","",clean_jg$major_category))
clean_jg$major_category <- gsub('Management Business and Financial', 'Business', clean_jg$major_category)
clean_jg$major_category <- gsub('Computer Engineering and Science', 'Science', clean_jg$major_category)
clean_jg$major_category <- gsub('Education Legal Community Service Arts and Media', 'Community Service', clean_jg$major_category)
clean_jg$major_category <- gsub('Healthcare Practitioners and Technical', 'Healthcare Practitioners', clean_jg$major_category)
clean_jg$major_category <- gsub('Natural Resources Construction and Maintenance', 'Construction', clean_jg$major_category)
clean_jg$major_category <- gsub('Production Transportation and Material Moving', 'Blue Collar', clean_jg$major_category)
categories <- unique(clean_jg[3]) 
print(categories)

# adding columun of factor: who earns more in the job -> female or male?
library(formattable)
clean_jg$earns_more_female <- (clean_jg$total_earnings_female / clean_jg$total_earnings)
clean_jg$earns_more_female <-percent(clean_jg$earns_more_female)
clean_jg$earns_more_male <- (clean_jg$total_earnings_male / clean_jg$total_earnings)
clean_jg$earns_more_male <-percent(clean_jg$earns_more_male)


library(ggplot2)
# plot 1 - Total Workers in the Dataset by Major
ggplot(clean_jg)  + 
  geom_bar(aes(major_category, workers_male),stat = "identity") + 
  geom_bar(aes(major_category, workers_female, fill = major_category),stat = "identity") +
  labs(title ="Total Workers in the Dataset by Major", x = "Major Catogory", y = "Total Workers") +
  guides(fill=FALSE) + annotate("text", x = 7.5, y = 7000^2,label = "Female = color") +
  annotate("text", x = 7.5, y = (6800^2),label = "Male = grey")

# plot 2 - Total Earnings Per Gender
ggplot(clean_jg) + 
  geom_point(aes(major_category, total_earnings_female, col = "female")) +
  geom_point(aes(major_category, total_earnings_male, col = "male"), alpha = .2) +
  labs(title ="Total Earnings Per Gender by Category", x = "Major Catogory", y = "Total Earnings") +
  labs(color = "Gender") 

# plot 3 - Who Earns More by Major
data(clean_jg, package = "reshape2")
ggplot(clean_jg, aes(x = major_category)) + 
  geom_point(aes(y = (earns_more_male),fill = major_category), stat = "identity") +
  geom_point(aes(y = (earns_more_female),fill = major_category, color = major_category, alpha = .1), 
             stat = "identity") + scale_y_continuous(labels=scales::percent) + geom_hline(yintercept = 1) +
  theme(legend.position = "none") + labs(title ="Percent Male to Female Avg/Total Earnings", 
                                         x = "Major Catogory", y = "Total Earnings")  + annotate("text", x = 7.4, y = 1.5,label = "Female = color") +
  annotate("text", x = 7.5, y = 1.45,label = "Male = black")

# plot 4 - Wage Percent of Male to Female Earnings
ggplot(clean_jg) + 
  geom_bar(aes(x = year, y = wage_percent_of_male, 
               fill = major_category), stat = "identity", position = "dodge") + 
  facet_grid(~ major_category) +
  labs(title ="Wage Percent of Male to Female Earnings", 
       x = "Year", y = "Percent Female") + 
  labs(fill = "Major Category") + 
  scale_y_continuous(labels = function(x) paste0(x, "%"))

# plot 5 - Percent Change in Female Workers Over the Years
ggplot(clean_jg) + 
  geom_bar(aes(x = year, y = percent_female, 
               fill = major_category), stat = "identity", position = "dodge") + 
  facet_grid(~ major_category) +
  labs(title ="Percent Change in Female Workers Over the Years", 
       x = "Year", y = "Percent Female") + 
  labs(fill = "Major Category") + 
  scale_y_continuous(labels = function(x) paste0(x, "%"))


#### CLEANING DATA ####
# Data transformation performed for feature engineering
jobs_gender <- jobs_gender[complete.cases(jobs_gender), ]
numerical_jg <- jobs_gender[c(1,5:12)]

library(stringr)
clean_jg <- jobs_gender
clean_jg$year <- as.factor(clean_jg$year)
# changing the name of majors 
clean_jg$major_category <- (gsub(",","",clean_jg$major_category))
clean_jg$major_category <- gsub('Management Business and Financial', 'Business', clean_jg$major_category)
clean_jg$major_category <- gsub('Computer Engineering and Science', 'Science', clean_jg$major_category)
clean_jg$major_category <- gsub('Education Legal Community Service Arts and Media', 'Community Service', clean_jg$major_category)
clean_jg$major_category <- gsub('Healthcare Practitioners and Technical', 'Healthcare Practitioners', clean_jg$major_category)
clean_jg$major_category <- gsub('Natural Resources Construction and Maintenance', 'Construction', clean_jg$major_category)
clean_jg$major_category <- gsub('Production Transportation and Material Moving', 'Blue Collar', clean_jg$major_category)
categories <- unique(clean_jg[3]) 
print(categories)

# adding columun of factor: who earns more in the job -> female or male?
library(formattable)
clean_jg$earns_more_female <- (clean_jg$total_earnings_female / clean_jg$total_earnings)

clean_jg$wage_gap <- clean_jg$total_earnings_male-clean_jg$total_earnings_female
clean_jg$gap_ratio <- clean_jg$wage_gap / clean_jg$total_earnings_female

clean_jg$woman_earn_more <- ifelse(clean_jg$wage_gap<0,1,0)

clean_jg$major_category <-  as.factor(clean_jg$major_category)
clean_jg$year <-  as.factor(clean_jg$year)
clean_jg <- clean_jg[c(-2,-4)]


########## PREDICTIONS ################
MSE <- function(p,t){mean((t-p)^2)} #predicted and true are input

  #### Percent Female Regression ####
# split into train and test
set.seed(2019)
trainSize <- 0.75
train_idx <- sample(1:nrow(clean_jg), size = floor(nrow(clean_jg) * trainSize)) 
train <- as.data.frame(clean_jg[train_idx,])
test <- as.data.frame(clean_jg[-train_idx,])

# look at the stats by major 
library(doBy)
summaryBy(. ~ major_category, data = train)

# Correlation
library(corrplot)
sapply(clean_jg, class)
cor_dataframe <- clean_jg[,c(-1,-2)]
cor <- cor(cor_dataframe)
corrplot(cor)
x <- cor[,4] # percent_female correlation
abs_x <- abs(x)
tail(sort(abs_x),8)
# top variables: earns_more_female, total_earnings, total_earnings_male, total_earnings_female, wage_gap
# variables of workers cant be used

# lasso model
library(glmnet)
library(glmnetUtils)
lasso_mod <- cv.glmnet(percent_female ~ .,
                       data = train, alpha = 1)
plot(lasso_mod)

coefs <- data.frame(
  lasso_lambda_min = as.matrix(round(coef(lasso_mod, s = "lambda.min"),3)),
  lasso_lambda_1se = as.matrix(round(coef(lasso_mod, s = "lambda.1se"),3))) 

colnames(coefs) <- c("Lasso Min","Lasso 1se")
print(coefs)

# which variables are selected:
# Lasso Min: year, major_category, wage_percent_of_male, earns_more_female, gap_ratio, woman_earn_more

# more managable set of variables no need for lasso 1se
# Lasso 1se: year, major_category, wage_percent_of_male, earns_more_female, gap_ratio, woman_earn_more

# lambda min values
lasso_mod$lambda.min 
# lambda 1se values
lasso_mod$lambda.1se

# MSE of lasso
indx <- which(lasso_mod$lambda == lasso_mod$lambda.min)
lasso_mod$cvm[indx]
# has the lowest MSE

mod_cor <- lm(percent_female ~ earns_more_female+total_earnings+total_earnings_male+total_earnings_female+wage_gap,
              data=train)
summary(mod_cor)

mod_lasso <- lm(percent_female ~ as.factor(year)+as.factor(major_category)+wage_percent_of_male+earns_more_female+
                  gap_ratio+woman_earn_more,data=train)
summary(mod_lasso)

# add prediction into the dataframe
# mod_cor
scores_train <- predict(mod_cor)
scores_test <- predict(mod_cor,newdata=test)
train$scores_train_cor <- scores_train
test$scores_test_cor <- scores_test
# mod_lasso
scores_train <- predict(mod_lasso)
scores_test <- predict(mod_lasso,newdata=test)
train$scores_train_lasso <- scores_train
test$scores_test_lasso <- scores_test

# mod_cor
MSE(train$scores_train_cor,train$percent_female)
MSE(test$scores_test_cor,test$percent_female)
# mod_lasso
MSE(train$scores_train_lasso,train$percent_female)
MSE(test$scores_test_lasso,test$percent_female)

#plot correlation predicted vs. true for train & test
library(ggplot2)
resids_train_cor <- train$percent_female - train$scores_train_cor
resids_test_cor <- test$percent_female - test$scores_test_cor

preds_df_cor <- data.frame(preds = c(train$scores_train_cor,test$scores_test_cor),
                           resids = c(resids_train_cor,resids_test_cor),
                           type = c(rep("train",nrow(train)),rep("test",nrow(test))))

ggplot(preds_df_cor, aes(x = preds, y = resids)) + geom_point(alpha = 1/10) + 
  facet_wrap(~type) + labs(x = "True Percent Female", y = "Predicted Percent Female") +
  labs(title="Correlation Model")

#plot lasso predicted vs. true for train & test
resids_train_lasso <- train$percent_female - train$scores_train_lasso
resids_test_lasso <- test$percent_female - test$scores_test_lasso

preds_df_lasso <- data.frame(preds = c(train$scores_train_lasso,test$scores_test_lasso),
                             resids = c(resids_train_lasso,resids_test_lasso),
                             type = c(rep("train",nrow(train)),rep("test",nrow(test))))

ggplot(preds_df_lasso, aes(x = preds, y = resids)) + geom_point(alpha = 1/10) + 
  facet_wrap(~type) + labs(x = "True Percent Female", y = "Predicted Percent Female") +
  labs(title="Lasso Model")

  #### Gap Ratio Regression ####
wage_data <- data.frame(clean_jg$major_category,clean_jg$gap_ratio)
library('scales')
sort_gap_ratio_DF <- wage_data[order((wage_data$clean_jg.gap_ratio)),]
sort_gap_ratio_DF$clean_jg.gap_ratio <- percent(sort_gap_ratio_DF$clean_jg.gap_ratio)
dim(sort_gap_ratio_DF)
print(sort_gap_ratio_DF[1:10,]) #the lowest wage gap
print(sort_gap_ratio_DF[1232:1242,]) #the biggest wage gap

# split into train and test
set.seed(2019)
trainSize <- 0.75
train_idx <- sample(1:nrow(clean_jg), size = floor(nrow(clean_jg) * trainSize)) 
train <- as.data.frame(clean_jg[train_idx,])
test <- as.data.frame(clean_jg[-train_idx,])

# Correlation
library(corrplot)
sapply(clean_jg, class)
cor_dataframe <- clean_jg[,c(-1,-2)]
cor <- cor(cor_dataframe)
corrplot(cor)
x <- cor[,11] # gap_ratio correlation
abs_x <- abs(x)
tail(sort(abs_x),8)
# top variables: earns_more_female, wage_gap, woman_earn_more, total_earnings_male, workers_male
# cant use wage_percent_of_male as it is the same as gap_ratio but for males

# lasso model
library(glmnet)
library(glmnetUtils)
lasso_mod <- cv.glmnet(gap_ratio ~ .,
                       data = train, alpha = 1)
plot(lasso_mod)

coefs <- data.frame(
  lasso_lambda_min = as.matrix(round(coef(lasso_mod, s = "lambda.min"),3)),
  lasso_lambda_1se = as.matrix(round(coef(lasso_mod, s = "lambda.1se"),3))) 

colnames(coefs) <- c("Lasso Min","Lasso 1se")
print(coefs)

# which variables are selected:
# Lasso Min: major_category, wage_percent_of_male, earns_more_female, woman_earn_more

# more managable set of variables no need for lasso 1se
# Lasso 1se: wage_percent_of_male, earns_more_female, woman_earn_more

# lambda min values
lasso_mod$lambda.min 
# lambda 1se values
lasso_mod$lambda.1se

# MSE of lasso
indx <- which(lasso_mod$lambda == lasso_mod$lambda.min)
lasso_mod$cvm[indx]
# has the lowest MSE

#### Gap_ratio Regression Model
mod_cor <- lm(gap_ratio ~ earns_more_female+wage_gap+woman_earn_more+total_earnings_male+workers_male,
              data=train)
summary(mod_cor)

mod_lasso <- lm(gap_ratio ~ as.factor(major_category)+wage_percent_of_male+earns_more_female+woman_earn_more,
                data=train)
summary(mod_lasso)

# add prediction into the dataframe
# mod_cor
scores_train <- predict(mod_cor)
scores_test <- predict(mod_cor,newdata=test)
train$scores_train_cor <- scores_train
test$scores_test_cor <- scores_test
# mod_lasso
scores_train <- predict(mod_lasso)
scores_test <- predict(mod_lasso,newdata=test)
train$scores_train_lasso <- scores_train
test$scores_test_lasso <- scores_test

# mod_cor
MSE(train$scores_train_cor,train$percent_female)
MSE(test$scores_test_cor,test$percent_female)
# mod_lasso
MSE(train$scores_train_lasso,train$percent_female)
MSE(test$scores_test_lasso,test$percent_female)

#plot correlation predicted vs. true for train & test
library(ggplot2)
resids_train_cor <- train$percent_female - train$scores_train_cor
resids_test_cor <- test$percent_female - test$scores_test_cor

preds_df_cor <- data.frame(preds = c(train$scores_train_cor,test$scores_test_cor),
                           resids = c(resids_train_cor,resids_test_cor),
                           type = c(rep("train",nrow(train)),rep("test",nrow(test))))

ggplot(preds_df_cor, aes(x = preds, y = resids)) + geom_point(alpha = 1/10) + 
  facet_wrap(~type) + labs(x = "True Gap Ratio", y = "Predicted Gap Ratio") +
  labs(title="Correlation Model")

#plot lasso predicted vs. true for train & test
resids_train_lasso <- train$percent_female - train$scores_train_lasso
resids_test_lasso <- test$percent_female - test$scores_test_lasso

preds_df_lasso <- data.frame(preds = c(train$scores_train_lasso,test$scores_test_lasso),
                             resids = c(resids_train_lasso,resids_test_lasso),
                             type = c(rep("train",nrow(train)),rep("test",nrow(test))))

ggplot(preds_df_lasso, aes(x = preds, y = resids)) + geom_point(alpha = 1/10) + 
  facet_wrap(~type) + labs(x = "True Gap Ratio", y = "Predicted Gap Ratio") +
  labs(title="Lasso Model")

  #### Woman Earn More Classification ####
DF_percent <- as.data.frame(summaryBy(woman_earn_more ~ major_category, data = train))
DF_percent$woman_earn_more.mean <- percent(DF_percent$woman_earn_more.mean)
print(DF_percent)

set.seed(2019)
trainSize <- 0.75
train_idx <- sample(1:nrow(clean_jg), size = floor(nrow(clean_jg) * trainSize)) 
train <- as.data.frame(clean_jg[train_idx,])
test <- as.data.frame(clean_jg[-train_idx,])

# Correlation
library(corrplot)
sapply(clean_jg, class)
cor_dataframe <- clean_jg[,c(-1,-2)]
cor <- cor(cor_dataframe)
corrplot(cor)
x <- cor[,12] # woman_earn_more correlation
abs_x <- abs(x)
tail(sort(abs_x),8)
# top variables: wage_percent_of_male, gap_ratio, earns_more_female, wage_gap, total_workers,
# total_earnings_male, workers_female

    #### Choose Variables With Lasso ####
lasso_mod <- cv.glmnet(woman_earn_more ~ .,data = train, alpha = 1)
plot(lasso_mod)

coefs <- data.frame(
  lasso_lambda_min = as.matrix(round(coef(lasso_mod, s = "lambda.min"),3)),
  lasso_lambda_1se = as.matrix(round(coef(lasso_mod, s = "lambda.1se"),3))) 

colnames(coefs) <- c("Lasso Min","Lasso 1se")
print(coefs)
# which variables are selected:
# Lasso Min: major_category,percent_female, wage_percent_of_male,earns_more_female, gap_ratio

# more managable set of variables no need for lasso 1se
# Lasso 1se: major_category, wage_percent_of_male, gap_ratio

# lambda min values
lasso_mod$lambda.min 
# lambda 1se values
lasso_mod$lambda.1se

# MSE of lasso
indx <- which(lasso_mod$lambda == lasso_mod$lambda.min)
lasso_mod$cvm[indx]
    #### ####
    # Lasso Min: major_category,percent_female, wage_percent_of_male,earns_more_female, gap_ratio

    #### Logistic Regression ####
# predicting why woman_earn_more in some work places
logit_fit_cor <- glm(woman_earn_more ~ wage_percent_of_male+gap_ratio+earns_more_female+wage_gap+
                       total_workers+total_earnings_male+workers_female, data = train, family = binomial)
summary(logit_fit_cor)
exp(logit_fit_cor$coefficients)

logit_fit_lasso <- glm(woman_earn_more ~ as.factor(major_category)+percent_female+wage_percent_of_male+earns_more_female+gap_ratio, 
                       data = train, family = binomial)
summary(logit_fit_lasso)
exp(logit_fit_lasso$coefficients)

# predict probability for the train and test 
# correlation
preds_train_cor <- data.frame(scores = predict(logit_fit_cor, newdata = train, type = "response"),train)
preds_test_cor <- data.frame(scores = predict(logit_fit_cor, newdata = test, type = "response"),test)
# lasso
preds_train_lasso <- data.frame(scores = predict(logit_fit_lasso, newdata = train, type = "response"),train)
preds_test_lasso <- data.frame(scores = predict(logit_fit_lasso, newdata = test, type = "response"),test)


# ROC Curve
library(plotROC)
# Correlation
# train
ROC_train <- ggplot(preds_train_cor, aes(m = scores, d = woman_earn_more)) + 
  geom_roc(labelsize = 3.5, cutoffs.at = c(.99,.9,.7,.5,.3,.1,0)) +
  labs(title = "ROC Plot Train")
plot(ROC_train)

# test
ROC_test <- ggplot(preds_test_cor, aes(m = scores, d = woman_earn_more)) + 
  geom_roc(labelsize = 3.5, cutoffs.at = c(.99,.9,.7,.5,.3,.1,0)) +
  labs(title = "ROC Plot Test")
plot(ROC_test)
calc_auc(ROC_train)
calc_auc(ROC_test)

# Lasso
# train
ROC_train <- ggplot(preds_train_lasso, aes(m = scores, d = woman_earn_more)) + 
  geom_roc(labelsize = 3.5, cutoffs.at = c(.99,.9,.7,.5,.3,.1,0)) +
  labs(title = "ROC Plot Train")
plot(ROC_train)

# test
ROC_test <- ggplot(preds_test_lasso, aes(m = scores, d = woman_earn_more)) + 
  geom_roc(labelsize = 3.5, cutoffs.at = c(.99,.9,.7,.5,.3,.1,0)) +
  labs(title = "ROC Plot Test")
plot(ROC_test)
calc_auc(ROC_train)
calc_auc(ROC_test)

# demonstrates that our ROC curve is great at identifying woman_earn_more with accuracy of 98.4%
# the thresholds do not affect our results
# false positive rate is lower on train cs test
# we know that the variables in this model greatly affect gow much woman earn

    #### Random Forest Tree ####
library(randomForest)
set.seed(2019)
random_forest_m3 <- randomForest(woman_earn_more ~ .,data = train, mtry = 7, 
                                 ntree = 500, importance = TRUE) 
# predict (train + test)
preds_train_bg <- predict(random_forest_m3)
preds_test_bg <- predict(random_forest_m3, newdata = test)
# MSE
MSE(preds_train_bg, train$woman_earn_more)
MSE(preds_test_bg, test$woman_earn_more)

    #### Evenning Out Data ####
library("ROSE")
table(train$woman_earn_more)

data_balanced_over <- ovun.sample(woman_earn_more ~ ., data = train, method = "over",N = 1818)$data
table(data_balanced_over$woman_earn_more)

data_balanced_both <- ovun.sample(woman_earn_more ~ ., data = train, method = "both", p=0.5,                             N=1000, seed = 1)$data
table(data_balanced_both$woman_earn_more)

# two new datasets: data_balanced_over and data_balanced_both
set.seed(2019)
random_forest_m3 <- randomForest(woman_earn_more ~ .,data = data_balanced_over, mtry = 7, 
                                 ntree = 500, importance = TRUE) 
# predict (train + test)
preds_train_bg <- predict(random_forest_m3)
preds_test_bg <- predict(random_forest_m3, newdata = test)
# MSE
MSE(preds_train_bg, train$woman_earn_more)
MSE(preds_test_bg, test$woman_earn_more)

set.seed(2019)
random_forest_m3 <- randomForest(woman_earn_more ~ .,data = data_balanced_both, mtry = 7,
                                 ntree = 500, importance = TRUE) 
# predict (train + test)
preds_train_bg <- predict(random_forest_m3)
preds_test_bg <- predict(random_forest_m3, newdata = test)
# MSE
MSE(preds_train_bg, train$woman_earn_more)
MSE(preds_test_bg, test$woman_earn_more)

varImpPlot(random_forest_m3)
plot(random_forest_m3)


#### INSIGHTS ####
# percentage of difference in wage gap by majpr
DF_percent_gap <- as.data.frame(summaryBy(gap_ratio ~ major_category, data = train))
DF_percent_gap$gap_ratio.mean <- percent(DF_percent_gap$gap_ratio.mean)
print(DF_percent_gap)

