# Project Title:   Germany COVID-19 - Regression Analysis

# Step 1: Clean up heap memory and plots - Optimizing memory of environment
# Clear environment
rm(list = ls())
# Clear console
cat("\014")
# Clear plots
if(!is.null(dev.list())) dev.off()
#Supressing unharmful warnings
warning = FALSE
#Avoiding sceintific notation of scales in graphs and plots
options(scipen=10000)

# Step 2: Installing packages and loading libraries

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(randomForest)) install.packages("randomForest", repos = "http://cran.us.r-project.org")
if(!require(e1071)) install.packages("e1071", repos = "http://cran.us.r-project.org")
#if(!require(XQuartz)) install.packages("XQuartz", repos = "http://xquartz.macosforge.org")
if(!require(summarytools)) install.packages("summarytools", repos = "http://cran.us.r-project.org")

library(caret)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(randomForest)
library(rpart)
library(summarytools)

# Step 3: Reading the data from CSV files and saving them to dataframes

#import data from csv file to a dataframe in Global Environment
coviddf <- read.csv(file = "covid_de.csv", head = TRUE, sep=",", stringsAsFactors = TRUE)
demographicsdf <- read.csv(file = "demographics_de.csv", head = TRUE, sep=",", stringsAsFactors = TRUE)

# Step 4: First Look at data
#Describe data
nrow(coviddf)
names(coviddf)
head(coviddf)
str(coviddf)

head(demographicsdf)
names(demographicsdf)
str(demographicsdf)

#An alternate variable **CUSUM_cases** (a cumulative sum at daily level rather than total daily cases) is an effective number for better performance in modeling, regression, and forecasting using different models. Hence, this number is calculated where and when required and used as predictive variable.
#Hence, the outcome we want to predict is daily cumulative cases and the features that we will use to predict the outcome are ndays, age group and gender.

# Step 5: Data Cleaning
#Below code finds out the presence of NA values in any of the columns and deletes the corresponding rows.
#Check for na and delete rows with na values. Since the values are not cumulative, the overall numbers will be calculated with not-null values
any(is.na(coviddf))
sum(is.na(coviddf))
coviddf <- coviddf %>% na.omit()

# Step 6: Data Wrangling and reshaping of the data - aka Data Munging
#Converting data from string to date format
coviddf$date <- as.Date(coviddf$date)
#Calculating time period in number of days
#The “Date” class means dates are stored as the number of days since January 1, 1970. The as.numeric function to view the raw values. With dates stored in this fashion we can calculate number of days between two dates.

coviddf <- coviddf %>% mutate(ndays1970 = as.integer(as.ts(date)))
coviddf <- coviddf %>% mutate(ndays = (ndays1970 - min(ndays1970)))

#Summing the population to a single number at country level
demographicsdf_state_pop <- demographicsdf %>% group_by(state) %>% summarize(total_pop = sum(population))
country_pop <- sum(demographicsdf_state_pop$total_pop)

# Step 7: Data Visualization - To better understand the data in visual form, following graphs and plots are drwan
# State Level Stats and visualization
coviddf_lab_state_totals <- coviddf %>% group_by(state) %>% summarize(tot_cases = sum(cases), tot_deaths = sum(deaths), tot_recover = sum(recovered))
coviddf_lab_state_cumtotals <- coviddf_lab_state_totals %>% mutate(CUSUM_cases=cumsum(tot_cases), cum_t_d=cumsum(tot_deaths), cum_t_r=cumsum(tot_recover/10000))

ggplot(coviddf_lab_state_totals, aes(x= tot_cases, y=state)) +
  geom_bar(position="stack", stat="identity")  + ggtitle("State vs Total Cases")+ theme(
    plot.title = element_text(color="red", size=12, face="bold.italic"),
    axis.title.y = element_text(size=10, face="bold")
  )
melt_coviddf <- melt(coviddf_lab_state_totals, id.var="state")
ggplot(melt_coviddf, aes(x = state, y = value, fill = variable)) +
  geom_bar(stat = "identity") + scale_fill_manual(values=c('blue','red','green')) + geom_density(alpha=0.3) + theme(axis.text.x=element_text(angle =- 90, vjust = 0.5)) + ggtitle("State vs Total Cases/Deaths/Recoveries")+ theme(
    plot.title = element_text(color="red", size=12, face="bold.italic"),
    axis.title.y = element_text(size=10, face="bold")
  )
#Country level stats
coviddf_date_totals <- coviddf %>% group_by(ndays) %>% summarize(tot_cases=sum(cases), tot_deaths=sum(deaths), tot_recover=sum(recovered))
coviddf_date_totals <- coviddf_date_totals %>% mutate(CUSUM_cases=cumsum(tot_cases), cum_t_d=cumsum(tot_deaths), cum_t_r=cumsum(tot_recover))

plot <- ggplot(coviddf_date_totals, aes(x=ndays))
plot <- plot + geom_line(aes(y=CUSUM_cases), color="blue", alpha = 1)
plot <- plot + geom_line(aes(y=cum_t_r), color="green", alpha = 1)
plot <- plot + geom_line(aes(y=cum_t_d), color="red", alpha = 1)
plot <- plot + ggtitle("Country Level Cumulative Cases") + ylab("CUSUM_cases")
plot <- plot + theme(
  plot.title = element_text(color="red", size=12, face="bold.italic"),
  axis.title.y = element_text(size=10, face="bold")
)
plot

#Top 3 states - Case Stats Visualization
coviddf_lab_3states_days <- coviddf %>% filter(state %in% c('Nordrhein-Westfalen', 'Bayern', 'Baden-Wuerttemberg')) %>% group_by(state, ndays) %>% summarize(tot_cases = sum(cases), tot_deaths = sum(deaths), tot_recover = sum(recovered))
coviddf_lab_3states_cum <- coviddf_lab_3states_days %>% mutate(CUSUM_cases=cumsum(tot_cases), cum_t_d=cumsum(tot_deaths), cum_t_r=cumsum(tot_recover))

coviddf_lab_3states_cum %>% ggplot(aes(ndays, CUSUM_cases, group = state)) +
  geom_line(aes(linetype = state, color = state)) + ggtitle("Top 3 states - Cumulative cases") + theme(
    plot.title = element_text(color="red", size=12, face="bold.italic"),
    axis.title.y = element_text(size=10, face="bold")
  ) + geom_point(aes(color = state)) +
  theme(legend.position = "top")

#Gender Stats and Visualizations
coviddf_date_gender_totals <- coviddf %>% group_by(date, gender) %>% summarize(tot_cases=sum(cases), tot_deaths=sum(deaths), tot_recover=sum(recovered)) %>% ungroup()
coviddf_date_gender_totals <- coviddf_date_gender_totals %>% group_by(date, gender) %>% mutate(CUSUM_cases=cumsum(tot_cases), cum_t_d=cumsum(tot_deaths), cum_t_r=cumsum(tot_recover))

coviddf_date_gender_totals %>%
  ggplot(aes(date, CUSUM_cases, color=gender)) +
  geom_line(alpha = 1)  + ggtitle("Gender vs Cumulative Cases") + theme(
    plot.title = element_text(color="red", size=12, face="bold.italic"),
    axis.title.y = element_text(size=10, face="bold")
  )

#Gender and Age Stats and Visualizations
coviddf_age_gender_totals <- coviddf %>% group_by(age_group, gender) %>% summarize(tot_cases=sum(cases), tot_deaths=sum(deaths), tot_recover=sum(recovered))
coviddf_age_gender_cumtotals <- coviddf_age_gender_totals %>% mutate(CUSUM_cases=cumsum(tot_cases), cum_t_d=cumsum(tot_deaths), cum_t_r=cumsum(tot_recover))

ggplot(coviddf_age_gender_totals, aes(x = age_group, y = (tot_cases), fill = gender)) +
  geom_bar(stat = "identity", position = 'dodge') + ggtitle("Age vs Total Cases (Male/Female)") + theme(
    plot.title = element_text(color="red", size=12, face="bold.italic"),
    axis.title.y = element_text(size=10, face="bold")
  )

# Commentary on Visualization:

#Followings **observations** are made at data level and their relationship

# Three states dominate and follow the country level spread of COVID-19.
# The age group that mostly affected is 35-59.
# There is no clear indication that a particular gender is significantly affected compared to other.
# It is clear that there is a relation between ndays and cumulative cases. Cases increase as the days passby.
# But, state and country level graphs represent that the relationship is not linear.
# Moreover, the curve also represents higher level order (exponential growth and sigmoidal growth) as applicable in case of infectious diseases.
# Hence, more complicated non-linear models are needed for regressing this behavior.

# Step 6: Defining RMSE function - A function that computes the RMSE for daily cumulative cases and the ir corresponding predictors, as a measure of accuracy for linear models
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

# Step 7: **Linear Modeling:** Though above graphs visually represent that the time series data is close to non-linear relation, we deduce the same by some linear methods
#Trying a grid plot with geom_smooth feature adding a smoothing line in order to see what the
#trends look like
ggplot(coviddf_date_totals, aes(x = ndays, y = CUSUM_cases)) +
  geom_point() +
  geom_smooth(method = "glm", formula = y~x) + ggtitle("Linear Modeling") + theme(
    plot.title = element_text(color="red", size=12, face="bold.italic"),
    axis.title.y = element_text(size=10, face="bold")
  )

fit_lm <- lm(CUSUM_cases ~ ndays, data = coviddf_date_totals)
y_hat <- predict(fit_lm, coviddf_date_totals)

#Calculate RMSE and insert into a tibble
rmse_lm <- RMSE((y_hat)/country_pop*10000, (coviddf_date_totals$CUSUM_cases/country_pop)*10000)
rmse_lm
rmse_table <- tibble(Method = "Linear Model", RMSE_or_Accuracy = rmse_lm)

#We have seen that linear regression was not flexible enough to capture the non-linear nature of
#CUSUM_cases. Hence, adopting smoothing approaches may provide an improvement.
#Bin smoothing to detect trends in the presence of noisy data. A 50-day span is used to
#compute the average of the values within that span.

span <- 50
fit <- with(coviddf_date_totals,
            ksmooth(ndays, CUSUM_cases, kernel = "normal", bandwidth = span))
coviddf_date_totals %>% mutate(smooth = fit$y) %>% ggplot(aes(ndays, CUSUM_cases)) +
  geom_point(size = 2, alpha = .5, color = "blue") + geom_line(aes(ndays, smooth), color="red") + ggtitle("Bin Smoothing") + theme(
    plot.title = element_text(color="red", size=12, face="bold.italic"),
    axis.title.y = element_text(size=10, face="bold")
  )

rmse1 <- RMSE((fit$y)/country_pop*10000, (coviddf_date_totals$CUSUM_cases/country_pop)*10000)
rmse1
rmse_table <- rmse_table %>% add_row(Method = "ksmooth", RMSE_or_Accuracy = rmse1)

#Local Weighted Regression (loess), another smoothing approach, assumes that data is locally
#linear. For this purpose a 25-day span used.

span <- 25
fit <- loess(CUSUM_cases~ndays, degree=2, span = span, data=coviddf_date_totals)
coviddf_date_totals %>% mutate(smooth = fit$fitted) %>% ggplot(aes(ndays, CUSUM_cases)) +
  geom_point(size =3, alpha = .5, color = "blue") + geom_line(aes(ndays, smooth), color="red") + ggtitle("'loess' Smoothing") + theme(
    plot.title = element_text(color="red", size=12, face="bold.italic"),
    axis.title.y = element_text(size=10, face="bold")
  )

rmse2 <- RMSE((fit$fitted)/country_pop*10000, (coviddf_date_totals$CUSUM_cases/country_pop)*10000)
rmse2
rmse_table <- rmse_table %>% add_row(Method = "loess", RMSE_or_Accuracy = rmse2)

# Commentary on Linear Regression Models:

#The features used above, geom_smooth, 'lm' function, bin smoothing, loess method in linear modeling and RMSE values represent that the trend is not linear and the data can't be fit and modeled realistically using parametric (linear) methods and hence, statistical predictions can not be performed with higher accuracy.
#Hence, to progress further, we move to non-linear modeling to decrease the bias. These approaches may improve the predictive power.

# Step 8: We start with Non-Linear approaches available in linear methods like polynomial regression and knn algorithim. These methods are known to provide some flexibility for non-linear regression

#Polynomial Regression - Extending linear regression to model the curve for capturing the
#nonlinear effects using higher-ordered values of the predictor. After several trails, polynomial
#term 14 was generating a smooth curve tracing the closely the curve generated by data.

polyfit2 <- lm(CUSUM_cases ~ poly(ndays, 14), data=coviddf_date_totals)

ntime <- seq(min(coviddf_date_totals$ndays), max(coviddf_date_totals$ndays), length=225)
preds <- predict (polyfit2, newdata = list(ndays = ntime), se=TRUE)

ggplot(coviddf_date_totals, aes(ndays, CUSUM_cases)) +
  geom_point(size =3, alpha = .5, color = "blue") + geom_line(aes(ntime, preds$fit), color="red", alpha = 1) + ggtitle("Polynomial Regression (14-degree)") + theme(
  plot.title = element_text(color="red", size=12, face="bold.italic"),
  axis.title.y = element_text(size=10, face="bold")
)

rmse3 <- RMSE((preds$fit)/country_pop*10000, (coviddf_date_totals$CUSUM_cases/country_pop)*10000)
rmse3
rmse_table <- rmse_table %>% add_row(Method = "poly-14degree", RMSE_or_Accuracy = rmse3)

#Knn algorithm is similar to bin and loess smoothing, but it maximizes accuracy, or minimizes the
#expected MSE
#Splitting the data into training and testing sets using caret library

set.seed(1, sample.kind="Rounding")
test_index <- createDataPartition(y = coviddf_date_totals$CUSUM_cases, times = 1, p = 0.3, list = FALSE)
testset <- coviddf_date_totals[test_index,]
trainset <- coviddf_date_totals[-test_index,]

#Cross Validaton - Performing 10-fold cross validation on the data using knn classifier for a
#sequence of k values

train_knn <- train(CUSUM_cases ~ ., method = "knn", data = trainset)
y_hat_knn <- predict(train_knn, testset, type = "raw", na.action = na.pass)
rmse5 <- RMSE((y_hat_knn)/country_pop*10000, (testset$CUSUM_cases/country_pop)*10000)
rmse5
rmse_table <- rmse_table %>% add_row(Method = "knn(k=5)", RMSE_or_Accuracy = rmse5)

#We want to pick the k that maximizes accuracy, or minimizes the expected MSE  

control <- trainControl(method = "cv", number = 10, p = .9)
train_knn_cv <- train(CUSUM_cases ~ ., method = "knn",
                      data = trainset,
                      tuneGrid = data.frame(k = seq(9, 71, 2)),
                      trControl = control)
train_knn_cv$bestTune

#The function 'predict' will use this best performing model. Here is the accuracy of the best #model when applied to the test set, which we have not used at all yet because the cross validation #was done on the training set

y_hat_knn <- predict(train_knn_cv, testset, type = "raw", na.action = na.pass)
rmse6 <- RMSE((y_hat_knn)/country_pop*10000, (testset$CUSUM_cases/country_pop)*10000)
rmse6
rmse_table <- rmse_table %>% add_row(Method = "knn-cross_valid", RMSE_or_Accuracy = rmse6)

# Step 9: Non-Linear Models - Regression Trees

#When the outcome is continuous, we can also use a method called 'regression tree'. Regression
#trees create partitions recursively.
par(mar=c(0,0,0,0)) #setting the margins for dynamic graph
fit4 <- rpart(CUSUM_cases ~ ndays, data = coviddf_date_totals)
plot(fit4, CUSUM_cases = 10, compress = TRUE, uniform = TRUE, digits = -2)
text(fit4, pretty=0, use.n = FALSE, cex = 0.75, n = TRUE, xpd = TRUE, digits = -2)

#Non-linear Train Function - Regression Tree

coviddf_date_totals %>%
  mutate(y_hat = predict(fit4)) %>% ggplot() +
  geom_point(aes(ndays, CUSUM_cases)) + geom_step(aes(ndays, y_hat), col="red")

train_rpart <- train(CUSUM_cases ~ ndays,
                     method = "rpart",
                     tuneGrid = data.frame(cp = seq(0, 0.05, len = 25)), data = coviddf_date_totals)
par(mar=c(0,0,0,0)) #setting the margins for dynamic graph
plot(train_rpart$finalModel, CUSUM_cases = 10, branch = 1, margin = 0, minbranch = 0.1, compress = TRUE, digits = -2)
text(train_rpart$finalModel, pretty=1, use.n = FALSE, cex = 0.75, n = TRUE, xpd = TRUE)
labels(train_rpart$finalModel, minlength=0)

#Non-linear - Regression Tree:

coviddf_date_totals %>%
  mutate(y_hat = predict(train_rpart)) %>% ggplot() +
  geom_point(aes(ndays, CUSUM_cases)) + geom_step(aes(ndays, y_hat), col="red")

#Gender, age_group estimates using 'rpart' function and drawing decision trees
coviddf_ndays_age_gender_totals <- coviddf %>% group_by(ndays, age_group, gender) %>% summarize(tot_cases=sum(cases), tot_deaths=sum(deaths), tot_recover=sum(recovered))
coviddf_ndays_age_gender_cumtotals <- coviddf_ndays_age_gender_totals %>% mutate(CUSUM_cases=cumsum(tot_cases), cum_t_d=cumsum(tot_deaths), cum_t_r=cumsum(tot_recover))
coviddf_ndays_age_gender_cumtotals <- coviddf_ndays_age_gender_cumtotals[, c(-4,-5,-6)]

index <- sample(1:nrow(coviddf_ndays_age_gender_cumtotals), nrow(coviddf_ndays_age_gender_cumtotals) *0.8)
train <- coviddf_ndays_age_gender_cumtotals[index,]
test <- coviddf_ndays_age_gender_cumtotals[-index,]

fit_tree <- rpart(gender~ ., data = train)
test$pred <- predict(fit_tree, newdata = test, type="class")

#Confusion Matrix and Accuracy
cm <- confusionMatrix(test$pred, test$gender)
cm$table
cm$overall["Accuracy"]
rmse_table <- rmse_table %>% add_row(Method = "DecisionTree - Gender", RMSE_or_Accuracy = cm$overall["Accuracy"])

#It is also important to examine sensitivity and specificity and not just accuracy
cm$byClass[c("Sensitivity","Specificity", "Prevalence")] #> Sensitivity Specificity Prevalence

table(test$gender)
par(mar=c(0,0,0,0)) #setting the margins for dynamic graph
plot(fit_tree, compress = TRUE, uniform = TRUE, digits = -2)
text(fit_tree, pretty=0, use.n = FALSE, cex = 0.75, n = TRUE, xpd = TRUE, digits = -2)
fit_tree <- rpart(age_group~ ., data = train)
test$pred <- predict(fit_tree, newdata = test, type="class")

#Confusion Matrix and Accuracy
cm <- confusionMatrix(test$pred, test$age_group)
cm$table
cm$overall["Accuracy"]
rmse_table <- rmse_table %>% add_row(Method = "DecisionTree - Age", RMSE_or_Accuracy = cm$overall["Accuracy"])

#It is also important to examine sensitivity and specificity and not just accuracy
cm$byClass[c("Sensitivity","Specificity", "Prevalence")] #> Sensitivity Specificity Prevalence

table(test$age_group)
par(mar=c(0,0,0,0)) #setting the margins for dynamic graph
plot(fit_tree, compress = TRUE, uniform = TRUE, digits = -2)
text(fit_tree, pretty=0, use.n = FALSE, cex = 0.75, n = TRUE, xpd = TRUE, digits = -2)

# Step 10: Non-Linear Models - Random forests

#'Random forest' is a very popular machine learning approach. It addresses the
#short-comings of decision trees. Its goal is to improve prediction performance and reduce
#instability by averaging multiple decision trees (a forest of trees constructed with randomness).

fit <- randomForest(CUSUM_cases~ndays, data = trainset, na.action = na.omit)
y_hat <- predict(fit, testset)
testset %>% ggplot() +
  geom_point(aes(ndays, CUSUM_cases)) +
  geom_line(aes(ndays, y_hat), col="red") + ggtitle("Random Forest - Country Level") + theme(
  plot.title = element_text(color="red", size=12, face="bold.italic"),
  axis.title.y = element_text(size=10, face="bold")
)

rmse7 <- RMSE((y_hat/country_pop)*10000, (testset$CUSUM_cases/country_pop)*10000)
rmse7
rmse_table <- rmse_table %>% add_row(Method = "rforest-country", RMSE_or_Accuracy = rmse7)

#Random Forest for sum of top 3 states
coviddf_lab_3states_cum_ndays <- coviddf_lab_3states_cum %>% group_by(ndays) %>% summarize(tot_cases = sum(tot_cases), tot_deaths = sum(tot_deaths), tot_recover = sum(tot_recover), CUSUM_cases = sum(CUSUM_cases), cum_t_d = sum(cum_t_d), cum_t_r = sum(cum_t_r))

test_index <- createDataPartition(y = coviddf_lab_3states_cum_ndays$CUSUM_cases, times = 1, p = 0.3, list = FALSE)
testset <- coviddf_lab_3states_cum_ndays[test_index,]
trainset <- coviddf_lab_3states_cum_ndays[-test_index,]

fit3 <- randomForest(CUSUM_cases~ndays, data = trainset)
y_hat = predict(fit3, newdata = testset)
testset %>% ggplot() +
  geom_point(aes(ndays, CUSUM_cases)) +
  geom_line(aes(ndays, y_hat), col="red") + ggtitle("Random Forest - Top 3 States") + theme(
  plot.title = element_text(color="red", size=12, face="bold.italic"),
  axis.title.y = element_text(size=10, face="bold")
)

rmse8 <- RMSE((y_hat/country_pop)*10000, (testset$CUSUM_cases/country_pop)*10000)
rmse8
rmse_table <- rmse_table %>% add_row(Method = "rforest-top3States", RMSE_or_Accuracy = rmse8)

# Step 11: Models Performance
print(rmse_table)