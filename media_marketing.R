library(ggplot2)
library(GGally)
library(knitr)
library(mgcv)
library(rstanarm)
library(coda) 
library(bayesplot) 
# Read the data from a CSV file
data <- read.csv("weekly_media_sample.csv")


#summaries 
summary(data)

#pairs plot to view corelations of variables
ggpairs(data[c(-2)])

####################1) Classic Modelling################################
############ 1)a   ##################################################
#start and end dates of training data
start_date <- as.Date("2012-08-06")
end_date <- as.Date("2016-05-30") 
train_data <- data[data$DATE >= start_date & data$DATE <= end_date, ]
# Fit the Generalised linear regression model
model <- glm(revenue ~ media1_S + media2_S + media3_S + competitor_sales + newsletter, 
             data = train_data, family = gaussian(link = "identity"))


# Get the coefficients and their p-values
coefficients <- coef(model)
p_values <- summary(model)$coefficients[, 4] 


# Print the summary of the model
summary(model)

#plotting residuals and Q-Q plots for the model
par(mfrow=c(2,2),pch=20)
plot(model)  



########################### 1b) Prediction ############################
#start and end dates for prediction
start_date <- as.Date("2016-06-06")
end_date <- as.Date("2016-07-25") 
#test dataset with revenues values
test_data <- data[data$DATE >= start_date & data$DATE <= end_date, ]

#test data with only independent variables
test <- data.frame(media1_S = test_data$media1_S, 
                   media2_S = test_data$media2_S,
                   media3_S = test_data$media3_S, 
                   competitor_sales= test_data$competitor_sales,
                   newsletter = test_data$newsletter) 

#predictions for GLM model
prediction <- predict(model, newdata = test, type = 'response', se.fit=TRUE) 
#95% CI for GLM model
lower <- prediction$fit - 1.96*prediction$se.fit
upper <- prediction$fit + 1.96*prediction$se.fit 


#Table with date, Actual and predicted values using GLM
pred_table <- data.frame(Date=test_data$DATE, Actual = test_data$revenue, Prediction = prediction$fit) 
kable(pred_table, caption = 'Actual and predcited values of Test data')  


#Plot to visualize the actual and predicted values along with 95%CI
ggplot() +
  geom_line(aes(x = test_data$X, y = prediction$fit, color = "Predictions")) +
  geom_line(aes(x = test_data$X, y = test_data$revenue, color = "Actual values")) +
  geom_line(aes(x = test_data$X, y = lower), linetype = "dashed", color = "95%CI") +
  geom_line(aes(x = test_data$X, y = upper), linetype = "dashed", color = "95%CI")+
  xlab("Week number") +
  ylab("Revenue") +
  ggtitle("Actual and Predicted values with 95% Confidence Interval") +
  scale_color_manual(values = c("Predictions" = "red", "Actual values" = "blue", "95%CI" = 'orange')) 


#root mean squred error
rmse<-(sqrt(mean(test_data$revenue - prediction$fit)^2))
rmse 

#r-squared vales
total <- sum((test_data$revenue-mean(test_data$revenue))^2)
residual <- sum((prediction$fit - test_data$revenue)^2) 
r_squared<- 1 - residual/total
r_squared  


####### 1c) Temporal Effects ####################################
#plot for checking seasonality in data
ggplot(data)+
  geom_line(aes(x= X, y= revenue))+ xlab("Week number")+ylab("Revenue")+
  ggtitle("Week Number vs Revenue") 


# Fit a GAM time-series model
gam_model <- gam(revenue ~ s(X, k = 50, bs="cs") + s(media1_S) + s(media2_S) + s(media3_S) +
                   s(competitor_sales) + s(newsletter),
                 data = train_data)

# Print the summary of the model
gam.check(gam_model)

#test data for GAM model
test <- cbind(X= test_data$X, test)
#prediction for GAM model
gam_predictions <- predict(gam_model, newdata = test, type = "response", se.fit = TRUE)

# 95% CI for GAM
gam_lower <- gam_predictions$fit - 1.96*gam_predictions$se
gam_upper <- gam_predictions$fit + 1.96*gam_predictions$se 

#Actual and Predicted values with 95% Confidence Interval for GAM
ggplot() +
  geom_line(aes(x = test_data$X, y = gam_predictions$fit, color = "Predictions")) +
  geom_line(aes(x = test_data$X, y = test_data$revenue, color = "Actual values")) +
  geom_line(aes(x = test_data$X, y = gam_lower), linetype = "dashed", color = "95%CI") +
  geom_line(aes(x = test_data$X, y = gam_upper), linetype = "dashed", color = "95%CI")+
  xlab("Week number") +
  ylab("Revenue") +
  ggtitle("Actual and Predicted values with 95% Confidence Interval using Temporal") +
  scale_color_manual(values = c("Predictions" = "red", "Actual values" = "blue", "95%CI" = 'orange'))


#RMSE for GAM model
gam_rmse<-(sqrt(mean(test_data$revenue - gam_predictions$fit)^2))
gam_rmse 

#R-squared for GAM model
total <- sum((test_data$revenue-mean(test_data$revenue))^2)
residual <- sum((gam_predictions$fit - test_data$revenue)^2) 
gam_r_squared <- 1 - residual/total
gam_r_squared 



######################## 2) Bayesians modelling #################################


# Set non-informative priors
prior_intercept <- normal(location = 0, scale = 1000)
prior <- cauchy(location = 0, scale = 1000)
#formula for model
formula<- revenue ~ media1_S + media2_S + media3_S + competitor_sales + newsletter
# Fit the MCMC glm model 
mc_model <- stan_glm(formula,
                     data = train_data, family = gaussian(), 
                     prior_intercept = prior_intercept,
                     prior = prior, 
                     chains = 4, iter = 2000, warmup = 500, seed = 123) 

#trace plots
mcmc_trace(mc_model, regex_pars = c("media","competitor",'news', 'Intercept'))
#ACF plots
mcmc_acf_bar(mc_model, regex_pars = c("media","competitor",'news', 'Intercept'))

#summary
summary(mc_model) 

#####################2b) Prior choice ####################################
#changing prior for media3_S
prior_media3 <- normal(location = 0, scale = 1)

#fitting Bayesian GLM with new prior for media3_S
mc_model2 <- stan_glm(formula,
                      data = train_data,family = gaussian(),
                      prior_intercept = prior_intercept,
                      prior = c(prior, prior_media3),
                      chains = 4,iter = 2000,warmup = 500,
                      seed = 123) 

#summary for new model
kable(summary(mc_model2),digits=3) 




#new priors
prior_intercept <- normal(location = 0, scale = 1000)
#new prior for media1
prior_media1_S <- normal(location = 2, scale = 0.5)
#new prior for media2
prior_media2_S <- normal(location = 4 * prior_media1_S$location, scale = 4 * prior_media1_S$scale)
#new prior for media3
prior_media3_S <- normal(location = 0, scale = 1)
#new prior for newsletter
prior_newsletter <- exponential(rate = 1)  # Strictly positive relationship
#new prior for competitor sales
prior_competitor_sales <- function() {
  sample <- rnorm(1, mean = 0.15, sd = 0.05)
  if (sample < 0 || sample > 0.3) sample_competitor_sales() else sample
}
#formula
formula <- revenue ~ media1_S + media2_S + media3_S + competitor_sales + newsletter
#fitting model with new prior for co-efficients of predictors
mc_model3 <- stan_glm(formula, data = train_data,
                      family = gaussian(),prior_intercept = prior_intercept,
                      prior = c(
                        prior_media1_S,
                        prior_media2_S,
                        prior_media3_S,
                        prior_competitor_sales,
                        prior_newsletter
                      ),
                      chains = 4,iter = 2000,warmup = 500,seed = 123
)

#trace plots for model3
mcmc_trace(mc_model3, regex_pars = c("media","competitor",'news', 'Intercept'))

#summary of model 3
kable(summary(mc_model3), caption="Summary of MCMC model with new priors",digits = 3) 