##Model1: Linear regression

train_Q1_wo<- train_Q1[-c(8)]
oct2015_lm_wo<- oct2015_lm[-c(8)]

linear_model_wo<- lm(TRUE.daily_sales_value~., 
                  data=train_Q1_wo)
str(train_Q1)

linear_model_wo
summary(linear_model_wo)

prediction_lm_wo<- predict(linear_model_wo, oct2015_lm_wo)
summary(prediction_lm)
prediction_lm

plot(oct2015_lm_wo$TRUE.daily_sales_value, type = 'l', lty =1.8, col= "blue")
lines(prediction_lm_wo, type= 'l', lty = 1.8, col= 'red')

## MSE
rmse_lm_wo<- sqrt(mean(prediction_lm_wo - oct2015_lm_wo$TRUE.daily_sales_value)^2)
rmse_lm_wo

mse_lm_wo<- (rmse_lm_wo)^2
mse_lm_wo

##Confidence Interval
s_wo = sd(prediction_lm_wo)
n_wo= length(prediction_lm_wo)
n_wo
sqrtn_wo <- sqrt(n_wo)

CI_95_wo<-cbind(CIlower = mean(prediction_lm_wo) - 1.96 * s_wo / sqrtn_wo, 
             CIupper = mean(prediction_lm_wo) + 1.96 * s_wo / sqrtn_wo)

CI_95_wo
