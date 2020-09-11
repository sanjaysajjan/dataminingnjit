## Splitting the merged_all into training and test dataset

split_data<- split(mydata, mydata$date_block_num!=33)
split_data1 <- split(mydata, mydata$date_block_num <= 3)

train_Q1<- data.frame(split_data1["TRUE"])
oct2015_lm<- data.frame(split_data["FALSE"])

train_Q1<- train_Q1[-c(4)]
oct2015_lm<- oct2015_lm[-c(4)]
names(oct2015_lm)[1] <- "TRUE.item_id"
names(oct2015_lm)[2] <- "TRUE.item_category_id"
names(oct2015_lm)[3] <- "TRUE.shop_id"
names(oct2015_lm)[4] <- "TRUE.date_block_num"
names(oct2015_lm)[5] <- "TRUE.item_price"
names(oct2015_lm)[6] <- "TRUE.item_cnt_day"
names(oct2015_lm)[7] <- "TRUE.daily_sales_value"
names(oct2015_lm)[8] <- "TRUE.smoothitem_monthlysales"
summary(oct2015_lm)

##Model1: Linear regression

linear_model<- lm(TRUE.smoothitem_monthlysales~., 
                  data=train_Q1)
str(train_Q1)

linear_model
summary(linear_model)

prediction_lm<- predict(linear_model, oct2015_lm)
summary(prediction_lm)
prediction_lm

plot(oct2015_lm$TRUE.smoothitem_monthlysales, type = 'l', lty =1.8, col= "blue")
lines(prediction_lm, type= 'l', lty = 1.8, col= 'red')

## MSE

rmse_lm<- sqrt(mean(prediction_lm - oct2015_lm$TRUE.smoothitem_monthlysales)^2)
rmse_lm

mse_lm<- (rmse_lm)^2
mse_lm

##Confidence interval
s = sd(prediction_lm)
n= length(prediction_lm)
n
sqrtn <- sqrt(n)

CI_95<-cbind(CIlower = mean(prediction_lm) - 1.96 * s / sqrtn, 
      CIupper = mean(prediction_lm) + 1.96 * s / sqrtn)

CI_95


CI_80<-cbind(CIlower = mean(prediction_lm) - 1.28 * s / sqrtn, 
             CIupper = mean(prediction_lm) + 1.28 * s / sqrtn)

CI_80


CI_90<-cbind(CIlower = mean(prediction_lm) - 1.64 * s / sqrtn, 
             CIupper = mean(prediction_lm) + 1.64 * s / sqrtn)

CI_90

predict(prediction_lm, Interval = 'confidence', level = 0.80)



