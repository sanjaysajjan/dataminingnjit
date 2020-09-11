## Load the data
items_category<- read.csv("item_categories.csv")
items<- read.csv("items.csv")
shops<- read.csv("shops.csv")
sales<- read.csv("sales_train_v2.csv")

## Merge the files
merged_items= merge(items, items_category)
merged_salesshop = merge(shops, sales)
merged_all = merge(merged_items, merged_salesshop)

summary(merged_all)
str(merged_all)

#names(merged_all)[1] <- "item_id"

## Delete columns which have no impact on modeling
mydata<- merged_all[-c(3:4,6)]
str(mydata)

## Missing values

mydata$item_cnt_day[mydata$item_cnt_day < 0] <- NA
mydata$item_price[mydata$item_price < 0] <- NA

#sum(is.na(training_set$item_cnt_day))
#sum(is.na(training_set$item_price))
table(is.na(mydata$item_cnt_day))

summary(mydata)

##Graphical representation for missing values

library(ggplot2)
library(naniar)
gg_miss_upset(mydata)

##Populating the missing values

mydata$item_cnt_day[is.na(mydata$item_cnt_day)]<- 
  round(mean(mydata$item_cnt_day, na.rm = "TRUE"))

mydata$item_price[is.na(mydata$item_price)]<- 
  round(mean(mydata$item_price, na.rm = "TRUE"))

#mean(mydata$item_cnt_day)
#mean(mydata$item_price)


##Creating new columns(new features)

mydata$daily_sales_value <- mydata$item_price * mydata$item_cnt_day

#mydata$log_daily_sales_price<- log10(mydata$daily_sales_value)

#plot(mydata$date_block_num, mydata$daily_sales_price,  type ='l')

#plot.default(mydata$date_block_num, mydata$daily_sales_price,  type ='l')

#scatter.smooth(mydata$date_block_num, mydata$daily_sales_price)

#ggplot2.scatterplot()

#?boxplot()
library(car)

#scatterplot(mydata$date_block_num,mydata$daily_sales_value)

mydata$month_number<-mydata$date_block_num %% 12

## Plotting the price and count values
library(dplyr)
monthwise_sales<- aggregate(mydata$daily_sales_value, 
                            by=list(mydata$date_block_num), FUN=sum)
shopIDwise_sales<- aggregate(mydata$daily_sales_value,
                             by=list(mydata$shop_id), FUN=sum)
itemIDwise_sales<- aggregate(mydata$daily_sales_value,
                             by=list(mydata$item_id), FUN=sum)
plot(monthwise_sales, type= 'l', xlab= "Month",ylab= "Sales")
plot(shopIDwise_sales, type= 'l', xlab= "Shop ID",ylab= "Sales")
plot(itemIDwise_sales, type= 'l', xlab= "Item ID",ylab= "Sales")

boxplot(monthwise_sales$x)

## Smoothing by min means

no_of_bins<- 200000
mydata$smoothitem_monthlysales<- round(ave(mydata$item_price, 
                                           rep(1:length(mydata$item_price), 
                                               each = no_of_bins, 
                                               length.out = 
                                                 length(mydata$item_price))))

summary(mydata$daily_sales_value)

##Feature selection

str(mydata)
library(lattice)
library(ggplot2)
library(mlbench)
library(caret)
# load the data
data(mydata)
# calculate correlation matrix

correlationMatrix<- cor(mydata$item_cnt_day, 
                        mydata$daily_sales_value)
correlationMatrix1<- cor(mydata$item_price, 
                         mydata$daily_sales_value)

print(correlationMatrix)
print(correlationMatrix1)

## Splitting the merged_all into training and test dataset

split_data <- split(mydata, mydata$date_block_num != 33)
split_data1<- split(mydata, mydata$date_block_num < 4)

summary(split_data)

training_set<- data.frame(split_data["TRUE"])
oct2015<- data.frame(split_data["FALSE"])

##Model1: Linear regression
train_lm<- data.frame(split_data1["TRUE"])
linear_model<- lm(train_lm$TRUE.daily_sales_value~., 
                  data=train_lm)



#print(linear_model)
linear_model
summary(linear_model)

#training_set$TRUE.date_block_num = 1.415e+01 + 6.303e-05 * training_set$TRUE.daily_sales_values

#modelsummary <- summary(linear_model)
#modelcoefficients <- modelsummary$coefficients
#betaestimate <- modelcoefficients["training_set$TRUE.daily_sales_values","Estimate"]

plot(training_set$TRUE.date_block_num, training_set$TRUE.daily_sales_value, 
     pch = 16, col = "blue")

#abline(1.415e+01,6.303e-05)
#abline(linear_model)

prediction_lm<- predict(linear_model, oct2015)
summary(prediction_lm)
prediction_lm

table_mat <- table(oct2015, prediction_lm)


linear_model_smooth<-lm(training_set$date_block_num ~ 
                          training_set$smoothitems_monthlysales, 
                        data=cars)
linear_model_smooth


## Model2: k- means
library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms & visualization
#kmeans2 <- kmeans(training_set, centers = 2, nstart = 25)
#-kmeans3 <- kmeans(training_set, centers = 3, nstart = 25)
#kmeans4 <- kmeans(training_set, centers = 4, nstart = 25)
#kmeans5 <- kmeans(training_set, centers = 5, nstart = 25)

# plots to compare
#p1 <- fviz_cluster(kmeans2, geom = "point",  data = training_set) + ggtitle("k = 2")
#p2 <- fviz_cluster(kmeans3, geom = "point",  data = training_set) + ggtitle("k = 3")
#p3 <- fviz_cluster(kmeans4, geom = "point",  data = training_set) + ggtitle("k = 4")
#p4 <- fviz_cluster(kmeans5, geom = "point",  data = training_set) + ggtitle("k = 5")

training_KM<- transform(training_set,
                        TRUE.date=as.numeric(TRUE.date)
)

sapply(training_KM, class)

kmeans2 <- kmeans(training_KM, centers = 2, nstart = 25)
kmeans3 <- kmeans(training_KM, centers = 3, nstart = 25)
kmeans4 <- kmeans(training_KM, centers = 4, nstart = 25)
kmeans5 <- kmeans(training_KM, centers = 5, nstart = 25)

# plots to compare
p1 <- fviz_cluster(kmeans2, geom = "point",  data = training_KM) + ggtitle("k = 2")
p2 <- fviz_cluster(kmeans3, geom = "point",  data = training_KM) + ggtitle("k = 3")
p3 <- fviz_cluster(kmeans4, geom = "point",  data = training_KM) + ggtitle("k = 4")
p4 <- fviz_cluster(kmeans5, geom = "point",  data = training_KM) + ggtitle("k = 5")
library(gridExtra)
grid.arrange(p1, p2, p3, p4, nrow = 2)

## Model3: Naive bayes 
library(naivebayes)
training_NB<- transform(training_set,
                        TRUE.item_id=as.factor(TRUE.item_id),
                        TRUE.item_category_id=as.factor(TRUE.item_category_id),
                        TRUE.shop_id=as.factor(TRUE.shop_id),
                        TRUE.date_block_num=as.factor(TRUE.date_block_num),
                        TRUE.item_price=as.factor(TRUE.item_price),
                        TRUE.item_cnt_day=as.factor(TRUE.item_cnt_day),
                        TRUE.daily_sales_value=as.factor(TRUE.daily_sales_value),
                        TRUE.month_number=as.factor(TRUE.month_number),
                        TRUE.smoothitem_monthlysales=as.factor(TRUE.smoothitem_monthlysales))
str(training_NB)

model_NB<- naive_bayes(TRUE.daily_sales_value ~ TRUE.date_block_num+ 
                         TRUE.item_price+
                         TRUE.item_id+TRUE.shop_id+
                         TRUE.item_cnt_day,
                       data = training_NB)

model_NB <- naive_bayes(training_set$TRUE.daily_sales_value ~ , 
                        data = training_set)
plot(model_NB)

str(training_set)
predict_NB<- predict(model_NB, test)

table_mat_NB <- table(test$G3, predict_NB)

#Model4: Random Forest
library(caTools)
library(randomForest)
require(caTools)

summary(training_set$TRUE.month_number)
summary(training_set$TRUE.date_block_num)
summary(mydata)

training_set_Q1 <- split(training_set, training_set$TRUE.date_block_num < 4)
training_set_Q1_TRUE<- data.frame(training_set_Q1["TRUE"])
training_set_Q1_False<- data.frame(training_set_Q1["FALSE"])
summary(training_set_Q1_TRUE$TRUE.TRUE.date_block_num)
summary(training_set_Q1_False$FALSE.TRUE.date_block_num)

summary(training_set)
summary(mydata)
rmtrain <- training_set[-c(4)]
rmtrain <- rmtrain[-c(9)]
rmtrain <- rmtrain[-c(8)]

summary(rmtrain)

rmtrain_Q1 <- split(rmtrain, rmtrain$TRUE.date_block_num < 4)
rmtrain_Q1_TRUE<- data.frame(rmtrain_Q1["TRUE"])
rmtrain_Q1_False<- data.frame(rmtrain_Q1["FALSE"])
summary(rmtrain_Q1_TRUE)

sapply(rmtrain_Q1_TRUE, class)

randFor <- randomForest(
  TRUE.TRUE.daily_sales_value ~ .,
  data=rmtrain_Q1_TRUE
)


summary(oct2015)
names(oct2015)[1] <- "TRUE.item_id"
names(oct2015)[2] <- "TRUE.item_category_id"
names(oct2015)[3] <- "TRUE.shop_id"
names(oct2015)[4] <- "TRUE.date"
names(oct2015)[5] <- "TRUE.date_block_num"
names(oct2015)[6] <- "TRUE.item_price"
names(oct2015)[7] <- "TRUE.item_cnt_day"
names(oct2015)[8] <- "TRUE.daily_sales_value"
summary(oct2015)

