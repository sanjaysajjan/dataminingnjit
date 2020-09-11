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

##Populating the missing values

mydata$item_cnt_day[is.na(mydata$item_cnt_day)]<- 
  round(mean(mydata$item_cnt_day, na.rm = "TRUE"))


mydata$item_price[is.na(mydata$item_price)]<- 
  round(mean(mydata$item_price, na.rm = "TRUE"))

#mean(mydata$item_cnt_day)
#mean(mydata$item_price)

##Graphical representation of missing values

library(ggplot2)
library(naniar)
#gg_miss_upset(mydata)

##Creating new columns(new features)

mydata$daily_sales_value <- mydata$item_price * mydata$item_cnt_day
merged_all$daily_sales_value <- merged_all$item_price * merged_all$item_cnt_day

#mydata$log_daily_sales_price<- log10(mydata$daily_sales_value)

#plot.default(mydata$date_block_num, mydata$daily_sales_price)

mydata$month_number<-mydata$date_block_num %% 12

## Plotting the price and count values
library(dplyr)
monthwise_sales<- aggregate(mydata$daily_sales_value, 
                            by=list(mydata$date_block_num), FUN=sum)
shopIDwise_sales<- aggregate(mydata$daily_sales_value,
                             by=list(mydata$shop_id), FUN=sum)
itemIDwise_sales<- aggregate(mydata$daily_sales_value,
                             by=list(mydata$item_id), FUN=sum)

#plot(monthwise_sales, type= 'l', xlab= "Month",ylab= "Sales")
#plot(shopIDwise_sales, type= 'l', xlab= "Shop ID",ylab= "Sales")
#plot(itemIDwise_sales, type= 'l', xlab= "Item ID",ylab= "Sales")


#boxplot(monthwise_sales$x)
#boxplot(shopIDwise_sales$x)
#boxplot(itemIDwise_sales$x)

## Smoothing by bin means

no_of_bins<- 200000
mydata$smoothitem_monthlysales<- round(ave(mydata$item_price, 
                                           rep(1:length(mydata$item_price), 
                                               each = no_of_bins, 
                                               length.out = 
                                                 length(mydata$item_price))))

#no_of_bins<- 200000
#mydata$smoothmeadianitem_monthlysales<- round(median(mydata$item_price, 
 #                                          rep(1:length(mydata$item_price), 
  #                                             each = no_of_bins, 
   #                                            length.out = 
    #                                             length(mydata$item_price))))

#plot(mydata$smoothmeadianitem_monthlysales)
summary(mydata$daily_sales_value)

date_block_numIDwise_smoothsales<- aggregate(mydata$smoothitem_monthlysales,
                                   by=list(mydata$date_block_num), FUN=sum)

#plot(date_block_numIDwise_smoothsales, type= 'l', xlab= "Item ID",ylab= "Sales")

dbn_entiredata<- aggregate(merged_all$daily_sales_value,
                                   by=list(merged_all$date_block_num), FUN=sum)
                                             
#plot(dbn_entiredata, type = 'l', lty =1.8, col= "blue", xlab= "Month /date block number",ylab= "Sales",main= "Red : Replacing noisy data with mean   , Blue : noisy data, Green : Smoothing by bin-menas on daily_sales")
#lines(monthwise_sales, type= 'l', col= 'red')
#lines(date_block_numIDwise_smoothsales, type='l', col='green')
##Correlation

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
split_data1 <- split(mydata, mydata$date_block_num <= 3)

training_set<- data.frame(split_data["TRUE"])
oct2015<- data.frame(split_data["FALSE"])

#oct2015<- subset(mydata, split_data == FALSE)

##Model1: Linear regression

library(caTools)

split_data1<- split(training_set, training_set$TRUE.date_block_num<=3)
train_lm<- data.frame(split_data1["TRUE"])

linear_model_fulldata<- lm(training_set$TRUE.date_block_num~training_set$TRUE.daily_sales_value,
                           data = mydata)

linear_model_q1<- lm(train_lm$TRUE.TRUE.daily_sales_value~., data=train_lm)

linear_model
summary(linear_model)
summary(linear_model_fulldata)
plot(train_lm$TRUE.date_block_num, train_lm$TRUE.daily_sales_value, 
     pch = 16, col = "blue")
plot(train_lm$TRUE.shop_id, train_lm$TRUE.daily_sales_value,
     pch = 16, col = "green")

abline(linear_model)

names(oct2015)[1] <- "TRUE.item_id"
names(oct2015)[2] <- "TRUE.item_category_id"
names(oct2015)[3] <- "TRUE.shop_id"
names(oct2015)[4] <- "TRUE.date"
names(oct2015)[5] <- "TRUE.date_block_num"
names(oct2015)[6] <- "TRUE.item_price"
names(oct2015)[7] <- "TRUE.item_cnt_day"
names(oct2015)[8] <- "TRUE.daily_sales_value"
summary(oct2015)

prediction_lm<- predict(linear_model_fulldata, oct2015)
summary(prediction_lm)
prediction_lm

table_mat <- table(oct2015, prediction_lm)
accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accuracy(table_mat)

##Confidence interval
s = sd(prediction_lm)
n= length(prediction_lm)
n
sqrtn <- sqrt(n)

CI_95<-cbind(CIlower = mean(prediction_lm) - 1.96 * s / sqrtn, 
             CIupper = mean(prediction_lm) + 1.96 * s / sqrtn)

CI_95

## Model2: k- means
library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms & visualization
library(SwarmSVM)
training_KM<- transform(training_set, TRUE.date=as.numeric(TRUE.date))

test_KM<- transform(oct2015, TRUE.date=as.numeric(TRUE.date))
sapply(training_KM, class)
sapply(test_KM, class)

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

library(grid)
library(lattice)
library(modeltools)
library(stats4)
library(flexclust)

pred_test <- predict(test_KM$FALSE.daily_sales_value, kmeans3)
#pred_KM<-



#KNN 

data(training_set)
str(training_set)
summary(training_set)
knntraindata <- training_set[,-c(4)]
#data(training_set)
str(knntraindata)
#summary(knntraindata$TRUE.date_block_num)
#summary(training_set$TRUE.date_block_num)
summary(mydata)
summary(knntraindata)

#summary(mydata$month_number)
#mydata_q1 <- 
#summary(mydata_q1)
normalize <-function(x) { return((x-min(x))/(max(x) - min(x))) }  

normalize(c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33))

norm_data <- as.data.frame(lapply(knntraindata[,c(1,2,3,4,5,6,7,8)], normalize))
summary(norm_data)
#summary(knntraindata)

str(norm_data)
summary(norm_data)
#require(class)
#k = 2

#training data = norm_data
# test data = 

#knn_split_wo <- split(knntraindata, knntraindata$date_block_num <33)
wo_norm_train_knn <- data.frame(knn_split_wo["TRUE"])
wo_norm_test_knn <- data.frame(knn_split_wo["FALSE"])
#str(wo_norm_train_knn)
#str(wo_norm_test_knn)
#summary(wo_norm_train_knn)
knn_split <- split(norm_data, norm_data$TRUE.date_block_num < 1)
knn_split_q1 <- split(norm_data, norm_data$TRUE.date_block_num < 0.12121212)

train_q1<- data.frame(knn_split_q1["TRUE"])
#split_data <- split(mydata, mydata$date_block_num != 33)
str(knn_split)
norm_train_knn <- data.frame(knn_split["TRUE"])
norm_test_knn <- data.frame(knn_split["FALSE"])
summary(norm_test_knn)
summary(norm_train_knn)
str(norm_test_knn)
str(norm_train_knn)

names(norm_test_knn)[1] <- "TRUE.item_id"
names(norm_test_knn)[2] <- "TRUE.item_category_id"
names(norm_test_knn)[3] <- "TRUE.shop_id"
#names(norm_test_knn)[4] <- "TRUE.date"
names(norm_test_knn)[4] <- "TRUE.date_block_num"
names(norm_test_knn)[5] <- "TRUE.item_price"
#names(norm_test_knn)[7] <- "TRUE.item_cnt_day"
#names(norm_test_knn)[8] <- "TRUE.daily_sales_value"

train_target <- wo_norm_train_knn[,5]
str(train_target)
summary(train_target)
#summary(knn_train)
#str(knn_train)
require(class)
m1 <- knn(train = norm_train_knn, test = norm_test_knn,cl = train_target, k= 1713)

k <- sqrt(2935849)
print(k)
summary(norm_train_knn)
summary(train_target)
str(norm_train_knn)
str(train_target)
str(training_set)
summary(merged_all)
