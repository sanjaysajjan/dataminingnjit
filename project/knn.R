#KNN 
#mydata<- mydata[-c(4)]
data(training_set)
str(training_set)
summary(training_set)
knntraindata <- mydata[,-c(2,4,9,10)]

summary(oct2015)
accuracy_test <- oct2015[,8]
str(accuracy_test)
summary(accuracy_test)
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

#normalize(c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33))

norm_data <- as.data.frame(lapply(knntraindata[,], normalize))
summary(norm_data)
#summary(knntraindata)

#str(norm_data)
#summary(norm_data)

#ran <- sample(1:nrow(mydata), 0.9 * nrow(mydata)) 



#require(class)
#k = 2

#training data = norm_data
# test data = 

knn_split<- split(norm_data, norm_data$date_block_num !=1 )
norm_train_knn <- data.frame(knn_split["TRUE"])
norm_test_knn <- data.frame(knn_split["FALSE"])
#str(wo_norm_train_knn)
#str(wo_norm_test_knn)

#normtestknn <- norm_data[]

summary(norm_train_knn)
summary(norm_test_knn)


#knn_split_q1 <- split(norm_data,norm_data$date_block_num < 0.12121212)

knn_split_m1<- split(norm_data, norm_data$date_block_num == 0)
train_m1<- data.frame(knn_split_m1["TRUE"])
summary(train_m1)

#train_m1 <- train_m1[-c(4)]
summary(train_m1)

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
names(norm_test_knn)[6] <- "TRUE.item_cnt_day"
names(norm_test_knn)[7] <- "TRUE.daily_sales_value"

#train_target <- train_m1[,7]
summary(training_set)
train_target <- training_set[,8]

#test_target <- train_m1[,7]

str(train_target)
summary(train_target)
#str(test_target)
#summary(test_target)

#summary(knn_train)
#str(knn_train)
require(class)

m1 <- knn(train = norm_train_knn, test = norm_test_knn, cl = train_target, k= 341)

knn_pred <-  knn(norm_train_knn, norm_test_knn,training_set[,8],k= 341)

summary(m1)
str(m1)
#plot(m1)

library(gmodels)

CrossTable(x = accuracy_test, y = m1, prop.chisq = FALSE)

tab <- table(m1,train_target)

summary(tab)

#predict(m1,oct2015)

accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accuracy(tab)



#rmse

sqrt(mean(m1- norm_test_knn$FALSE.daily_sales_value)^2)

library(KODAMA)
library(caTools)
prediction_knn<- predict(train_m1, norm_test_knn, train_target, k=341, agg.meth="majority")


k <- sqrt(115690)
print(k)
summary(norm_train_knn)
summary(train_target)
str(norm_train_knn)
str(train_target)
str(training_set)
summary(merged_all)

library(ISLR)
library(caret)

trControl = trainControl(method = "repeatedcv", number = 10, repeats = 3)
set.seed(222)
fit <- train(train_m1$TRUE.daily_sales_value ~ .,
             data = train_m1, 
             method= 'knn', tuneLength = 20,
             trControl = trControl,
             preProc = c("center","scale"))
#head(train_m1)
#fit

