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
