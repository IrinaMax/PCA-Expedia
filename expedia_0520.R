##My work with Expedia dataset
## load data  Kaggle Expedia

setwd("/Users/irinamahmudjanova/Desktop")
### Kaggle Expedia
ex <-read.csv("~/Desktop/Expedia/train.csv", header = T, sep=",", stringsAsFactors = F)
require("plyr")
require("dplyr")
require("xgboost")
require("reshape2")
require("caret")
ex <-read.csv("~/Desktop/Expedia/train.csv", header = T, sep=",", stringsAsFactors = F)
ex1 <- ex[ sample(1:nrow(ex), 1000, replace = FALSE)]
ex2 <- ex[ sample(1:nrow(ex), 1000, replace = FALSE)]

dim(ex1)
ex1
str(ex1)
names(ex1)


hotel_name_idx <- which(names(ex)=='srch_destination_id')
hotel_name_idx <- which(names(ex1)=='srch_destination_id')
names(ex1)[hotel_name_idx] <- "dest_id"

...
...

user_row_cnt <- group_by(ex1, user_id) %>% summarise(rowcnt=n(), cntsum=sum(cnt)) %>% as.data.frame
hist(user_row_cnt[,2], breaks=100)
hist(user_row_cnt[,3], breaks=100)

# 7.9% of rows are bookings
nrow(h[h$is_booking==1,])/nrow(h)

# 6.8% of users book
length(unique((h[h$is_booking==1,c("user_id")])))/usercnt

length(unique(h$hotel_id))
length(unique(paste(h$user_id, h$hotel_id, sep=",")))
length(unique(paste(h$user_id, h$hotel_id, h$is_mobile, sep=",")))

unique(h$hotel_country)

hotel_country_cnt <- group_by(h, hotel_country) %>% summarise(rowcnt=n()) %>% as.data.frame
hist(hotel_country_cnt[,2], breaks=300)


source("../common.R")
require("plyr")
require("dplyr")
require("xgboost")
require("reshape2")
require("caret")

header.names <- read.csv("header.csv", header=F, sep=",", stringsAsFactors = F)
header.names
test <- read.csv("~/Desktop/Expedia/test.csv", header = T, sep=",", stringsAsFactors = F)
##t <- fread("~/Desktop/Expedia/test.csv", header = T, sep=",", stringsAsFactors = F)
head(test, 10)
apply(test, 2, mean)
apply(test, 2, var)
dim(test)
str(test)
names(test)
test$date_time <- NULL
test$srch_ci <- NULL
test$srch_co <- NULL

set.seed(12345678)
mysam1test  <- test[sample(1:nrow(test), 1000, replace= FALSE),]
summary(mysam1test)  ##  sample for test 

set.seed(12345678)
mysam1ex<- ex[sample(1:nrow(ex), 1000, replace= FALSE),]
summary(mysam1ex)    ## sample for train
head(mysam1ex, 10)


hotel_name_idx <- which(names(ex)=='srch_destination_id')
names(ex)[hotel_name_idx] <- "dest_id"

table(test$site_name)
hist(test$site_name)

table(test$user_location_country)
hist(test$user_location_country, breaks=100)
hist(test$orig_destination_distance, breaks=100)
hist(test$srch_destination_id, breaks=100)

usercnt <- length(unique(test$user_id))
nrow(test)/length(unique(test$user_id))

user_row_cnt <- group_by(test, user_id) %>% summarise(rowcnt=n(), cntsum=sum(cnt)) %>% as.data.frame
hist(user_row_cnt[,2], breaks=100)
hist(user_row_cnt[,3], breaks=100)

# 7.9% of rows are bookings
nrow(ex[ex$is_booking==1,])/nrow(ex)
nrow(test[test$is_booking==1,])/nrow(test)

# 6.8% of users book
length(unique((ex[ex$is_booking==1,c("user_id")])))/usercnt

length(unique(h$hotel_id))
length(unique(paste(h$user_id, h$hotel_id, sep=",")))
length(unique(paste(h$user_id, h$hotel_id, h$is_mobile, sep=",")))

unique(test$hotel_country)

hotel_country_cnt <- group_by(test, hotel_country) %>% summarise(rowcnt=n()) %>% as.data.frame
hist(hotel_country_cnt[,2], breaks=300)

str(test)

hist(test$srch_children_cnt)
hist(test$srch_children_cnt)
hist(test$srch_rm_cnt)

length(unique(test$srch_destination_id))
length(unique(test$hotel_market))
