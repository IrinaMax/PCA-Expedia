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

