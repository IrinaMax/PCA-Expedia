##Expedia data analysis

require("plyr")
require("dplyr")
require("xgboost")
require("reshape2")
require("caret")

ex <- read.csv("~/Desktop/Expedia/train.csv", header = T, sep=",", stringsAsFactors = F)
ex1 <- ex[sample(1:nrow(1000), 1000, stringsAsFactors = F)]
##dim(ex1)
dim(ex)
str(ex)
names(ex)
ex1 <- ex[sample(1:nrow(ex), 1000,
                          replace=FALSE),]
ex2<- ex[sample(1:nrow(ex), 1000,
                      replace=FALSE),]

apply(ex,2,mean)
apply(ex1, 2, mean)
table(ex$site_name)
hist(ex$site_name)

table(ex$hotel_cluster)
hist(ex$hotel_cluster, breaks=100)

length(unique(ex$user_id))
nrow(ex)/length(unique(ex$user_id))
length(unique(ex1$user_id))
nrow(ex1)/length(unique(ex1$user_id))

user_row_cnt <- group_by(ex, user_id) %>% summarise(rowcnt=n()) %>% as.data.frame
hist(user_row_cnt[,2], breaks=100)

hotel_name_idx <- which(names(ex)=='srch_destination_id')
names(ex)[hotel_name_idx] <- "dest_id"

user_row_cnt <- group_by(ex, user_id) %>% summarise(rowcnt=n(), cntsum=sum(cnt)) %>% as.data.frame
hist(user_row_cnt[,2], breaks=100)
hist(user_row_cnt[,3], breaks=100)

# 7.9% of rows are bookings
nrow(ex[ex$is_booking==1,])/nrow(ex)

# 6.8% of users book
length(unique((ex[ex$is_booking==1,c("user_id")])))/nrow(user_row_cnt)

length(unique(ex$hotel_id))
length(unique(paste(ex$user_id, ex$hotel_id, sep=",")))
length(unique(paste(ex$user_id, ex$hotel_id, ex$is_mobile, sep=",")))

unique(ex$hotel_country)

hotel_country_cnt <- group_by(ex, hotel_country) %>% summarise(rowcnt=n()) %>% as.data.frame
hist(hotel_country_cnt[,2], breaks=300)

hist(hotel_country_cnt[,2], breaks=200)
avg(ex1$orig_destination_distance)
ex11<- ex1[ -c(1, 5, 12,13)] ## uberet  colonki nomer1, 12,13 esli oni ne nujni ili tam otvlekayushie komponenti
ex12 <- ex1[ c()]

apply(ex1,2,mean)
apply(ex11,2,mean)
apply(ex1,2, var)
apply(ex11,2, var)
pcaex.out=prcomp(ex11, scale=TRUE)
pcaex.out

dest <- read.csv("~/Desktop/Expedia/destinations.csv", header = T, sep=",", stringsAsFactors = F)
head(dest)
apply(dest, 2, mean)
apply(dest, 2, var)
dim(dest)
ddd <- dest[rnorm1:1000, 1:15]

pcaex.out=prcomp(ddd, scale=TRUE)
pcaex.out

library(leaps)
regfit.full = regsubsets(d1~d6, ddd, really.big = T)
summary(regfit.full)

## forward
regfit1.full = regsubsets(d1~d2, ddd, method ="forward")
summary(regfit1.full)

##I took random sample
mysam1 = <- dest[sample(1:nrow10(dest), 50, replace= F,]
