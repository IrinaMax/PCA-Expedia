# Kaggle Expedia

setwd("C:/Users/dalyea/Documents/R/Expedia")

source("../common.R")
require("plyr")
require("dplyr")
require("xgboost")
require("reshape2")
require("caret")

header.names <- read.csv("header.csv", header=F, sep=",", stringsAsFactors = F)
header.names
h <- read.csv("train.17.csv", header=T, sep=",", stringsAsFactors = F)
dim(h)
str(h)
names(h)
names(h) <- header.names

# Rename for convenience
hotel_name_idx <- which(names(h)=='srch_destination_id')
names(h)[hotel_name_idx] <- "dest_id"

table(h$site_name)
hist(h$site_name)

table(h$hotel_cluster)
hist(h$hotel_cluster, breaks=100)

usercnt <- length(unique(h$user_id))
nrow(h)/length(unique(h$user_id))

user_row_cnt <- group_by(h, user_id) %>% summarise(rowcnt=n(), cntsum=sum(cnt)) %>% as.data.frame
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

str(h)

hist(h$srch_children_cnt)
hist(h$srch_children_cnt)
hist(h$srch_rm_cnt)

length(unique(h$dest_id))
length(unique(h$hotel_market))

# -- Begin ranks definition
# Level of interest hierarchy
# is_mobile
# dest_id
# hotel_market (assume unique to continent, country - research this later)
# srch_adults_cnt - bucketize 1, 2, 3+
# srch_children_cnt - bucketize 0, 1+
# Run the command below 10 times, once for each file h = h0..h9

h <- read.csv("train.0.csv", header=T, sep=",", stringsAsFactors = F)
dim(h)

# Column names
names(h) <- header.names

# Rename for convenience
hotel_name_idx <- which(names(h)=='srch_destination_id')
names(h)[hotel_name_idx] <- "dest_id"

# Look for NA
dim(subset(h, is.na(is_mobile)))
dim(subset(h, is.na(dest_id)))
dim(subset(h, is.na(hotel_market)))
dim(subset(h, is.na(srch_adults_cnt)))
dim(subset(h, is.na(srch_children_cnt)))
# Bin searches for adults and kids
# h$adult_bin <- apply(h, 1, FUN=function(x) max(x[c("srch_adults_cnt")], 3))
# h$child_bin <- apply(h, 1, FUN=function(x) max(x[c("srch_children_cnt")], 2))
h$adult_bin <- h$srch_adults_cnt
h[h$adult_bin>3,c("adult_bin")] <- 3
h$child_bin <- h$srch_children_cnt
h[h$child_bin>2,c("child_bin")] <- 2
table(h$adult_bin)
table(h$child_bin)

rank <- group_by(h, is_mobile, dest_id, hotel_market, adult_bin, child_bin, hotel_cluster) %>%
  summarise(CNT=n()) %>% as.data.frame()
ranknames <- names(rank)

rankn <- group_by(h, is_mobile=NA, dest_id, hotel_market, adult_bin, child_bin, hotel_cluster) %>%
  summarise(CNT=n()) %>% as.data.frame()
rank <- rbind(rank, rankn)

rankn <- group_by(h, is_mobile=NA, dest_id=NA, hotel_market, adult_bin, child_bin, hotel_cluster) %>%
  summarise(CNT=n()) %>% as.data.frame()
rank <- rbind(rank, rankn)

rankn <- group_by(h, is_mobile=NA, dest_id=NA, hotel_market=NA, adult_bin, child_bin, hotel_cluster) %>%
  summarise(CNT=n()) %>% as.data.frame()
rank <- rbind(rank, rankn)

# dealloc :-)
rm(rankn)

# Check the column names
names(rank)

# First time
rankall <- rank

# Second etc. time
rankall <- rbind(rankall, rank)

# -- End defining ranks

# Now sum out the 10 files
ranksum <- group_by(rankall, is_mobile, dest_id, hotel_market, adult_bin, child_bin, hotel_cluster) %>%
  summarise(TOTAL=sum(CNT)) %>% as.data.frame()

dim(rankall)
dim(ranksum)

# Now read in test file and use the ranksum data frame to product top 5 
