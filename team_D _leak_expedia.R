# For leak sorting
hc <- subset(h, !is.na(user_location_region) & !is.na(user_location_city))

hc <- hc %>% group_by(user_location_region, user_location_city, hotel_cluster) %>%
 summarise(CNT=sum(cnt), NBR=n(), BOOKCNT=sum(is_booking*cnt), BOOKNBR=sum(is_booking)) %>%
 data.frame

drank <- hc %>%
 arrange(user_location_region, user_location_city, -NBR) %>%
 group_by(user_location_region, user_location_city) %>%
 mutate(rank=row_number()) %>% data.frame

# Re-order
drank <- arrange(drank, user_location_region, user_location_city, rank)

# Save to file
write.csv(drank, "region.city.hc.csv", row.names = F)

# Save for use later
regioncityhc <- drank

# Simple leak grouping - region/city/dist
hc <- subset(h, !is.na(user_location_region) & !is.na(user_location_city) & !is.na(orig_destination_distance))

hc <- hc %>% group_by(user_location_region, user_location_city, orig_destination_distance, hotel_cluster) %>%
 summarise(CNT=sum(cnt), NBR=n(), BOOKCNT=sum(is_booking*cnt), BOOKNBR=sum(is_booking)) %>%
 data.frame

# Key for joining
regioncityhc$rchkey <- paste(regioncityhc$user_location_region, regioncityhc$user_location_city, regioncityhc$hotel_cluster, sep=":")
hc$rchkey <- paste(hc$user_location_region, hc$user_location_city, hc$hotel_cluster, sep=":")

# Join in city pop by nbr
hc <- merge(hc, regioncityhc[,c("rchkey", "rank")], by="rchkey", all.x=T)

# Sort value
hc$NBR2 <- hc$NBR + (100-hc$rank)/10000

# Rank all by NBR2 metric - this will produce generally 1..8
# with few at 5..8
drank <- hc %>%
 arrange(user_location_region, user_location_city, orig_destination_distance, -NBR2) %>%
 group_by(user_location_region, user_location_city, orig_destination_distance) %>%
 mutate(rrank=row_number()) %>% data.frame

# Re-order
drank <- arrange(drank, user_location_region, user_location_city, orig_destination_distance, rrank)

# Save to file
write.csv(drank, "drank.6.csv", row.names = F)


[
9:15
]
 
I was using NBR=n() for (region, city) as the tie breaker for the leak.  Now that I look at it, I should be adding in overallPopHC to further tiebreak.


[
9:17
]
 
$ paste single_hc_submission_2016-06-07-16-55.csv 66 | awk -F"\t" '$1!=$2 {print}' | wc -l
33064


[
9:19
]
 
WOW!  Our leaks are quite different.


[
9:19
]
 
Here is a range showing the differences - yours on the left, mine on the right.


[
9:19
]
 
510901,46       510901,80
510939,37       510939,70
511105,59       511105,25
511140,15       511140,28
511260,2        511260,16
511280,70       511280,18
511298,37       511298,42
511503,22       511503,99
511684,71       511684,16
511698,48       511698,91
511710,80       511710,65
511846,50       511846,33
511886,16       511886,33
511907,10       511907,37
511975,80       511975,96
512015,40       512015,23
512032,10       512032,37
512093,21       512093,25
512098,15       512098,55
512122,4        512122,77
512144,1        512144,0
512174,6        512174,55
512289,15       512289,63
512372,48       512372,91
512379,6        512379,55
512519,46       512519,64
512537,16       512537,33
512656,33       512656,16
512727,18       512727,68
512844,95       512844,56
512860,4        512860,6
512951,91       512951,41
513049,90       513049,41
513142,13       513142,6
513151,36       513151,22
513216,25       513216,59
513239,59       513239,25
 
0.28386 vs. 0.28350 = 36 ticks.  I'll have to do the math,  but I think with a 33K row diff, we are BOTH under what would be optimal.
