## R version of most popular local hotels
library(data.table)
exp_train_sorted <- fread("~/Desktop/Expedia/train.csv", header=TRUE, select= c("is_booking","orig_destination_distance","hotel_cluster","srch_destination_id"))
expedia_test <- fread("~/Desktop/Expedia/test.csv", header=T,sep=",", stringsAsFactors = F)

_______
##load destinations.cvs data
dest <- read.csv("~/Desktop/Expedia/destinations.csv", header = T, sep=",", stringsAsFactors = F)
head(dest, 10)
apply(dest, 2, mean)
apply(dest, 2, var)
dim(dest)

require(leaps)
## All big data set PCA
pca_dest <- princomp(dest, cor=TRUE)
##pca_dest00 <- princomp(dest, cor=TRUE, scores = T)
summary(pca_dest)
loadings(pca_dest)             # pc loadings 
pca_dest$sd^2                  # component variances
screeplot(pca_dest)               # if you would like a scree plot
plot(pca_dest,type="lines")    # scree plot in lines
pca_dest$scores             # the principal components

pr_dest <- prcomp(dest, scale. = TRUE)                  
summary(pr_dest) # to see cumulative proportion for the best 2 0r 3 components
newpr_dest <- data.frame(pr_dest$x[,1:2])
dim(newpr_dest)
predict(pr_dest$score, dest)
lm(newpr_dest$scores[,1] ~ newpr_dest$scores[,2])

#----------
sum_and_count <- function(x){
  sum(x)*0.85 + length(x) *(1-0.85)
}

dest_id_hotel_cluster_count <- 
  exp_train_sorted[,sum_and_count(is_booking),by=list(orig_destination_distance, hotel_cluster)]
dest_id_hotel_cluster_count1 <- 
  exp_train_sorted[,sum_and_count(is_booking),by=list(srch_destination_id, hotel_cluster)]

dest_id_hotel_cluster_count2 <- 
  expedia_t


top_five <- function(hc,v1){
  hc_sorted <- hc[order(v1,decreasing=TRUE)]
  n <- min(5,length(hc_sorted))
  paste(hc_sorted[1:n],collapse=" ")
}

five_orig <- dest_id_hotel_cluster_count[,top_five(hotel_cluster,V1),by=orig_destination_distance]
five_srch <- dest_id_hotel_cluster_count1[,top_five(hotel_cluster,V1),by=srch_destination_id]

result_orig <- merge(expedia_test,five_orig, by="orig_destination_distance",all.x=TRUE)[order(id),list(id,V1)]

result_srch <- merge(expedia_test,five_srch, by="srch_destination_id",all.x=TRUE)[order(id),list(id,V1)]

result_orig$V1[is.na(result_orig$V1)] <- result_srch$V1[is.na(result_orig$V1)] 

setnames(result_orig,c("id","hotel_cluster"))

write.csv(result_orig, file='submission_combo_expedia.csv', row.names=FALSE)

dim(result_orig)

