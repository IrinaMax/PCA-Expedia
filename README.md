# PCA-Expedia
Principal Components Analysis (PCA) is a variable reduction technique which  maximizes the amount of variance accounted for in the observed variables by a smaller group of variables called COMPONENTS. I was interested to do PCA with destinations.csv object wich has 150 variables. And I represent the PCA with forward and backward best subset selection.
## load data
    dest <- read.csv("destinations.csv", header = T, sep=",", stringsAsFactors = F)
    head(dest, 10)
    apply(dest, 2, mean)
    apply(dest, 2, var)
    dim(dest)

    pca_dest <- princomp(dest, cor=TRUE)
    summary(pca_dest)
    loadings(pca_dest)             # pc loadings 
    pca_dest$sd^2                  # component variances
    plot(pca_dest,type="lines")    # scree plot 
    pca_dest$scores                # the principal components

##  All destinations PCA to big to emplement but possible, it just take a time and space on the memory
    biplot(pca_dest)  
##  plot pca of sdev and scale     
    biplot(pca_dest$sdev, pca_dest$scale)`

##extract the component scores
    first.component.scores <- pca_dest$scores[,1]
    summary(first.component.scores)
    length(first.component.scores)

    second.component.scores <- pca_dest$scores[,2]
    summary(second.component.scores)
    length(second.component.scores)


## I want to represent PCA with Forward and Backward  best subset selecton
    require(leaps)

##I took tiny random sample
    set.seed(12345678)
    mysam1  <- dest[sample(1:nrow(dest), 50, replace= FALSE),]
    summary(mysam1)

## 1 forward
    forward = regsubsets(d1~., mysam1, method ="forward")
    summary(forward)

## its get me d35, d52,d57, d62, d89, d119, d145 for subset to use for PCA and it will be (36,53,58, 63, 90, 120, 146) set
    mysam2 <- mysam1[, c(36,53,58, 63, 90, 120, 146)]
    mysam2
    summary(mysam2)
## PCA with forward
    pcaex1.out=prcomp(mysam2, scale=TRUE)
    pcaex1.out
    hist(pcaex1.out$rotation)

    pca_dest1 <- princomp(mysam2, cor=TRUE)
    summary(pca_dest1)
    loadings(pca_dest1) # pc loadings 
    plot(pca_dest1,type="lines") # scree plot 
    pca_dest1$scores # the principal components
    biplot(pca_dest1, cex = .5)

## 2 backward
    backward = regsubsets( d1~., mysam1,nbest = 1, nvmax = 8, method ="backward")
    summary(backward)

## backward selection gave me d12, d13,d18, d22, d24, d33, d36  for subset to use for PCA
    mysam3 <- mysam1[, c(13, 14, 19, 23, 25, 34, 37)]  # my backward sample 
    mysam3
    summary(mysam3)
## PCA

    pcaex2.out=prcomp(mysam3, scale=TRUE)
    pcaex2.out
    hist(pcaex2.out$rotation)

    pca_dest2 <- princomp(mysam3, cor=TRUE)
    summary(pca_dest2)
    loadings(pca_dest2) # pc loadings 
    plot(pca_dest2,type="lines")             # scree plot 
    pca_dest2$scores # the principal components
    biplot(pca_dest2, cex = .5)
