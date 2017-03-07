 
## I cooked this up really quick to show merge (which is in the base package)

df1 <- data.frame(id=integer(100), val1=character(100))
df2 <- data.frame(id=integer(80), val2=character(80))

df1$id <- 1:100
df2$id <- 1:80
df1$val1 <- apply(df1, 1, FUN=function(x) rawToChar(as.raw(runif(10,65,90))))
df2$val2 <- apply(df2, 1, FUN=function(x) rawToChar(as.raw(runif(15,97,122))))

# Basic merge with same identification key field
merge(df1, df2, by="id")

# What if different keys?
names(df2)[1] <- "lowerid"

str(df2)

merge(df1, df2, by.x="id", by.y="lowerid")

# Very often you want to keep all rows from the  "main" data. frame - use all.x (there is also all.y and all)
merge(df1, df2, by.x="id", by.y="lowerid", all.x=T) (edited)
1

##And now, using data.table - this is wicked fast compared to merge on big data sets, like for Expedia

require("data.table")

df1 <- as.data.table(df1)
df2 <- as.data.table(df2)

setkey(df1, id)
setkey(df2, lowerid)

df1[df2]
