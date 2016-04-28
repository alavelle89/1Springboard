library(dplyr)
library(tidyr)
titanicdata = read.csv("titanic_original.csv")
tbl_df(titanicdata)
mean_age <- colMeans(titanicdata[5], na.rm=TRUE)
titanicdata$embarked[285] <- c("S")
titanicdata$age[is.na(titanicdata$age)] <- mean_age
titanicdata$has_cabin <- c(ifelse(titanicdata$cabin != "",1,0))
titanicdata$boat[titanicdata$boat == ""] <- NA
head(titanicdata, n=28)
write.table(titanicdata, file="titanic_clean.csv")