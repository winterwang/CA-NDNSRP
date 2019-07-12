##%######################################################%##
#                                                          #
####              try factorminer package               ####
#                                                          #
##%######################################################%##


## data is available from the following webpage: http://factominer.free.fr/bookV2/


library(FactoMineR)
library(readr)
work_women <- read_delim("data_factminer/work_women.csv",
                         ";", escape_double = FALSE, trim_ws = TRUE)

work <- read.table("http://factominer.free.fr/bookV2/work_women.csv",
                   header=TRUE,row.names=1,sep=";")
summary(work)
chisq.test(work[,1:3])
res.test.chi2 <- chisq.test(work_women[,2:4])

round(res.test.chi2$expected, 1)
round(res.test.chi2$residuals^2, 2)

round(100*res.test.chi2$residuals^2/res.test.chi2$statistic, 2)


dd <- rbind(work,apply(work[,1:3],2,sum))
dd
rownames(dd)[4] <- "Mean profile"


round(prop.table(as.matrix(dd),margin=1),3)


res.ca <- CA(work[,1:3])
plot(res.ca,invisible="col")
plot(res.ca,invisible="row")

summary(res.ca)
barplot(res.ca$eig[,1],main="Eigenvalues",
        names.arg=1:nrow(res.ca$eig))


res.ca2 <- CA(work,col.sup=4:ncol(work))


data(JO)
res.jo <- CA(JO)
summary(res.jo)

plot(res.jo, cex = 0.6)
