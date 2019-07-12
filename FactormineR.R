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

##%######################################################%##
#                                                          #
####                olympic medals data                 ####
#                                                          #
##%######################################################%##

data(JO)
res.jo <- CA(JO)
summary(res.jo)

plot(res.jo, cex = 0.6)


##%######################################################%##
#                                                          #
####                     Wine data                      ####
#                                                          #
##%######################################################%##

data.wine = read.table("http://factominer.free.fr/bookV2/wine.csv",
                       header=TRUE,row.names=1,sep=";",check.names=FALSE)
res.ca=CA(data.wine,col.sup=11,row.sup=nrow(data.wine))

barplot(res.ca$eig[,1],main="Eigenvalues", 
        names.arg=1:nrow(res.ca$eig))

summary(res.ca, nb.dec=2, ncp=2)

plot(res.ca)


##%######################################################%##
#                                                          #
####                   mortality data                   ####
#                                                          #
##%######################################################%##


death <- read.table("http://factominer.free.fr/bookV2/death.csv",
                    header=TRUE,sep=";",row.names=1)
colnames(death) <- c("0-1","1-4","5-14","15-24","25-34","35-44",
                     "45-54","55-64","65-74","75-84","85-94","95+")


res.ca <- CA(death,row.sup=66:nrow(death), graph=FALSE)
plot(res.ca)
round(res.ca$call$marge.col,3)
round(res.ca$call$marge.row[order(res.ca$call$marge.row)],3)
par(las=1)
barplot(res.ca$call$marge.col,horiz=TRUE)
barplot(res.ca$call$marge.row[order(res.ca$call$marge.row)],horiz=TRUE)
par(las=0)
res.ca <- CA(death,row.sup=66:nrow(death))
summary(res.ca, nb.dec=4)

# decompose the inertia by row and by column
100*res.ca$col$inertia/sum(res.ca$col$inertia)

100*res.ca$row$inertia[rev(order(res.ca$row$inertia))]/sum(res.ca$row$inertia)


bb<-round(cbind.data.frame(res.ca$call$marge.col,
                           sqrt(res.ca$col$inertia/res.ca$call$marge.col),
                           res.ca$col$inertia,res.ca$col$inertia/sum(res.ca$col$inertia)),4)
colnames(bb)<-c("Weight","Distance","Inertia","% of inertia")
bb


plot.CA(res.ca, axes = c(1, 2), autoLab = "yes", cex = 0.7)

library(factoextra)
fviz_ca_biplot(res.ca, label ="col", 
               invisible = c("row", "row.sup"))

fviz_ca_biplot(res.ca, label = c("col","row"), 
               # invisible = c("row.sup"),
               select.row = list(contrib = 20),
               repel = T)

