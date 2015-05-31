# http://stats.stackexchange.com/questions/87307/predict-only-the-first-n-principal-components-in-a-pca-analysis
# using only selected attributes for prediction

library(MASS)

################
###train data###
################
data(iris)
dat <- as.matrix(iris[,-5])

##############
###new data###
##############
#here is some new data that has the same covariances bewteen variables ar the iris data set
require(MASS)
setosa.mean <- apply(iris[iris$Species=="setosa",-5], 2, mean)
setosa.cov <- cov(iris[iris$Species=="setosa",-5])

versicolor.mean <- apply(iris[iris$Species=="versicolor",-5], 2, mean)
versicolor.cov <- cov(iris[iris$Species=="versicolor",-5])

virginica.mean <- apply(iris[iris$Species=="virginica",-5], 2, mean)
virginica.cov <- cov(iris[iris$Species=="virginica",-5])

set.seed(1)
n <- 30
newdat <- as.data.frame(
    rbind(
        mvrnorm(n, setosa.mean, setosa.cov),
        mvrnorm(n, versicolor.mean, versicolor.cov),
        mvrnorm(n, virginica.mean, virginica.cov)
    )
)
newdat <- cbind(newdat, SPP=rep(c("setosa", "versicolor", "virginica"), each=n))
head(newdat)



##############
###full pca###
##############
#scale data
dat.sc <- scale(dat, center=TRUE, scale=FALSE)

#PCA
pca <- svd(dat.sc)
pcs <- dat.sc %*% pca$v # results are similar to pca$u but includes the units contained in pca$d
plot(scale(pca$u), scale(pcs)); abline(0,1,col=2) # demonstration of the similarity of pca$u and pcs

#prediction
keep <- 1:2 # there are many ways to define which PCs you want to use for prediction (e.g. "tol" setting in prcomp)

newdat.sc <- scale(newdat[,1:4], center=attr(dat.sc, "scaled:center"), scale=FALSE) # you must use the variable centering and scaling from the original data
pred <- newdat.sc %*% pca$v[,keep]


###Plot PCs 1 & 2 for orig. and new data
COL <- 2:4
plot(pcs, col=COL[iris$Species], cex=1, xlab=paste0("PC 1", " (", round(pca$d[1]^2/sum(pca$d^2)*100,0), "%)"), ylab=paste0("PC 2", " (", round(pca$d[2]^2/sum(pca$d^2)*100,0), "%)"))
points(pred, col=COL[match(newdat$SPP, levels(iris$Species))], pch=16)
legend("topright", legend=levels(iris$Species), col=COL, pch=16)
legend("topleft", legend=c("Original data", "New data"), col=1, pch=c(1,16))