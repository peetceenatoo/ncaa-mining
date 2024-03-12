data_sporco1 <- `cbb.(1)`
data <- data_sporco1 [, 3:21]

risultato <- as.factor(data_sporco1[,22])

#nuova variabile
win_ratio <- data[,2]/data[,1]
min(win_ratio)
max(win_ratio)

data_new <- cbind(data, win_ratio)
p<- 20
n <- 1757

colMeans(data_new)
sapply(data_new, mean)
sapply(data_new, sd)
sapply(data_new, var)
cov(data_new)
cor(data_new)



###  scaled dataset (if we need it)
data.sd <- data.frame((data_new))

### perform PCA
PCA.sd <- princomp(data.sd)
summary(PCA.sd)

PCA.loadings <- PCA.sd$loadings
# usually comment the loadings
PCA.scores <- PCA.sd$scores

###  screeplot 
x11()
layout(matrix(c(2,3,1,3),2,byrow=T))
barplot(PCA.sd$sdev^2, las=2, main='Principal Components', ylab='Variances')
barplot(sapply(data.sd,sd)^2, las=2, main='Original variables', ylab='Variances')
plot(cumsum(PCA.sd$sdev^2)/sum(PCA.sd$sde^2), type='b', axes=F, xlab='number of components', ylab='contribution to the total variance', ylim=c(0,1))
abline(h=1, col='blue')
box()
axis(2,at=0:10/10,labels=0:10/10)
axis(1,at=1:ncol(data.sd),labels=1:ncol(data.sd),las=2)
# plot a horizontal threshold (in this case 80%)
abline(h=0.8, lty=2, col='blue')


### variability of the original variables / PCA scores
x11()
layout(matrix(c(1,2),2))
boxplot(data.sd, las=2, col='gold', main='Original variables')
boxplot(PCA.scores, las=2, col='gold', main='Principal components')

### plot of the loadings of the first nComp Components
nComp <- 3
x11()
par(mfcol=c(nComp,1))
for(i in 1:nComp){
  barplot(PCA.loadings[,i], ylim = c(-1, 1), main=paste("PC",i))
}



nComp <- 3
x11()
par(mar = c(1,nComp,0,2), mfrow = c(nComp,1))
for(i in 1:nComp) {
  barplot(ifelse(abs(PCA.loadings[,i]) < 0.3, 0, PCA.loadings[,i]),
          ylim = c(-1, 1))
  abline(h=0)
}

x11()
par(mfrow=c(2,5))
matplot(t(data.sd), type='l', main='Data', ylim=range(data.sd))
meanF <- colMeans(data.sd)
matplot(meanF, type='l', main = '0 PC', lwd=2, ylim=range(data.sd))
for(i in 1:3) {
  projection <- matrix(meanF, dim(data.sd)[[1]], dim(data.sd)[[2]], byrow=T) +
    PCA.scores[,i] %*% t(PCA.loadings[,i])
  matplot(t(projection), type='l', main = paste(i, 'PC'), ylim=range(data.sd))
  matplot(meanF, type='l', lwd=2, add=T)
}


x11()
plot(PCA.scores[,1], matrix(ncol = dim(data)[1], nrow = 1, data = 0) , xlab="PC1",ylab="PC2", asp=1, pch = 19, col = "red")


x11()
plot(PCA.scores[,1],PCA.scores[,2],xlab="PC1",ylab="PC2", asp=1, pch = 19, col = as.factor(data$size))
abline(h = 0)
abline(v = 0)




M <- colMeans(data.sd)
S <- cov(data.sd)


library(rgl)
open3d()
# only if p=3
# points3d(data.sd, asp=1, size=4) 
axes3d()

PCA.sd123 <- NULL
for(i in 1:n) {
  PCA.sd123 <- rbind(PCA.sd123, PCA.sd$loadings[,1]*PCA.sd$scores[i,1] + PCA.sd$loadings[,2]*PCA.sd$scores[i,2]
                     + PCA.sd$loadings[,3]*PCA.sd$scores[i,3] + M)
}
points3d(PCA.sd123, col='red', size=6)

# only if p=3
# for(i in 1:n) {
#   lines3d(rbind(data.sd[i,], PCA.sd123[i,]),col='blue')
# }

lines3d(rbind(M + 2*PCA.sd$sdev[1] * PCA.sd$loadings[,1], M - 2*PCA.sd$sdev[1] * PCA.sd$loadings[,1]),
        col='forestgreen',lwd=2) 
lines3d(rbind(M + 2*PCA.sd$sdev[2] * PCA.sd$loadings[,2], M - 2*PCA.sd$sdev[2] * PCA.sd$loadings[,2]),
        col='forestgreen',lwd=2) 
lines3d(rbind(M + 2*PCA.sd$sdev[3] * PCA.sd$loadings[,3], M - 2*PCA.sd$sdev[3] * PCA.sd$loadings[,3]),
        col='forestgreen',lwd=2)


par(mfrow = c(1,1))
biplot(PCA.sd)
