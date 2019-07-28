library(data.table)
library(caret)

data = fread('data_final.csv')[,c(-1,-13)]
data_no_easement = data[,-57]

# save to rda
data_final = fread('data_final.csv')
save(data_final,file = 'data_final.rda')

#center: if TRUE, the objects' column means are subtracted from the values in those columns (ignoring NAs); 
#if FALSE, centering is not performed
#scale: if TRUE, the centered column values are divided by the column's standard deviation (when center is also TRUE; otherwise, the root mean square is used); 
#if FALSE, scaling is not performed

data2 = predict(preProcess(data[,-55], method = c('center', 'scale')), data[,-55])
data_for_pca = data.frame(cbind(data2,data[,55]))

#data2_2 = predict(preProcess(data_no_easement, method = c('center', 'scale')), data_no_easement)


# 法一： caret/preProcess
data2_pca <- preProcess(data2,method="pca", thresh = 0.9)
print(data2_pca)
plot(data2_pca)
names(data2_pca)
summary(data2_pca)

data2_pca2<- predict(data2_pca, data2)
#plot(data2_pca2)
summary(data2_pca2)

# 法二：prcomp
data2_prcomp <- prcomp(data_for_pca, scale = TRUE)
# 只有这个可以做图
plot(data2_prcomp)
pcaCharts(data2_prcomp)
summary(data2_prcomp)

names(data2_prcomp)
data2_prcomp$sdev
data2_prcomp$rotation
data2_prcomp$center
data2_prcomp$scale
data2_prcomp$x



pcaCharts <- function(x) {
    x.var <- x$sdev ^ 2
    x.pvar <- x.var/sum(x.var)
    print("proportions of variance:")
    print(x.pvar)
    
    par(mfrow=c(2,2))
    plot(x.pvar,xlab="Principal component", ylab="Proportion of variance explained", ylim=c(0,1), type='b')
    plot(cumsum(x.pvar),xlab="Principal component", ylab="Cumulative Proportion of variance explained", ylim=c(0,1), type='b')
    screeplot(x)
    plot(x,type="l")
    par(mfrow=c(1,1))
}
