rm(list=ls())

installIfAbsentAndLoad <- function(neededVector) {
  for(thispackage in neededVector) {
    if( ! require(thispackage, character.only = T) )
    { install.packages(thispackage)}
    require(thispackage, character.only = T)
  }
}
needed  <-  c("ISLR", "MASS", "class", "boot", "knitr", "ggplot2", "reshape2", "leaps", "xlsx")      
installIfAbsentAndLoad(needed)

file <- read.csv('data.csv')
df <- data.frame(file)

### Best Subset Selection
regfit.full = regsubsets(diagnosis ~ ., df, nvmax=30)
reg.summary = summary(regfit.full)
reg.summary

par(mfrow=c(2,2))
plot(reg.summary$rss, xlab = 'Number of Variables', ylab = 'RSS')
plot(reg.summary$adjr2, xlab = 'Number of Variables', ylab = 'Adjusted R^2')

which.max(reg.summary$adjr2)
points(14, reg.summary$adjr2[14], col='red', cex=2, pch=20)

plot(reg.summary$cp, xlab = 'No. of Variables', ylab= 'CP')
points(14, reg.summary$adjr2[14], col='red', cex=2, pch=20)
bestcp <- which(reg.summary$cp < sd(reg.summary$cp + min(reg.summary$cp)))[1]
points(bestcp, reg.summary$cp[bestcp], col='blue', cex = 2, pch=20)

plot(reg.summary$bic, xlab = 'No of Variables', ylab = 'BIC')
points(11, reg.summary$bic[11], col = 'red', cex = 2, pch = 20)
bestbic <- which(reg.summary$bic < sd(reg.summary$bic + min(reg.summary$bic)))[1]
points(bestbic, reg.summary$bic[bestbic], col='blue', cex = 2, pch = 20)

#plot(regfit.full, scale = 'r2')
#plot(regfit.full, scale = 'adjr2')
#plot(regfit.full, scale = 'Cp')
#plot(regfit.full, scale = 'bic')

#11 predictors: compactness_mean + concavity_mean + radius_se + smoothness_se + concavity_se + radius_worst + texture_worst + area_worst + concave.points_worst + symmetry_worst + fractal_dimension_worst
#14 predictors: compactness_mean + concavity_mean + radius_se + smoothness_se + concavity_se + radius_worst + texture_worst + area_worst + symmetry_worst + fractal_dimension_worst + + radius_mean + concave.points_mean + concave.points_se + concavity_worst

lda11matrix <- matrix(nrow=100, ncol=5)
dimnames(lda11matrix) <- list(NULL, c("Success Rate", "Type 1 Error", "Type 2 Error", "Power", "Precision"))
lda14matrix <- matrix(nrow=100, ncol=5)
dimnames(lda14matrix) <- list(NULL, c("Success Rate", "Type 1 Error", "Type 2 Error", "Power", "Precision"))
qda11matrix <- matrix(nrow=100, ncol=5)
dimnames(qda11matrix) <- list(NULL, c("Success Rate", "Type 1 Error", "Type 2 Error", "Power", "Precision"))
qda14matrix <- matrix(nrow=100, ncol=5)
dimnames(qda14matrix) <- list(NULL, c("Success Rate", "Type 1 Error", "Type 2 Error", "Power", "Precision"))
knn11matrix <- matrix(nrow=100, ncol=5)
dimnames(knn11matrix) <- list(NULL, c("Success Rate", "Type 1 Error", "Type 2 Error", "Power", "Precision"))
knn14matrix <- matrix(nrow=100, ncol=5)
dimnames(knn14matrix) <- list(NULL, c("Success Rate", "Type 1 Error", "Type 2 Error", "Power", "Precision"))

for(i in 1:100){
### Creation of Train and Test Sets
n <- nrow(df)
trainprop <- .8
train <- sample(n, trainprop*n)
test <- setdiff(1:n, train)

trainset <- df[train,]
testset <- df[test,]

### LDA with 11 Predictors
lda.fit <- lda(diagnosis ~ compactness_mean + concavity_mean + radius_se + smoothness_se + concavity_se + 
                 radius_worst + texture_worst + area_worst + concave.points_worst + symmetry_worst + 
                 fractal_dimension_worst, data = df, subset = train)
lda.pred <- predict(lda.fit, testset)
ldatable <- table(testset$diagnosis, lda.pred$class)

lda11success <- (ldatable[1,1] + ldatable[2,2])/sum(ldatable)
lda11type1err <- ldatable[1,2]/sum(ldatable[1,])
lda11type2err <- ldatable[2,1]/sum(ldatable[2,])
lda11power <- 1 - lda11type2err
lda11precision <- ldatable[2,2]/sum(ldatable[,2])

lda11matrix[i,] <- c(lda11success, lda11type1err, lda11type2err, lda11power, lda11precision)

#print(paste("Success Rate: ", lda11success))
#print(paste("Overall Error Rate: ", 1-lda11success))
#print(paste("Type 1 Error Rate: ", lda11type1err))
#print(paste("Type 2 Error Rate: ", lda11type2err))
#print(paste("Power (Sensitivity): ", 1-lda11type2err))
#print(paste("Precision: ", lda11precision))

### LDA with 14 Predictors
lda.fit <- lda(diagnosis ~ radius_mean + compactness_mean + concavity_mean + 
                 radius_se + smoothness_se + concavity_se + radius_worst + texture_worst + area_worst + 
                 symmetry_worst + fractal_dimension_worst + concave.points_mean + concave.points_se + concavity_worst, data = df, subset = train)
lda.pred <- predict(lda.fit, testset)
ldatable <- table(testset$diagnosis, lda.pred$class)

lda14success <- (ldatable[1,1] + ldatable[2,2])/sum(ldatable)
lda14type1err <- ldatable[1,2]/sum(ldatable[1,])
lda14type2err <- ldatable[2,1]/sum(ldatable[2,])
lda14power <- 1 - lda14type2err
lda14precision <- ldatable[2,2]/sum(ldatable[,2])

lda14matrix[i,] <- c(lda14success, lda14type1err, lda14type2err, lda14power, lda14precision)

#print(paste("Success Rate: ", lda14success))
#print(paste("Overall Error Rate: ", 1-lda14success))
#print(paste("Type 1 Error Rate: ", lda14type1err))
#print(paste("Type 2 Error Rate: ", lda14type2err))
#print(paste("Power (Sensitivity): ", 1-lda14type2err))
#print(paste("Precision: ", lda14precision))

### QDA with 11 Predictors
qda.fit <- qda(diagnosis ~ compactness_mean + concavity_mean + radius_se + smoothness_se + 
                 concavity_se + radius_worst + texture_worst + area_worst + concave.points_worst + 
                 symmetry_worst + fractal_dimension_worst, data = df, subset = train)
qda.pred <- predict(qda.fit, testset)
qdatable <- table(testset$diagnosis, qda.pred$class)

qda11success <- (qdatable[1,1] + qdatable[2,2])/sum(qdatable)
qda11type1err <- qdatable[1,2]/sum(qdatable[1,])
qda11type2err <- qdatable[2,1]/sum(qdatable[2,])
qda11power <- 1 - qda11type2err
qda11precision <- qdatable[2,2]/sum(qdatable[,2])

qda11matrix[i,] <- c(qda11success, qda11type1err, qda11type2err, qda11power, qda11precision)

#print(paste("Success Rate: ", qda11success))
#print(paste("Overall Error Rate: ", 1-qda11success))
#print(paste("Type 1 Error Rate: ", qda11type1err))
#print(paste("Type 2 Error Rate: ", qda11type2err))
#print(paste("Power (Sensitivity): ", 1-qda11type2err))
#print(paste("Precision: ", qda11precision))

### QDA with 14 Predictors
qda.fit <- qda(diagnosis ~ radius_mean + compactness_mean + concavity_mean + radius_se + 
                 smoothness_se + concavity_se + radius_worst + texture_worst + area_worst + symmetry_worst + 
                 fractal_dimension_worst + concave.points_mean + concave.points_se + concavity_worst, data = df, subset = train)
qda.pred <- predict(qda.fit, testset)
qdatable <- table(testset$diagnosis, qda.pred$class)

qda14success <- (qdatable[1,1] + qdatable[2,2])/sum(qdatable)
qda14type1err <- qdatable[1,2]/sum(qdatable[1,])
qda14type2err <- qdatable[2,1]/sum(qdatable[2,])
qda14power <- 1 - qda14type2err
qda14precision <- qdatable[2,2]/sum(qdatable[,2])

qda14matrix[i,] <- c(qda14success, qda14type1err, qda14type2err, qda14power, qda14precision)

#print(paste("Success Rate: ", qda14success))
#print(paste("Overall Error Rate: ", 1-qda14success))
#print(paste("Type 1 Error Rate: ", qda14type1err))
#print(paste("Type 2 Error Rate: ", qda14type2err))
#print(paste("Power (Sensitivity): ", 1-qda14type2err))
#print(paste("Precision: ", qda14precision))

### KNN - Best K with 11 Predictors
train.x <- cbind(df$compactness_mean, df$concavity_mean, df$radius_se, df$smoothness_se, 
                 df$concavity_se, df$radius_worst, df$texture_worst, df$area_worst, df$concave.points_worst, 
                 df$symmetry_worst, df$fractal_dimension_worst)[train,]
test.x <- cbind(df$compactness_mean, df$concavity_mean, df$radius_se, df$smoothness_se, 
                df$concavity_se, df$radius_worst, df$texture_worst, df$area_worst, df$concave.points_worst, 
                df$symmetry_worst, df$fractal_dimension_worst)[test,]
train.diagnosis <- df$diagnosis[train]
numreps <- 100
test.errors <- rep(0, numreps)
train.errors <- rep(0, numreps)
for(k in 1:numreps) {
  knn.pred <- knn(train.x, test.x,  train.diagnosis, k = k)
  mytable <- table(testset$diagnosis, knn.pred)
  test.errors[k] <- mean(testset$diagnosis != knn.pred)
}
knn.pred <- knn(train.x, test.x, train.diagnosis, k=which.min(test.errors))
knntable <- table(testset$diagnosis, knn.pred)

knn11success <- (knntable[1,1] + knntable[2,2])/sum(knntable)
knn11type1err <- knntable[1,2]/sum(knntable[1,])
knn11type2err <- knntable[2,1]/sum(knntable[2,])
knn11power <- 1 - knn11type2err
knn11precision <- knntable[2,2]/sum(knntable[,2])

knn11matrix[i,] <- c(knn11success, knn11type1err, knn11type2err, knn11power, knn11precision)

#print(paste("Success Rate: ", knn11success))
#print(paste("Overall Error Rate: ", 1-knn11success))
#print(paste("Type 1 Error Rate: ", knn11type1err))
#print(paste("Type 2 Error Rate: ", knn11type2err))
#print(paste("Power (Sensitivity): ", 1-knn11type2err))
#print(paste("Precision: ", knn11precision))

### KNN - Best K with 14 Predictors
train.x <- cbind(df$radius_mean, df$concave.points_mean, df$concave.points_se, df$concavity_worst,
                 df$compactness_mean, df$concavity_mean, df$radius_se, df$smoothness_se, df$concavity_se, 
                 df$radius_worst, df$texture_worst, df$area_worst, df$symmetry_worst, df$fractal_dimension_worst)[train,]
test.x <- cbind(df$radius_mean, df$concave.points_mean, df$concave.points_se, df$concavity_worst, 
                df$compactness_mean, df$concavity_mean, df$radius_se, df$smoothness_se, df$concavity_se, 
                df$radius_worst, df$texture_worst, df$area_worst, df$symmetry_worst, df$fractal_dimension_worst)[test,]
train.diagnosis <- df$diagnosis[train]
numreps <- 100
test.errors <- rep(0, numreps)
train.errors <- rep(0, numreps)
for(k in 1:numreps) {
  knn.pred <- knn(train.x, test.x,  train.diagnosis, k = k)
  mytable <- table(testset$diagnosis, knn.pred)
  test.errors[k] <- mean(testset$diagnosis != knn.pred)
}
knn.pred <- knn(train.x, test.x, train.diagnosis, k=which.min(test.errors))
knntable <- table(testset$diagnosis, knn.pred)

knn14success <- (knntable[1,1] + knntable[2,2])/sum(knntable)
knn14type1err <- knntable[1,2]/sum(knntable[1,])
knn14type2err <- knntable[2,1]/sum(knntable[2,])
knn14power <- 1 - knn14type2err
knn14precision <- knntable[2,2]/sum(knntable[,2])

knn14matrix[i,] <- c(knn14success, knn14type1err, knn14type2err, knn14power, knn14precision)

#print(paste("Success Rate: ", knn14success))
#print(paste("Overall Error Rate: ", 1-knn14success))
#print(paste("Type 1 Error Rate: ", knn14type1err))
#print(paste("Type 2 Error Rate: ", knn14type2err))
#print(paste("Power (Sensitivity): ", 1-knn14type2err))
#print(paste("Precision: ", knn14precision))
}

total <- matrix(nrow = 6, ncol = 5)
dimnames(total) <- list(c('LDA 11', 'LDA 14', 'QDA 11', 'QDA 14', 'KNN 11', 'KNN 14'), c('Success Rate', 'Type 1 Error','Type 2 Error',
                  'Power', 'Precision'))

total[1,] <- c(mean(lda11matrix[,1]), mean(lda11matrix[,2]), mean(lda11matrix[,3]), mean(lda11matrix[,4]), mean(lda11matrix[,5]))
total[2,] <- c(mean(lda14matrix[,1]), mean(lda14matrix[,2]), mean(lda14matrix[,3]), mean(lda14matrix[,4]), mean(lda14matrix[,5]))
total[3,] <- c(mean(qda11matrix[,1]), mean(qda11matrix[,2]), mean(qda11matrix[,3]), mean(qda11matrix[,4]), mean(qda11matrix[,5]))
total[4,] <- c(mean(qda14matrix[,1]), mean(qda14matrix[,2]), mean(qda14matrix[,3]), mean(qda14matrix[,4]), mean(qda14matrix[,5]))
total[5,] <- c(mean(knn11matrix[,1]), mean(knn11matrix[,2]), mean(knn11matrix[,3]), mean(knn11matrix[,4]), mean(knn11matrix[,5]))
total[6,] <- c(mean(knn14matrix[,1]), mean(knn14matrix[,2]), mean(knn14matrix[,3]), mean(knn14matrix[,4]), mean(knn14matrix[,5]))

write.csv(total, "totalcsv.csv")
