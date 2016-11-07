library(glmnet)
library(ROCR)

EnetBinaryClassification <- function(trainDat, 
                                     Xtest,
                                     respName,
                                     nfolds = 10,
                                     alphaGrid = seq(0, 1, by = 0.1),
                                     cvSeed = NULL) {
  if (!is.null(cvSeed)) {
    set.seed(cvSeed)
  }
  respColumn <- which(colnames(trainDat) == respName)
  ytrain <- trainDat[, respColumn]
  Xtrain <- trainDat[, -respColumn]
  featNames <- colnames(Xtrain)
  myform <- as.formula(paste(" ~ -1 + ", paste(featNames, collapse = " + "), sep = ""))
  Xtrain <- model.matrix(myform, Xtrain)
  Xtest <- model.matrix(myform, Xtest)
  featNames <- colnames(Xtrain)
  nalpha <- length(alphaGrid)
  lambs <- rep(NA, nalpha)
  alphaError <- rep(NA, nalpha)
  for (j in seq(nalpha)) {
    cat("alpha = ", alphaGrid[j], "\n")
    cvFit <- cv.glmnet(Xtrain, ytrain, nfolds = nfolds, alpha = alphaGrid[j], 
                       family = "binomial", type.measure = "auc")
    lambs[j] <- cvFit$lambda.min
    alphaError[j] <- cvFit$cvm[which(cvFit$lambda == cvFit$lambda.min)]
  }  
  best <- which.min(alphaError)
  bestAlpha <- alphaGrid[best]
  bestLambda <- lambs[best]
  bestFit <- glmnet(Xtrain, ytrain, alpha = bestAlpha, lambda = bestLambda, family = "binomial") 
  betas <- bestFit$beta[, 1]
  selFeatures <- featNames[betas != 0]
  pred <- predict(bestFit, newx = Xtest, s = bestLambda, type = "response")
  list(pred = pred, 
       betas = betas, 
       selFeatures = selFeatures, 
       bestAlpha = bestAlpha, 
       bestLambda = bestLambda)
}


######################################################
######################################################
######################################################


n <- 300
p <- 30

X <- matrix(rnorm(n*p), n, p)
colnames(X) <- paste("f", seq(p), sep = "")
beta <- rep(0, p)
beta[1:round(p/2)] <- 0.5

prob <- pnorm(X %*% beta)
hist(prob)

y <- rep(1, n)
y[prob <= 0.5] <- 0

y <- factor(y)

dat <- data.frame(y, X)

trainDat <- dat[seq(round(n/2)),]
testDat <- dat[-seq(round(n/2)),]


o <- EnetBinaryClassification(trainDat, Xtest = testDat[, -1], respName = "y", alphaGrid = seq(0, 1, by = 0.1))
predObj <- prediction(o$pred, testDat[, 1])

o$bestAlpha

plot(o$betas, ylim = c(min(beta, o$betas), max(beta, o$betas)))
points(beta, col = "blue", pch = 20)

o$selFeatures

## auc
performance(predObj, "auc")@y.values[[1]]

## ROC
perf <- performance(predObj, "tpr", "fpr")
plot(perf)


