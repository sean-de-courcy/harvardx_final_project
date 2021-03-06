if (!require("Rtsne", quietly = TRUE)){install.packages("Rtsne")}
if (!require("tidyverse", quietly = TRUE)){install.packages("tidyverse")}
if (!require("ggthemes", quietly = TRUE)){install.packages("ggthemes")}
if (!require("caret", quietly = TRUE)){install.packages("caret")}
if (!require("pbapply", quietly = TRUE)){install.packages("pbapply")}
if (!require("rpart", quietly = TRUE)){install.packages("rpart")}
if (!require("rpart.plot", quietly = TRUE)){install.packages("rpart.plot")}
if (!require("ggridges", quietly = TRUE)){install.packages("ggridges")}

library(caret)
library(tidyverse)
library(ggthemes)
library(Rtsne)
library(ggridges)
library(pbapply)
library(rpart)
library(rpart.plot)

### Control vs Case ###
## Importaing Data ##

ctrl_vs_case.genes <- read.csv("ctrl_vs_case.csv") # ctrl = 0, case = 1


## For filtering out generally un-transcribed genes ##

filter.col_mean.cvc <- apply(select(ctrl_vs_case.genes, -c(CtrlVsCase_Classifier, Participant_ID)), 2, function(x) mean(x)>10)
filter.col_mean.cvc <- c(TRUE, TRUE, filter.col_mean.cvc)


## Test index = 1/3 data because small data set ##

test_index.cvc <- sample(c(TRUE, FALSE), size = length(ctrl_vs_case.genes$CtrlVsCase_Classifier), prob = c(0.33, 0.67), replace = TRUE)

test.cvc.genes <- select(ctrl_vs_case.genes[test_index.cvc, filter.col_mean.cvc], -Participant_ID)
train.cvc.genes <- select(ctrl_vs_case.genes[-test_index.cvc, filter.col_mean.cvc], -Participant_ID)

## Z-test for sig diff in gene transcript dist in ctrl vs case ##
filter.case <- (train.cvc.genes$CtrlVsCase_Classifier == 1)
train.cvc.genes.z_test <- apply(select(train.cvc.genes, -CtrlVsCase_Classifier), 2,
                                       function(x) (mean(x[filter.case]) - mean(x[-filter.case]))/sqrt(sd(x[filter.case])^2 + sd(x[-filter.case])^2))

filter.train.cvc.genes.sig_diff <- abs(train.cvc.genes.z_test - mean(train.cvc.genes.z_test)) >= 3.1*sd(train.cvc.genes.z_test)

## T-SNE ##
train.cvc.genes.tsne <- Rtsne(train.cvc.genes[,c(FALSE, filter.train.cvc.genes.sig_diff)])
train.cvc.genes.tsne$Y <- data.frame(train.cvc.genes.tsne$Y) %>% mutate(CtrlVsCase_Classifier = train.cvc.genes$CtrlVsCase_Classifier)

plot.train.cvc.genes.tsne <- train.cvc.genes.tsne$Y %>% ggplot(aes(x = X1, y = X2, color = as.factor(CtrlVsCase_Classifier))) + geom_point() + labs(color = 'Ctrl = 0, Case = 1')
plot.train.cvc.genes.tsne

## PCA ##
train.cvc.genes.pca <- prcomp(train.cvc.genes[,c(FALSE, filter.train.cvc.genes.sig_diff)], center = TRUE, scale. = TRUE)
train.cvc.genes.pca$x <- data.frame(train.cvc.genes.pca$x) %>% mutate(CtrlVsCase_Classifier = train.cvc.genes$CtrlVsCase_Classifier)

plot.train.cvc.genes.pca <- train.cvc.genes.pca$x %>% ggplot(aes(x = PC1, y = PC2, color = as.factor(ifelse(CtrlVsCase_Classifier == 1, "Case", "Control")))) + geom_point() + labs(color = "ALS Status") + theme_few() + scale_color_economist() + theme(legend.position = "right")
plot.train.cvc.genes.pca

## LDA ##
filter.ctrl <- (train.cvc.genes$CtrlVsCase_Classifier == 0)
extras <- sum(train.cvc.genes$CtrlVsCase_Classifier == 1) - sum(train.cvc.genes$CtrlVsCase_Classifier == 0)
len <- length(train.cvc.genes$CtrlVsCase_Classifier)
I <- 1:extras
weighted.train.cvc.genes <- train.cvc.genes[,c(TRUE, filter.train.cvc.genes.sig_diff)]
for (i in I) {
  weighted.train.cvc.genes[len + i,] <- apply(train.cvc.genes[filter.ctrl,c(TRUE, filter.train.cvc.genes.sig_diff)], 2, function(x) sample(x, size = 1, replace = TRUE))
}
rm(extras, len, i, I)
lda.train.cvc.genes <- train(as.factor(CtrlVsCase_Classifier) ~ ., method = "lda", data = weighted.train.cvc.genes)

prediction.lda.train.cvc.genes <- predict(lda.train.cvc.genes, test.cvc.genes[,c(FALSE, filter.train.cvc.genes.sig_diff)])
lda.cvc.balancedAccuracy <- confusionMatrix(prediction.lda.train.cvc.genes, as.factor(test.cvc.genes$CtrlVsCase_Classifier), positive = '1')$byClass[['Balanced Accuracy']]

cvc.genes <- select(ctrl_vs_case.genes[,filter.col_mean.cvc], -Participant_ID)
cvc.genes$X1 <- apply(cvc.genes[,c(FALSE, filter.train.cvc.genes.sig_diff)], 1, function(x) sum(x*lda.train.cvc.genes$finalModel$scaling))

lda.cutoff.cvc <- (sum(lda.train.cvc.genes$finalModel$means[1,]*lda.train.cvc.genes$finalModel$scaling) + sum(lda.train.cvc.genes$finalModel$means[2,]*lda.train.cvc.genes$finalModel$scaling))/2

plot.cvc.jitter.lda <- cvc.genes %>% ggplot(aes(x = X1, y = rep(0, times = length(X1)), color = as.factor(ifelse(CtrlVsCase_Classifier == 1, "Case", "Control")))) +
  geom_hline(yintercept = - 0.15, color = 'black') +
  geom_density_ridges(jittered_points = TRUE, position = position_raincloud(height = 0.2, ygap = 0.05), fill = '#00000011') +
  geom_density(inherit.aes = FALSE, mapping = aes(x = X1, y = (..count.. * -1)/max(..count..) - 0.3, color = as.factor(ifelse(CtrlVsCase_Classifier == 1, "Case", "Control"))), fill = '#00000011') +
  geom_vline(xintercept = lda.cutoff.cvc, color = 'black', linetype = 'dashed') + labs(color = "ALS Status") + theme_few() + scale_color_economist() +
  scale_fill_economist() + theme(legend.position = "top", axis.title.y = element_blank(), axis.ticks.y = element_blank(), axis.text.y = element_blank()) + xlim(min(cvc.genes$X1), max(cvc.genes$X1))
plot.cvc.jitter.lda

## Classification Tree ##
tree.train.cvc.genes <- train(as.factor(ifelse(CtrlVsCase_Classifier == 1, 'Case', 'Control')) ~ ., method = 'rpart', tuneGrid = data.frame(cp = seq(0.01, 0.05, 5)), data = weighted.train.cvc.genes)
prediction.tree.train.cvc.genes <- predict(tree.train.cvc.genes, newdata = test.cvc.genes)
tree.cvc.balancedAccuracy <- confusionMatrix(prediction.tree.train.cvc.genes, as.factor(ifelse(test.cvc.genes$CtrlVsCase_Classifier == 1, 'Case', 'Control')), positive = 'Case')$byClass[['Balanced Accuracy']]
prp(tree.train.cvc.genes$finalModel, box.palette = 'BuGy')

## Monte Carlo Assessment ##
cvc.genes_nullHypothesisTrial <- function(){
  randomized <- cvc.genes
  num_cols <- length(randomized)
  for (i in 2:num_cols) {
    randomized[,i] <- sample(randomized[,i], replace = TRUE)
  }
  test_index.r <- sample(c(TRUE, FALSE), size = length(randomized$CtrlVsCase_Classifier), replace = TRUE, prob = c(0.33, 0.67))
  test.randomized <- randomized[test_index.r,]
  train.randomized <- randomized[-test_index.r,]
  filter.case.r <- (train.randomized$CtrlVsCase_Classifier == 1)
  train.randomized.z_test <- apply(select(train.randomized, -CtrlVsCase_Classifier), 2,
                                  function(x) (mean(x[filter.case.r]) - mean(x[-filter.case.r]))/sqrt(sd(x[filter.case.r])^2 + sd(x[-filter.case.r])^2))
  filter.randomized.sig_diff <- abs(train.randomized.z_test - mean(train.randomized.z_test)) >= 3.1*sd(train.randomized.z_test)
  extras <- sum(filter.case.r) - sum(!filter.case.r)
  weighted.train.randomized <- train.randomized[,c(TRUE, filter.randomized.sig_diff)]
  num_rows <- length(train.randomized$CtrlVsCase_Classifier)
  for (i in 1:extras) {
    weighted.train.randomized[num_rows + i,] <- apply(train.randomized[!filter.case.r,c(TRUE, filter.randomized.sig_diff)], 2, function(x) sample(x, size = 1))
  }
  lda.train.randomized <- train(as.factor(CtrlVsCase_Classifier)~., method = 'lda', data = weighted.train.randomized)
  prediction.lda.train.randomized <- predict(lda.train.randomized, test.randomized[,c(FALSE, filter.randomized.sig_diff)])
  lda.randomized.balancedAccuracy <- confusionMatrix(prediction.lda.train.randomized, as.factor(test.randomized$CtrlVsCase_Classifier), positive = '1')$byClass[['Balanced Accuracy']]
  tree.train.randomized <- train(as.factor(CtrlVsCase_Classifier) ~ ., method = 'rpart', tuneGrid = data.frame(cp = seq(0.01, 0.05, 5)), data = weighted.train.randomized)
  prediction.tree.train.randomized <- predict(tree.train.randomized, newdata = test.randomized)
  tree.randomized.balancedAccuracy <- confusionMatrix(prediction.tree.train.randomized, as.factor(test.randomized$CtrlVsCase_Classifier), positive = '1')$byClass[['Balanced Accuracy']]
  return(c(lda.randomized.balancedAccuracy >= lda.cvc.balancedAccuracy, tree.randomized.balancedAccuracy >= tree.cvc.balancedAccuracy))
}
cvc.monteCarloResults <- pbreplicate(100, cvc.genes_nullHypothesisTrial())
cvc.p_value.lda <- mean(cvc.monteCarloResults[1,])
cvc.conf_int <- 1.96*sd(cvc.monteCarloResults[1,])
cvc.p_value.tree <- mean(cvc.monteCarloResults[2,])
cvc.conf_int.tree <- 1.96*sd(cvc.monteCarloResults[2,])


### Bulbar vs Limb ###
## Importing Data ##
bulbar_vs_limb.genes <- read.csv("bulbar_vs_limb.csv") # bulbar = 0, limb = 1

filter.col_mean.bvl <- apply(select(bulbar_vs_limb.genes, -c(SiteOnset_Class, Participant_ID)), 2, function(x) mean(x)>10)
filter.col_mean.bvl <- c(TRUE, TRUE, filter.col_mean.bvl)

## Test Index ##
test_index.bvl <- sample(c(TRUE, FALSE), size = length(bulbar_vs_limb.genes$SiteOnset_Class), replace = TRUE, prob = c(0.33, 0.67))

test.bvl.genes <- select(bulbar_vs_limb.genes[test_index.bvl, filter.col_mean.bvl], -Participant_ID)
train.bvl.genes <- select(bulbar_vs_limb.genes[-test_index.bvl, filter.col_mean.bvl], -Participant_ID)

## Z-test ##
filter.limb <- (train.bvl.genes$SiteOnset_Class == 1)
train.bvl.genes.z_test <- apply(select(train.bvl.genes, -SiteOnset_Class), 2,
                                function(x) (mean(x[filter.limb]) - mean(x[-filter.limb]))/sqrt(sd(x[filter.limb])^2 + sd(x[-filter.limb])^2))

filter.train.bvl.genes.sig_diff <- abs(train.bvl.genes.z_test - mean(train.bvl.genes.z_test)) >= 3.1*sd(train.bvl.genes.z_test)

## T-SNE ##
train.bvl.genes.tsne <- Rtsne(train.bvl.genes[,c(FALSE, filter.train.bvl.genes.sig_diff)])
train.bvl.genes.tsne$Y <- data.frame(train.bvl.genes.tsne$Y) %>% mutate(SiteOnset_Class = train.bvl.genes$SiteOnset_Class)

plot.train.bvl.genes.tsne <- train.bvl.genes.tsne$Y %>% ggplot(aes(x = X1, y = X2, color = as.factor(SiteOnset_Class))) + geom_point() + labs(color = 'Ctrl = 0, Case = 1')
plot.train.bvl.genes.tsne

## PCA ##
train.bvl.genes.pca <- prcomp(train.bvl.genes[,c(FALSE, filter.train.bvl.genes.sig_diff)], center = TRUE, scale. = TRUE)
train.bvl.genes.pca$x <- data.frame(train.bvl.genes.pca$x) %>% mutate(SiteOnset_Class = train.bvl.genes$SiteOnset_Class)

plot.train.bvl.genes.pca <- train.bvl.genes.pca$x %>% ggplot(aes(x = PC1, y = PC2, color = as.factor(ifelse(SiteOnset_Class == 1, "Case", "Control")))) + geom_point() + labs(color = "ALS Status") + theme_few() + scale_color_economist() + theme(legend.position = "right")
plot.train.bvl.genes.pca

## LDA ##
filter.bulb <- (train.bvl.genes$SiteOnset_Class == 0)
extras <- sum(train.bvl.genes$SiteOnset_Class == 1) - sum(train.bvl.genes$SiteOnset_Class == 0)
len <- length(train.bvl.genes$SiteOnset_Class)
I <- 1:extras
weighted.train.bvl.genes <- train.bvl.genes[,c(TRUE, filter.train.bvl.genes.sig_diff)]
for (i in I) {
  weighted.train.bvl.genes[len + i,] <- apply(train.bvl.genes[filter.bulb,c(TRUE, filter.train.bvl.genes.sig_diff)], 2, function(x) sample(x, size = 1, replace = TRUE))
}
rm(extras, len, i, I)
lda.train.bvl.genes <- train(as.factor(SiteOnset_Class) ~ ., method = "lda", data = weighted.train.bvl.genes)

prediction.lda.train.bvl.genes <- predict(lda.train.bvl.genes, test.bvl.genes[,c(FALSE, filter.train.bvl.genes.sig_diff)])
lda.bvl.balancedAccuracy <- confusionMatrix(prediction.lda.train.bvl.genes, as.factor(test.bvl.genes$SiteOnset_Class), positive = '1')$byClass[['Balanced Accuracy']]

bvl.genes <- select(bulbar_vs_limb.genes[,filter.col_mean.bvl], -Participant_ID)
bvl.genes$X1 <- apply(bvl.genes[,c(FALSE, filter.train.bvl.genes.sig_diff)], 1, function(x) sum(x*lda.train.bvl.genes$finalModel$scaling))

lda.cutoff.bvl <- (sum(lda.train.bvl.genes$finalModel$means[1,]*lda.train.bvl.genes$finalModel$scaling) + sum(lda.train.bvl.genes$finalModel$means[2,]*lda.train.bvl.genes$finalModel$scaling))/2

plot.bvl.jitter.lda <- bvl.genes %>% ggplot(aes(x = X1, y = rep(0, times = length(X1)), color = as.factor(ifelse(SiteOnset_Class == 1, "Case", "Control")))) +
  geom_hline(yintercept = - 0.15, color = 'black') +
  geom_density_ridges(jittered_points = TRUE, position = position_raincloud(height = 0.2, ygap = 0.05), fill = '#00000011') +
  geom_density(inherit.aes = FALSE, mapping = aes(x = X1, y = (..count.. * -1)/max(..count..) - 0.3, color = as.factor(ifelse(SiteOnset_Class == 1, "Case", "Control"))), fill = '#00000011') +
  geom_vline(xintercept = lda.cutoff.bvl, color = 'black', linetype = 'dashed') + labs(color = "ALS Status") + theme_few() + scale_color_economist() +
  scale_fill_economist() + theme(legend.position = "top", axis.title.y = element_blank(), axis.ticks.y = element_blank(), axis.text.y = element_blank()) + xlim(min(bvl.genes$X1), max(bvl.genes$X1))
plot.bvl.jitter.lda

## Classification Tree ##
tree.train.bvl.genes <- train(as.factor(ifelse(SiteOnset_Class == 1, 'Limb', 'Bulbar')) ~ ., method = 'rpart', tuneGrid = data.frame(cp = seq(0.01, 0.01, len = 5)), data = weighted.train.bvl.genes)
prediction.tree.train.bvl.genes <- predict(tree.train.bvl.genes, newdata = test.bvl.genes)
tree.bvl.balancedAccuracy <- confusionMatrix(prediction.tree.train.bvl.genes, as.factor(ifelse(test.bvl.genes$SiteOnset_Class == 1, 'Limb', 'Bulbar')), positive = 'Limb')$byClass[['Balanced Accuracy']]
prp(tree.train.bvl.genes$finalModel, box.palette = 'BuGy')

## Monte Carlo Assessment ##
bvl.genes_nullHypothesisTrial <- function(){
  randomized <- bvl.genes
  num_cols <- length(randomized)
  for (i in 2:num_cols) {
    randomized[,i] <- sample(randomized[,i], replace = TRUE)
  }
  test_index.r <- sample(c(TRUE, FALSE), size = length(randomized$SiteOnset_Class), replace = TRUE, prob = c(0.33, 0.67))
  test.randomized <- randomized[test_index.r,]
  train.randomized <- randomized[-test_index.r,]
  filter.case.r <- (train.randomized$SiteOnset_Class == 1)
  train.randomized.z_test <- apply(select(train.randomized, -SiteOnset_Class), 2,
                             function(x) (mean(x[filter.case.r]) - mean(x[-filter.case.r]))/sqrt(sd(x[filter.case.r])^2 + sd(x[-filter.case.r])^2))
  filter.randomized.sig_diff <- abs(train.randomized.z_test - mean(train.randomized.z_test)) >= 3.1*sd(train.randomized.z_test)
  extras <- sum(filter.case.r) - sum(!filter.case.r)
  weighted.train.randomized <- train.randomized[,c(TRUE, filter.randomized.sig_diff)]
  num_rows <- length(train.randomized$SiteOnset_Class)
  for (i in 1:extras) {
    weighted.train.randomized[num_rows + i,] <- apply(train.randomized[!filter.case.r,c(TRUE, filter.randomized.sig_diff)], 2, function(x) sample(x, size = 1))
  }
  lda.train.randomized <- train(as.factor(SiteOnset_Class)~., method = 'lda', data = weighted.train.randomized)
  prediction.lda.train.randomized <- predict(lda.train.randomized, test.randomized[,c(FALSE, filter.randomized.sig_diff)])
  lda.randomized.balancedAccuracy <- confusionMatrix(prediction.lda.train.randomized, as.factor(test.randomized$SiteOnset_Class), positive = '1')$byClass[['Balanced Accuracy']]
  tree.train.randomized <- train(as.factor(SiteOnset_Class)~., method = 'rpart', tuneGrid = data.frame(cp = seq(0.01, 0.05, 5)), data = weighted.train.randomized)
  prediction.tree.train.randomized <- predict(tree.train.randomized, newdata = test.randomized)
  tree.randomized.balancedAccuracy <- confusionMatrix(prediction.tree.train.randomized, as.factor(test.randomized$SiteOnset_Class), positive = '1')$byClass[['Balanced Accuracy']]
  return(c(lda.randomized.balancedAccuracy >= lda.bvl.balancedAccuracy, tree.randomized.balancedAccuracy >= tree.bvl.balancedAccuracy))
}
bvl.monteCarloResults <- pbreplicate(100, bvl.genes_nullHypothesisTrial())
bvl.p_value.lda <- mean(bvl.monteCarloResults[1,])
bvl.conf_int.lda <- 1.96*sd(bvl.monteCarloResults[1,])
bvl.p_value.tree <- mean(bvl.monteCarloResults[2,])
bvl.conf_int.tree <- 1.96*sd(bvl.monteCarloResults[2,])
