#diafora tests
########################
#save and test to csv file
#these are the boruta selected features (with and without tentatives)
host_categories <- k_9_test$host_categories
k_9_tent <- cbind(host_categories, k_9_test[,c(boruta_signif)])
k_9_fix <- cbind(host_categories, k_9_test[,c(boruta_no_Tent)])
# Export DataFrame to CSV file
write_csv(k_9_tent, path="k_9_withTent.csv" )
write_csv(k_9_fix, path="k_9_noTent.csv" )
#test <- read_csv("k_9_withTent.csv")
#test$host_categories <- as.factor(test$host_categories)


#######
#encoders
rm(k_9_test)
k_9_test <- k_9[65:85, 3:4 ]

#simple
k_9_test$host_categories <- as.numeric(factor(k_9_test$host_categories)) #den 3erw poies times einai poio host

#works 
k_9_test$host_categories <- as.factor(k_9_test$host_categories)
k_9_test$common_species_names <- as.factor(k_9_test$common_species_names)
k_9_test <- data.frame(k_9_test)
k_9_test$host_categories <- encode_ordinal(k_9_test[,1,drop=FALSE], order = c('human','birds', 'pig'), out.int = TRUE)
k_9_test$common_species_names <- encode_ordinal(k_9_test[,2,drop=FALSE], order = c('human','turkey', 'pig'), out.int = TRUE)

#the best 
#encoders -- alphabetical order
library(CatEncoders)
#encode common_species_names 
lab = LabelEncoder.fit(k_9$common_species_names)
k_9$common_species_names = transform(lab, k_9$common_species_names)
#dixnei tis arxikes times
list <- unique(inverse.transform(lab, k_9$common_species_names))
ogvalues_common_species_names <- list[order(unlist(list))]
ogvalues_common_species_names

#encode host_categories
lab = LabelEncoder.fit(k_9_test$host_categories)
k_9_test$host_categories = transform(lab, k_9_test$host_categories)
#dixnei tis arxikes times
list <- unique(inverse.transform(lab, k_9$host_categories))
ogvalues_host_categories <- list[order(unlist(list))]
ogvalues_host_categories


#smaller sample size for testing
library(dplyr)
sample_k9 <- slice_sample(k_9[,-c(1:3)], n = 60 ) #n=how many rand rows


##################
#install.packages("DataExplorer")
library(DataExplorer)
plot_intro(k_9_test)
plot_bar(k_9_test)
plot_correlation(k_9_test) #too many var - 128gb
###

#############
#Variable Importance from Machine Learning Algorithms
#den xwrane 
install.packages("caret")
library(caret)
set.seed(100)
rPartMod <- train(host_categories ~ ., data=k_9_test, method="rpart")
rpartImp <- varImp(rPartMod)
print(rpartImp)

set.seed(100)
rrfMod <- train(Class ~ ., data=trainData, method="RRF")
rrfImp <- varImp(rrfMod, scale=F)
rrfImp

#####################
#Recursive Feature Elimination (RFE)
#takes too long to run 
library(caret)
library("randomForest")
control <- rfeControl(functions = rfFuncs, # random forest
                      method = "repeatedcv", # repeated cv
                      repeats = 5, # number of repeats
                      number = 10) # number of folds
# Features
x <- k_9_test[,-c(1)]
# Target variable
y <-  as.factor(k_9_test$host_categories)
# Training: 80%; Test: 20%
set.seed(2021)
inTrain <- createDataPartition(y, p = .80, list = FALSE)[,1]
x_train <- x[ inTrain, ]
x_test  <- x[-inTrain, ]
y_train <- y[ inTrain]
y_test  <- y[-inTrain]

# Run RFE
result_rfe1 <- rfe(x = x_train, 
                   y = y_train, 
                   sizes = c(32768), #65536, 98304), 
                   rfeControl = control)
# Print the results
result_rfe1
# Print the selected features
predictors(result_rfe1)
# Print the results visually
ggplot(data = result_rfe1, metric = "Accuracy") + theme_bw()
ggplot(data = result_rfe1, metric = "Kappa") + theme_bw()


############
#Ridge, Lasso, Elastic-Net
library(glmnet)
library(caret)
set.seed(42) 
# Features
x <- model.matrix(host_categories~. , k_9_temp)[,-1] #memory issues 
# Target variable
y <- k_9_test$host_categories
# Training: 80%; Test: 20%
set.seed(2021)
inTrain <- createDataPartition(y, p = .80, list = FALSE)[,1]
x.train <- x[ inTrain, ]
x.test  <- x[-inTrain, ]
y.train <- y[ inTrain]
y.test  <- y[-inTrain]


#just Ridge
alpha0.fit <- cv.glmnet(x.train, y.train, type.measure="deviance", 
                        alpha=0, family="multinomial")
alpha0.predicted <- 
  predict(alpha0.fit, s=alpha0.fit$lambda.1se, newx=x.test)
mean((y.test - alpha0.predicted)^2)

#just LASSO
alpha1.fit <- cv.glmnet(x.train, y.train, type.measure="deviance", 
                        alpha=1, family="multinomial")

alpha1.predicted <- 
  predict(alpha1.fit, s=alpha1.fit$lambda.1se, newx=x.test)

mean((y.test - alpha1.predicted)^2)

#just Elastic net with a=0.5
alpha0.5.fit <- cv.glmnet(x.train, y.train, type.measure="mse", 
                          alpha=0.5, family="gaussian")

alpha0.5.predicted <- 
  predict(alpha0.5.fit, s=alpha0.5.fit$lambda.1se, newx=x.test)

mean((y.test - alpha0.5.predicted)^2)

#all together - error
list.of.fits <- list()
for (i in 0:10) {
  fit.name <- paste0("alpha", i/10)
  list.of.fits[[fit.name]] <-
    cv.glmnet(x.train, y.train, type.measure="deviance", alpha=i/10, 
              family="multinomial")
  #t.m = deviance, family= binomial gia logistic regression
}
results <- data.frame()
for (i in 0:10) {
  fit.name <- paste0("alpha", i/10)
  ## Use each model to predict 'y' given the Testing dataset
  predicted <- 
    predict(list.of.fits[[fit.name]], 
            s=list.of.fits[[fit.name]]$lambda.1se, newx=x.test)
  ## Calculate the Mean Squared Error...
  mse <- mean((y.test - predicted)^2)
  ## Store the results
  temp <- data.frame(alpha=i/10, mse=mse, fit.name=fit.name)
  results <- rbind(results, temp)
}
results


##############################
#Random Forest try
library(ggplot2)
library(cowplot)
library(randomForest)
set.seed(42)

k_9_test$host_categories <- as.factor(k_9_test$host_categories)
#do i need this?
#data.imputed <- rfImpute(host_categories ~ ., data = k_9_test, iter=6) 

model <- randomForest(host_categories ~ ., data=k_9_test, proximity=TRUE)
model

oob.error.data <- data.frame(
  Trees=rep(1:nrow(model$err.rate), times=3),
  Type=rep(c("OOB", "Healthy", "Unhealthy"), each=nrow(model$err.rate)),
  Error=c(model$err.rate[,"OOB"], 
          model$err.rate[,"Healthy"], 
          model$err.rate[,"Unhealthy"]))

ggplot(data=oob.error.data, aes(x=Trees, y=Error)) +
  geom_line(aes(color=Type))


## If we want to compare this random forest to others with different values for
## mtry (to control how many variables are considered at each step)...
oob.values <- vector(length=10)
for(i in 1:10) {
  temp.model <- randomForest(hd ~ ., data=data.imputed, mtry=i, ntree=1000)
  oob.values[i] <- temp.model$err.rate[nrow(temp.model$err.rate),1]
}
oob.values
## find the minimum error
min(oob.values)
## find the optimal value for mtry...
which(oob.values == min(oob.values))
## create a model for proximities using the best value for mtry
model <- randomForest(hd ~ ., 
                      data=data.imputed,
                      ntree=1000, 
                      proximity=TRUE, 
                      mtry=which(oob.values == min(oob.values)))


## Now let's create an MDS-plot to show how the samples are related to each 
## other.
##
## Start by converting the proximity matrix into a distance matrix.
distance.matrix <- as.dist(1-model$proximity)

mds.stuff <- cmdscale(distance.matrix, eig=TRUE, x.ret=TRUE)

## calculate the percentage of variation that each MDS axis accounts for...
mds.var.per <- round(mds.stuff$eig/sum(mds.stuff$eig)*100, 1)

## now make a fancy looking plot that shows the MDS axes and the variation:
mds.values <- mds.stuff$points
mds.data <- data.frame(Sample=rownames(mds.values),
                       X=mds.values[,1],
                       Y=mds.values[,2],
                       Status=data.imputed$hd)

ggplot(data=mds.data, aes(x=X, y=Y, label=Sample)) + 
  geom_text(aes(color=Status)) +
  theme_bw() +
  xlab(paste("MDS1 - ", mds.var.per[1], "%", sep="")) +
  ylab(paste("MDS2 - ", mds.var.per[2], "%", sep="")) +
  ggtitle("MDS plot using (1 - Random Forest Proximities)")
# ggsave(file="random_forest_mds_plot.pdf")
