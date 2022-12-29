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

#change back
k_9_fix$host_categories[k_9_fix$host_categories == '1'] <- 'birds'
k_9_fix$host_categories[k_9_fix$host_categories == '2'] <- 'cat'
k_9_fix$host_categories[k_9_fix$host_categories == '3'] <- 'cattle'
k_9_fix$host_categories[k_9_fix$host_categories == '4'] <- 'dog'
k_9_fix$host_categories[k_9_fix$host_categories == '5'] <- 'grey-headed_flying_fox'
k_9_fix$host_categories[k_9_fix$host_categories == '6'] <- "horse"
k_9_fix$host_categories[k_9_fix$host_categories == '7'] <- 'human'
k_9_fix$host_categories[k_9_fix$host_categories == '8'] <- 'pig'
k_9_fix$host_categories <- as.factor(k_9_fix$host_categories)

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
#install.packages("caret")
library(caret)
#varimp rpart
set.seed(100)
rPartMod <- train(host_categories ~ ., data=k_9_fix, method="rpart")
rpartImp <- varImp(rPartMod)
print(rpartImp)
plot(rpartImp, top = 15, main='Variable Importance')


#varimp random forest
set.seed(100)
rfMod <- train(host_categories ~ ., data=k_9_fix, method="rf")
rfImp <- varImp(rfMod)
rfImp
plot(rfImp, top = 20, main='Variable Importance')




#####################
#Recursive Feature Elimination (RFE)
library(caret)
library("randomForest")
control <- rfeControl(functions = rfFuncs, # random forest
                      method = "repeatedcv", # repeated cv
                      repeats = 5, # number of repeats
                      number = 10) # number of folds
# Features
x <- k_9_fix[,-c(1)]
# Target variable
y <-  as.factor(k_9_fix$host_categories)
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
                   sizes = c(50,100,150,200,250,300,350,400,450), 
                   rfeControl = control)
# Print the results
result_rfe1
# Print the selected features
predictors(result_rfe1)
# Print the results visually
ggplot(data = result_rfe1, metric = "Accuracy") + theme_bw()
ggplot(data = result_rfe1, metric = "Kappa") + theme_bw()

varimp_data <- data.frame(feature = row.names(varImp(result_rfe1))[1:469],
                          importance = varImp(result_rfe1)[1:469, 1])
ggplot(data = varimp_data[1:30,], 
       aes(x = reorder(feature, -importance), y = importance, fill = feature)) +
  geom_bar(stat="identity") + 
  labs(x = "Features", y = "Variable Importance", 
       title = "RFE Feature Importance [top 30]") + 
  geom_text(aes(label = round(importance, 2)), vjust=1.6, size=4) + 
  theme_bw() + theme(legend.position = "none") +
  scale_x_discrete(guide = guide_axis(n.dodge = 3)) 


# Post prediction
postResample(predict(result_rfe1, x_test), y_test)


############
#Ridge, Lasso, Elastic-Net
library(glmnet)
library(caret)
set.seed(42) 
# Features
x <- model.matrix(host_categories~. , k_9_fix)[,-1] #memory issues 
# Target variable
y <- k_9_fix$host_categories
# Training: 80%; Test: 20%
set.seed(2021)
inTrain <- createDataPartition(y, p = .80, list = FALSE)[,1]
x.train <- x[ inTrain, ]
x.test  <- x[-inTrain, ]
y.train <- y[ inTrain]
y.test  <- y[-inTrain]

#ta mean den douleuoun

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


###########

#Recursive Feature Elimination (RFE)
#check tent(700)
library(caret)
library("randomForest")
control <- rfeControl(functions = rfFuncs, # random forest
                      method = "repeatedcv", # repeated cv
                      repeats = 5, # number of repeats
                      number = 10) # number of folds

# Features
a <- list(199,1000,5000,10000)
host_categories <- k_9_test$host_categories
for(n in a){
  Subsets <- list(k_9_fix,k_9_tent)
  for(j in 1:10){
    b <- cbind(host_categories, k_9_test[,sample(1:ncol(k_9_test),n)])
    b <-list(b)
    Subsets <-append(Subsets,b)
  }

  feat_acc <- data.frame(set=numeric(), 
                          variables=numeric(),
                          Accuracy=numeric(), 
                          Kappa=numeric())
  b <- 1 
    
  for(i in Subsets){
    x <- i[,-c(1)]
    y <- as.factor(i$host_categories)
    set.seed(2021)
    inTrain <- createDataPartition(y, p = .80, list = FALSE)[,1]
    x_train <- x[ inTrain, ]
    x_test  <- x[-inTrain, ]
    y_train <- y[ inTrain]
    y_test  <- y[-inTrain]
    
    # Run RFE
    result_rfe <- rfe(x = x_train, 
                      y = y_train, 
                      sizes = seq(50,ncol(i),by=100),#posa apo ta features na parei
                      rfeControl = control)
    result_rfe
    for(j in 1:nrow(result_rfe$results) ){
      feat_acc[nrow(feat_acc)+1, ] <-c(ncol(i), 
                                       result_rfe$results$Variables[j],
                                       result_rfe$results$Accuracy[j], 
                                       result_rfe$results$Kappa[j])
      }
    
    b <- b+1
  }
}

#Plots 
No_of_Variables=as.factor(feat_acc$variables)
Size_of_Subset=as.factor(feat_acc$set)

#Accuracy line
ggplot(data=feat_acc, aes(x=No_of_Variables, y=Accuracy, group=Size_of_Subset))+
  geom_line(aes(color=Size_of_Subset),linewidth=1.5)+
  geom_point()+
  scale_color_brewer(palette="Set1")+
  theme_bw()+
  labs(color = "Size of Dataset")+
  labs(caption = "470=after Tentative fix, \n 730=before Tentative fix")+
  ggtitle("Accuracy per number of Variables")

#Kappa line
ggplot(data=feat_acc, aes(x=No_of_Variables, y=Kappa, group=Size_of_Subset))+
  geom_line(aes(color=Size_of_Subset),linewidth=1.5)+
  geom_point()+
  theme_bw()+
  labs(color = "Size of Dataset")+
  labs(caption = "470=after Tentative fix, \n 730=before Tentative fix")+
  ggtitle("Kappa per number of Variables")

#Accuracy boxplot
ggplot(data=feat_acc, aes( x=Size_of_Subset, y=Accuracy))+
  geom_boxplot(lwd=1,aes(color=Size_of_Subset)) + 
  geom_dotplot(binaxis='y', stackdir='center',dotsize=1,binwidth = 0.001)+
  theme_bw()+
  labs(caption = "470=after Boruta fix, \n 730=before Tentative fix")+
  labs(x = "Size of Dataset")+
  ggtitle("Accuracy per subset of Variables")
  



# Post prediction
postResample(predict(result_rfe, x_test), y_test)

#rm(list= c("control", "x", "x_train", "inTrain", "y", "y_test","y_train"))