#diafora tests

#clean environment everytime so run what you need from the start
#rm(list=ls()) 
#rm(list=c())
#& 
#gc() #for cleaning


summary(k_9)
str(k_9)
View(k_9)


########################
#save and test to csv file

library(readr)
#these are the boruta selected features (with and without tentatives)
host_categories <- k_9_test$host_categories
k_9_tent <- cbind(host_categories, k_9_test[,c(boruta_signif)])
k_9_fix <- cbind(host_categories, k_9_test[,c(boruta_no_Tent)])
# Export DataFrame to CSV file
write_csv(k_9_tent, path="k_9_withTent.csv" )
write_csv(k_9_fix, path="k_9_noTent.csv" )
#import
k_9_fix <- read_csv("k_9_noTent.csv")
k_9_tent <- read_csv("k_9_withTent.csv")
k_9_fix$host_categories <- as.factor(k_9_fix$host_categories)
k_9_tent$host_categories <- as.factor(k_9_tent$host_categories)

#change fox name when host_categories is factor
levels(k_9_fix$host_categories)[levels(k_9_fix$host_categories) == 'grey-headed_flying_fox'] <- 'fox'
levels(k_9_tent$host_categories)[levels(k_9_tent$host_categories) == 'grey-headed_flying_fox'] <- 'fox'
levels(k_9_test$host_categories)[levels(k_9_test$host_categories) == 'grey-headed_flying_fox'] <- 'fox'

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
k_9_fix$host_categories[k_9_fix$host_categories == '5'] <- 'grey-headed_flying_fox' #or 'fox'
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
#use this to find best subset of features 
#given the output of boruta - 470 vs 730

library(caret)
library("randomForest")
library(ggplot2)
control <- rfeControl(functions = rfFuncs, # random forest
                      method = "repeatedcv", # repeated cv
                      repeats = 5, # number of repeats
                      number = 10) # number of folds

# Features
a <- list(k_9_fix,k_9_tent)#dokimase na to baleis kateu8eian
b <- 1 
feat_acc <- data.frame(subset    = numeric(), 
                       variables = numeric(),
                       Accuracy  = numeric(), 
                       Kappa     = numeric())
for(i in a){
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
                    sizes = seq(50,ncol(i),by=50),
                    rfeControl = control)
  result_rfe
  for(j in 1:nrow(result_rfe$results) ){
    feat_acc[nrow(feat_acc)+1, ] <-c(ncol(i), 
                                     result_rfe$results$Variables[j],
                                     result_rfe$results$Accuracy[j], 
                                     result_rfe$results$Kappa[j])
  }
  # Post prediction
  postResample(predict(result_rfe, x_test), y_test)
  
  b <- b+1
}
#Plots 
No_of_Variables=as.factor(feat_acc$variables)
Size_of_Subset=as.factor(feat_acc$subset)

#Accuracy line
ggplot(data=feat_acc, aes(x=No_of_Variables, y=Accuracy, group=Size_of_Subset))+
  geom_line(aes(color=Size_of_Subset),size = 1.2)+
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
  geom_boxplot(lwd=1,aes(color=Size_of_Subset),show.legend = FALSE) + 
  geom_dotplot(binaxis='y', stackdir='center',dotsize=0.75,binwidth = 0.001)+
  theme_bw()+
  labs(caption = "470=after Boruta fix, \n 730=before Tentative fix")+
  labs(x = "Size of Dataset")+
  ggtitle("Accuracy per subset of Boruta-Variables")

######
# how to make a list of dataframes
#datalist <- list()
#for (i in 1:8){
 # error.mtry <- data.frame(Trees=numeric(), Error=numeric())
  #...
 # datalist[i] <- list(error.mtry)
#}
#datalist <- setNames(datalist, hosts)


#########

#LOOP

#Recursive Feature Elimination (RFE)
#use this to find best subset of features 
#given the boruta output and random subsets of various sizes
library(caret)
library("randomForest")
control <- rfeControl(functions = rfFuncs, # random forest
                      method = "repeatedcv", # repeated cv
                      repeats = 5, # number of repeats
                      number = 10) # number of folds

# Features
a <- list(199,1000,5000,10000)
host_categories <- k_9_test$host_categories
feat_acc <- data.frame(set=numeric(), 
                       variables=numeric(),
                       Accuracy=numeric(), 
                       Kappa=numeric())
for(n in a){
  Subsets <- list(k_9_fix,k_9_tent)
  for(j in 1:10){
    b <- cbind(host_categories, k_9_test[,sample(1:ncol(k_9_test),n)])
    b <-list(b)
    Subsets <-append(Subsets,b)
  }
  
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
  }
}

#Plots 
No_of_Variables=as.factor(feat_acc$variables)
Size_of_Subset=as.factor(feat_acc$set)

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


#####

#NEW

#Recursive Feature Elimination (RFE)
#use this to find best subset of features 
#given the boruta output and random subsets of various sizes
library(caret)
library("randomForest")

control <- rfeControl(functions = rfFuncs, # random forest
                      method = "repeatedcv", # repeated cv
                      repeats = 5, # number of repeats
                      number = 10) # number of folds

feat_acc <- data.frame(set=numeric(),       #data for the boxplot
                       variables=numeric(),
                       Accuracy=numeric(), 
                       Kappa=numeric())

host_categories <- k_9_test$host_categories

##here we change the n for each subset we test 
##there is code in "testfile.R" to do this in a for-loop,but
##it takes days. I broke it up as a security measure

##for the first run of the model keep the "Subsets" just the boruta sets
##(so do not run the first for loop below) 
##so the end results have run these sets the same amount as the rest (~100x)


#size of subsets to plot/compare
#change n for each subset
#values : 199,999,4999,9999
n <- 0

#create 10 of each subset of features + the boruta sets
Subsets <- list(k_9_fix,k_9_tent) #just the boruta sets
for(i in 1:10){
  j <- cbind(host_categories, k_9_test[,sample(1:ncol(k_9_test),n)])
  j <- list(j)
  Subsets <-append(Subsets,j) #boruta+new subset
}

#run this for all subsets (boruta, 199, 999, 4999, 9999)
#and collect the model output needed of each subset
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
                    sizes = seq(50, ncol(i), by=1000),#posa apo ta features na parei
                    rfeControl = control)
  #boruta:by=10,  200:by=20,  1000:by=100,  5000:by=500,  10000:by=1000   
  
  for(j in 1:nrow(result_rfe$results) ){
    feat_acc[nrow(feat_acc)+1, ] <-c(ncol(i), 
                                     result_rfe$results$Variables[j],
                                     result_rfe$results$Accuracy[j], 
                                     result_rfe$results$Kappa[j])
  }
}

#Plots 
No_of_Variables=as.factor(feat_acc$variables)
Size_of_Subset=as.factor(feat_acc$set)

#Accuracy boxplot
ggplot(data=feat_acc, aes( x=Size_of_Subset, y=Accuracy))+
  geom_boxplot(lwd=1,aes(color=Size_of_Subset)) + 
  geom_dotplot(binaxis='y', stackdir='center',dotsize=0.75,binwidth = 0.001)+
  theme_bw()+
  labs(caption = "Boruta fix for Tentatives: \n 470=after fix, 730=before fix")+
  labs(x = "Size of Dataset")+
  ggtitle("Accuracy per subset of Variables")

#Kappa boxplot
ggplot(data=feat_acc, aes( x=Size_of_Subset, y=Kappa))+
  geom_boxplot(lwd=1,aes(color=Size_of_Subset)) + 
  geom_dotplot(binaxis='y', stackdir='center',dotsize=1,binwidth = 0.001)+
  theme_bw()+
  scale_color_brewer(palette="Dark2")+
  labs(caption = "Boruta fix for Tentatives: \n 470=after fix, 730=before fix")+
  labs(x = "Size of Dataset")+
  ggtitle("Kappa per subset of Variables")



#For lines
#dokimase prwta mono to 199
#a <- list(199,1000,5000,10000)
a <- 199
Subsets <- list(k_9_fix,k_9_tent)
host_categories <- k_9_test$host_categories
feat_acc <- data.frame(set=numeric(), 
                       variables=numeric(),
                       Accuracy=numeric(), 
                       Kappa=numeric())
for(j in a){
  b <- cbind(host_categories, k_9_test[,sample(1:ncol(k_9_test),a)])
  b <-list(b)
  Subsets <-append(Subsets,b)    
}


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
}

#Plots 
No_of_Variables=as.factor(feat_acc$variables)
Size_of_Subset=as.factor(feat_acc$set)

#Accuracy line for 1 of each subset
ggplot(data=feat_acc, aes(x=No_of_Variables, y=Accuracy, group=Size_of_Subset))+
  geom_line(aes(color=Size_of_Subset),linewidth=1.5)+
  geom_point()+
  scale_color_brewer(palette="Set1")+
  theme_bw()+
  labs(color = "Size of Dataset")+
  labs(caption = "470=after Tentative fix, \n 730=before Tentative fix")+
  ggtitle("Accuracy per number of Variables")

# Post prediction
#postResample(predict(result_rfe, x_test), y_test)

#rm(list= c("control", "x", "x_train", "inTrain", "y", "y_test","y_train"))


testdataline <- feat_acc
testdataline <- testdataline[c(seq(1,43, by=5), 43, 
                               seq(44,112, by=5), 112, 
                               170:178, 
                               274:284, 
                               389:399, 
                               503:513),]
#Plots 
No_of_Variables=as.factor(testdataline$variables)
Size_of_Subset=as.factor(testdataline$set)

#Accuracy line for 1 of each subset
ggplot(data=testdataline, aes(x=No_of_Variables, y=Accuracy, group=Size_of_Subset))+
  geom_line(aes(color=Size_of_Subset),linewidth=1.5)+
  geom_point()+
  #scale_color_brewer(palette="Set1")+
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  theme_bw()+
  labs(color = "Size of Dataset")+
  labs(caption = "470=after Tentative fix, \n 730=before Tentative fix")+
  ggtitle("Accuracy per number of Variables")

##############
#temp 
#single ova

#there are comments in the code for 
#each dataset 

#to save the variables' importance
ovaimp_470 <- list()  
ovaimp_284 <- list()

#temporary dataset
#temp_fix <- k_9_fix #this for 470
#or
temp_fix <- fix    #this for 285

hosts <- unique(k_9_fix$host_categories)
hosts <- as.character(hosts)

#change for each host
#1=human, 2=cattle, 3=birds, 4=pig,
#5=horse, 6=dog,    7=cat,   8=fox,
i <- 8

temp_fix$host_categories <- ifelse(k_9_fix$host_categories==hosts[i],hosts[i],"other")
temp_fix$host_categories <- as.factor(temp_fix$host_categories)

#default parameters
ova_model <- randomForest(host_categories ~ ., data=temp_fix, proximity=TRUE)
ova_model

error.mtry <- data.frame(Host = character(), Trees=numeric(), Error=numeric())
a <- 0
for(y in c(500,1000)){
  c <- 0
  for(j in 1:32) #470->1:42, 284->1:32
    {
    ova_model <- randomForest(host_categories ~ ., data=temp_fix, mtry=j, ntree=y)
    c <- j + a
    error.mtry[c,] <- c(hosts[i],
                        y,
                        ova_model$err.rate[nrow(ova_model$err.rate),1])
                            #the oob err at max trees
  }
  #470->42, 284->32
  a <- a+32
}

min_error <- min(error.mtry$Error)
pos <- which(error.mtry$Error == min_error)
best_trees <- as.numeric(error.mtry$Trees[pos[1]])

#this is for printing the right mtry 
#470->42, 284->32
if(pos[1]>32)
{
  min_mtry <- pos[1] - 32
} else {
  min_mtry <- pos[1]
}

#print 
min_error 
min_mtry <-18
best_trees

## final model using the best ntree and the best mtry

#run this until oob error is smaller than the default
ova_model <- randomForest(host_categories ~ ., 
                          data=temp_fix,
                          ntree=best_trees,
                          proximity=TRUE, 
                          mtry=min_mtry)
ova_model

#save the most imp features after the best accuracy above is found

#a list of the top 20 most important k-mers for each host
#ovaimp_470[i] <- list(ova_model$importance[order(ova_model$importance[,1],decreasing=TRUE),][1:20])
#or 
ovaimp_284[i] <- list(ova_model$importance[order(ova_model$importance[,1],decreasing=TRUE),][1:20])

#MDS-plot - how the samples are related to each other
#proximity matrix -> distance matrix
distance.matrix <- as.dist(1-ova_model$proximity)
mds.stuff <- cmdscale(distance.matrix, eig=TRUE, x.ret=TRUE)
# calculate the percentage of variation that each MDS axis accounts for
mds.var.per <- round(mds.stuff$eig/sum(mds.stuff$eig)*100, 1)
#plot that shows the MDS axes and the variation:
mds.values <- mds.stuff$points
mds.data <- data.frame(Sample=rownames(mds.values),
                       X=mds.values[,1],
                       Y=mds.values[,2],
                       Hosts=temp_fix$host_categories) 

ggplot(data=mds.data, aes(x=X, y=Y,color=Hosts)) + 
  theme_bw() +
  geom_point(alpha = 8/10, size=2,)+
  scale_color_brewer(palette="Set1")+
  xlab(paste("MDS1 - ", mds.var.per[1], "%", sep="")) +
  ylab(paste("MDS2 - ", mds.var.per[2], "%", sep="")) +
  ggtitle(paste("MDS for", hosts[i],"vs the rest - 284")) #-470 or -284


#now run again with new i for next host 


#when all host are computed 
#print the variable importances for each one

#fix - 284

#name the list
ovaimp_284 <- setNames(ovaimp_284, hosts)
#plot the imp features 
for(i in 1:8){
  par(mar=c(5,7,4,1))
  barplot(ovaimp_284[[i]],horiz = TRUE,las=1, 
          main=paste("Most important features -", names(ovaimp_284[i])), 
          xlab="Mean Decrease Gini") 
}

#then create a new dataset 
#removing the same kmers found between hosts

#keep only k-mers for each host
#(removes Gini score)
for(i in 1:8){
  ovaimp_284[[i]] <- names(ovaimp_284[[i]])
}
#find what kmers are the same between hosts
samekmers <- c()
for(i in 1:7){
  for(j in (i+1):8){
    x<-intersect(ovaimp_284[[i]],ovaimp_284[[j]])
    if(identical(x,character(0))){next}
    cat(names(ovaimp_284[i]),names(ovaimp_284[j]), x, "\n")
    samekmers <- c(samekmers,x) 
  }
}
#remove 
fix_new <- fix[!fix %in% fix[samekmers]]





#########
#NOT USEFULL -REMOVE

#then create a new dataset 
#removing the same kmers found between hosts

#keep only k-mers for each host
#(removes Gini score)
for(i in 1:8){
  ovaimp_470[[i]] <- names(ovaimp_470[[i]])
}


#find what kmers are the same between hosts
samekmers <- c()
for(i in 1:7){
  for(j in (i+1):8){
    x<-intersect(ovaimp_470[[i]],ovaimp_470[[j]])
    if(identical(x,character(0))){next}
    cat(names(ovaimp_470[i]),names(ovaimp_470[j]), x, "\n")
    samekmers <- c(samekmers,x) 
  }
}
#remove 
k_9_new <- k_9_fix[!k_9_fix %in% k_9_fix[samekmers]]



#the same for 

#then create a new dataset 
#removing the same kmers found between hosts

#keep only k-mers for each host
#(removes Gini score)
for(i in 1:8){
  ovaimp_284[[i]] <- names(ovaimp_284[[i]])
}
#find what kmers are the same between hosts
samekmers <- c()
for(i in 1:7){
  for(j in (i+1):8){
    x<-intersect(ovaimp_284[[i]],ovaimp_284[[j]])
    if(identical(x,character(0))){next}
    cat(names(ovaimp_284[i]),names(ovaimp_284[j]), x, "\n")
    samekmers <- c(samekmers,x) 
  }
}
#remove 
fix_new <- fix[!fix %in% fix[samekmers]]





#######




# Load library
library("VennDiagram")
library("gridExtra")


#keep only k-mers for each host
#(removes Gini score)
#470
for(i in 1:8){
  ovaimp_470[[i]] <- names(ovaimp_470[[i]])
}
#284
for(i in 1:8){
  ovaimp_284[[i]] <- names(ovaimp_284[[i]])
}



#Common kmers between sets - One vs All 

#save the same kmers 
ovaimp_all <- list()
for(i in 1:8){
  x<-intersect(ovaimp_470[[i]],ovaimp_284[[i]])
  ovaimp_all[i] <- list(x) 
  
  #plot
  grid.newpage()
  venn <- draw.pairwise.venn(area1=20, area2=20,
                             cross.area=length(ovaimp_all[[i]]),  
                             category=c("ova-470","ova-284"),fill=c("Red","Yellow"), 
                             cex=1.3, cat.cex = 1.2, 
                             cat.dist = 0.040, cat.pos = c(-35,35),
                             ind = FALSE)
  
  grid.arrange(gTree(children = venn),
               top= paste("OvA - Common kmers between sets -",names(ovaimp_470[i])))
}
#give names
ovaimp_all <- setNames(ovaimp_all, names(ovaimp_284))



#find same kmers between HOSTS and remove them
#keeps only unique kmers 

samekmers <- c()
for(i in 1:7){
  for(j in (i+1):8){
    x<-intersect(ovaimp_all[[i]],ovaimp_all[[j]])
    if(identical(x,character(0))){next}
    cat(names(ovaimp_all[i]),names(ovaimp_all[j]), x, "\n")
    samekmers <- c(samekmers,x) 
    #remove the kmers
    ovaimp_all[[i]] <- ovaimp_all[[i]][-which(ovaimp_all[[i]] %in% samekmers )]
    ovaimp_all[[j]] <- ovaimp_all[[j]][-which(ovaimp_all[[j]] %in% samekmers )]
  }
}


#Common kmers between sets - Random Forest

rfimp_470_new <- names(rfimp_470)
rfimp_284_new <- names(rfimp_284)

x<-intersect(rfimp_470_new,rfimp_284_new)
#save same kmers 
rfimp_all <- x

#plot
grid.newpage()
venn <- draw.pairwise.venn(area1=50, area2=50,
                           cross.area=length(rfimp_all),  
                           category=c("rf-470","rf-284"),fill=c("green","deepskyblue"), 
                           cex=1.3, cat.cex = 1.2, 
                           cat.dist = 0.040, cat.pos = c(-35,35),
                           ind = FALSE)

grid.arrange(gTree(children = venn), 
             top= paste("Random Forest - Common kmers between sets"))




##Common kmers between methods OvA-RF
#the common of rf for each of common of ova

ova_rf_imp <-list()
for(i in 1:8){
  x<-intersect(ovaimp_all[[i]], rfimp_all)
  ova_rf_imp[i] <- list(x) 
  
  #plot
  grid.newpage()
  venn <- draw.pairwise.venn(area1 = length(rfimp_all), 
                             area2 = length(ovaimp_all[[i]]),
                             cross.area=length(ova_rf_imp[[i]]),  
                             category=c("Random Forest","One vs All"),
                             fill=c("cyan","hotpink"), 
                             cex=1, cat.cex = 1.2, 
                             cat.dist = 0.040, cat.pos = c(-20,15), 
                             ind = FALSE)
  
  grid.arrange(gTree(children = venn),
               top= "Common kmers between Methods",
               bottom = paste("One vs All -",names(ovaimp_all[i])))
}
#give names
ova_rf_imp <- setNames(ova_rf_imp, names(ovaimp_all))
ova_rf_imp

