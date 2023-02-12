
##This is the code

#here is the project step by step.

#there is some extra code in the "testfile.R"
#for parts that were usefull or an other way to do some of them


#####
#Data Exlporation

library(readr)
k_9_og <- read_csv("k_9_total_for_classification.csv") 

#NA's?
any(is.na(k_9)) #is False == no NAs

#which hosts
unique(k_9[c(3,4)]) 

#keep only host_categories and kmers
k_9_test <- k_9_og[,-c(1:2,4)]  
#rm(k_9_og)   #call read_csv again if you want original(saves space)


library(DataExplorer)
plot_intro(fix)
plot_bar(fix) 

#####
#Boruta
#install.packages('Boruta')
library(Boruta)
k_9_test$host_categories <- as.factor(k_9_test$host_categories)
boruta_output <- Boruta(host_categories ~ ., data=k_9_test, doTrace=2)
names(boruta_output)
# Get significant variables including tentatives
boruta_signif <- getSelectedAttributes(boruta_output, withTentative = TRUE)
print(boruta_signif) 
# Do a tentative rough fix
roughFixMod <- TentativeRoughFix(boruta_output)
boruta_no_Tent <- getSelectedAttributes(roughFixMod)
print(boruta_no_Tent)
# Variable Importance Scores
#(meanImp,medianImp,minImp,maxImp,normHits,decision) for all features
imps <- attStats(roughFixMod)
imps2 = imps[imps$decision != 'Rejected', c('meanImp', 'decision')]
head(imps2[order(-imps2$meanImp), ])  # descending sort
# Plot variable importance
#plot(boruta_output, cex.axis=.7, las=2, xlab="", main="Variable Importance") 

host_categories <- k_9_test$host_categories
k_9_tent <- cbind(host_categories, k_9_test[,c(boruta_signif)])
k_9_fix <- cbind(host_categories, k_9_test[,c(boruta_no_Tent)])


#change fox name when host_categories is factor
levels(k_9_test$host_categories)[levels(k_9_test$host_categories) == 'grey-headed_flying_fox'] <- 'fox'
levels(k_9_fix$host_categories)[levels(k_9_fix$host_categories) == 'grey-headed_flying_fox'] <- 'fox'
levels(k_9_tent$host_categories)[levels(k_9_tent$host_categories) == 'grey-headed_flying_fox'] <- 'fox'

#"save and test to csv file" in testfile.R


#########

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
a <- list(k_9_fix,k_9_tent)
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



##############

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
##(so do not run the first for-loop below) 
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
                    sizes = seq(50, ncol(i), by=10),#posa apo ta features na parei
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



#For line-plot
#a <- list(199,1000,5000,10000)
a <- 199
Subsets <- list(k_9_fix,k_9_tent)
host_categories <- k_9_test$host_categories
feat_acc <- data.frame(set=numeric(), 
                       variables=numeric(),
                       Accuracy=numeric(), 
                       Kappa=numeric())
for(j in a){
  b <- cbind(host_categories, k_9_test[,sample(1:ncol(k_9_test),j)])
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
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  #scale_color_brewer(palette="Set1")+
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



############

#Feature selection for Correlation and Variance

#correlation
library(mlbench)
library(caret)
set.seed(7)

#correlation matrix
correlationMatrix <- cor(k_9_fix[,-c(1)] )
which(correlationMatrix == max(correlationMatrix[correlationMatrix!=max(correlationMatrix)]), arr.ind = TRUE)
#plot
library(corrplot)
corrplot(correlationMatrix[100:115, 100:115], 
         method = "color", tl.col = "black", tl.srt = 45,
         title = "Correlation Heatmap for values 100-115",
         mar = c(1,0,2,0))
#attributes that are highly correlated (position)
highlyCorrelated <- findCorrelation(cor(k_9_fix[,-c(1)] ), cutoff=0.9)
#picked 180
colnames(k_9_fix[,highlyCorrelated])
#keep the rest of the data
corr_data <- k_9_fix[,-highlyCorrelated]

#dataset only correlation
corr_data

#variance 

#check for variance in original (boruta = 470) dataset

#find if features have zero variance
low_var <- nearZeroVar(k_9_fix[,-c(1)], saveMetrics = TRUE)
sapply(low_var, unique)
#no feature has repeating values

#find low variance
low_var <- sapply(k_9_fix[,-c(1)], var)
#features with variance less than 0.015
low_var <- names(low_var[which(low_var < 0.015)])
#keep the rest
var_data <- k_9_fix[!k_9_fix %in% k_9_fix[low_var]]

#dataset only variance
var_data


#variance 

#check for variance in correlation dataset

#find if features have zero variance
low_var <- nearZeroVar(corr_data[,-c(1)], saveMetrics = TRUE)
sapply(low_var, unique)
#no feature has repeating values

#find low variance
low_var <- sapply(corr_data[,-c(1)], var)
#features with variance less than 0.015
low_var <- names(low_var[which(low_var < 0.015)])
#keep the rest
fix <- corr_data[!corr_data %in% corr_data[low_var]]

#dataset both
fix



###########

#Recursive Feature Elimination (RFE)
#use this to find best subset of features 
#given the output of boruta - 470 and correlation , variance and both

library(caret)
library("randomForest")
control <- rfeControl(functions = rfFuncs, # random forest
                      method = "repeatedcv", # repeated cv
                      repeats = 5, # number of repeats
                      number = 10) # number of folds

# Features
a <- list(k_9_fix,fix,corr_data,var_data)
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
                    sizes = seq(50,ncol(i),by=20),
                    rfeControl = control)
  print(result_rfe)
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
  geom_line(aes(color=Size_of_Subset), size=1.2)+
  geom_point()+
  scale_color_brewer(palette="Set1")+
  theme_bw()+
  labs(color = "Size of Dataset")+
  labs(caption = "470=boruta output, 460=removed only variance, \n 
                  290=removed only correlation, 284=removed both")+
  ggtitle("Accuracy per number of Variables")

#Kappa line
ggplot(data=feat_acc, aes(x=No_of_Variables, y=Kappa, group=Size_of_Subset))+
  geom_line(aes(color=Size_of_Subset),size = 1.2)+
  geom_point()+
  theme_bw()+
  labs(color = "Size of Dataset")+
  labs(caption = "470=boruta output, 460=removed only variance, \n 
                  290=removed only correlation, 284=removed both")+
  ggtitle("Kappa per number of Variables")

#Accuracy boxplot
ggplot(data=feat_acc, aes( x=Size_of_Subset, y=Accuracy))+
  geom_boxplot(aes(color= Size_of_Subset), lwd=1) + 
  geom_dotplot(binaxis='y', stackdir='center',dotsize=0.75,binwidth = 0.001)+
  theme_bw()+
  labs(caption = "470=boruta output, 460=removed only variance, \n 
                  290=removed only correlation, 284=removed both")+
  labs(x = "Size of Dataset")+
  ggtitle("Accuracy per Dataset")



#####

#Random Forest 
library(ggplot2)
library(cowplot)
library(randomForest)
set.seed(42)

#this is the 470 variables dataset - k_9_fix


#first try with default parameters
k_9_fix$host_categories <- as.factor(k_9_fix$host_categories)
model <- randomForest(host_categories ~ ., data=k_9_fix, proximity=TRUE)
model

#plot a df of the error rates to check for different ntree
model <- randomForest(host_categories ~ ., data=k_9_fix,ntree=1500, proximity=TRUE)

oob.error.data <- data.frame(
  Trees=rep(1:nrow(model$err.rate), times=9), #1:500 9x, (once for each type)
  Type=rep(c("OOB", "birds", "cat", "cattle", "dog", "fox",
             "horse","human", "pig"), each=nrow(model$err.rate)),
  Error=c(model$err.rate[,"OOB"], model$err.rate[,"birds"],
          model$err.rate[,"cat"], model$err.rate[,"cattle"],
          model$err.rate[,"dog"], model$err.rate[,"fox"],
          model$err.rate[,"horse"], model$err.rate[,"human"],
          model$err.rate[,"pig"]))

ggplot(data=oob.error.data, aes(x=Trees, y=Error)) +
  geom_line(aes(color=Type))+
  ggtitle("Random Forest error per number of trees")

#find best mtry and best ntree together
oob.values <- vector(length=150)
error.mtry <- data.frame(Trees=numeric(), Error=numeric())
a <- 0
for(y in c(500,1000,1500)){
  c <- 0
  for(i in 1:42) #+- the og = 21
  {
    model <- randomForest(host_categories ~ ., data=k_9_fix, mtry=i, ntree=y)
    c <- i + a
    oob.values[c] <- model$err.rate[nrow(model$err.rate),1]#the oob err at 500 trees
    error.mtry[c,] <- c(y,oob.values[c])
  }
  a <- a+42
}
No_of_trees <-as.factor(error.mtry$Trees)
ggplot(data=error.mtry, aes( x=No_of_trees, y=Error))+
  geom_boxplot(aes(color= No_of_trees), lwd=1) + 
  theme_bw() +
  geom_dotplot(binaxis='y', stackdir='center',dotsize=0.5,binwidth = 0.001)+
  ggtitle("Random Forest error per number of Trees for every mtry")

#find which mtry&tree have the min error
min_error<-min(error.mtry$Error)
pos <- as.numeric(which(error.mtry$Error == min_error))

#if more than 1 mtry have min error
#pick the first 
if(length(pos)>1){
  pos <- pos[1] 
}
best_trees <- error.mtry$Trees[pos]

#this is for printing the right mtry 
if(pos>42){
  min_mtry <- pos - 42
} else {
  min_mtry <- pos
}
#print 
min_mtry
min_error
best_trees 


## final model for proximities using the best ntree and the best mtry
model_rf <- randomForest(host_categories ~ ., 
                         data=k_9_fix,
                         ntree=best_trees,
                         proximity=TRUE, 
                         mtry=min_mtry)
model_rf

#varimp
rfimp_470 <- model_rf$importance[order(model_rf$importance[,1],decreasing=TRUE),][1:50]
rfimp_470[1:5]

#plot the varimp
par(mar=c(5,7,4,1))
barplot(rfimp_470[1:20],horiz = TRUE,las=1,
        main="Random Forest - Most important features - 470 \n top 20 out of 50",
        xlab="Mean Decrease Gini")



#Dimensionality Reduction

#MDS-plot - how the samples are related to each other
#proximity matrix -> distance matrix
distance.matrix <- as.dist(1-model_rf$proximity)
mds.stuff <- cmdscale(distance.matrix, eig=TRUE, x.ret=TRUE)

# calculate the percentage of variation that each MDS axis accounts for
mds.var.per <- round(mds.stuff$eig/sum(mds.stuff$eig)*100, 1)

#plot that shows the MDS axes and the variation:
mds.values <- mds.stuff$points
mds.data <- data.frame(Sample=rownames(mds.values),
                       X=mds.values[,1],
                       Y=mds.values[,2],
                       Host=k_9_fix$host_categories)

ggplot(data=mds.data, aes(x=X, y=Y,color=Host)) + 
  geom_point(alpha = 9/10, size=2)+
  theme_bw() +
  xlab(paste("MDS1 - ", mds.var.per[1], "%", sep="")) +
  ylab(paste("MDS2 - ", mds.var.per[2], "%", sep="")) +
  ggtitle("MDS for Random Forest - 470")





##########

#Random Forest 
library(ggplot2)
library(cowplot)
library(randomForest)
set.seed(42)

#this is the 284 variables dataset - fix

#first try with default parameters
fix$host_categories <- as.factor(fix$host_categories)
model <- randomForest(host_categories ~ ., data=fix, proximity=TRUE)
model

#plot a df of the error rates to check for different ntree
model <- randomForest(host_categories ~ ., data=fix,ntree=1500, proximity=TRUE)

oob.error.data <- data.frame(
  Trees=rep(1:nrow(model$err.rate), times=9), #1:500 9x, (for hosts and oob)
  Host=rep(c("OOB", "birds", "cat", "cattle", "dog", "fox",
             "horse","human", "pig"), each=nrow(model$err.rate)),
  Error=c(model$err.rate[,"OOB"], model$err.rate[,"birds"],
          model$err.rate[,"cat"], model$err.rate[,"cattle"],
          model$err.rate[,"dog"], model$err.rate[,"fox"],
          model$err.rate[,"horse"], model$err.rate[,"human"],
          model$err.rate[,"pig"]))

ggplot(data=oob.error.data, aes(x=Trees, y=Error)) +
  theme_bw() +
  geom_line(aes(color=Host), linewidth=0.75)+
  ggtitle("OOB error rates per number of trees for each host")


oob.values <- vector(length=150)
error.mtry <- data.frame(Trees=numeric(), Error=numeric())
a <- 0
for(y in c(500,1000,1500)){
  c <- 0
  for(i in 1:32) {
    temp.model <- randomForest(host_categories ~ ., data=fix, mtry=i, ntree=y)
    c <- i + a
    oob.values[c] <- temp.model$err.rate[nrow(temp.model$err.rate),1]#the oob err at 500 trees
    error.mtry[c,] <- c(y,oob.values[c])
  }
  a <- a+32
}
No_of_trees <-as.factor(error.mtry$Trees)
ggplot(data=error.mtry, aes( x=No_of_trees, y=Error))+
  geom_boxplot(aes(color= No_of_trees), lwd=1) + 
  theme_bw() +
  geom_dotplot(binaxis='y', stackdir='center',dotsize=0.5,binwidth = 0.001)+
  ggtitle("Random Forest error per number of Trees for every mtry")

#find which mtry&tree have the min error
min_error<-min(error.mtry$Error)
pos <- as.numeric(which(error.mtry$Error == min_error))
#if more than 1 mtry have min error
#pick the first
if(length(pos)>1){
  pos <- pos[1] 
}
best_trees <- error.mtry$Trees[pos]

#this is for printing the right mtry 
if(pos>32){
  min_mtry <- pos - 32
} else {
  min_mtry <- pos
}
#print 
min_mtry 
min_error
best_trees 


## final model for proximities using the best ntree and the best mtry
model_rf <- randomForest(host_categories ~ ., 
                         data=fix,
                         ntree=best_trees,
                         proximity=TRUE, 
                         mtry=min_mtry)
model_rf

#varimp
rfimp_284 <- model_rf$importance[order(model_rf$importance[,1],decreasing=TRUE),][1:50]

#plot the varimp
par(mar=c(5,7,4,1))
barplot(rfimp_284[1:20],horiz = TRUE,las=1,
        main="Random Forest - Most important features - 284 \n top 20 out of 50",
        xlab="Mean Decrease Gini")


#MDS-plot - how the samples are related to each other
#proximity matrix -> distance matrix
distance.matrix <- as.dist(1-model_rf$proximity)
mds.stuff <- cmdscale(distance.matrix, eig=TRUE, x.ret=TRUE)

# calculate the percentage of variation that each MDS axis accounts for
mds.var.per <- round(mds.stuff$eig/sum(mds.stuff$eig)*100, 1)

#plot that shows the MDS axes and the variation:
mds.values <- mds.stuff$points
mds.data <- data.frame(Sample=rownames(mds.values),
                       X=mds.values[,1],
                       Y=mds.values[,2],
                       Host=fix$host_categories)

ggplot(data=mds.data, aes(x=X, y=Y,color=Host)) + 
  geom_point(alpha = 9/10, size=2)+
  theme_bw() +
  xlab(paste("MDS1 - ", mds.var.per[1], "%", sep="")) +
  ylab(paste("MDS2 - ", mds.var.per[2], "%", sep="")) +
  ggtitle("MDS for Random Forest - 284")





######

#One vs all Random Forest classifier

#this is only to check for trees!

library(ggplot2)
library(cowplot)
library(randomForest)
set.seed(42)

#this is to check if the 1500 trees are necessary for the ova analysis
#there are comments to change for each dataset.

#this for 470
temp_fix <- k_9_fix #temporary dataset
#this for 285
#temp_fix <- fix

hosts <- unique(k_9_fix$host_categories)
hosts <- as.character(hosts)
datalist <- data.frame(Host = character(),Trees=numeric(), Error=numeric())

for (i in 1:8){
  #create new data for each host as host - other
  temp_fix$host_categories <- ifelse(k_9_fix$host_categories==hosts[i],hosts[i],"other")
  temp_fix$host_categories <- as.factor(temp_fix$host_categories)
  
  error.mtry <- data.frame(Host = character(), Trees=numeric(), Error=numeric())
  a <- 0
  for(y in c(500,1000,1500))#once with and once without 1500 
    {
    c <- 0
    for(j in 1:42) #470->1:42, 284->1:32
      {
      ova_model <- randomForest(host_categories ~ ., data=temp_fix, mtry=j, ntree=y)
      c <- j + a
      error.mtry[c,] <- c(hosts[i],
                          y,
                          ova_model$err.rate[nrow(ova_model$err.rate),1])
                            #the oob err at max trees
    }
    #470->42, 284->32
    a <- a+42 
  }
  datalist <- rbind(datalist, error.mtry)
}

datalist$Error <- as.numeric(datalist$Error)
datalist$Trees <- as.numeric(datalist$Trees)
No_of_trees <-as.factor(datalist$Trees)

#this is for 1500
ggplot(data=datalist, aes( x=Host, y=Error, fill= No_of_trees))+
  geom_boxplot()+
  theme_bw()+
  ggtitle("One vs Rest classification -  
          OOB error rate per host for 500,1000,1500 trees")

#this is without 1500
ggplot(data=datalist, aes( x=Host, y=Error, fill= No_of_trees))+
  geom_boxplot()+
  theme_bw()+
  ggtitle("One vs Rest classification -  
          OOB error rate per host for 500,1000 trees")



########

#One vs All Random Forest classifier 

#there are comments in the code for 
#each dataset (470 or 284)

#to save the variables' importance
ovaimp_470 <- list()  
ovaimp_284 <- list()

#temporary dataset
temp_fix <- k_9_fix #this for 470
#or
#temp_fix <- fix    #this for 284

hosts <- unique(k_9_fix$host_categories)
hosts <- as.character(hosts)

#change for each host
#1=human, 2=cattle, 3=birds, 4=pig,
#5=horse, 6=dog,    7=cat,   8=fox,
i <- 0

temp_fix$host_categories <- ifelse(k_9_fix$host_categories==hosts[i],hosts[i],"other")
temp_fix$host_categories <- as.factor(temp_fix$host_categories)

#default parameters
ova_model <- randomForest(host_categories ~ ., data=temp_fix, proximity=TRUE)
ova_model

error.mtry <- data.frame(Host = character(), Trees=numeric(), Error=numeric())
a <- 0
for(y in c(500,1000)){
  c <- 0
  for(j in 1:42) #470->1:42, 284->1:32
  {
    ova_model <- randomForest(host_categories ~ ., data=temp_fix, mtry=j, ntree=y)
    c <- j + a
    error.mtry[c,] <- c(hosts[i],
                        y,
                        ova_model$err.rate[nrow(ova_model$err.rate),1])
    #the oob err at max trees
  }
  #470->42, 284->32
  a <- a+42
}

min_error <- min(error.mtry$Error)
pos <- which(error.mtry$Error == min_error)
best_trees <- as.numeric(error.mtry$Trees[pos[1]])

#this is for printing the right mtry 
if(pos[1]>42)
  #470->42, 284->32
{
  min_mtry <- pos[1] - 42
} else {
  min_mtry <- pos[1]
}

#print 
min_error
min_mtry 
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
ovaimp_470[i] <- list(ova_model$importance[order(ova_model$importance[,1],decreasing=TRUE),][1:20])
#or 
#ovaimp_284[i] <- list(ova_model$importance[order(ova_model$importance[,1],decreasing=TRUE),][1:20])



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


#now run again with new i for the next host 


##########

#Variable Importance

#when all host are computed 
#print the variable importances for each host

#k_9_fix - 470

#name the list
ovaimp_470 <- setNames(ovaimp_470, hosts)
#plot the imp features 
for(i in 1:8){
  par(mar=c(5,7,4,1))
  barplot(ovaimp_470[[i]],horiz = TRUE,las=1, 
          main=paste("Most important features -", names(ovaimp_470[i])), 
          xlab="Mean Decrease Gini") 
}

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



##Compare the variable importance to find 
##which kmers are the most important 

# Load library
library("VennDiagram")
library("gridExtra")


#keep only k-mers for each host (removes Gini score)
#the rank of importance stays from first as the most

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

rfimp_all


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

#print the top 6 of each host
ovaimp_all$human  [1:6]
ovaimp_all$cattle [1:6]
ovaimp_all$birds  [1:6]
ovaimp_all$pig    [1:6]
ovaimp_all$horse  [1:6]
ovaimp_all$dog    [1:6]
ovaimp_all$cat    [1:6]
ovaimp_all$fox    [1:6]
