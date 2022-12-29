#This is the code 

#clean environment everytime so run what you need from the start
#rm(list=ls()) 
#rm(list=c())
#& 
#gc() #for cleaning

#####
library(readr)
k_9_og <- read_csv("k_9_total_for_classification.csv") #.
k_9 <- k_9_og #.
rm(k_9_og)   #call read_csv again if you want original(saves space)

#k_9_test <- k_9[,-c(1:3)]     #keep only common_species_names and kmers
k_9_test <- k_9_og[,-c(1:2,4)]   #keep only host_categories and kmers

summary(k_9)
str(k_9)
View(k_9)

#NA's?
any(is.na(k_9)) #is False == no NAs

#which hosts
unique(k_9[c(3,4)]) 


library(DataExplorer)
plot_intro(k_9_test)
plot_bar(k_9_test)
#plot_correlation(k_9_test) #too many var - 128gb

#####
#Boruta
#install.packages('Boruta')
library(Boruta)
k_9_test$host_categories <- as.factor(k_9_test$host_categories)
boruta_output <- Boruta(host_categories ~ ., data=k_9_test, doTrace=1)
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


#####
#varImp(caret)
library(caret)
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


#####

#Random Forest 
library(ggplot2)
library(cowplot)
library(randomForest)
set.seed(42)

#first try with default parameters
k_9_fix$host_categories <- as.factor(k_9_fix$host_categories)
model <- randomForest(host_categories ~ ., data=k_9_fix, proximity=TRUE)
model

#plot a df of the error rates to check for different ntree
oob.error.data <- data.frame(
  Trees=rep(1:nrow(model$err.rate), times=3), #1:500 3x, (once for each type)
  Type=rep(c("OOB", "birds", "cat", "cattle", "dog", "grey-headed_flying_fox",
             "horse","human", "pig"), each=nrow(model$err.rate)),
  Error=c(model$err.rate[,"OOB"], model$err.rate[,"birds"],
          model$err.rate[,"cat"], model$err.rate[,"cattle"],
          model$err.rate[,"dog"], model$err.rate[,"grey-headed_flying_fox"],
          model$err.rate[,"horse"], model$err.rate[,"human"],
          model$err.rate[,"pig"]))

ggplot(data=oob.error.data, aes(x=Trees, y=Error)) +
  geom_line(aes(color=Type))


oob.values <- vector(length=150)
error.mtry <- data.frame(Trees=numeric(), Error=numeric())
a <- 0
for(y in c(500,1000,1500)){
   c <- 0
   for(i in 1:50) {
     temp.model <- randomForest(host_categories ~ ., data=k_9_fix, mtry=i, ntree=y)
     c <- i + a
     oob.values[c] <- temp.model$err.rate[nrow(temp.model$err.rate),1]#the oob err at 500 trees
     error.mtry[c,] <- c(y,oob.values[c])
   }
   a <- a+50
}
No_of_trees <-as.factor(error.mtry$Trees)
ggplot(data=error.mtry, aes( x=No_of_trees, y=Error))+
   geom_boxplot() + 
   geom_dotplot(binaxis='y', stackdir='center',dotsize=0.5,binwidth = 0.001)
 

#find which mtry&tree have the min error
min_error<-min(error.mtry$Error)
pos <- as.numeric(which(error.mtry$Error == min_error))
#if more than 1 mtry have min error
#pick the last
if(length(pos)>1){
   pos <- pos[length(pos)]
}
best_trees <- error.mtry$Trees[pos]

#this is for printing the right mtry 
if(pos>50){
   min_mtry <- pos - 50
} else {
   min_mtry <- pos
   }
#print 
min_mtry 
min_error
best_trees


## final model for proximities using the best ntree and the best mtry
model <- randomForest(host_categories ~ ., 
                      data=k_9_fix,
                      ntree=best_trees,
                      proximity=TRUE, 
                      mtry=min_mtry)
model


#####

#MDS-plot - how the samples are related to each other
#proximity matrix -> distance matrix
distance.matrix <- as.dist(1-model$proximity)
mds.stuff <- cmdscale(distance.matrix, eig=TRUE, x.ret=TRUE)

# calculate the percentage of variation that each MDS axis accounts for
mds.var.per <- round(mds.stuff$eig/sum(mds.stuff$eig)*100, 1)

#plot that shows the MDS axes and the variation:
mds.values <- mds.stuff$points
mds.data <- data.frame(Sample=rownames(mds.values),
                       X=mds.values[,1],
                       Y=mds.values[,2],
                       Status=k_9_fix$host_categories)

ggplot(data=mds.data, aes(x=X, y=Y, label=Sample)) + 
  geom_text(aes(color=Status)) +
  theme_bw() +
  xlab(paste("MDS1 - ", mds.var.per[1], "%", sep="")) +
  ylab(paste("MDS2 - ", mds.var.per[2], "%", sep="")) +
  ggtitle("MDS plot using (1 - Random Forest Proximities)")



#####

#Recursive Feature Elimination (RFE)
#use this to find best subset of features 

library(caret)
library("randomForest")
control <- rfeControl(functions = rfFuncs, # random forest
                      method = "repeatedcv", # repeated cv
                      repeats = 5, # number of repeats
                      number = 10) # number of folds

# Features
a <- list(small=k_9_fix,big=k_9_tent)#dokimase na to baleis kateu8eian
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
  geom_boxplot() + 
  geom_dotplot(binaxis='y', stackdir='center',dotsize=0.75,binwidth = 0.001)+
  theme_bw()+
  labs(caption = "470=after Boruta fix, \n 730=before Tentative fix")+
  labs(x = "Size of Dataset")+
  ggtitle("Accuracy per subset of Boruta-Variables")




