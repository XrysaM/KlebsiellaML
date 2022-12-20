#This is the code 

#clean environment everytime so run what you need from the start
#rm(list=ls()) 
#rm(list=c())
#& 
#gc() #for cleaning

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
  Trees=rep(1:nrow(model$err.rate), times=3),
  Type=rep(c("OOB", "birds", "cat", "cattle", "dog", "grey-headed_flying_fox",
             "horse","human", "pig"), each=nrow(model$err.rate)),
  Error=c(model$err.rate[,"OOB"], model$err.rate[,"birds"],
          model$err.rate[,"cat"], model$err.rate[,"cattle"],
          model$err.rate[,"dog"], model$err.rate[,"grey-headed_flying_fox"],
          model$err.rate[,"horse"], model$err.rate[,"human"],
          model$err.rate[,"pig"]))

ggplot(data=oob.error.data, aes(x=Trees, y=Error)) +
  geom_line(aes(color=Type))

#this is to try different ntrees
model <- randomForest(host_categories ~ ., data=k_9_fix, ntree=1500, proximity=TRUE)
model

#number of mtry("No. of var tried at each split") 
#try 1-10
oob.values <- vector(length=10)
for(i in 1:10) {
  temp.model <- randomForest(host_categories ~ ., data=k_9_fix, mtry=i, ntree=1000)
  oob.values[i] <- temp.model$err.rate[nrow(temp.model$err.rate),1]
}
oob.values #oob err.rates for 1-10 mtry
## find the minimum error
min(oob.values)
which(oob.values == min(oob.values))


## final model for proximities using the best ntree and the best mtry
model <- randomForest(host_categories ~ ., 
                      data=k_9_fix,
                      ntree=1000,
                      proximity=TRUE, 
                      mtry=which(oob.values == min(oob.values)))
model



#create an MDS-plot to show how the samples are related to each other
#convert the proximity matrix into a distance matrix.
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

