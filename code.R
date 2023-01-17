#This is the code 

#clean environment everytime so run what you need from the start
#rm(list=ls()) 
#rm(list=c())
#& 
#gc() #for cleaning

#####
library(readr)
k_9_og <- read_csv("k_9_total_for_classification.csv") #.
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
plot_intro(fix)
plot_bar(fix) 
#plot_correlation(k_9_test) #too many var - 128gb

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


#########


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

#varimp
model$importance[order(model$importance[,1],decreasing=TRUE),]

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





######
#One vs all Random Forest classifier

fix <- k_9_fix #test
hosts <- unique(k_9_fix$host_categories)
hosts <- as.character(hosts)
datalist <- data.frame(Host = character(),Trees=numeric(), Error=numeric())
ovaimp <- list()  #to save the variables' importance
for (i in 1:8){
  fix$host_categories <- ifelse(k_9_fix$host_categories==hosts[i],hosts[i],"other")
  fix$host_categories <- as.factor(fix$host_categories)
  model <- randomForest(host_categories ~ ., data=fix, proximity=TRUE)
  print(model)
  
  error.mtry <- data.frame(Host = character(), Trees=numeric(), Error=numeric())
  a <- 0
  for(y in c(500,1000)){
    c <- 0
    for(j in 1:20) {
      model <- randomForest(host_categories ~ ., data=fix, mtry=j, ntree=y)
      c <- j + a
      error.mtry[c,] <- c(hosts[i],
                          y,
                          model$err.rate[nrow(model$err.rate),1])
      #the oob err at max trees
    }
    a <- a+20
  }
  datalist <- rbind(datalist, error.mtry)
  
  #find which mtry&tree have the min error
  min_error <- min(error.mtry$Error)
  pos <- which(error.mtry$Error == min_error)
  
  #if more than 1 mtry have min error
  #pick the first
  best_trees <- as.numeric(error.mtry$Trees[pos[1]])
  
  #this is for printing the right mtry 
  if(pos[1]>20){
    min_mtry <- pos[1] - 20
  } else {
    min_mtry <- pos[1]
  }
  ## final model for proximities using the best ntree and the best mtry
  model <- randomForest(host_categories ~ ., 
                        data=fix,
                        ntree=best_trees,
                        proximity=TRUE, 
                        mtry=min_mtry)
  print(model)
  
  #varimp
  #a list of the top 20 most important k-mers for each host
  ovaimp[i] <- list(model$importance[order(model$importance[,1],decreasing=TRUE),][1:20])
}
#name the list
ovaimp <- setNames(ovaimp, hosts)
#plot the imp features 
for(i in 1:8){
  barplot(ovaimp[[i]],horiz = TRUE,las=1, 
          main=paste("Most important features -", names(ovaimp[i])), 
          xlab="Mean Decrease Gini") 
}

datalist$Error <- as.numeric(datalist$Error)
datalist$Trees <- as.numeric(datalist$Trees)
No_of_trees <-as.factor(datalist$Trees)
ggplot(data=datalist, aes( x=Host, y=Error, fill= No_of_trees))+
  geom_boxplot()+
  ggtitle("One vs Rest classification -  
          OOB error rate per host for 500 and 1000 trees")

#keep only k-mers
for(i in 1:8){
  ovatest[[i]] <- names(ovatest[[i]])
}
#find what top kmers are the same between hosts
for(i in 1:7){
  for(j in (i+1):8){
    x<-intersect(ovatest[[i]],ovatest[[j]])
    if(identical(x,character(0))){next}
    cat(names(ovatest[i]),names(ovatest[j]), x, "\n")
  }
}

