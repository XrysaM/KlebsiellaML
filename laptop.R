#laptop stuff

#########
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
#attributes that are highly correlated 
highlyCorrelated <- findCorrelation(cor(k_9_fix[,-c(1)] ), cutoff=0.9)
#picked 180
colnames(k_9_fix[,highlyCorrelated])
#keep the rest of the data
corr_data <- k_9_fix[,-highlyCorrelated]



#variance 
#find if features have zero variance
var_data <- nearZeroVar(corr_data[,-c(1)], saveMetrics = TRUE)
sapply(var_data, unique)
#no feature has repeating values

#find low variance
var_data <- sapply(corr_data[,-c(1)], var)
#features with variance less than 0.015
low_var <- names(var_data[which(var_data < 0.015)])
#keep the rest
fix <- corr_data[!corr_data %in% corr_data[low_var]]


###########

#Recursive Feature Elimination (RFE)
#use this to find best subset of features 
#given the output of boruta - 470 vs 730

library(caret)
library("randomForest")
control <- rfeControl(functions = rfFuncs, # random forest
                      method = "repeatedcv", # repeated cv
                      repeats = 5, # number of repeats
                      number = 10) # number of folds

# Features
a <- list(k_9_fix,fix1,fix2,fix3)#dokimase na to baleis kateu8eian
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
  geom_boxplot() + 
  geom_dotplot(binaxis='y', stackdir='center',dotsize=0.75,binwidth = 0.001)+
  theme_bw()+
  labs(caption = "470=boruta output, 460=removed only variance, \n 
                  290=removed only correlation, 284=removed both")+
  labs(x = "Size of Dataset")+
  ggtitle("Accuracy per Dataset")


#top variables of RFE
rfe_top50 <- rownames(result_rfe$fit$importance)[order(result_rfe$fit$importance[,"MeanDecreaseGini"], 
                                                       decreasing = TRUE)][1:50]

##########

#Random Forest 
library(ggplot2)
library(cowplot)
library(randomForest)
set.seed(42)

#first try with default parameters
fix$host_categories <- as.factor(fix$host_categories)
model <- randomForest(host_categories ~ ., data=fix, proximity=TRUE)
model

#plot a df of the error rates to check for different ntree
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
  geom_line(aes(color=Host), size=0.75)+
  ggtitle("OOB error rates per number of trees for each host")


oob.values <- vector(length=150)
error.mtry <- data.frame(Trees=numeric(), Error=numeric())
a <- 0
for(y in c(500,1000,1500)){
  c <- 0
  for(i in 1:50) {
    temp.model <- randomForest(host_categories ~ ., data=fix, mtry=i, ntree=y)
    c <- i + a
    oob.values[c] <- temp.model$err.rate[nrow(temp.model$err.rate),1]#the oob err at 500 trees
    error.mtry[c,] <- c(y,oob.values[c])
  }
  a <- a+50
}
No_of_trees <-as.factor(error.mtry$Trees)
ggplot(data=error.mtry, aes( x=No_of_trees, y=Error))+
  geom_boxplot() + 
  theme_bw() +
  geom_dotplot(binaxis='y', stackdir='center',dotsize=0.3,binwidth = 0.001)+
  ggtitle("Model performance per number of trees")

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
model_rf <- randomForest(host_categories ~ ., 
                      data=fix,
                      ntree=best_trees,
                      proximity=TRUE, 
                      mtry=min_mtry)
model_rf

#varimp
top50rf <- model$importance[order(model$importance[,1],decreasing=TRUE),][1:50]


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
  ggtitle("MDS plot using (1 - Random Forest Proximities)")


######
#One vs all Random Forest classifier

temp_fix <- fix

hosts <- unique(fix$host_categories)
hosts <- as.character(hosts)
datalist <- data.frame(Host = character(),Trees=numeric(), Error=numeric())
ovaimp <- list()  #to save the variables' importance
for (i in 1:8){
  #create new data for each host as host - other
  temp_fix$host_categories <- ifelse(fix$host_categories==hosts[i],hosts[i],"other")
  temp_fix$host_categories <- as.factor(temp_fix$host_categories)
  model <- randomForest(host_categories ~ ., data=temp_fix,proximity=TRUE)
  print(model)
  
  error.mtry <- data.frame(Host = character(), Trees=numeric(), Error=numeric())
  a <- 0
  for(y in c(500,1000)){
    c <- 0
    for(j in 1:20) {
      model <- randomForest(host_categories ~ ., data=temp_fix, mtry=j, ntree=y)
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
                        data=temp_fix,
                        ntree=best_trees,
                        proximity=TRUE,
                        mtry=min_mtry)
  print(model)
  
  #varimp
  #a list of the top 20 most important k-mers for each host
  ovaimp[i] <- list(model$importance[order(model$importance[,1],decreasing=TRUE),][1:20])

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
                         Hosts=temp_fix$host_categories) #or fix if all
  
  print(
    ggplot(data=mds.data, aes(x=X, y=Y,color=Hosts)) + 
      theme_bw() +
      geom_point(alpha = 8/10, size=2)+
      scale_color_brewer(palette="Set1")+
      xlab(paste("MDS1 - ", mds.var.per[1], "%", sep="")) +
      ylab(paste("MDS2 - ", mds.var.per[2], "%", sep="")) +
      ggtitle(paste("MDS for", hosts[i],"vs the rest"))
  )
}
#name the list
ovaimp <- setNames(ovaimp, hosts)
#plot the imp features 
for(i in 1:8){
  par(mar=c(5,7,4,1))
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
  ovaimp[[i]] <- names(ovaimp[[i]])
}
#find what top kmers are the same between hosts
for(i in 1:7){
  for(j in (i+1):8){
    x<-intersect(ovaimp[[i]],ovaimp[[j]])
    if(identical(x,character(0))){next}
    cat(names(ovaimp[i]),names(ovaimp[j]), x, "\n")
  }
}
