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

#change fox name when host_categories is factor
levels(k_9_fix$host_categories)[levels(k_9_fix$host_categories) == 'grey-headed_flying_fox'] <- 'fox'
levels(k_9_tent$host_categories)[levels(k_9_tent$host_categories) == 'grey-headed_flying_fox'] <- 'fox'


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


model <- randomForest(host_categories ~ ., data=k_9_fix,ntree=1500, proximity=TRUE)
model
#plot a df of the error rates to check for different ntree
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
  for(i in 1:40) #+- the og = 21
  {
    model <- randomForest(host_categories ~ ., data=k_9_fix, mtry=i, ntree=y)
    c <- i + a
    oob.values[c] <- model$err.rate[nrow(model$err.rate),1]#the oob err at 500 trees
    error.mtry[c,] <- c(y,oob.values[c])
  }
  a <- a+40
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
#pick the last
if(length(pos)>1){
  pos <- pos[length(pos)]
}
best_trees <- error.mtry$Trees[pos]

#this is for printing the right mtry 
if(pos>40){
  min_mtry <- pos - 40
} else {
  min_mtry <- pos
}
#print 
min_mtry 
min_error
best_trees


## final model for proximities using the best ntree and the best mtry
## final model for proximities using the best ntree and the best mtry
model_rf <- randomForest(host_categories ~ ., 
                         data=fix,
                         ntree=best_trees,
                         proximity=TRUE, 
                         mtry=min_mtry)
model_rf

#varimp
top50rf <- model$importance[order(model$importance[,1],decreasing=TRUE),][1:50]

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
#for the boruta output - 470 variables

library(ggplot2)
library(cowplot)
library(randomForest)
set.seed(42)

temp_fix <- k_9_fix #temporary dataset
hosts <- unique(k_9_fix$host_categories)
hosts <- as.character(hosts)

datalist <- data.frame(Host = character(),Trees=numeric(), Error=numeric())
ovaimp_470 <- list()  #to save the variables' importance

for (i in 1:8){
  #create new data for each host as host - other
  temp_fix$host_categories <- ifelse(k_9_fix$host_categories==hosts[i],hosts[i],"other")
  temp_fix$host_categories <- as.factor(temp_fix$host_categories)
  #default settings RF pred
  ova_model <- randomForest(host_categories ~ ., data=temp_fix, proximity=TRUE)
  print(ova_model)
  
  error.mtry <- data.frame(Host = character(), Trees=numeric(), Error=numeric())
  a <- 0
  for(y in c(500,1000)){
    c <- 0
    for(j in 1:42) {
      ova_model <- randomForest(host_categories ~ ., data=temp_fix, mtry=j, ntree=y)
      c <- j + a
      error.mtry[c,] <- c(hosts[i],
                          y,
                          ova_model$err.rate[nrow(ova_model$err.rate),1])
                            #the oob err at max trees
    }
    a <- a+42
  }
  datalist <- rbind(datalist, error.mtry)
  
  #find which mtry&tree have the min error
  min_error <- min(error.mtry$Error)
  pos <- which(error.mtry$Error == min_error)
  
  best_trees <- as.numeric(error.mtry$Trees[pos[1]])
  
  #this is for printing the right mtry 
  #picks the 1st mtry if more than one
  if(pos[1]>42){
    min_mtry <- pos[1] - 42
  } else {
    min_mtry <- pos[1]
  }
  ## final model for proximities using the best ntree and the best mtry
  ova_model <- randomForest(host_categories ~ ., 
                        data=temp_fix,
                        ntree=best_trees,
                        proximity=TRUE, 
                        mtry=min_mtry)
  print(ova_model)
  
  #varimp
  #a list of the top 20 most important k-mers for each host
  ovaimp_470[i] <- list(ova_model$importance[order(ova_model$importance[,1],decreasing=TRUE),][1:20])
  
  
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
ovaimp_470 <- setNames(ovaimp_470, hosts)
#plot the imp features 
for(i in 1:8){
  barplot(ovaimp_470[[i]],horiz = TRUE,las=1, 
          main=paste("Most important features -", names(ovaimp_470[i])), 
          xlab="Mean Decrease Gini") 
}

datalist$Error <- as.numeric(datalist$Error)
datalist$Trees <- as.numeric(datalist$Trees)
No_of_trees <-as.factor(datalist$Trees)
ggplot(data=datalist, aes( x=Host, y=Error, fill= No_of_trees))+
  geom_boxplot()+
  theme_bw()+
  ggtitle("One vs Rest classification -  
          OOB error rate per host for 500,1000 trees")

#keep only k-mers for each host
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
