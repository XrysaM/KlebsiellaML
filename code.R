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
                         data=fix,
                         ntree=best_trees,
                         proximity=TRUE, 
                         mtry=min_mtry)
model_rf

#varimp
rfimp_470 <- model_rf$importance[order(model_rf$importance[,1],decreasing=TRUE),][1:50]

#plot the varimp
par(mar=c(5,7,4,1))
barplot(rfimp_470[1:20],horiz = TRUE,las=1,
        main="Random Forest - Most important features - 470 \n top 20 out of 50",
        xlab="Mean Decrease Gini")


#####

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
#each dataset 

#to save the variables' importance
ovaimp_470 <- list()  
ovaimp_284 <- list()

#temporary dataset
temp_fix <- k_9_fix #this for 470
#or
#temp_fix <- fix    #this for 285

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


#now run again with new i for next host 


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



##Compare the variable importances to find 
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
