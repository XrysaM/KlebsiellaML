#This is the code 

#clean environment everytime so run what you need from the start
#rm(list=ls()) #& gc() for cleaning

library(readr)
k_9_og <- read_csv("k_9_total_for_classification.csv") #.
k_9 <- k_9_og #.
rm(k_9_og)   #call read_csv again if you want original(saves space)

summary(k_9)
str(k_9)
View(k_9)

#NA's?
any(is.na(k_9)) #is False == no NAs

#which hosts
unique(k_9[c(3,4)]) 


#encoders -- alphabetical order
library(CatEncoders)
#encode common_species_names 
lab = LabelEncoder.fit(k_9$common_species_names)
k_9$common_species_names = transform(lab, k_9$common_species_names)
  #dixnei tis arxikes times
list <- unique(inverse.transform(lab, k_9$common_species_names))
ogvalues_common_species_names <- list[order(unlist(list))]
ogvalues_common_species_names


#smaller sample size for testing
library(dplyr)
sample_k9 <- slice_sample(k_9[,-c(1:3)], n = 60 ) #sample size
k_9_test <- k_9[,-c(1:3)] #keep only common_species_names and kmers


#Random Forest try
library(ggplot2)
library(cowplot)
library(randomForest)
set.seed(42)

k_9_test$common_species_names <- as.factor(k_9_test$common_species_names)
#do i need this?
#data.imputed <- rfImpute(common_species_names ~ ., data = k_9_test, iter=6) 

model <- randomForest(common_species_names ~ ., data=k_9_test, proximity=TRUE)
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


