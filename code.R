#This is the code 

#clean environment everytime so run what you need from the start
#rm(list=ls()) & gc() for cleaning

library(readr)
k_9_og <- read_csv("k_9_total_for_classification.csv") #.
summary(k_9)
View(k_9)

any(is.na(k_9)) #is False == no NAs
unique(k_9[c(3,4)]) #poia hosts kai posa

#encoders
k_9 <- k_9_og #.
#rm(k_9_og)   #call read_csv again if you want original(saves space)

#alphabetical order
library(CatEncoders)
#encode host_categories
lab = LabelEncoder.fit(k_9$host_categories)
k_9$host_categories = transform(lab, k_9$host_categories)
  #dixnei tis arxikes times
list <- unique(inverse.transform(lab, k_9$host_categories))
ogvalues_host_categories <- list[order(unlist(list))]
ogvalues_host_categories

#encode common_species_names - this
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


#PCA - dimensionality reduction
pca <- prcomp(k_9_test , scale = TRUE)
#simple plot, rough not necessary
plot(pca$x[,1], pca$x[,2])
## make a scree plot
pca.var <- pca$sdev^2
pca.var.per <- round(pca.var/sum(pca.var)*100, 1)
barplot(pca.var.per, main="Scree Plot", xlab="Principal Component", ylab="Percent Variation")
#better plot
library(ggplot2)
pca.data <- data.frame( X=pca$x[,1], Y=pca$x[,2], 
                        Samples = rownames(k_9_test)) 
pca.data
hosts <- factor(k_9_test$common_species_names)
ggplot(data=pca.data, aes(x=X, y=Y, label=Samples, colour= hosts)) +
  geom_point() +
  xlab(paste("PC1 - ", pca.var.per[1], "%", sep="")) +
  ylab(paste("PC2 - ", pca.var.per[2], "%", sep="")) +
  theme_bw() +
  ggtitle("My PCA Graph")

#loading scores - most important kmers for the variation
loading_scores <- pca$rotation[,1]
gene_scores <- abs(loading_scores) ## get the magnitudes
gene_score_ranked <- sort(gene_scores, decreasing=TRUE)
top_kmers <- names(gene_score_ranked[1:10]) #how many kmers i want

top_kmers ## show the names of the top 10 genes
pca$rotation[top_kmers,1] ## show the scores (and +/- sign)



#LDA - Linear Discriminant Analysis  -- doest't work :(
library(MASS)
model <- lda(formula = Hosts ~ ., data = k_9_test)
# get the x,y coordinates for the LDA plot
data.lda.values <- predict(model)
# create a dataframe that has all the info we need to draw a graph
plot.data <- data.frame(X=data.lda.values$x[,1], Y=data.lda.values$x[,2], 
                        Hosts=k_9_test$common_species_names)

head(plot.data)

# draw a graph using ggplot2
p <- ggplot(data=plot.data, aes(x=X, y=Y)) +
  geom_point(aes(color=Hosts)) +
  theme_bw()
p
