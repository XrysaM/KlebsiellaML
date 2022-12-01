#This is the code 

library(readr)
k_9 <- read_csv("k_9_total_for_classification.csv")
summary(k_9)
View(k_9)

any(is.na(k_9)) #is False == no NAs
unique(k_9[c(3,4)]) #poia hosts kai posa

#encoders
rm(k_9_test2)
k_9_test2 <- k_9

#alphabetical order
library(CatEncoders)
labs1 = LabelEncoder.fit(k_9_test2$host_categories)
labs2 = LabelEncoder.fit(k_9_test2$common_species_names)
k_9_test2$host_categories = transform(labs1, k_9_test2$host_categories)
k_9_test2$common_species_names = transform(labs2, k_9_test2$common_species_names)
#dixnei tis arxikes times
list1 <- unique(inverse.transform(labs1, k_9_test2$host_categories))  
list2 <- unique(inverse.transform(labs2, k_9_test2$common_species_names))
ogvalues_host_categories <- list1[order(unlist(list1))]
ogvalues_common_species_names <- list2[order(unlist(list2))]
ogvalues_host_categories
ogvalues_common_species_names


#smaller sample size for testing
library(dplyr)
testk_9 <- slice_sample(k_9_test2[,-c(1:3)], n = 60 ) #keep common_species_names and kmers

#PCA - dimensionality reduction
pca <- prcomp(k_9_test2[,-c(1:3)] , scale = TRUE)
#plot
plot(pca$x[,1], pca$x[,2])
## make a scree plot
pca.var <- pca$sdev^2
pca.var.per <- round(pca.var/sum(pca.var)*100, 1)
barplot(pca.var.per, main="Scree Plot", xlab="Principal Component", ylab="Percent Variation")

#doesnt work :(
library(ggplot2)

pca.data <- data.frame(Sample=rownames(pca$x), X=pca$x[,1], Y=pca$x[,2])
pca.data

ggplot(data=pca.data, aes(x=X, y=Y, label=Sample)) +
  geom_text() +
  xlab(paste("PC1 - ", pca.var.per[1], "%", sep="")) +
  ylab(paste("PC2 - ", pca.var.per[2], "%", sep="")) +
  theme_bw() +
  ggtitle("My PCA Graph")

#loading scores
loading_scores <- pca$rotation[,1]
gene_scores <- abs(loading_scores) ## get the magnitudes
gene_score_ranked <- sort(gene_scores, decreasing=TRUE)
top_10_genes <- names(gene_score_ranked[1:10])

top_10_genes ## show the names of the top 10 genes

pca$rotation[top_10_genes,1] ## show the scores (and +/- sign)

