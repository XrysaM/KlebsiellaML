#dimensionality reduction examples


#PCA - 
pca <- prcomp(k_9_test , scale = TRUE)
#simple plot, rough not necessary
plot(pca$x[,1], pca$x[,2])
## make a scree plot
pca.var <- pca$sdev^2
pca.var.per <- round(pca.var/sum(pca.var)*100, 1)
barplot(pca.var.per, main="Scree Plot", 
        xlab="Principal Component", ylab="Percent Variation")
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

top_kmers ## show the names of the top 10 kmers
pca$rotation[top_kmers,1] ## show the scores (and +/- sign)



#LDA - Linear Discriminant Analysis  -- doest't work :(
library(MASS)
model <- lda(formula = Hosts ~ ., data = k_9_test) #"cannot allocate vector of size 64GB"
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
