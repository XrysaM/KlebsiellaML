
#dimensionality reduction examples

###############

#PCA - 
#encode host_categories
library(CatEncoders)
library(ggplot2)

#temp save to change labels
k_9_saved <- k_9_test
#k_9_saved <- k_9_fix
#k_9_saved <- fix

lab = LabelEncoder.fit(k_9_saved$host_categories)
k_9_saved$host_categories = transform(lab, k_9_saved$host_categories)
#deixnei tis arxikes times
list <- unique(inverse.transform(lab, k_9_saved$host_categories))
ogvalues_host_categories <- list[order(unlist(list))]
ogvalues_host_categories

pca <- prcomp(k_9_saved , scale = TRUE)
## make a scree plot
pca.var <- pca$sdev^2
pca.var.per <- round(pca.var/sum(pca.var)*100, 1)
barplot(pca.var.per, main=paste("Scree Plot - ", length(k_9_saved)), 
        xlab="Principal Component", ylab="Percent Variation")
#plot
pca.data <- data.frame( X=pca$x[,1], Y=pca$x[,2], 
                        Samples = rownames(k_9_fix)) 
pca.data
hosts <- factor(k_9_saved$host_categories)
ggplot(data=pca.data, aes(x=X, y=Y, label=Samples, colour= hosts)) +
  geom_point() +
  xlab(paste("PC1 - ", pca.var.per[1], "%", sep="")) +
  ylab(paste("PC2 - ", pca.var.per[2], "%", sep="")) +
  theme_bw() +
  labs(caption = "1=birds, 2=cat, 3=cattle, 4=dog, \n 5=fox, 6=horse, 7=human, 8=pig")+
  ggtitle(paste("My PCA Graph - " , length(k_9_saved)))


#loading scores - most important kmers for the variation
loading_scores <- pca$rotation[,1]
gene_scores <- abs(loading_scores) ## get the magnitudes
gene_score_ranked <- sort(gene_scores, decreasing=TRUE)

top_kmers <- names(gene_score_ranked[1:100]) #how many kmers i want
top_kmers ## show the names of the top 100 kmers
pca$rotation[top_kmers,1] ## show the scores (and +/- sign)

#rm the temp dataframes
rm(k_9_saved)

##############

#LDA - Linear Discriminant Analysis  
library(MASS)
library(ggplot2)
#for each dataset
lda_set <- k_9_test
lda_set <- k_9_fix
lda_set <- fix

model <- lda(formula = host_categories ~ ., data = k_9_test) 
# get the x,y coordinates for the LDA plot
data.lda.values <- predict(model)
# create a dataframe that has all the info we need to draw a graph
plot.data <- data.frame(X=data.lda.values$x[,1], Y=data.lda.values$x[,2], 
                        Hosts=lda_set$host_categories)

head(plot.data)

# draw a graph using ggplot2
ggplot(data=plot.data, aes(x=X, y=Y)) +
  geom_point(aes(color=Hosts)) +
  theme_bw()+
  ggtitle(paste("LDA Graph - " , length(lda_set)))

#rm(lda_set)

#QDA - Quadratic Discriminant Analysis -- Error :some group is too small for 'qda'
library(MASS)
# Fit the model
model <- qda(host_categories~., data = k_9_fix)
model
# Make predictions
predictions <- model %>% predict(test.transformed)
# Model accuracy
mean(predictions$class == test.transformed$Species)


########

#MDS - PCoA 
#classical Multi Dimensional scaling - Principal Coordinate Analysis
#same as PCA but calculate distances instead of correlations between samples.

#it doesnt work with negative values

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

