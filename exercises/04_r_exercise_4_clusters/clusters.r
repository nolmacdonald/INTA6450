# Clustering example
library(MASS)
library(ggplot2)
wages<-read.csv('http://inta.gatech.s3.amazonaws.com/wage2.csv')
subwages <-wages[,c("wage","IQ","KWW","educ","age","married")]
# Cluster based only on a subset of the data
cluster.results<-kmeans(subwages, 4, nstart=5)
# Calculate clusters, using 5 different starting samples
print(cluster.results)
# Look at cluster results
subwages$cluster<-as.factor(cluster.results$cluster)
# The 'cluster' element of the results is the cluster which each data
# point belongs

ggplot(data=subwages, aes(x=educ, y=wage)) + geom_point(aes(colour=cluster))  + 
  geom_point(data=data.frame(cluster.results$centers), colour='red', size=7)
ggplot(data=subwages, aes(x=educ, y=IQ)) + geom_point(aes(colour=cluster)) + 
  geom_point(data=data.frame(cluster.results$centers), colour='red', size=7)
# We can plot the data points with colors for the different clusters.
# The colors match up differently along different dimensions

# If you want to see these plots with the data points less on top of each other,
# 'geom_jitter' adds soem noise to each point to move them off of each other:
ggplot(data=subwages, aes(x=educ, y=wage)) + geom_jitter(aes(colour=cluster)) + 
  geom_point(data=data.frame(cluster.results$centers), colour='red', size=7)

# When you look at these graphs, it's clear that clustering is mostly happening 
# on wage. Why is that? How could you fix it?

####################
# Standardize before doing clustering
zscore<-function(x){
  out<-(x - mean(x))/sd(x)
  return(out)
}
subwages2<-subwages
subwages2$cluster<-NULL
for (col in names(subwages2)){
  subwages2[,col]<-zscore(subwages2[,col])
}
# This loop standardizes the data (subtracts mean and divides by standard deviation)
# so that the data is in z-scores

cluster.results2<-kmeans(subwages2, 4, nstart=5)
subwages$cluster2<-as.factor(cluster.results2$cluster)
ggplot(data=subwages, aes(x=educ, y=wage)) + geom_point(aes(colour=cluster2))
ggplot(data=subwages, aes(x=educ, y=IQ)) + geom_point(aes(colour=cluster2))
# Plot the original data using the new clusters - note there is more of an IQ-based 
# pattern and less a pure wages one.

# Explore clusters in this data:
# 1) What happens if you use a different number of clusters?
# 2) How many clusters do you think there should be?
# 3) How good a fit can you get of wages using these clusters?

##########
# Principal Components Analysis
wages<-read.csv('http://inta.gatech.s3.amazonaws.com/wage2.csv')
subwages <-wages[,c("IQ","KWW","educ")]
# Create a subset of wages with just a few columns
principal.components <- prcomp(subwages, retx=T, center=T, scale=T)
# Do a principal components analysis on just the columns in subwages
print(principal.components$rotation) # The weights on each variable
summary(principal.components) # The proportion of variance explained
plot(principal.components) # Graphical depiction of proportion of variance
sw <- cbind(wages,data.frame(principal.components$x))
# Add the new principal components as columns to a new data frame, sw
# (New so we don't muck up going back and running other stuff on wages now)
one.pc <- lm(wage ~ PC1, data= sw)
all.variables <- lm(wage ~ IQ + KWW + educ, data= sw)
summary(one.pc)
summary(all.variables)
# How much better does having three variables do compared to the one principal
# component here? (Look at R^2)

# Try to replicate this using more variables, including perhaps age and marital 
# Status. Would you see the same thing?

# Elbow plot for cluster analysis
wss <- (nrow(subwages2) - 1) * sum(apply(subwages2, 2, var))
for (i in 2:15) {
  wss[i] <- sum(kmeans(subwages2, centers=i, nstart=5)$tot.withinss)
}
plot(1:15, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares", main="Elbow Plot for Cluster Analysis")

# Scree plot for PCA
plot(principal.components, type="lines", main="Scree Plot for PCA")

# Extract R-squared values
r_squared_values <- data.frame(
  Model = c("PC1 Only", "All Variables"),
  R_squared = c(summary(one.pc)$r.squared, summary(all.variables)$r.squared)
)
# Bar plot for R-squared values comparison
r_squared_plot <- ggplot(r_squared_values, aes(x = Model, y = R_squared, fill = Model)) +
  geom_bar(stat = "identity", width = 0.5) +
  labs(title = "Comparison of R-squared Values",
       x = "Model",
       y = "R-squared") +
  theme_minimal() +
  theme(legend.position = "none")

# Display the plot
print(r_squared_plot)

# Save the plot
ggsave("r_squared_comparison_plot.png", plot = r_squared_plot, width = 6, height = 4, dpi = 300)