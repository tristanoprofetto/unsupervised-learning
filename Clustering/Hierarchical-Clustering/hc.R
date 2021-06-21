# Hierarchical Clustering

# Importing the dataset
df = read.csv('Mall_Customers.csv')
df = df[4:5]

# Splitting the dataset into the Training set and Test sets..

library(caTools)

split = sample.split(df$DependentVariable, SplitRatio = 0.8)
training_set = subset(df, split == TRUE)
test_set = subset(df, split == FALSE)

# Feature Scaling
training_set = scale(training_set)
test_set = scale(test_set)

# Using the dendrogram to find the optimal number of clusters
dendrogram = hclust(d = dist(df, method = 'euclidean'), method = 'ward.D')
plot(dendrogram,
     main = paste('Dendrogram'),
     xlab = 'Customers',
     ylab = 'Euclidean distances')

# Fitting Model
hc = hclust(d = dist(df, method = 'euclidean'), method = 'ward.D')
y_hc = cutree(hc, 5)

# Visualising the clusters
library(cluster)
clusplot(dataset,
         y_hc,
         lines = 0,
         shade = TRUE,
         color = TRUE,
         labels= 2,
         plotchar = FALSE,
         span = TRUE,
         main = paste('Clusters of customers'),
         xlab = 'Annual Income',
         ylab = 'Spending Score')