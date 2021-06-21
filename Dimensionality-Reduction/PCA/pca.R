# PCA

# Importing the dataset
df = read.csv('Wine.csv')

# Splitting the dataset into the Training set and Test set

library(caTools)

split = sample.split(df$Customer_Segment, SplitRatio = 0.8)
train = subset(df, split == TRUE)
test = subset(df, split == FALSE)

# Feature Scaling
train[-14] = scale(train[-14])
test[-14] = scale(test[-14])

# Applying PCA
# install.packages('caret')
library(caret)
# install.packages('e1071')
library(e1071)
pca = preProcess(x = train[-14], method = 'pca', pcaComp = 2)
train = predict(pca, train)
train = train[c(2, 3, 1)]
test = predict(pca, test)
test = test[c(2, 3, 1)]

# Fitting SVM to the Training set
# install.packages('e1071')
library(e1071)
classifier = svm(formula = Customer_Segment ~ .,
                 data = train,
                 type = 'C-classification',
                 kernel = 'linear')

# Predicting the Test set results
y_pred = predict(classifier, newdata = test[-3])

# Making the Confusion Matrix
cm = table(test[, 3], y_pred)

# Visualising the Training set results
library(ElemStatLearn)

X1 = seq(min(train[, 1]) - 1, max(train[, 1]) + 1, by = 0.01)
X2 = seq(min(train[, 2]) - 1, max(train[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('PC1', 'PC2')
y_grid = predict(classifier, newdata = grid_set)
plot(train[, -3],
     main = 'SVM (Training set)',
     xlab = 'PC1', ylab = 'PC2',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 2, 'deepskyblue', ifelse(y_grid == 1, 'springgreen3', 'tomato')))
points(train, pch = 21, bg = ifelse(train[, 3] == 2, 'blue3', ifelse(train[, 3] == 1, 'green4', 'red3')))

# Visualising the Test set results
library(ElemStatLearn)

X1 = seq(min(test[, 1]) - 1, max(test[, 1]) + 1, by = 0.01)
X2 = seq(min(test[, 2]) - 1, max(test[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('PC1', 'PC2')
y_grid = predict(classifier, newdata = grid_set)
plot(test[, -3], main = 'SVM (Test set)',
     xlab = 'PC1', ylab = 'PC2',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 2, 'deepskyblue', ifelse(y_grid == 1, 'springgreen3', 'tomato')))
points(test, pch = 21, bg = ifelse(test[, 3] == 2, 'blue3', ifelse(test[, 3] == 1, 'green4', 'red3')))