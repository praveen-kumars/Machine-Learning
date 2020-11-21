#Loading packages
install.packages("FSelector")
install.packages("rpart")
install.packages("caret", dependencies = TRUE)
install.packages("dplyr")
install.packages("rpart.plot")
install.packages("xlsx")
install.packages("data.tree")
install.packages("catools")
library(FSelector)
library(rpart)
library(caret)
library(rpart.plot)
library(dplyr)
library(xlsx)
library(data.tree)
library(caTools)
library(ElemStatLearn)

#Loading the Excel file after setting working directory

setwd("C:\\Users\\hp\\Videos\\Decision Tree Recording")
add<-"ship.xls"
df <- read.xlsx(add, sheetName = "ship1") 

#Selecting only the meaningful columns for prediction

df <- select(df,survived, class, sex, age) 
df<-mutate(df,survived=factor(survived), class= as.numeric(class),age= as.numeric(age))

#Splitting into training and testing data

set.seed(123)
sample = sample.split(df$survived, SplitRatio = .70)
train = subset(df, sample == TRUE)
test = subset(df, sample == FALSE)

#Training the Decision Tree Classifier

tree <- rpart(survived ~.,data = train)

#Predictions

tree.survived.predicted <- predict(tree,test,type='class')

#Confusion Matrix for evaluating the model

confusionMatrix(tree.survived.predicted,test$survived)

#Visualizing the decision tree

prp(tree)

#Visualizing the 2D decision boundary

test <- test[,c(2,4,1)]
set = test
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('class', 'age')
y_grid = tree.survived.predicted
plot(set[, -3], main = 'Decision Tree Classification (Test set)',
     xlab = 'class', ylab = 'age',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))
