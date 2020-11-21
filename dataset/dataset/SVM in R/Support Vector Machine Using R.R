#SVM implementation using R studio
#read data from csv file 
animal_data <- read.csv(file.choose(), header = TRUE )

#create an is horse indicator variable
ishorse<- c(rep(-1,10),rep(+1,10))


#install e1071 library
install.packages("e1071")

#include library 
library(e1071)

#create data frame for performing svm
my.data <- data.frame(Height= animal_data['Height'],
                      Weight= animal_data['Weight'],
                      animal=as.factor(ishorse))

#view the created data frame
my.data

#plot the data 
plot(my.data[,-3],col=(ys+3)/2, pch=19); abline(h=0,v=0,lty=3)

# perform svm by calling the svm method and passing the parameters 
svm.model <- svm(animal ~ .,
                 data=my.data,
                 type='C-classification',
                 kernel='linear',
                 scale=FALSE)
