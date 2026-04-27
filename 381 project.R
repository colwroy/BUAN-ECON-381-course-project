library(dplyr)
library(ggplot2)
library(tidyverse)

#load data
housing1 <- read.csv("https://raw.githubusercontent.com/colwroy/BUAN-ECON-381-course-project/refs/heads/main/Housing.csv")
summary(housing1)

#binary variables changed from yes/no to 1 and 0
housing1$mainrbinary <- ifelse(housing1$mainroad == 'yes', 1, 0)
housing1$groombinary <- ifelse(housing1$guestroom == 'yes', 1, 0)
housing1$basementbinary <- ifelse(housing1$basement == 'yes', 1, 0)
housing1$hwaterbinary <- ifelse(housing1$hotwaterheating == 'yes', 1, 0)
housing1$acbinary <- ifelse(housing1$airconditioning == 'yes', 1, 0)
housing1$prefareabinary <- ifelse(housing1$prefarea == 'yes', 1, 0)

#create new DF with new binary columns
housing2 <- housing1[1:5]
housing2 <- cbind(housing2,housing1[11])
housing2 <- cbind(housing2,housing1[14:19])

#correlation matrix
#strongest correlates with price are area, number of bathrooms,
#number of stories, and acbinary
cor(housing2)


#split the data first into training (70%) and holdout (30%)
set.seed(123)
housing2$randnum <- runif(dim(housing2)[1], min = 0, max = 1)
housing2_training <- housing2[housing2$randnum<0.7,]
housing2_holdout <- housing2[housing2$randnum>0.7,]

#split holdout into testing (50%) and holdout (50%)
housing2_holdout$randnum <- runif(dim(housing2_holdout)[1], min = 0, max = 1)
housing2_testing <- housing2_holdout[housing2_holdout$randnum<0.5,]
housing2_validation <- housing2_holdout[housing2_holdout$randnum>0.5,]

#export split data for sharing in CSV format
write.csv(housing2_training, "housing2_training.csv", row.names = TRUE)
write.csv(housing2_testing, "housing2_testing.csv", row.names = TRUE)
write.csv(housing2_validation, "housing2_validation.csv", row.names = TRUE)

#regression
############################################################################

#bivariate model using house area and price
priceVarea <- lm(price~area, data = housing2_training)
summary(priceVarea)
ggplot(housing2_training, aes(x = area, y = price)) + geom_point() +geom_smooth(method = "lm")

#logistic classifcation model
summary(glm(acbinary~price+stories,data=housing1, family = "binomial"))



