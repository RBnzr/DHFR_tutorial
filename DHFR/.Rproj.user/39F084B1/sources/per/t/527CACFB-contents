library(datasets)
library(tidyverse)
library(skimr)
library(RCurl)
library(caret)
library(ggplot2)

#Load Dataset
dhfr <- read_csv("dhfr_data.csv")

## summary statistics ##

dhfr$Y <- as.factor(dhfr$Y)
summary(dhfr)
summary(dhfr$Y)

# check If NA values exist
sum(is.na(dhfr))


skim(dhfr) #perform skim to show summary statistics 

#group data by Y(biological activity) then perform skim

skimed_data <-dhfr %>%
  group_by(Y) %>%
  skim()


## Quick Data Vizualizations

#scatter plots

plot(dhfr$moe2D_zagreb, dhfr$moe2D_weinerPol, xlab = "2D Zagreb", ylab = "2D WeinerPol", col = dhfr$Y ) # red circle show activeity (Y)

#histogram
hist(dhfr$moe2D_zagreb)


# feature plot

## install.packages(c('caret', 'skimr', 'RANN', 'randomForest', 'fastAdaboost', 'gbm', 'xgboost', 'caretEnsemble', 'C50', 'earth'))


featurePlot(x = dhfr[,2:21],
            y = dhfr$Y,
            plot = "box",
            strip = strip.custom(par.strip.text=list(cex=.7)),
            scales = list(x = list(relation="free"),
                          y = list(relation="free")))

# set seed
set.seed(100)

#Split model randomly

trainingIndex <- createDataPartition(dhfr$Y, p = 0.8, list = FALSE) # create index for 80% of dataset
trainingSet <- dhfr[trainingIndex,] # Split dataset with index (80% of dataset used for training)
testingSet <- dhfr[-trainingIndex,] # Use remaining 20% for testing 

#Compare Training Set and Testing Set plot

##skimed_data %>%
##  ggplot(aes()) +
##  geom_bar() +
##  facet_wrap(~Y, scales = "free") ## idelly use all 325 variables and show what proportion of them is active vs inactive (not sure how to add more than 2 variables in ggplot)


### building the mdoels

# Create SVM model 

model <- train(Y ~ ., data = trainingSet,
               method = "svmPoly",
               na.action = na.omit,
               preProcess= c("scale", "center"),
               trControl = trainControl(method="cv", number=10),
               TuneGrid = data.frame(degree=1, scale=1, C=1)
)

# Build CV model
model.cv <- train(Y ~ ., data = trainingSet,
                  method = "svmPoly",
                  na.action = na.omit,
                  trControl = trainControl(method="cv",number=10),
                  tuneGrid = data.frame(degree=1,scale=1,C=1)
)

# Apply model for prediction

model.training <- predict(model, trainingSet) 
model.testing <- predict(model,testingSet)
model.cv <- predict(model.cv, trainingSet)

# Model performance 
model.training.confusion <- confusionMatrix(model.training,trainingSet$Y)
model.testing.confusion <- confusionMatrix(model.testing,testingSet$Y)
model.cv.confusion <- confusionMatrix(model.cv,trainingSet$Y)

model.training.confusion
model.testing.confusion
model.cv.confusion

#Feature Importance


Importance <- varImp(model)
plot(Importance)
