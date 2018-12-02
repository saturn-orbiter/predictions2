---
title: "Prediction Assignment"
author: "Wilfredo Rada"
date: "December 2, 2018"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = FALSE)
```

## Predicting Efficiency of Weightlifting Exercises

This projection is about predicting the efficiency of weight lifting exercises classified as A, B, C, D or E, storead in a variable called _classe_ based on several measurements numbering 52 from a total of 160 after removing non-pertinent, and practically missing data.

The files that contain data for analysis are _pml-training.csv_, and _pml-testing.csv_ which are referred in the code as datasetA, and datasetB, respectively.

The source of the data is the following study:

__Velloso, E.; Bulling, A.; Gellersen, H.; Ugulino, W.; Fuks, H. Qualitative Activity Recognition of Weight Lifting Exercises. Proceedings of 4th International Conference in Cooperation with SIGCHI (Augmented Human '13) . Stuttgart, Germany: ACM SIGCHI, 2013.__

The modeling and cross-validation methods applied are _gbm_, and _k-fold_ respectively.

The model is then applied to predict the efficiencies for the data in datasetB.

### Preliminary Analysis

Premilinary inspection of the two datasets 
gave the following information:

```{r datasets, echo=TRUE}
datasetA  <- read.csv("pml-training.csv")
datasetB  <- read.csv("pml-testing.csv")
```

The number observation and variables in the training set is:

```{r datasetA_str}
dim(datasetA)
```

The number observation and variables in the testing set is:

```{r datasetB_str}
dim(datasetB)
```

Further inspection on the supposed 'training' data shows that there are a lot of columnns with a significant number of, if not entirely contains, NA values. 


```{r NA_Count, echo=TRUE}
na_count <-sapply(datasetA, function(y) sum(length(which(is.na(y)))))
na_count <- data.frame(na_count)

```

Inspection of the second dataset, datasetB, supposedly the testing set shows that there is no column named _classe_! Instead it contains the column named _problem\_id_ that does not appear in datasetA, and its values are identical to those of the column _X_. The latter column will be removed from both datasets, _problem\_id_ from datasetB, and all those with significantly many NA values from datasetA. It can be calculated that 97.9% of values in every of the said columns is NA.

### Removal of Some Columns

Columns to be removed from datasetA are:
1. All columns with predominantly NA values
2. Columns 1 to 7 (from result in previous step)
3. Columns 5 to 13 (from result in previous step)
4. Several columns containing little data.

The same process is applied to datasetB.

```{r removal}
rmColsFrom <- function(datasetA) {
# get names of all columns with predominantly NA values
# add first column of row names
na_count = data.frame(rname=rownames(na_count), na_count)
names.rm = rownames(na_count[na_count$na_count != 0,])
datasetA = datasetA[, !(colnames(datasetA) %in% names.rm )]
# remove additional columns not relevant or practically no data
datasetA = datasetA[, -(1:7)]
datasetA = datasetA[, -(5:13)]
c1 = c("kurtosis_roll_arm","kurtosis_picth_arm", "kurtosis_yaw_arm")
c2 = c("skewness_roll_arm","skewness_pitch_arm", "skewness_yaw_arm" )
c3 = c("kurtosis_roll_dumbbell","kurtosis_picth_dumbbell", "kurtosis_yaw_dumbbell")
c4 = c("skewness_roll_dumbbell","skewness_pitch_dumbbell", "skewness_yaw_dumbbell" )
c5 = c("max_yaw_dumbbell", "min_yaw_dumbbell", "amplitude_yaw_dumbbell")
c6 = c("kurtosis_roll_forearm","kurtosis_picth_forearm", "kurtosis_yaw_forearm")
c7 = c("skewness_roll_forearm","skewness_pitch_forearm", "skewness_yaw_forearm" )
c8 = c("max_yaw_forearm", "min_yaw_forearm", "amplitude_yaw_forearm")
names.rm = c(c1,c2,c3,c4,c5,c6,c7,c8)
datasetA = datasetA[, !(colnames(datasetA) %in% names.rm )]
}

# make 2 calls
datasetA <- rmColsFrom(datasetA)
datasetB <- rmColsFrom(datasetB)

```
### Modeling and Cross-Validation

After the removal of some columns in datasetA, it is the one used in k-fold cross validation, and is fit with method set to "gbm".

```{r lib}
require(caret)
```
```{r gbm, echo=TRUE}


set.seed(101) # set this so next statement is reproducible
train_Control = trainControl(method="cv", number=10)

gbm.fit <- train(classe ~., method="gbm", data=datasetA, verbose=FALSE, trControl=train_Control)

print(gbm.fit)
```
To see a comparison, let us fit the data with method set to 'lda' as follows:

```{r lda, echo=TRUE}

set.seed(501)
# create data partition
inTrain = createDataPartition(datasetA$classe, p=0.7, list=FALSE)
training = datasetA[inTrain,]
testing = datasetA[-inTrain,]
lda = train(classe ~., method="lda", data=training)
p.lda = predict(lda, testing)
confusionMatrix(p.lda, testing$classe)$overall[1]
```

### Predictions

For the datasetB on which prediction will be made on the 20 observations units, we have:

```{r predict20, echo=TRUE}
p20.gbm <- predict(gbm.fit, datasetB)
print(p20.gbm)
```

Using the lda model, here are the predictions:
```{r lda.p, echo=TRUE}
print(predict(lda, datasetB))
```