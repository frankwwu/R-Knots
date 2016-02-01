# Human Activity Recognition

This machine learning project is to predict the correct or incorrect way of the unilateral dumbbell biceps curl based in the measured data. The data collected includes five different fashions: exactly according to the specification (Class A), throwing the elbows to the front (Class B), lifting the dumbbell only halfway (Class C), lowering the dumbbell only halfway (Class D) and throwing the hips to the front (Class E). [Detail](http://groupware.les.inf.puc-rio.br/har) 

Both [training data](https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv) and [test data](https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv) are provided.

The analysis begins with data cleaning and feature selection. The Random Forest training model with parallel processing is then created. The model also includes k-fold cross validation. In the end, the test data is used for prediction.

### Loading libraries


```r
library(randomForest)
library(doParallel)
library(caret)
library(ParallelForest)
```

### Reading the training data


```r
training<-read.csv("pml-training.csv", na.strings=c("NA", "#DIV/0!"))
dim(training)
```

```
## [1] 19622   160
```

### Feature engineering

Subset the original training dataset to include only the predictor features and the outcome variable, classe.


```r
isMissing <- sapply(training, function (x) any(is.na(x) | x == ""))
isPredictor <- !isMissing & grepl("classe|belt|[^(fore)]arm|dumbbell|forearm", names(isMissing))
features <- names(isMissing)[isPredictor]
features
```

```
##  [1] "roll_belt"            "pitch_belt"           "yaw_belt"            
##  [4] "total_accel_belt"     "gyros_belt_x"         "gyros_belt_y"        
##  [7] "gyros_belt_z"         "accel_belt_x"         "accel_belt_y"        
## [10] "accel_belt_z"         "magnet_belt_x"        "magnet_belt_y"       
## [13] "magnet_belt_z"        "roll_arm"             "pitch_arm"           
## [16] "yaw_arm"              "total_accel_arm"      "gyros_arm_x"         
## [19] "gyros_arm_y"          "gyros_arm_z"          "accel_arm_x"         
## [22] "accel_arm_y"          "accel_arm_z"          "magnet_arm_x"        
## [25] "magnet_arm_y"         "magnet_arm_z"         "roll_dumbbell"       
## [28] "pitch_dumbbell"       "yaw_dumbbell"         "total_accel_dumbbell"
## [31] "gyros_dumbbell_x"     "gyros_dumbbell_y"     "gyros_dumbbell_z"    
## [34] "accel_dumbbell_x"     "accel_dumbbell_y"     "accel_dumbbell_z"    
## [37] "magnet_dumbbell_x"    "magnet_dumbbell_y"    "magnet_dumbbell_z"   
## [40] "roll_forearm"         "pitch_forearm"        "yaw_forearm"         
## [43] "total_accel_forearm"  "gyros_forearm_x"      "gyros_forearm_y"     
## [46] "gyros_forearm_z"      "accel_forearm_x"      "accel_forearm_y"     
## [49] "accel_forearm_z"      "magnet_forearm_x"     "magnet_forearm_y"    
## [52] "magnet_forearm_z"     "classe"
```

```r
cleanData = training[,features]
dim(cleanData)
```

```
## [1] 19622    53
```

### Random Forest training model with parallel processing

Configuring parallel processing based on the [discription](https://github.com/lgreski/datasciencectacontent/blob/master/markdown/pml-randomForestPerformance.md). The 5-fold validation is specified in the <code>trainControl</code> function.


```r
# convention to leave 1 core for OS
cluster <- makeCluster(detectCores() - 1)
registerDoParallel(cluster)
# The number of resampling iterations is 5.
fitControl <- trainControl(method = "cv", number = 5, allowParallel = TRUE)
```

Creating the Random Forest training model; printing out the detail of resampling and accuracy.


```r
modelRF <- train(classe~., data=cleanData, method="rf", trControl=fitControl)
modelRF
```

```
## Random Forest 
## 
## 19622 samples
##    52 predictor
##     5 classes: 'A', 'B', 'C', 'D', 'E' 
## 
## No pre-processing
## Resampling: Cross-Validated (5 fold) 
## Summary of sample sizes: 15698, 15697, 15699, 15696, 15698 
## Resampling results across tuning parameters:
## 
##   mtry  Accuracy   Kappa      Accuracy SD   Kappa SD    
##    2    0.9940370  0.9924571  0.0015155204  0.0019169169
##   27    0.9938844  0.9922640  0.0004765701  0.0006026645
##   52    0.9892978  0.9864614  0.0008632824  0.0010912547
## 
## Accuracy was used to select the optimal model using  the largest value.
## The final value used for the model was mtry = 2.
```

Deregistering the parallel processing cluster; and printing out the cross-validated confusion matrix.


```r
stopCluster(cluster)
confusionMatrix.train(modelRF)
```

```
## Cross-Validated (5 fold) Confusion Matrix 
## 
## (entries are percentages of table totals)
##  
##           Reference
## Prediction    A    B    C    D    E
##          A 28.4  0.1  0.0  0.0  0.0
##          B  0.0 19.2  0.1  0.0  0.0
##          C  0.0  0.0 17.3  0.2  0.0
##          D  0.0  0.0  0.0 16.1  0.0
##          E  0.0  0.0  0.0  0.0 18.3
```

### Displaying and saving the model variables


```r
varImp(modelRF)
```

```
## rf variable importance
## 
##   only 20 most important variables shown (out of 52)
## 
##                      Overall
## roll_belt             100.00
## yaw_belt               79.70
## magnet_dumbbell_z      68.60
## magnet_dumbbell_y      62.75
## pitch_belt             62.02
## pitch_forearm          59.23
## magnet_dumbbell_x      50.73
## roll_forearm           49.67
## accel_belt_z           45.95
## magnet_belt_z          43.87
## accel_dumbbell_y       42.82
## roll_dumbbell          41.85
## magnet_belt_y          38.38
## accel_dumbbell_z       36.96
## roll_arm               34.66
## accel_forearm_x        34.20
## gyros_belt_z           32.63
## total_accel_dumbbell   29.61
## accel_dumbbell_x       28.05
## yaw_dumbbell           27.73
```

```r
modelRF$finalModel
```

```
## 
## Call:
##  randomForest(x = x, y = y, mtry = param$mtry) 
##                Type of random forest: classification
##                      Number of trees: 500
## No. of variables tried at each split: 2
## 
##         OOB estimate of  error rate: 0.41%
## Confusion matrix:
##      A    B    C    D    E  class.error
## A 5579    1    0    0    0 0.0001792115
## B   10 3783    4    0    0 0.0036871214
## C    0   16 3405    1    0 0.0049678551
## D    0    0   39 3174    3 0.0130597015
## E    0    0    1    6 3600 0.0019406709
```

```r
save(modelRF, file="modelRF.RData")
```

### Predicting with the test data set

Use the above prediction model to predict 20 different test cases specified in the test data file; then printing out the predicted classes of the 20 tests.


```r
test<-read.csv("pml-testing.csv", na.strings=c("NA", "#DIV/0!"))
isMissing <- sapply(test, function (x) any(is.na(x) | x == ""))
isPredictor <- !isMissing & grepl("classe|belt|[^(fore)]arm|dumbbell|forearm", names(isMissing))
features <- names(isMissing)[isPredictor]
testData<-test[,features]
predRF<-predict(modelRF, testData)
predRF
```

```
##  [1] B A B A A E D B A A B C B A E E A B B B
## Levels: A B C D E
```

### Conclusion

The Random Forest model can achieve high accuracy with relatively short runtime in this analysis.

### References

[Prediction Assignment Writeup](https://www.coursera.org/learn/practical-machine-learning/peer/R43St/prediction-assignment-writeup)

[Improving Performance of Random Forest in caret::train()](https://github.com/lgreski/datasciencectacontent/blob/master/markdown/pml-randomForestPerformance.md)

[Qualitative Activity Recognition of Weight Lifting Exercises](http://groupware.les.inf.puc-rio.br/public/papers/2013.Velloso.QAR-WLE.pdf)



