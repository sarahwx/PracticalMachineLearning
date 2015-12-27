# download file
setwd("./Workspace/xwu/project8")
fileUrl1 <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
fileUrl2 <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
download.file(fileUrl1, destfile = "./pml-training.csv", method = "curl")
download.file(fileUrl2, destfile = "./pml-testing.csv", method = "curl")
training <- read.csv("pml-training.csv")
testing <- read.csv("pml-testing.csv")

# load caret pacakge
library(caret)

# subset dataset with appropriate data
list <- c(8:11, 46:49, 84:86, 102, 122:124, 140, 160)
training_sub <- training[,list]
testing_sub <- testing[,list]

# 60% as training dataset, 40% as test dataset, with the 20 given test dataset for validation only
inTrain <- createDataPartition(y=training_sub$classe, p=0.6, list=FALSE)
training_60 <- training_sub[inTrain,]
training_40 <- training_sub[-inTrain,]

# preprocess with principal components analysis (PCA) to reduce number of predictors
# test different models
library(caret)
model_rf <- train(classe ~ ., data = training_60, preProcess = "pca", method = "rf")
predict_rf <- predict(model_rf, testing_sub)
model_gbm <- train(classe ~ ., data = training_60, preProcess = "pca", method = "gbm", verbose = FALSE)
predict_gbm <- predict(model_gbm, testing_sub)

# exploratory data analysis
table(predict_rf, predict_gbm)
eq_predict <- (predict_rf == predict_gbm)
qplot(total_accel_belt, total_accel_forearm, color = eq_predict, data=testing_sub)

# generate txt answers
answers = c("B", "A", "B", "A" ,"A", "E" ,"D", "B" ,"A", "A", "B", "C", "B", "A", "E", "E", "A", "B", "B", "B")
pml_write_files = function(x){
        n = length(x)
        for(i in 1:n){
                filename = paste0("problem_id_",i,".txt")
                write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
        }
}
pml_write_files(answers)

