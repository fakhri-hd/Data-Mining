library(naivebayes)
library(psych)
library(caret)
library(tidyverse)

##Upload Data
data <- read.csv("data.data", header = FALSE, fileEncoding="utf-16", sep = ";")
head(data)
sum(is.na(data))
str(data)

##Membagi Data menjadi Testing Data dan Training Data
datasample <- sample(2, nrow(data), replace = TRUE, prob = c(0.8,0.2))
trainingdata <- data[datasample==1, ]
testingdata <- data[datasample==2, ]

##Membuat model untuk V7
modelnaivInf <- naive_bayes(V7~.,data=trainingdata[,-8],laplace = TRUE)
modelnaivInf
summary(modelnaivInf)

##Melakukan Prediksi dan memeriksa tingkat Akurasi untuk model V7
prediksiInf <- predict(modelnaivInf, testingdata[,-8])
confusionMatrix(table(prediksiInf,testingdata$V7))

##Membuat model untuk V8
modelnaivNeph <- naive_bayes(V8~.,data=trainingdata[,-7],laplace = TRUE)
modelnaivNeph
summary(modelnaivNeph)

##Melakukan Prediksi dan memeriksa tingkat Akurasi untuk model V8
prediksiNeph <- predict(modelnaivNeph, testingdata[,-7])
confusionMatrix(table(prediksiNeph,testingdata$V8))
