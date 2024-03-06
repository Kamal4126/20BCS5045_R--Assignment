#Asignment1
library(caret) 
library(class) 
library(ggplot2)
library(rpart)
library(randomForest)

# Load the diabetes dataset
data <- read.csv("D:/data analysis/meriskill/meriskill project 2/diabetes.csv")

head(data)
summary(data)

features <- c("Pregnancies", "Glucose", "BloodPressure", "SkinThickness", "Insulin", "BMI", "DiabetesPedigreeFunction", "Age","Outcome")

# checking columns wise NAN values
colSums(is.na(data))

#Heatmap
correlation_matrix <- cor(data[, -9])
correlation_data <- reshape2::melt(correlation_matrix)
ggplot(data = correlation_data, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "red", high = "green") + 
  labs(title = "Correlation Heatmap", x = "", y = "")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10))

#Splitting the dataset
set.seed(40)
index <- createDataPartition(data$Outcome, p = 0.85, list=FALSE)
train_data <- data[index, ]
test_data <- data[-index, ]
train_labels <- factor(data$Outcome[index], levels = c(0,1))
test_labels <- factor(data$Outcome[-index], levels = c(0,1))

#Linear Regression
data$BloodPressure <- NULL
data$SkinThickness <- NULL
train <- data[1:540,]
test <- data[541:768,]
model <-glm(Outcome ~.,family=binomial(link='logit'),data=train)
summary(model)
#The top three most relevant features are “Glucose”, “BMI” and “Number of times pregnant” because of the low p-values.

#decision Tree
model2 <- rpart(Outcome ~ Pregnancies + Glucose + BMI + DiabetesPedigreeFunction, data=train,method="class")
plot(model2, uniform=TRUE, 
     main="Classification Tree for Diabetes")
text(model2, use.n=TRUE, all=TRUE, cex=.8)
box(which = "outer", lty = "solid")
#This means if a person’s BMI less than 45.4 and his/her Diabetes Pedigree function less than 0.8745, then the person is more likely to have diabetes.

#KNN classification
train_data$Outcome <- as.factor(train_data$Outcome)
test_data$Outcome <- as.factor(test_data$Outcome)

knn_model <- knn(train_data[, features], test_data[, features], train_data$Outcome, k = 3)

confusion_matrix <- table(knn_model, test_data$Outcome)
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
cat("k-NN Accuracy:", accuracy, "\n")
print(confusion_matrix)

#In this project, we compared the performance of Linear Regression, Decision Tree and KNN algorithms and found that Linear Regression performed better on this standard, unaltered dataset. After, Linear Regression there comes Decision Tree algorithm with more accuracy than the Decision Tree. Accuracy given by Linear Regression was 79%, Decision Tree was 74% and KNN was 71.3% 