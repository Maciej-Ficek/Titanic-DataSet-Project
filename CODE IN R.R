
# setup
#install.packages('dplyr')
#install.packages("utils")
#install.packages("nnet")
#install.packages("rpart")
#install.packages("rpart.plot")
#install.packages("ggplot2")
library("rpart")
library("rpart.plot")
library("dplyr")
library("utils")
library("nnet")
library("forecast")
library("ggplot2")
library("pROC")

# wczytanie danych
# zmień ścieżki na swoje przed uruchomieniem
values <- read.csv('C:\\Users\\HP\\Downloads\\gender_submission.csv')
train <- read.csv('C:\\Users\\HP\\Downloads\\train.csv')
test <- read.csv('C:\\Users\\HP\\Downloads\\test.csv')
#podgląd danych
View(values)
View(train)
View(test)
# preporcessing danych
train <- data.frame(train)
test <- data.frame(test)
columns_with_na_train <- colSums(is.na(train)) > 0
columns_with_na_test <- colSums(is.na(test)) > 0
columns_with_na_train
columns_with_na_test
sum(is.na(train))
sum(is.na(test))

train <- train %>%
  group_by(Sex, Pclass) %>%
  mutate(Age = ifelse(is.na(Age), mean(Age, na.rm = TRUE), Age)) %>%	ungroup()
test <- test %>%
  group_by(Sex, Pclass) %>%
  mutate(Age = ifelse(is.na(Age), mean(Age, na.rm = TRUE), Age)) %>%
  ungroup()

replacement <- c("male" = 0, "female" = 1)
train$Sex <- replacement[train$Sex]
test$Sex <- replacement[test$Sex]
cor(train[, c("Survived", "Pclass", "Age", "Sex", "SibSp")])
cor(values$Survived, test[, c("Pclass", "Age", "Sex", "SibSp")])
train$Cabin <- NULL
test$Cabin <- NULL
train$Ticket <- NULL
test$Ticket <- NULL


# trenowanie modeli, predykcje, pomiary błędów
lr_model <- glm(Survived ~ Pclass + Sex + Age + SibSp + Parch + Embarked + Fare, data = train, family = binomial)
lr_model
dt_model <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Embarked + Fare, data = train, method = "class")
dt_model$y <- dt_model$y - 1
rpart.plot(dt_model, extra=1, under=TRUE)

lr_probabilities <- predict(lr_model, newdata = test, type = "response") 
dt_probabilities <- predict(dt_model, newdata = test, type = "class") 

lr_result <- data.frame(PassengerId = values$PassengerId, ProbabilityToSurvive = lr_probabilities)
dt_result <- data.frame(PassengerId = values$PassengerId, ProbabilityToSurvive = dt_probabilities)
values$lr_predicted <- round(lr_result$ProbabilityToSurvive)
values$dt_predicted <- dt_result$ProbabilityToSurvive
values$dt_predicted <- as.double(values$dt_predicted - 1)
View(values)

lr_bad <- sum(abs(values$lr_predicted - values$Survived))
dt_bad <- sum(abs(as.integer(values[, 4]) -1 - values[,2]))

lr_conf_matrix <- table(values$Survived, values$lr_predicted)

dt_conf_matrix <- table(values$Survived, values$dt_predicted)

anova_result <- anova(lr_model, dt_model)

# te same modele ale tylko zmienne Pclass i Sex

lr2_model <- glm(Survived ~ Pclass + Sex, data = train, family = binomial)
dt2_model <- rpart(Survived ~ Pclass + Sex, data = train, method = "class")
dt2_model$y <- dt2_model$y - 1
rpart.plot(dt2_model, extra=1, under=TRUE)

lr2_probabilities <- predict(lr2_model, newdata = test, type = "response") 
dt2_probabilities <- predict(dt2_model, newdata = test, type = "class") 
values2 <- values
lr2_result <- data.frame(PassengerId = values$PassengerId, ProbabilityToSurvive = lr2_probabilities)
dt2_result <- data.frame(PassengerId = values$PassengerId, ProbabilityToSurvive = dt2_probabilities)
values2$lr2_predicted <- round(lr2_result$ProbabilityToSurvive)
values2$dt2_predicted <- dt2_result$ProbabilityToSurvive
View(values2)

lr2_bad <- sum(abs(values2$lr2_predicted - values2$Survived))
dt2_bad <- sum(abs(as.integer(values2[, 6]) -1 - values2[,2]))

lr2_conf_matrix <- table(values2$Survived, values2$lr2_predicted)

dt2_conf_matrix <- table(values2$Survived, values2$dt2_predicted)

anova2_result <- anova(lr2_model, dt2_model)

# porównanie modeli regresji

AIC(lr_model)
BIC(lr_model)

AIC(lr2_model)
BIC(lr2_model)

lr_model$coefficients
lr2_model$coefficients

lr_error_train_abs <- sum(abs(train$Survived - lr_model$fitted.values))
lr2_error_train_abs <- sum(abs(train$Survived - lr2_model$fitted.values))
lr_error_train_mse <- sum((train$Survived - lr_model$fitted.values)**2)
lr2_error_train_mse <- sum((train$Survived - lr2_model$fitted.values)**2)
lr_error_test_abs <- sum(abs(values$Survived - lr_probabilities))
lr2_error_test_abs <- sum(abs(values$Survived - lr2_probabilities))
lr_error_test_mse <- sum((values$Survived - lr_probabilities)**2)
lr2_error_test_mse <- sum((values$Survived - lr2_probabilities)**2)
