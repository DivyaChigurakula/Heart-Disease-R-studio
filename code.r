# Get the current working directory
getwd()

# Set the working directory to a specific path
setwd("C:/Users/Admin/OneDrive - The University of Memphis/Documents")
s
# Read data from the file 'heart.dat' with headers
mydata <- read.table("heart.dat", header = TRUE)

# Display the data
mydata

# Convert a column of the data to a factor
a <- as.factor(mydata$X2)

# Create a logistic regression model
m <- glm(a ~ . - X2, family = binomial, data = mydata)

# Display a summary of the logistic regression model
summary(m)

# Calculate AIC (Akaike Information Criterion) for the model
AIC(m)

# Calculate BIC (Bayesian Information Criterion) for the model
BIC(m)

# Predict probabilities using the logistic regression model
P_logis <- predict(m, newdata = mydata, type = "response")

# Create a vector for the predicted class labels
C_logis <- rep(1, length(a))

# Assign class labels based on a probability threshold of 0.5
C_logis[P_logis > 0.5] <- 0

# Create a confusion matrix
C = table(C_logis, a)

# Calculate the accuracy from the confusion matrix
accuracy <- sum(diag(C)) / sum(C)

# Display accuracy
accuracy

# Create a new logistic regression model with specific predictor variables
mr <- glm(a ~ X4.0 + X130.0 + X109.0 + X3.0 + X3.0.1 + X1.0, family = binomial, data = mydata)

# Display a summary of the new logistic regression model
summary(mr)

# Predict probabilities using the new logistic regression model
P_logis1 <- predict(mr, newdata = mydata, type = "response")

# Create a vector for the predicted class labels
C_logis1 <- rep(1, length(a))

# Assign class labels based on a probability threshold of 0.5
C_logis1[P_logis1 > 0.5] <- 0

# Create a confusion matrix
C1 = table(C_logis1, a)

# Calculate the accuracy from the confusion matrix
accuracy1 <- sum(diag(C1)) / sum(C1)

# Display accuracy
accuracy1

# Compare AIC and BIC between the two logistic regression models
AIC(m, mr)
BIC(m, mr)

# Install and load the 'e1071' package
install.packages("e1071")
library(e1071)

# Create a Naive Bayes model
M_NB <- naiveBayes(X2 ~ X70.0 + X4.0 + X130.0 + X322.0 + X0.0 + X2.0 + X109.0 + X0.0.1 + X2.4 + X2.0.1 + X3.0 + X3.0.1 + X1.0, data = mydata)

# Predict probabilities and class labels using the Naive Bayes model
P_NB <- predict(M_NB, newdata = mydata, type = "raw")
C_NB <- predict(M_NB, newdata = mydata, type = "class")

# Create a confusion matrix
C = table(C_NB, mydata$X2)

# Calculate the accuracy from the confusion matrix
accuracy <- sum(diag(C)) / sum(C)

# Display accuracy
accuracy

# Install and load the 'MASS' package
install.packages("MASS")
library(MASS)

# Create a Linear Discriminant Analysis (LDA) model
M_LDA <- lda(X2 ~ X70.0 + X4.0 + X130.0 + X322.0 + X0.0 + X2.0 + X109.0 + X0.0.1 + X2.4 + X2.0.1 + X3.0 + X3.0.1 + X1.0, data = mydata)

# Predict posterior probabilities and class labels using the LDA model
P_LDA <- predict(M_LDA)$posterior
C_LDA <- predict(M_LDA)$class

# Create a confusion matrix
C = table(C_LDA, mydata$X2)

# Calculate the accuracy from the confusion matrix
accuracy <- sum(diag(C)) / sum(C)

# Display accuracy
accuracy

# Create a Quadratic Discriminant Analysis (QDA) model
M_QDA <- qda(X2 ~ X70.0 + X4.0 + X130.0 + X322.0 + X0.0 + X2.0 + X109.0 + X0.0.1 + X2.4 + X2.0.1 + X3.0 + X3.0.1 + X1.0, data = mydata)

# Predict posterior probabilities and class labels using the QDA model
P_QDA <- predict(M_QDA)$posterior
C_QDA <- predict(M_QDA)$class

# Create a confusion matrix
C = table(C_QDA, mydata$X2)

# Calculate the accuracy from the confusion matrix
accuracy <- sum(diag(C)) / sum(C)

# Display accuracy
accuracy

# Install and load the 'ROCR' package
install.packages("ROCR")
library(ROCR)

# Create a prediction object for logistic regression
prd_logis <- prediction(P_logis, a)

# Create a performance object for ROC analysis
perf_logis <- performance(prd_logis, "tpr", "fpr")

# Plot the ROC curve for logistic regression
plot(perf_logis, col = "red", asp = 1, lwd = 2, main = paste0("ROC curves for logistic regression"))
abline(h = 0:5 * 0.2, v = 0:5 * 0.2, lty = 2)

# Create a prediction object for Naive Bayes
prd_NB <- prediction(P_NB[, 2], a)

# Create a performance object for ROC analysis
perf_NB <- performance(prd_NB, "tpr", "fpr")

# Plot the ROC curve for Naive Bayes
plot(perf_NB, col = "black", asp = 1, lwd = 2, main = paste0("ROC curves for naive bayes"))
abline(h = 0:5 * 0.2, v = 0:5 * 0.2, lty = 2)

# Create a prediction object for LDA
prd_LDA <- prediction(P_LDA[, 2], a)

# Create a performance object for ROC analysis
par(new = TRUE)
plot(perf_LDA, col = "blue", asp = 1, lwd = 2)

# Create a prediction object for QDA
prd_QDA <- prediction(P_QDA[, 2], a)

# Create a performance object for ROC analysis
par(new = TRUE)
plot(perf_QDA, col = "green", lwd = 2, asp = 1)

# Calculate the AUC (Area Under the Curve) for the logistic regression model
AUC_logis <- performance(prd_logis, "auc")@y.values[[1]]

# Calculate the AUC for the Naive Bayes model
AUC_NB <- performance(prd_NB, "auc")@y.values[[1]]

# Calculate the AUC for the LDA model
AUC_LDA <- performance(prd_LDA, "auc")@y.values[[1]]

# Calculate the AUC for the QDA model
AUC_QDA <- performance(prd_QDA, "auc")@y.values[[1]]

# Create a legend with AUC scores and model labels
legend(
  0.5, 0.3,
  legend = c("AUC scores:", paste0("by logis: ", round(AUC_logis, digits = 3)),
              paste0("by NB: ", round(AUC_NB, digits = 3)),
              paste0("by LDA: ", round(AUC_LDA, digits = 3)),
              paste0("by QDA: ", round(AUC_QDA, digits = 4))),
  col = c(NA, "red", "black", "blue", "green"),  # Colors for legend items
  lwd = 2,  # Line width
  cex = 0.8  # Text size
)


