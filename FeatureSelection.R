# Load the required libraries ----
library(caret);
library(infotheo);

# Reading the Dataset ----
data <- read.csv("Final_Processed_Data.csv");

# Converting categorical variables into factors ----
categorical_columns <- c(13,14);
data[, categorical_columns] <- lapply(data[, categorical_columns], as.factor);

# Split Data into predictors and target ----
X <- data[, -ncol(data)];
Y <- data[, ncol(data)];

# Recursive Feature Elimination ----

cat("\n\nRFE Model \n\n");
# Define Control parameters for RFE
control <- rfeControl(functions = rfFuncs, method = "cv", number = 10);

# Perform RFE
result = rfe(x = X, y = Y, sizes = c(1:ncol(X)), rfeControl = control);

# Printing and Plotting
print(result);

plot(result);

# Mutual Information ----
cat("\n\nMutual Information\n\n");
X = X[,-ncol(X)];
X_discrete = apply(X, 2, function(x) cut(x, breaks=5, labels = FALSE));

# Getting the score 
mi_score = sapply(X_discrete, function(x) mutinformation(x, Y));

# Ranking the features
ranked_features = rank_features(mi_score);

# Select the top 10 features 
top_features = cutoff.k(rank_features, k=10);

# printing top features 
print(top_features);
