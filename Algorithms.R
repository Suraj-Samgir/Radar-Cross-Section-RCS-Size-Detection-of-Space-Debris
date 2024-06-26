# Load the Dataset ----
d <- read.csv("Final_Processed_Data.csv");

# Install and load required packages ----
# if (!requireNamespace("caret", quietly = TRUE)){
#   install.packages("caret"); 
# }
# 
# if (!requireNamespace("e1071", quietly = TRUE)){
#   install.packages("e1071");
# }
# 
# if (!requireNamespace("randomForest", quietly = TRUE)){
#   install.packages("randomForest");
# }

library(caret);
library(randomForest);
library(e1071);

# Converting categorical variables into factors ----
categorical_columns <- c(13,14);
d[, categorical_columns] <- lapply(d[, categorical_columns], as.factor);

# Shuffle data and split it for training and testing ----
set.seed(123); # for reproducibility
train_indices <- createDataPartition(d$RCS_SIZE, p = 0.8, list = FALSE); # 80% for training, 20% for testing
train_data <- d[train_indices, ];
test_data <- d[-train_indices, ];

# Create a training control object ----
ctrl = trainControl(method="repeatedcv", number = 10);

# Train the Gradient Boosting Machine (GBM) model ----
cat("\nGradient Boosting Machine (GBM) \n");
set.seed(123);
gbm_model = train(x = train_data[-c(14)], y = train_data$RCS_SIZE, method = "gbm", trControl=ctrl);

# Make predictions using the trained SVM model on test data
tp1 <- predict(gbm_model, test_data[, -c(14)]); # column 14 is the target variable
test_data$test_preds1 = tp1;

# Confusion matrix
cm1 <- confusionMatrix(test_data$RCS_SIZE, test_data$test_preds1);

# Show confusion matrix in table format
levels <- c("LARGE","MEDIUM","SMALL");
cmt1 <- table(factor(test_data$RCS_SIZE, levels = levels), factor(test_data$test_preds1, levels = levels), dnn = c("Actual","Predicted"));

#Calculate confusino matrix values for each class
tplarge1 = cmt1["LARGE","LARGE"];
fnlarge1 = sum(cmt1["LARGE",c('MEDIUM','SMALL')]);
fplarge1 = sum(c(cmt1["MEDIUM","LARGE"], cmt1["SMALL","LARGE"]));
tnlarge1 = sum(c(sum(cmt1["MEDIUM",c('MEDIUM','SMALL')]),sum(cmt1["SMALL",c('MEDIUM', 'SMALL')]))); 

tpmedium1 = cmt1["MEDIUM","MEDIUM"];
fnmedium1 = sum(cmt1["MEDIUM",c('LARGE','SMALL')]);
fpmedium1 = sum(c(cmt1["LARGE","MEDIUM"], cmt1["SMALL","MEDIUM"]));
tnmedium1 = sum(c(sum(cmt1["LARGE",c('LARGE','SMALL')]), sum(cmt1["SMALL",c('LARGE','SMALL')])));

tpsmall1 = cmt1["SMALL","SMALL"];
fnsmall1 = sum(cmt1["SMALL",c('LARGE','MEDIUM')]);
fpsmall1 = sum(c(cmt1["LARGE","SMALL"], cmt1["MEDIUM","SMALL"]));
tnsmall1 = sum(c(sum(cmt1["LARGE",c('LARGE','MEDIUM')]), sum(cmt1["MEDIUM",c('LARGE','MEDIUM')])));

# Extracting performance metrics
acc1 <- cm1$overall['Accuracy'];

P_large1 <- tplarge1/(tplarge1+fplarge1);
P_medium1 <- tpmedium1/(tpmedium1+fpmedium1);
P_small1 <- tpsmall1/(tpsmall1+fpsmall1);

R_large1 = tplarge1/(tplarge1+fnlarge1);
R_medium1 = tpmedium1/(tpmedium1+fnmedium1);
R_small1 = tpsmall1/(tpsmall1+fnsmall1);

f1_large1 = 2*(P_large1*R_large1)/(P_large1+R_large1);
f1_medium1 = 2*(P_medium1*R_medium1)/(P_medium1+R_medium1);
f1_small1 = 2*(P_small1*R_small1)/(P_small1+R_small1);

S_large1 = tnlarge1/(tnlarge1+fplarge1);
S_medium1 = tnmedium1/(tnmedium1+fpmedium1);
S_small1 = tnsmall1/(tnsmall1+fpsmall1);

# Printing them
print(paste("Accuracy:",acc1));

print(paste("Precision Large:",P_large1));
print(paste("Precision Medium:",P_medium1));
print(paste("Precision Small:",P_small1));

print(paste("Recall Large:",R_large1));
print(paste("Recall Medium:",R_medium1));
print(paste("Recall Small:",R_small1));

print(paste("F1 Score Large:",f1_large1));
print(paste("F1 Score Medium:",f1_medium1));
print(paste("F1 Score Small:",f1_small1));

print(paste("Specificity Large:",S_large1));
print(paste("Specificity Medium:",S_medium1));
print(paste("Specificity Small:",S_small1));

# Train the Random Forest Model ----
cat("\nRandom Forest (RF) \n");

set.seed(123);
rf_model <- randomForest(RCS_SIZE~., data = train_data, ntree = 500);
# model = train(x = train_data[-c(12)], y = train_data$RCS_SIZE, method="rf", trControl=ctrl);

# Make predictions using the trained SVM model on test data 
tp2 <- predict(rf_model, test_data[, -c(14)]); # column 12 is the target variable
test_data$test_preds2 = tp2;

# Confusion matrix
cm2 <- confusionMatrix(test_data$RCS_SIZE, test_data$test_preds2);

# Show confusion matrix in table format
levels <- c("LARGE","MEDIUM","SMALL");
cmt2 <- table(factor(test_data$RCS_SIZE, levels = levels), factor(test_data$test_preds2, levels = levels), dnn = c("Actual","Predicted"));

#Calculate confusino matrix values for each class
tplarge2 = cmt2["LARGE","LARGE"];
fnlarge2 = sum(cmt2["LARGE",c('MEDIUM','SMALL')]);
fplarge2 = sum(c(cmt2["MEDIUM","LARGE"], cmt2["SMALL","LARGE"]));
tnlarge2 = sum(c(sum(cmt2["MEDIUM",c('MEDIUM','SMALL')]),sum(cmt2["SMALL",c('MEDIUM', 'SMALL')])));                    
tpmedium2 = cmt2["MEDIUM","MEDIUM"];
fnmedium2 = sum(cmt2["MEDIUM",c('LARGE','SMALL')]);
fpmedium2 = sum(c(cmt2["LARGE","MEDIUM"], cmt2["SMALL","MEDIUM"]));
tnmedium2 = sum(c(sum(cmt2["LARGE",c('LARGE','SMALL')]), sum(cmt2["SMALL",c('LARGE','SMALL')])));

tpsmall2 = cmt2["SMALL","SMALL"];
fnsmall2 = sum(cmt2["SMALL",c('LARGE','MEDIUM')]);
fpsmall2 = sum(c(cmt2["LARGE","SMALL"], cmt2["MEDIUM","SMALL"]));
tnsmall2 = sum(c(sum(cmt2["LARGE",c('LARGE','MEDIUM')]), sum(cmt2["MEDIUM",c('LARGE','MEDIUM')])));

# Extracting performance metrics
acc2 <- cm2$overall['Accuracy'];

P_large2 <- tplarge2/(tplarge2+fplarge2);
P_medium2 <- tpmedium2/(tpmedium2+fpmedium2);
P_small2 <- tpsmall2/(tpsmall2+fpsmall2);

R_large2 = tplarge2/(tplarge2+fnlarge2);
R_medium2 = tpmedium2/(tpmedium2+fnmedium2);
R_small2 = tpsmall2/(tpsmall2+fnsmall2);

f1_large2 = 2*(P_large2*R_large2)/(P_large2+R_large2);
f1_medium2 = 2*(P_medium2*R_medium2)/(P_medium2+R_medium2);
f1_small2 = 2*(P_small2*R_small2)/(P_small2+R_small2);

S_large2 = tnlarge2/(tnlarge2+fplarge2);
S_medium2 = tnmedium2/(tnmedium2+fpmedium2);
S_small2 = tnsmall2/(tnsmall2+fpsmall2);

# Printing them
print(paste("Accuracy:",acc2));

print(paste("Precision Large:",P_large2));
print(paste("Precision Medium:",P_medium2));
print(paste("Precision Small:",P_small2));

print(paste("Recall Large:",R_large2));
print(paste("Recall Medium:",R_medium2));
print(paste("Recall Small:",R_small2));

print(paste("F1 Score Large:",f1_large2));
print(paste("F1 Score Medium:",f1_medium2));
print(paste("F1 Score Small:",f1_small2));

print(paste("Specificity Large:",S_large2));
print(paste("Specificity Medium:",S_medium2));
print(paste("Specificity Small:",S_small2));

# Train the Multinomial Logistic Regression Model ----
cat("\nMultinomial Logistic Regression (MLR) \n");

set.seed(123);
mlr_model <- train(RCS_SIZE~., data=train_data, method="multinom", trControl=ctrl);

# Make predictions using the trained SVM model on test data 
tp3 <- predict(mlr_model, test_data[, -c(14)]); # column 12 is the target variable
test_data$test_preds3 = tp3;

# Confusion matrix
cm3 <- confusionMatrix(test_data$RCS_SIZE, test_data$test_preds3);

# Show confusion matrix in table format
levels <- c("LARGE","MEDIUM","SMALL");
cmt3 <- table(factor(test_data$RCS_SIZE, levels = levels), factor(test_data$test_preds3, levels = levels), dnn = c("Actual","Predicted"));

#Calculate confusino matrix values for each class
tplarge3 = cmt3["LARGE","LARGE"];
fnlarge3 = sum(cmt3["LARGE",c('MEDIUM','SMALL')]);
fplarge3 = sum(c(cmt3["MEDIUM","LARGE"], cmt3["SMALL","LARGE"]));
tnlarge3 = sum(c(sum(cmt3["MEDIUM",c('MEDIUM','SMALL')]),sum(cmt3["SMALL",c('MEDIUM', 'SMALL')])));                    
tpmedium3 = cmt3["MEDIUM","MEDIUM"];
fnmedium3 = sum(cmt3["MEDIUM",c('LARGE','SMALL')]);
fpmedium3 = sum(c(cmt3["LARGE","MEDIUM"], cmt3["SMALL","MEDIUM"]));
tnmedium3 = sum(c(sum(cmt3["LARGE",c('LARGE','SMALL')]), sum(cmt3["SMALL",c('LARGE','SMALL')])));

tpsmall3 = cmt3["SMALL","SMALL"];
fnsmall3 = sum(cmt3["SMALL",c('LARGE','MEDIUM')]);
fpsmall3 = sum(c(cmt3["LARGE","SMALL"], cmt3["MEDIUM","SMALL"]));
tnsmall3 = sum(c(sum(cmt3["LARGE",c('LARGE','MEDIUM')]), sum(cmt3["MEDIUM",c('LARGE','MEDIUM')])));

# Extracting performance metrics
acc3 <- cm3$overall['Accuracy'];

P_large3 <- tplarge3/(tplarge3+fplarge3);
P_medium3 <- tpmedium3/(tpmedium3+fpmedium3);
P_small3 <- tpsmall3/(tpsmall3+fpsmall3);

R_large3 = tplarge3/(tplarge3+fnlarge3);
R_medium3 = tpmedium3/(tpmedium3+fnmedium3);
R_small3 = tpsmall3/(tpsmall3+fnsmall3);

f1_large3 = 2*(P_large3*R_large3)/(P_large3+R_large3);
f1_medium3 = 2*(P_medium3*R_medium3)/(P_medium3+R_medium3);
f1_small3 = 2*(P_small3*R_small3)/(P_small3+R_small3);

S_large3 = tnlarge3/(tnlarge3+fplarge3);
S_medium3 = tnmedium3/(tnmedium3+fpmedium3);
S_small3 = tnsmall3/(tnsmall3+fpsmall3);

# Printing them
print(paste("Accuracy:",acc3));

print(paste("Precision Large:",P_large3));
print(paste("Precision Medium:",P_medium3));
print(paste("Precision Small:",P_small3));

print(paste("Recall Large:",R_large3));
print(paste("Recall Medium:",R_medium3));
print(paste("Recall Small:",R_small3));

print(paste("F1 Score Large:",f1_large3));
print(paste("F1 Score Medium:",f1_medium3));
print(paste("F1 Score Small:",f1_small3));

print(paste("Specificity Large:",S_large3));
print(paste("Specificity Medium:",S_medium3));
print(paste("Specificity Small:",S_small3));

# Train the Naive Bayes Model ----
cat("\nNaive Bayes (NB) \n");

set.seed(123);
nb_model <- naiveBayes(RCS_SIZE~., data = train_data);

# Make predictions using the trained SVM model on test data 
tp4 <- predict(nb_model, test_data[, -c(14)]); # column 12 is the target variable
test_data$test_preds4 = tp4;

# Confusion matrix
cm4 <- confusionMatrix(test_data$RCS_SIZE, test_data$test_preds4);

# Show confusion matrix in table format
levels <- c("LARGE","MEDIUM","SMALL");
cmt4 <- table(factor(test_data$RCS_SIZE, levels = levels), factor(test_data$test_preds4, levels = levels), dnn = c("Actual","Predicted"));

#Calculate confusino matrix values for each class
tplarge4 = cmt4["LARGE","LARGE"];
fnlarge4 = sum(cmt4["LARGE",c('MEDIUM','SMALL')]);
fplarge4 = sum(c(cmt4["MEDIUM","LARGE"], cmt4["SMALL","LARGE"]));
tnlarge4 = sum(c(sum(cmt4["MEDIUM",c('MEDIUM','SMALL')]),sum(cmt4["SMALL",c('MEDIUM', 'SMALL')])));                    
tpmedium4 = cmt4["MEDIUM","MEDIUM"];
fnmedium4 = sum(cmt4["MEDIUM",c('LARGE','SMALL')]);
fpmedium4 = sum(c(cmt4["LARGE","MEDIUM"], cmt4["SMALL","MEDIUM"]));
tnmedium4 = sum(c(sum(cmt4["LARGE",c('LARGE','SMALL')]), sum(cmt4["SMALL",c('LARGE','SMALL')])));

tpsmall4 = cmt4["SMALL","SMALL"];
fnsmall4 = sum(cmt4["SMALL",c('LARGE','MEDIUM')]);
fpsmall4 = sum(c(cmt4["LARGE","SMALL"], cmt4["MEDIUM","SMALL"]));
tnsmall4 = sum(c(sum(cmt4["LARGE",c('LARGE','MEDIUM')]), sum(cmt4["MEDIUM",c('LARGE','MEDIUM')])));

# Extracting performance metrics
acc4 <- cm4$overall['Accuracy'];

P_large4 <- tplarge4/(tplarge4+fplarge4);
P_medium4 <- tpmedium4/(tpmedium4+fpmedium4);
P_small4 <- tpsmall4/(tpsmall4+fpsmall4);

R_large4 = tplarge4/(tplarge4+fnlarge4);
R_medium4 = tpmedium4/(tpmedium4+fnmedium4);
R_small4 = tpsmall4/(tpsmall4+fnsmall4);

f1_large4 = 2*(P_large4*R_large4)/(P_large4+R_large4);
f1_medium4 = 2*(P_medium4*R_medium4)/(P_medium4+R_medium4);
f1_small4 = 2*(P_small4*R_small4)/(P_small4+R_small4);

S_large4 = tnlarge4/(tnlarge4+fplarge4);
S_medium4 = tnmedium4/(tnmedium4+fpmedium4);
S_small4 = tnsmall4/(tnsmall4+fpsmall4);

# Printing them
print(paste("Accuracy:",acc4));

print(paste("Precision Large:",P_large4));
print(paste("Precision Medium:",P_medium4));
print(paste("Precision Small:",P_small4));

print(paste("Recall Large:",R_large4));
print(paste("Recall Medium:",R_medium4));
print(paste("Recall Small:",R_small4));

print(paste("F1 Score Large:",f1_large4));
print(paste("F1 Score Medium:",f1_medium4));
print(paste("F1 Score Small:",f1_small4));

print(paste("Specificity Large:",S_large4));
print(paste("Specificity Medium:",S_medium4));
print(paste("Specificity Small:",S_small4));

# Train the Support Vector Machine Model ----
cat("\nSupport Vector Machine (SVM) \n");

set.seed(123);
svm_model <- svm(formula = RCS_SIZE~., data = train_data, kernel = "linear");

# Make predictions using the trained SVM model on test data 
tp5 <- predict(svm_model, test_data[, -c(14)]); # column 12 is the target variable
test_data$test_preds5 = tp5;

# Confusion matrix
cm5 <- confusionMatrix(test_data$RCS_SIZE, test_data$test_preds5);

# Show confusion matrix in table format
levels <- c("LARGE","MEDIUM","SMALL");
cmt5 <- table(factor(test_data$RCS_SIZE, levels = levels), factor(test_data$test_preds5, levels = levels), dnn = c("Actual","Predicted"));

#Calculate confusino matrix values for each class
tplarge5 = cmt5["LARGE","LARGE"];
fnlarge5 = sum(cmt5["LARGE",c('MEDIUM','SMALL')]);
fplarge5 = sum(c(cmt5["MEDIUM","LARGE"], cmt5["SMALL","LARGE"]));
tnlarge5 = sum(c(sum(cmt5["MEDIUM",c('MEDIUM','SMALL')]),sum(cmt5["SMALL",c('MEDIUM', 'SMALL')])));                    
tpmedium5 = cmt5["MEDIUM","MEDIUM"];
fnmedium5 = sum(cmt5["MEDIUM",c('LARGE','SMALL')]);
fpmedium5 = sum(c(cmt5["LARGE","MEDIUM"], cmt5["SMALL","MEDIUM"]));
tnmedium5 = sum(c(sum(cmt5["LARGE",c('LARGE','SMALL')]), sum(cmt5["SMALL",c('LARGE','SMALL')])));

tpsmall5 = cmt5["SMALL","SMALL"];
fnsmall5 = sum(cmt5["SMALL",c('LARGE','MEDIUM')]);
fpsmall5 = sum(c(cmt5["LARGE","SMALL"], cmt5["MEDIUM","SMALL"]));
tnsmall5 = sum(c(sum(cmt5["LARGE",c('LARGE','MEDIUM')]), sum(cmt5["MEDIUM",c('LARGE','MEDIUM')])));

# Extracting performance metrics
acc5 <- cm5$overall['Accuracy'];

P_large5 <- tplarge5/(tplarge5+fplarge5);
P_medium5 <- tpmedium5/(tpmedium5+fpmedium5);
P_small5 <- tpsmall5/(tpsmall5+fpsmall5);

R_large5 = tplarge5/(tplarge5+fnlarge5);
R_medium5 = tpmedium5/(tpmedium5+fnmedium5);
R_small5 = tpsmall5/(tpsmall5+fnsmall5);

f1_large5 = 2*(P_large5*R_large5)/(P_large5+R_large5);
f1_medium5 = 2*(P_medium5*R_medium5)/(P_medium5+R_medium5);
f1_small5 = 2*(P_small5*R_small5)/(P_small5+R_small5);

S_large5 = tnlarge5/(tnlarge5+fplarge5);
S_medium5 = tnmedium5/(tnmedium5+fpmedium5);
S_small5 = tnsmall5/(tnsmall5+fpsmall5);

# Printing them
print(paste("Accuracy:",acc5));

print(paste("Precision Large:",P_large5));
print(paste("Precision Medium:",P_medium5));
print(paste("Precision Small:",P_small5));

print(paste("Recall Large:",R_large5));
print(paste("Recall Medium:",R_medium5));
print(paste("Recall Small:",R_small5));

print(paste("F1 Score Large:",f1_large5));
print(paste("F1 Score Medium:",f1_medium5));
print(paste("F1 Score Small:",f1_small5));

print(paste("Specificity Large:",S_large5));
print(paste("Specificity Medium:",S_medium5));
print(paste("Specificity Small:",S_small5));

# Train the Decision Tree Model ----
cat("\nDecision Tree (DT) \n");

set.seed(123);
dt_model = train(RCS_SIZE~., data = train_data, method = "rpart", trControl=ctrl);

# Make predictions using the trained SVM model on test data 
tp6 <- predict(dt_model, test_data[, -c(14)]); # column 12 is the target variable
test_data$test_preds6 = tp6;

# Confusion matrix
cm6 <- confusionMatrix(test_data$RCS_SIZE, test_data$test_preds6);

# Show confusion matrix in table format
levels <- c("LARGE","MEDIUM","SMALL");
cmt6 <- table(factor(test_data$RCS_SIZE, levels = levels), factor(test_data$test_preds6, levels = levels), dnn = c("Actual","Predicted"));

#Calculate confusino matrix values for each class
tplarge6 = cmt6["LARGE","LARGE"];
fnlarge6 = sum(cmt6["LARGE",c('MEDIUM','SMALL')]);
fplarge6 = sum(c(cmt6["MEDIUM","LARGE"], cmt6["SMALL","LARGE"]));
tnlarge6 = sum(c(sum(cmt6["MEDIUM",c('MEDIUM','SMALL')]),sum(cmt6["SMALL",c('MEDIUM', 'SMALL')])));                    
tpmedium6 = cmt6["MEDIUM","MEDIUM"];
fnmedium6 = sum(cmt6["MEDIUM",c('LARGE','SMALL')]);
fpmedium6 = sum(c(cmt6["LARGE","MEDIUM"], cmt6["SMALL","MEDIUM"]));
tnmedium6 = sum(c(sum(cmt6["LARGE",c('LARGE','SMALL')]), sum(cmt6["SMALL",c('LARGE','SMALL')])));

tpsmall6 = cmt6["SMALL","SMALL"];
fnsmall6 = sum(cmt6["SMALL",c('LARGE','MEDIUM')]);
fpsmall6 = sum(c(cmt6["LARGE","SMALL"], cmt6["MEDIUM","SMALL"]));
tnsmall6 = sum(c(sum(cmt6["LARGE",c('LARGE','MEDIUM')]), sum(cmt6["MEDIUM",c('LARGE','MEDIUM')])));

# Extracting performance metrics
acc6 <- cm6$overall['Accuracy'];

P_large6 <- tplarge6/(tplarge6+fplarge6);
P_medium6 <- tpmedium6/(tpmedium6+fpmedium6);
P_small6 <- tpsmall6/(tpsmall6+fpsmall6);

R_large6 = tplarge6/(tplarge6+fnlarge6);
R_medium6 = tpmedium6/(tpmedium6+fnmedium6);
R_small6 = tpsmall6/(tpsmall6+fnsmall6);

f1_large6 = 2*(P_large6*R_large6)/(P_large6+R_large6);
f1_medium6 = 2*(P_medium6*R_medium6)/(P_medium6+R_medium6);
f1_small6 = 2*(P_small6*R_small6)/(P_small6+R_small6);

S_large6 = tnlarge6/(tnlarge6+fplarge6);
S_medium6 = tnmedium6/(tnmedium6+fpmedium6);
S_small6 = tnsmall6/(tnsmall6+fpsmall6);

# Printing them
print(paste("Accuracy:",acc6));

print(paste("Precision Large:",P_large6));
print(paste("Precision Medium:",P_medium6));
print(paste("Precision Small:",P_small6));

print(paste("Recall Large:",R_large6));
print(paste("Recall Medium:",R_medium6));
print(paste("Recall Small:",R_small6));

print(paste("F1 Score Large:",f1_large6));
print(paste("F1 Score Medium:",f1_medium6));
print(paste("F1 Score Small:",f1_small6));

print(paste("Specificity Large:",S_large6));
print(paste("Specificity Medium:",S_medium6));
print(paste("Specificity Small:",S_small6));

# Majority Based Ensemble Classification (Voting Based) ----

cat("\nMajority Ensembling \n");

# Adding the predictions from different models in test_preds
test_preds <- test_data[,c(15,16,17,18,19,20)];

# Calculating the max occuring predictions row wise and storing it in max_preds column
test_preds$max_pred <- apply(test_preds,1,function(row){
  tbl = table(row)
  names(tbl)[which.max(tbl)]
});

# Adding RCS_SIZE Column to test_preds
test_preds$RCS_SIZE = test_data$RCS_SIZE;

# Convert max_preds to factor
test_preds$max_pred = factor(test_preds$max_pred);

# Deriving Confusion Matrix
ensembled_cm <- confusionMatrix(test_preds$RCS_SIZE, test_preds$max_pred);

# Show confusion matrix in table format
levels <- c("LARGE","MEDIUM","SMALL");
cmt_en <- table(factor(test_preds$RCS_SIZE, levels = levels), factor(test_preds$max_pred, levels = levels), dnn = c("Actual","Predicted"));


#Calculate confusino matrix values for each class
tplarge_en = cmt_en["LARGE","LARGE"];
fnlarge_en = sum(cmt_en["LARGE",c('MEDIUM','SMALL')]);
fplarge_en = sum(c(cmt_en["MEDIUM","LARGE"], cmt_en["SMALL","LARGE"]));
tnlarge_en = sum(c(sum(cmt_en["MEDIUM",c('MEDIUM','SMALL')]),sum(cmt_en["SMALL",c('MEDIUM', 'SMALL')])));                    
tpmedium_en = cmt_en["MEDIUM","MEDIUM"];
fnmedium_en = sum(cmt_en["MEDIUM",c('LARGE','SMALL')]);
fpmedium_en = sum(c(cmt_en["LARGE","MEDIUM"], cmt_en["SMALL","MEDIUM"]));
tnmedium_en = sum(c(sum(cmt_en["LARGE",c('LARGE','SMALL')]), sum(cmt_en["SMALL",c('LARGE','SMALL')])));

tpsmall_en = cmt_en["SMALL","SMALL"];
fnsmall_en = sum(cmt_en["SMALL",c('LARGE','MEDIUM')]);
fpsmall_en = sum(c(cmt_en["LARGE","SMALL"], cmt_en["MEDIUM","SMALL"]));
tnsmall_en = sum(c(sum(cmt_en["LARGE",c('LARGE','MEDIUM')]), sum(cmt_en["MEDIUM",c('LARGE','MEDIUM')])));

# Extracting performance metrics
ensembled_acc <- ensembled_cm$overall['Accuracy'];

P_large_en <- tplarge_en/(tplarge_en+fplarge_en);
P_medium_en <- tpmedium_en/(tpmedium6+fpmedium_en);
P_small_en <- tpsmall_en/(tpsmall_en+fpsmall_en);

R_large_en = tplarge_en/(tplarge_en+fnlarge_en);
R_medium_en = tpmedium_en/(tpmedium_en+fnmedium_en);
R_small_en = tpsmall_en/(tpsmall_en+fnsmall_en);

f1_large_en = 2*(P_large_en*R_large_en)/(P_large_en+R_large_en);
f1_medium_en = 2*(P_medium_en*R_medium_en)/(P_medium_en+R_medium_en);
f1_small_en = 2*(P_small_en*R_small_en)/(P_small_en+R_small_en);

S_large_en = tnlarge_en/(tnlarge_en+fplarge_en);
S_medium_en = tnmedium_en/(tnmedium_en+fpmedium_en);
S_small_en = tnsmall_en/(tnsmall_en+fpsmall_en);

# Printing them
cat("\nAccuraccy of Ensembled Data : ",ensembled_acc);

print(paste("Precision Large:",P_large_en));
print(paste("Precision Medium:",P_medium_en));
print(paste("Precision Small:",P_small_en));

print(paste("Recall Large:",R_large_en));
print(paste("Recall Medium:",R_medium_en));
print(paste("Recall Small:",R_small_en));

print(paste("F1 Score Large:",f1_large_en));
print(paste("F1 Score Medium:",f1_medium_en));
print(paste("F1 Score Small:",f1_small_en));

print(paste("Specificity Large:",S_large_en));
print(paste("Specificity Medium:",S_medium_en));
print(paste("Specificity Small:",S_small_en));





