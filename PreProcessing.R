# Load the Dataset ----
data = read.csv("space_decay.csv");

cat("\n\n----------Before Pre-Processing----------\n");

# Display the null values and blank values per column in the dataset ----
null_counts = colSums(is.na(data));
empty_counts = colSums(data == ""); 
cat("\nNull Values Per Columns : \n",null_counts);
cat("\n\nEmpty Values Per Columns : \n",empty_counts);
cat("\n\nNumber of rows in Dataset : ",nrow(data));
cat("\n\nNumber of columns in Dataset : ",ncol(data));
data_dist = table(data$RCS_SIZE);
cat("\n\nData Distribution (Blank, Large, Medium, Small) : ",data_dist);

# Remove the Unwanted Columns ----
data <- data[, -c(which(names(data) %in% c("CCSDS_OMM_VERS", "COMMENT", "ORIGINATOR", "CENTER_NAME", "REF_FRAME", "TIME_SYSTEM", "MEAN_ELEMENT_THEORY","MEAN_MOTION_DDOT", "EPHEMERIS_TYPE", "CLASSIFICATION_TYPE", "ELEMENT_SET_NO", "DECAY_DATE", "CREATION_DATE","OBJECT_NAME","OBJECT_ID","EPOCH","NORAD_CAT_ID","REV_AT_EPOCH","BSTAR","MEAN_MOTION_DOT","COUNTRY_CODE","SITE","FILE","GP_ID","TLE_LINE0","TLE_LINE1","TLE_LINE2")))];

# Remove the rows with NULL values from the dataset (Column Taken = LAUNCH_DATE) ----
data <- data[!is.na(data$LAUNCH_DATE),];

# Remove the rows with blank cells in the dataset (Column Taken = RCS_SIZE)
data <- subset(data, RCS_SIZE != "");

# Plot Graph for the class balance in the data (column = RCS_SIZE) ----
library(ggplot2);
plt = ggplot(data, aes(x=RCS_SIZE))+geom_bar()+labs(title = "Class Distribution of RCS_SIZE");
print(plt);

# Install and Load requirements ----

if (!requireNamespace("ROSE", quietly = TRUE)) {
  install.packages("ROSE")
}

library(ROSE);

# Creating Subsets with 2 labels for class balancing ----
subdata1 = subset(data,RCS_SIZE == "MEDIUM");
subdata1 = rbind(subdata1, subset(data,RCS_SIZE == "SMALL"));

subdata2 = subset(data,RCS_SIZE == "LARGE");
subdata2 = rbind(subdata2, subset(data,RCS_SIZE == "SMALL"));

# Class Balancing using ovun.sample ----
balanced_data = ovun.sample(RCS_SIZE~., data = subdata1, method = "over", seed=123)$data;
over2 = ovun.sample(RCS_SIZE~., data = subdata2, method = "over", seed=123)$data;

# Combining the data of 3 classes into one data frame ----
balanced_data = rbind(balanced_data,subset(over2,RCS_SIZE == "LARGE"));

# Converting LAUNCH_DATE to numeric -----
balanced_data$LAUNCH_DATE = as.numeric(balanced_data$LAUNCH_DATE);

# Creating the OBJECT_AGE Column and Removing LAUNCH_DATE Column ----
balanced_data$OBJECT_AGE = 2024-balanced_data$LAUNCH_DATE;
balanced_data <- balanced_data[, -c(which(names(balanced_data) %in% c("LAUNCH_DATE")))];

# Crating the CENT_FOCUS_DIST Column from SEMIMAJOR_AXIS & ECCENTRICITY Columns ----
balanced_data$CENT_FOCUS_DIST <- balanced_data$SEMIMAJOR_AXIS * balanced_data$ECCENTRICITY;

cat("\n\n\n----------After Pre-Processing----------\n");

# Display the Null values and blank values in each column of the dataset ----
null_counts = colSums(is.na(balanced_data));
empty_counts = colSums(balanced_data == "");
cat("\nNull Values Per Columns : \n",null_counts);
cat("\n\nEmpty Values Per Columns : \n",empty_counts);
cat("\n\nNumber of rows in Dataset : ",nrow(balanced_data));
cat("\n\nNumber of columns in Dataset : ",ncol(balanced_data));
data_dist = table(balanced_data$RCS_SIZE);
cat("\n\nData Distribution (Large, Medium, Small) : ",data_dist);

# Display the graph for class balance in the dataset (column = RCS_SIZE) ----
plt = ggplot(balanced_data, aes(x=RCS_SIZE))+geom_bar()+labs(title = "Class Distribution of RCS_SIZE");
print(plt);

# Writing the Processed Data to a CSV file to create a Processed Dataset.
write.csv(balanced_data, "Processed_Data.csv", row.names = FALSE);
