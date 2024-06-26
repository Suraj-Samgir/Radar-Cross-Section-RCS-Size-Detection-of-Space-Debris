library(ggplot2);

# Load the Dataset ----
data <- read.csv("Processed_Data.csv");

# Removing the categorical variables before normalization ----
new_data <- data[, -c(which(names(data) %in% c("OBJECT_TYPE","RCS_SIZE")))];

# converting the OBJECT_AGE to numeric ----
new_data$OBJECT_AGE = as.numeric(new_data$OBJECT_AGE);

# Visualize Data before normalization ----
scatp = ggplot(data, aes(x=ECCENTRICITY, y=INCLINATION, color=RCS_SIZE)) + geom_point() + labs(x="ECCENTRICITY",y="INCLINATION",title="Before Normalization");
print(scatp);

# creating function ...
min_max_normalization <- function(x) {
  return((x-min(x))/(max(x)-min(x)));
}

# Applying min-max normalization on all the columns of new_data dataset
normalized_data = as.data.frame(lapply(new_data, min_max_normalization));

# Adding the OBJECT_TYPE AND RCS_SIZE Column to the normalized dataset.
normalized_data$OBJECT_TYPE = data$OBJECT_TYPE;
normalized_data$RCS_SIZE = data$RCS_SIZE;

# Visualize Data after normalization ----
scatp2 = ggplot(normalized_data, aes(x=ECCENTRICITY, y=INCLINATION, color=RCS_SIZE)) + geom_point() + labs(x="ECCENTRICITY",y="INCLINATION",title="After Normalization");
print(scatp2);

# Calculating the InterQuatile Range (IQR) for MEAN_MOTION ----

cat("\n\n----------Before Removing Outliers----------\n");
cat("\n\nNumber of rows in Dataset : ",nrow(normalized_data));
cat("\n\nNumber of columns in Dataset : ",ncol(normalized_data));

q1 = quantile(normalized_data$MEAN_MOTION, 0.25);
q3 = quantile(normalized_data$MEAN_MOTION, 0.75);

iqr <- q3-q1;

lower_bound <- q1 - 1.5*iqr;
upper_bound <- q3 + 1.5*iqr;

# Identifying outliers

outliers <- normalized_data$MEAN_MOTION[normalized_data$MEAN_MOTION < lower_bound | normalized_data$MEAN_MOTION > upper_bound];

normalized_data <- normalized_data[!normalized_data$MEAN_MOTION %in% outliers, ];

# Calculating the InterQuatile Range (IQR) for ECCENTRICITY ----

q1 = quantile(normalized_data$ECCENTRICITY, 0.25);
q3 = quantile(normalized_data$ECCENTRICITY, 0.75);

iqr <- q3-q1;

lower_bound <- q1 - 1.5*iqr;
upper_bound <- q3 + 1.5*iqr;

# Identifying outliers

outliers <- normalized_data$ECCENTRICITY[normalized_data$ECCENTRICITY < lower_bound | normalized_data$ECCENTRICITY > upper_bound];

normalized_data <- normalized_data[!normalized_data$ECCENTRICITY %in% outliers, ];

# Calculating the InterQuatile Range (IQR) for INCLINATION ----

q1 = quantile(normalized_data$INCLINATION, 0.25);
q3 = quantile(normalized_data$INCLINATION, 0.75);

iqr <- q3-q1;

lower_bound <- q1 - 1.5*iqr;
upper_bound <- q3 + 1.5*iqr;

# Identifying outliers

outliers <- normalized_data$INCLINATION[normalized_data$INCLINATION < lower_bound | normalized_data$INCLINATION > upper_bound];

normalized_data <- normalized_data[!normalized_data$INCLINATION %in% outliers, ];

# Calculating the InterQuatile Range (IQR) for RA_OF_ASC_NODE ----

q1 = quantile(normalized_data$RA_OF_ASC_NODE, 0.25);
q3 = quantile(normalized_data$RA_OF_ASC_NODE, 0.75);

iqr <- q3-q1;

lower_bound <- q1 - 1.5*iqr;
upper_bound <- q3 + 1.5*iqr;

# Identifying outliers

outliers <- normalized_data$RA_OF_ASC_NODE[normalized_data$RA_OF_ASC_NODE < lower_bound | normalized_data$RA_OF_ASC_NODE > upper_bound];

normalized_data <- normalized_data[!normalized_data$RA_OF_ASC_NODE %in% outliers, ];

# Calculating the InterQuatile Range (IQR) for ARG_OF_PERICENTER ----

q1 = quantile(normalized_data$ARG_OF_PERICENTER, 0.25);
q3 = quantile(normalized_data$ARG_OF_PERICENTER, 0.75);

iqr <- q3-q1;

lower_bound <- q1 - 1.5*iqr;
upper_bound <- q3 + 1.5*iqr;

# Identifying outliers

outliers <- normalized_data$ARG_OF_PERICENTER[normalized_data$ARG_OF_PERICENTER < lower_bound | normalized_data$ARG_OF_PERICENTER > upper_bound];

normalized_data <- normalized_data[!normalized_data$ARG_OF_PERICENTER %in% outliers, ];


# Calculating the InterQuatile Range (IQR) for MEAN_ANOMALY ----

q1 = quantile(normalized_data$MEAN_ANOMALY, 0.25);
q3 = quantile(normalized_data$MEAN_ANOMALY, 0.75);

iqr <- q3-q1;

lower_bound <- q1 - 1.5*iqr;
upper_bound <- q3 + 1.5*iqr;

# Identifying outliers

outliers <- normalized_data$MEAN_ANOMALY[normalized_data$MEAN_ANOMALY < lower_bound | normalized_data$MEAN_ANOMALY > upper_bound];

normalized_data <- normalized_data[!normalized_data$MEAN_ANOMALY %in% outliers, ];


# Calculating the InterQuatile Range (IQR) for SEMIMAJOR_AXIS ----

q1 = quantile(normalized_data$SEMIMAJOR_AXIS, 0.25);
q3 = quantile(normalized_data$SEMIMAJOR_AXIS, 0.75);

iqr <- q3-q1;

lower_bound <- q1 - 1.5*iqr;
upper_bound <- q3 + 1.5*iqr;

# Identifying outliers

outliers <- normalized_data$SEMIMAJOR_AXIS[normalized_data$SEMIMAJOR_AXIS < lower_bound | normalized_data$SEMIMAJOR_AXIS > upper_bound];

normalized_data <- normalized_data[!normalized_data$SEMIMAJOR_AXIS %in% outliers, ];


# Calculating the InterQuatile Range (IQR) for PERIOD ----

q1 = quantile(normalized_data$PERIOD, 0.25);
q3 = quantile(normalized_data$PERIOD, 0.75);

iqr <- q3-q1;

lower_bound <- q1 - 1.5*iqr;
upper_bound <- q3 + 1.5*iqr;

# Identifying outliers

outliers <- normalized_data$PERIOD[normalized_data$PERIOD < lower_bound | normalized_data$PERIOD > upper_bound];

normalized_data <- normalized_data[!normalized_data$PERIOD %in% outliers, ];


# Calculating the InterQuatile Range (IQR) for APOAPSIS ----

q1 = quantile(normalized_data$APOAPSIS, 0.25);
q3 = quantile(normalized_data$APOAPSIS, 0.75);

iqr <- q3-q1;

lower_bound <- q1 - 1.5*iqr;
upper_bound <- q3 + 1.5*iqr;

# Identifying outliers

outliers <- normalized_data$APOAPSIS[normalized_data$APOAPSIS < lower_bound | normalized_data$APOAPSIS > upper_bound];

normalized_data <- normalized_data[!normalized_data$APOAPSIS %in% outliers, ];


# Calculating the InterQuatile Range (IQR) for PERIAPSIS ----

q1 = quantile(normalized_data$PERIAPSIS, 0.25);
q3 = quantile(normalized_data$PERIAPSIS, 0.75);

iqr <- q3-q1;

lower_bound <- q1 - 1.5*iqr;
upper_bound <- q3 + 1.5*iqr;

# Identifying outliers

outliers <- normalized_data$PERIAPSIS[normalized_data$PERIAPSIS < lower_bound | normalized_data$PERIAPSIS > upper_bound];

normalized_data <- normalized_data[!normalized_data$PERIAPSIS %in% outliers, ];



# Calculating the InterQuatile Range (IQR) for CENT_FOCUS_DIST ----

q1 = quantile(normalized_data$CENT_FOCUS_DIST, 0.25);
q3 = quantile(normalized_data$CENT_FOCUS_DIST, 0.75);

iqr <- q3-q1;

lower_bound <- q1 - 1.5*iqr;
upper_bound <- q3 + 1.5*iqr;

# Identifying outliers

outliers <- normalized_data$CENT_FOCUS_DIST[normalized_data$CENT_FOCUS_DIST < lower_bound | normalized_data$CENT_FOCUS_DIST > upper_bound];

normalized_data <- normalized_data[!normalized_data$CENT_FOCUS_DIST %in% outliers, ];


# Calculating the InterQuatile Range (IQR) for OBJECT_AGE ----

q1 = quantile(normalized_data$OBJECT_AGE, 0.25);
q3 = quantile(normalized_data$OBJECT_AGE, 0.75);

iqr <- q3-q1;

lower_bound <- q1 - 1.5*iqr;
upper_bound <- q3 + 1.5*iqr;

# Identifying outliers

outliers <- normalized_data$OBJECT_AGE[normalized_data$OBJECT_AGE < lower_bound | normalized_data$OBJECT_AGE > upper_bound];

normalized_data <- normalized_data[!normalized_data$OBJECT_AGE %in% outliers, ];

# Removing the values with 0 from the columns containing them ----

# colSums(normalized_data == 0);
normalized_data = subset(normalized_data, OBJECT_AGE != 0);
normalized_data = subset(normalized_data, ARG_OF_PERICENTER != 0);
normalized_data = subset(normalized_data, CENT_FOCUS_DIST != 0);

cat("\n\n----------Before Removing Outliers----------\n");
cat("\n\nNumber of rows in Dataset : ",nrow(normalized_data));
cat("\n\nNumber of columns in Dataset : ",ncol(normalized_data));

# Writing the Processed Data to a CSV file to create a Processed Dataset.
# write.csv(normalized_data, "Final_Processed_Data.csv", row.names = FALSE);