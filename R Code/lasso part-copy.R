####################################
###### Packages and Libraries ######
####################################
library(dplyr)
library(ggplot2)
library(reshape2)

####################################################
##### Data Preparation - (1) Response Variable #####
####################################################

# Load the data sets
credit_record <- read.csv("credit_record.csv")
application_record <- read.csv('application_record.csv')

# Compute the minimum MONTHS_BALANCE for each ID to find users' account open month
begin_month <- credit_record %>%
  group_by(ID) %>%
  summarize(begin_month = min(MONTHS_BALANCE))

# Merge 'data' with 'begin_month' based on the 'ID' column
new_data <- merge(application_record, begin_month, by = "ID", all.x = TRUE)

# Function to assign approval status (1="Not Approved", 2="Approved")
assign_approval_status <- function(status) {
  if(any((status %in% c("2", "3", "4", "5")))) {
    return("1")
  } else {
    return("0")
  }
}

# Create a data set showing the approval status for each customer
credit_data <- credit_record %>%
  group_by(ID) %>%
  summarise(approval_status = assign_approval_status(STATUS)) %>%
  ungroup()

# Checking the approval status
#View(credit_data)
#sum(credit_data$approval_status == "0")
#sum(credit_data$approval_status == "1")
value_counts <- table(credit_data$approval_status)

# Print the counts
print(value_counts)

# Calculate the normalized counts
normalized_counts <- prop.table(value_counts)
print(normalized_counts)

# Merge 'new_data' with 'credit_data' based on the 'ID' column
new_data <- merge(new_data, credit_data, by = "ID", all = FALSE)

# Rename the columns in easier way
names(new_data) <- c('ID','Gender', 'Car', 'Realty', 'Children', 'Income', 
                     'Inc_type','Edu_type','Marital_status','Hou_type','Days_birth',
                     'Days_employed','Mob_phone','Work_phone','Phone','Email',
                     'Occupation','Fam_size','Begin_month','Approval')

####################################################
##### Data Preparation - (2) Feature Variables #####
####################################################

###### Binary Variables ######
## Gender ##
# Replace 'F' with 0 and 'M' with 1 in the 'Gender' column
new_data$Gender <- ifelse(new_data$Gender == 'F', 0, 1)
# Print the value counts
table(new_data$Gender)

## Car ##
# Replace 'N' with 0 and 'Y' with 1 in the 'Car' column
new_data$Car <- ifelse(new_data$Car == 'N', 0, 1)
# Print the value counts
table(new_data$Car)

## Realty ##
# Replace 'N' with 0 and 'Y' with 1 in the 'Realty' column
new_data$Realty <- ifelse(new_data$Realty == 'N', 0, 1)
# Print the value counts
table(new_data$Realty)

# Mobile phone
# Work phone
# Phone
# Email


#######Function for creating dummies#######
create_dummy <- function(data, column_name) {
  dummies <- model.matrix(~ data[[column_name]] - 1) # '-1' removes intercept
  colnames(dummies) <- gsub(paste(column_name, "data", sep = "_"), "", colnames(dummies))
  data <- cbind(data, dummies)
  data <- data[, !names(data) %in% column_name, drop = FALSE]
  return(data)
}

###### Continuous Variables ######
## Children
# Replace values in 'ChldNo' column
#new_data$Children[new_data$Children >= 2] <- 'more than 2'
# Print the value counts
#frequency <- table(new_data$Children)
#frequency
# Apply the function to 'ChldNo'
#new_data <- create_dummy(new_data, 'Children')
#colnames(new_data)[colnames(new_data) == "data[[column_name]]0"] <- "Children_0"
#colnames(new_data)[colnames(new_data) == "data[[column_name]]1"] <- "Children_1"
#colnames(new_data)[colnames(new_data) == "data[[column_name]]more than 2"] <- "Children_2more"

## Income
## Age(Days_birth)
new_data$Age <- -floor(new_data$Days_birth/365)
unique(new_data$Age)
new_data <- new_data[, !colnames(new_data) %in% "Days_birth"]

## Work Years(Days_employed)
new_data$Work_year <- -floor(new_data$Days_employed/365)
new_data$Work_year[new_data$Work_year < 0] <- NA
unique(new_data$Work_year)
new_data <- new_data[, !colnames(new_data) %in% "Days_employed"]

## Famliy Size


###### Categorical Variables ######
## Education Type
unique(new_data$Edu_type)
new_data$Edu_type[new_data$Edu_type == "Academic degree"] <- "Higher education"
#new_data$Edu_type <- as.integer(as.factor(new_data$Edu_type))

new_data <- create_dummy(new_data, 'Edu_type')
colnames(new_data)[colnames(new_data) == "data[[column_name]]Higher education"] <- "High_ed"
colnames(new_data)[colnames(new_data) == "data[[column_name]]Incomplete higher"] <- "Incomp_high"
colnames(new_data)[colnames(new_data) == "data[[column_name]]Lower secondary"] <- "Low_secondary"
colnames(new_data)[colnames(new_data) == "data[[column_name]]Secondary / secondary special"] <- "Secondary"


## Marital Status
unique(new_data$Marital_status)
new_data$Marital_status[new_data$Marital_status == "Civil marriage"] <- "Married"
new_data$Marital_status[new_data$Marital_status == "Single / not married"] <- "Single/Not Married"
new_data$Marital_status[new_data$Marital_status == "Widow"] <- "Separated"
#new_data <- create_dummy(new_data, 'Marital_status')
#colnames(new_data)[colnames(new_data) == "data[[column_name]]Married"] <- "Married"
#colnames(new_data)[colnames(new_data) == "data[[column_name]]Single/Not Married"] <- "Single"
#colnames(new_data)[colnames(new_data) == "data[[column_name]]Separated"] <- "Seperated"

# 1-married, 2-separated, 3-single
new_data$Marital_status <- as.integer(as.factor(new_data$Marital_status))


## House Type
# Homeowner = 1, others = 0
new_data$Hou_type <- ifelse(new_data$Hou_type == 'House / apartment', 1, 0)
colnames(new_data)[colnames(new_data) == "Hou_type"] <- "Homeowner"

## Income Type
unique(new_data$Inc_type)
new_data$Inc_type <- as.integer(as.factor(new_data$Inc_type))
#new_data <- new_data[, !colnames(new_data) %in% "Inc_type"]

## Occupation
unique(new_data$Occupation)
new_data <- new_data[, !colnames(new_data) %in% "Occupation"]


##✮✮✮✮✮Randomly select number of sample rows from new_data that has approval status = 0 that is slighter smaller than with approval status = 1, select all rows from new_data that has approval status = 1, and combine them into a new dataset
# Filter rows with approval status = 0 and randomly select 600 samples
sample_0 <- new_data %>% 
  filter(Approval == "0") %>%
  sample_n(600)

# Filter rows with approval status = 1
sample_1 <- new_data %>% 
  filter(Approval == "1")

# Combine the two samples to create a new dataset
combined_data <- rbind(sample_0, sample_1)

##✮✮✮✮✮Split combined_data into training(70%) data and test data(30%)
# Set seed
set.seed(123)  

# Set train dataset (70% of combined_data)
install.packages("caret")  
library(caret)
train_index <- createDataPartition(combined_data$Approval, p = 0.7, list = FALSE)
train_data <- combined_data[train_index, ]

# Create test dataset (30% of combined_data)
control_data <- combined_data[-train_index, ]


######################
##### Modeling #####
######################
library(caret)
library(tree)
library(rpart)
library(glmnet)

colnames(new_data)

############################
##✮✮✮✮✮LASSO MODELING✮✮✮✮✮##
############################
##✮✮✮✮✮Train LASSO Model
##✮✮✮✮✮Prepare the Data
library(glmnet)
#impute missing values with the median for numerics, and impute with the mode for binaries.
numeric_vars <- c("Income", "Fam_size", "Begin_month", "Age", "Work_year")
for (var in numeric_vars) {
  train_data[is.na(train_data[[var]]), var] <- median(train_data[[var]], na.rm = TRUE)
}
binary_categorical_vars <- c("Gender", "Car", "Realty", "Children", "Inc_type", "Marital_status", "Homeowner", 
                             "Mob_phone", "Work_phone", "Phone", "Email", "High_ed", "Incomp_high", 
                             "Low_secondary", "Secondary")

for (var in binary_categorical_vars) {
  train_data[is.na(train_data[[var]]), var] <- as.integer(names(sort(table(train_data[[var]]), decreasing = TRUE)[1]))
}

# Matrix of predictors (excluding 'ID' and 'Approval')
X <- as.matrix(train_data[, c("Gender", "Car", "Realty", "Children", "Income", "Inc_type", "Marital_status",
                              "Homeowner", "Mob_phone", "Work_phone", "Phone", "Email", "Fam_size", 
                              "Begin_month", "Age", "Work_year", "High_ed", "Incomp_high", "Low_secondary", "Secondary")])
# Response vector
y <- train_data$Approval



# Fit LASSO model: Use 'binomial' since it's binary classification.
lasso_model <- glmnet(X, y, family = "binomial", alpha = 1)  


##✮✮✮✮✮Select the Best Lambda using Cross-Validation
set.seed(123)
cv_lasso <- cv.glmnet(X, y, family = "binomial", alpha = 1)
# Optimal lambda
best_lambda <- cv_lasso$lambda.min
# Print the optimal lambda
print(best_lambda)
# Fit the model using the optimal lambda
best_lasso_model <- glmnet(X, y, family = "binomial", alpha = 1, lambda = best_lambda)


##✮✮✮✮✮Apply our model on our testing data
## Prepare predictor matrix for test data
# Handle missing data for numeric variables in control_data
for (var in numeric_vars) {
  control_data[is.na(control_data[[var]]), var] <- median(train_data[[var]], na.rm = TRUE)  # use median from train_data
}

# Handle missing data for binary and categorical variables in control_data
for (var in binary_categorical_vars) {
  control_data[is.na(control_data[[var]]), var] <- as.integer(names(sort(table(train_data[[var]]), decreasing = TRUE)[1]))  # use mode from train_data
}

X_control <- as.matrix(control_data[, c("Gender", "Car", "Realty", "Children", "Income", "Inc_type", "Marital_status", 
                                        "Homeowner", "Mob_phone", "Work_phone", "Phone", "Email", "Fam_size", 
                                        "Begin_month", "Age", "Work_year", "High_ed", "Incomp_high", "Low_secondary", "Secondary")])

# Ensure that the number of rows in X_control matches the number of rows in control_data
X_control <- X_control[1:nrow(control_data), ]


# Make predictions on control_data
probabilities_control <- predict(best_lasso_model, newx = X_control, type = "response")
predicted_labels <- ifelse(probabilities_control > 0.5, 1, 0)


##✮✮✮✮✮Test our model's accuracy on control data
library(caret)
library(pROC)

# 1. Ensure 'probabilities_control' is not a matrix
if (is.matrix(probabilities_control)) {
  probabilities_control <- probabilities_control[,1]
}

# Define the actual labels from control_data
actual_labels <- control_data$Approval

# 2. Re-check the lengths
cat("Length of actual labels: ", length(actual_labels), "\n")
cat("Length of predicted labels: ", length(predicted_labels), "\n")
cat("Length of predicted probabilities: ", length(probabilities_control), "\n")

# If the lengths don't match, there's a fundamental issue in data preparation or prediction.
# Assuming they match, proceed:

# 3. Accuracy Assessment
# Confusion Matrix
confusion_matrix <- confusionMatrix(as.factor(predicted_labels), as.factor(actual_labels))
print(confusion_matrix)

# ROC and AUC
roc_obj <- roc(actual_labels, as.vector(probabilities_control))
auc_val <- auc(roc_obj)

print(auc_val)
plot(roc_obj, main="ROC Curve for LASSO Model")
#The highest accuracy we got after adjusting

#4. f1 score evaluation
# Compute confusion matrix using caret
cm <- confusionMatrix(as.factor(predicted_labels), as.factor(actual_labels))

# Extract precision and recall from the confusion matrix
precision <- cm$byClass["Pos Pred Value"]
recall <- cm$byClass["Sensitivity"]

# Compute F1 score
f1_score <- 2 * (precision * recall) / (precision + recall)

print(f1_score)



######Use K-means######
library(gplots)

## Step 1: Elbow Method to find optimal clusters
combined_data_df <- do.call(rbind, combined_data)
# Check for NAs and Infs
na_rows <- apply(is.na(combined_data_df), 1, any)
inf_rows <- apply(is.infinite(combined_data_df), 1, any)

# Combine the two to identify any row that has either a NA or an Inf
invalid_rows <- na_rows | inf_rows

# Filter out these rows
cleaned_data <- combined_data_df[!invalid_rows, ]
wss <- numeric(15)  # Will test up to 15 clusters
for (k in 1:15) {
  kmeans_model <- kmeans(cleaned_data[, -1], centers=k)  # Excluding the ID column
  wss[k] <- kmeans_model$tot.withinss
}

plot(1:15, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares",
     main="Elbow Method to find optimal cluster number")
##Great! Elbow method determines cluster numbe to be 2!
# Perform K-means clustering with k = 2
set.seed(123)
kmeans_result <- kmeans(cleaned_data[, -1], centers=3,nstart = 15)








# Load necessary libraries for plotting
library(pheatmap)
library(reshape2)  # For data reshaping

# Assuming your K-means result is stored in 'kmeans_result'
# Extract cluster means
cluster_means <- kmeans_result$centers

# Convert cluster means to a dataframe
cluster_means_df <- as.data.frame(cluster_means)

# Create row and column names for the heatmap
rownames(cluster_means_df) <- 1:nrow(cluster_means_df)  # Rows represent clusters
colnames(cluster_means_df) <- colnames(cluster_means_df)  # Columns represent variables

# Create the heatmap using pheatmap
pheatmap(cluster_means_df, 
         cluster_rows = FALSE,  # Do not cluster rows
         cluster_cols = FALSE,  # Do not cluster columns
         color = colorRampPalette(c("blue", "white", "red"))(100),  # Define colors
         main = "Cluster Means Heatmap")





# Remove "Approval" variable from the data
cleaned_data <- cleaned_data[, -which(names(cleaned_data) == "Approval")]

# Perform K-means clustering with k = 2
set.seed(123)
kmeans_result <- kmeans(cleaned_data[, -1], centers = 3, nstart = 15)

# Load necessary libraries for plotting
library(pheatmap)
library(reshape2)  # For data reshaping

# Assuming your K-means result is stored in 'kmeans_result'
# Extract cluster means
cluster_means <- kmeans_result$centers

# Convert cluster means to a dataframe
cluster_means_df <- as.data.frame(cluster_means)

# Create row and column names for the heatmap
rownames(cluster_means_df) <- 1:nrow(cluster_means_df) 
colnames(cluster_means_df) <- colnames(cluster_means_df)  

# Create the heatmap using pheatmap
pheatmap(cluster_means_df, 
         cluster_rows = FALSE,  # Do not cluster rows
         cluster_cols = FALSE,  # Do not cluster columns
         color = colorRampPalette(c("blue", "white", "red"))(100),
         main = "Cluster Means Heatmap")










