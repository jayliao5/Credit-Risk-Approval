####################################
###### Packages and Libraries ######
####################################
library(dplyr)
library(ggplot2)
library(reshape2)
library(caret)

####################################################
##### Data Preparation - (1) Response Variable #####
####################################################
#setwd("/Users/myojin/Downloads/Duke Fall1/Data Science")
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



#################################
###### Splitting Data #########
################################
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
train_index <- createDataPartition(combined_data$Approval, p = 0.7, list = FALSE)
train_data <- combined_data[train_index, ]

# Create test dataset (30% of combined_data)
control_data <- combined_data[-train_index, ]

# Convert Approval to factor
train_data$Approval <- as.factor(train_data$Approval)
control_data$Approval <- as.factor(control_data$Approval)

# Ensure factor levels are valid R variable names
levels(train_data$Approval) <- c("Class0", "Class1")
levels(control_data$Approval) <- c("Class0", "Class1")

##### Modeling #####
library(caret)
library(tree)
library(rpart)
library(glmnet)
library(randomForest)
library(h2o)
library(ROSE)


# Handle NAs
train_data <- na.omit(train_data)

# Address the class imbalance using ROSE
train_data_balanced <- ROSE(Approval ~ ., data = train_data)$data 
train_data_balanced$Approval <- as.factor(train_data_balanced$Approval)
levels(train_data_balanced$Approval) <- c("Class0", "Class1")

# Define train control parameters for cross-validation
cv_control <- trainControl(method = "cv", 
                           number = 10, 
                           classProbs = TRUE, 
                           summaryFunction = twoClassSummary)

####### RANDOM FOREST #######

rf_model <- train(Approval ~ ., 
                  data = train_data_balanced, 
                  method = "rf", 
                  metric = "ROC", 
                  ntree = 500,
                  trControl = cv_control)

# Calculating the F1 score using Sensitivity and Specificity
rf_f1 <- 2 * (rf_model$results$Sens * rf_model$results$Spec) / (rf_model$results$Sens + rf_model$results$Spec)

# Print the results of the cross-validation for Random Forest
print(rf_model$results)
print(paste("Best F1-score for Random Forest:", max(rf_f1)))
maxf1rf<-max(rf_f1)
### Decision Tree ###

dt_model <- train(Approval ~ ., 
                  data = train_data_balanced, 
                  method = "rpart", 
                  metric = "ROC",
                  control = rpart.control(),
                  trControl = cv_control)

# Calculating the F1 score using Sensitivity and Specificity
dt_f1 <- 2 * (dt_model$results$Sens * dt_model$results$Spec) / (dt_model$results$Sens + dt_model$results$Spec)

# Print the results of the cross-validation for Decision Tree
print(dt_model$results)
print(paste("Best F1-score for Decision Tree:", max(dt_f1)))
maxf1dt <- max(dt_f1)
######################
##### Modeling - Logistic #####
######################
library(caret)
library(tree)
library(rpart)
library(glmnet)

colnames(new_data)

################################
##LOGISTIC REGRESSION Version1##
################################
#Convert the Approval Column to a Factor:
train_data$Approval <- as.factor(train_data$Approval)



##########################################cross validation and f1 score##################
# load the necessary packages
library(caret)

# Set up the training control:
train_control <- trainControl(method = "cv", number = 10, 
                              savePredictions = "final", classProbs = TRUE, 
                              summaryFunction = twoClassSummary)

# Train the logistic regression model using train():
# Check current levels
levels(train_data$Approval)

# Rename factor levels (assuming it has levels '0' and '1')
levels(train_data$Approval) <- c("Class0", "Class1")

# Try the cross-validation process again
set.seed(123) # for reproducibility
train_control <- trainControl(method = "cv", 
                              number = 10, 
                              summaryFunction = twoClassSummary, 
                              classProbs = TRUE, 
                              verboseIter = FALSE)

set.seed(123) # for reproducibility
model_loan_credit_cv <- train(Approval ~ Begin_month, 
                              data = train_data, 
                              method = "glm", 
                              family = "binomial",
                              metric = "ROC", 
                              trControl = train_control)


# Extract the resampled results to get F1 scores:
sensitivity <- model_loan_credit_cv$results[, "Sens"]
specificity <- model_loan_credit_cv$results[, "Spec"]

# Calculate Precision
precision <- sensitivity / (sensitivity + (1 - specificity))

# Calculate F1 Score
logistic_f1_score <- 2 * (precision * sensitivity) / (precision + sensitivity)
print(logistic_f1_score)




############################
##✮✮✮✮✮LASSO MODELING✮✮✮✮✮##
############################
# PLEASE CONSULT ANOTHER FILE FOR LASSO MODEL

######Visualization
# Required Libraries
library(ggplot2)

# Data
model_names <- c("Random Forest", "Decision Tree", "Lasso", "Logistic Regression")
#Lasso is from another file
metrics <- c(maxf1rf, maxf1dt, 0.585752, logistic_f1_score)

data_to_plot <- data.frame(models = model_names, values = metrics)

# Bar Plot
ggplot(data_to_plot, aes(x = models, y = values, fill = models)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label=round(values,3)), vjust=-0.3, size=3.5) +
  labs(title = "Model Performance Comparison", 
       x = "Model", 
       y = "F1 Score") +
  theme_minimal() +
  theme(legend.position = "none")
