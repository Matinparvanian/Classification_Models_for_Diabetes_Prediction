rm(list = ls())
ls()

# 2.Preparation of Dataset

#########################################################################################################
# 2.1 Data collection
#########################################################################################################
# Importation
library(class) 
library(MASS) 
library(e1071)
library(DescTools)
library(ggplot2)
library(pROC)
library(reshape)
library(naniar)
library(knitr)
library(corrplot)
library(dplyr)
#########################################################################################################

setwd('E:\\2023-2024A\\Statistical Learning\\data')
Diabetes = read.csv("diabetes.csv")
attach(Diabetes)
# An overview of dataset
str(Diabetes)

# check the dimension of Diabetes dataset
dim(Diabetes)

###################################################################################
# 2.2 Preprocessing 
###################################################################################
# number and percentage of missing values in Diabetes Dataset.
miss_var_summary = miss_var_summary(Diabetes)
miss_var_summary



for (i in c(2:6)) {
  num_zeros = length(which(Diabetes[,i] == 0))
  print(paste("Column", names(Diabetes)[i], "has", num_zeros, "zeros."))
}

# let's change the zero variables with median of their column.
Diabetes[,2:6][Diabetes[,2:6] == 0] = NA 


for (i in c(2, 3, 4, 5, 6)) {
  NA_values = which(is.na(Diabetes[, i]), arr.ind = TRUE)
  Diabetes[, i][NA_values] = median(Diabetes[, i][!is.na(Diabetes[, i])])
}

#know let's attach new variables to our dataset
attach(Diabetes)


###########################################################################################
# Exploratory and Data Visualization
###########################################################################################
ggplot(Diabetes, aes(x = Pregnancies, fill = factor(Outcome))) +
  geom_bar() +
  scale_fill_manual(values = c("#76EEC6", "#838B83"), name = "Outcome") +
  labs(x = "Number of Pregnancies", y = "Frequency")



ggplot(Diabetes, aes(x = Pregnancies)) +
  geom_histogram(fill = '#76EEC6', color = '#838B83', alpha = 0.6, 
                 binwidth = 1, position = "dodge") +
  facet_wrap(~Outcome, ncol = 2, scales = "free_y") +
  labs(x = "Number of Pregnancies", y = "Frequency") +
  scale_fill_manual(values = c("#76EEC6", "#838B83"), name = "Outcome") +
  theme_bw()


#From these charts, we can see that the majority of women have less than 5
#pregnancies, regardless of their diabetic status. However, among women with
#6 or more pregnancies, the proportion of diabetic women is higher than 
#non-diabetic women. This suggests that having a higher number of pregnancies
#may increase the risk of developing diabetes
 

ggplot(Diabetes, aes(x = Glucose)) +
  geom_density(fill='#76EEC6', alpha=0.6) +
  geom_histogram(aes(y=..density..), fill='#8EE5EE', color='#838B83', alpha=0.6) +
  labs(x ="Glucose", y="Density") +
  theme_classic()

#From the plot, we can see that the glucose levels in the dataset are 
#mostly distributed around the 100-150 mg/dL range. The density plot 
#shows that the probability density of glucose levels is highest at around
#120 mg/dL and decreases as we move away from this value. 
#The histogram shows that the majority of the observations fall in the 
#range of 75-200 mg/dL. Additionally, the shape of the distribution appears
#to be slightly skewed to the right, which suggests that there may be some 
#outliers or a long tail in the higher glucose values

ggplot(Diabetes, aes(x = Glucose, fill = factor(Outcome))) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("#8EE5EE", "#838B83"), name = "Outcome") +
  labs(x = "Glucose", y = "Density") +
  theme_classic()
#### check if it is possible to add percent for each group!!!
#The chart shows that people with Outcome 1 (diabetes) have a higher 
#density of glucose levels compared to those with Outcome 0 (no diabetes).
#It also shows that the distribution of glucose levels for Outcome 1 is 
#more spread out than Outcome 0, indicating that there is more variation 
#in glucose levels among people with diabetes.


ggplot(Diabetes, aes(x = BloodPressure)) +
  geom_density(fill='#EE7621', alpha=0.6) +
  geom_histogram(aes(y=..density..), fill='#8EE5EE', color='#838B83', alpha=0.6) +
  labs(x ="Blood Pressure", y="Density") +
  theme_classic()
#### check normal percentages!!!!
#From the plot, we can see that the blood pressure levels in the dataset
#are mostly distributed around the 60-90 mm Hg range. The density plot 
#shows that the probability density of blood pressure levels is highest
#at around 70 mm Hg and decreases as we move away from this value. 
#The histogram shows that the majority of the observations fall in 
#the range of 60-100 mm Hg. Additionally, the shape of the distribution 
#appears to be slightly skewed to the right, which suggests that there may
#be some outliers or a long tail in the higher blood pressure values

ggplot(Diabetes, aes(x = BloodPressure, fill = factor(Outcome))) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("#8EE5EE", "#838B83"), name = "Outcome") +
  labs(x = "Bloodpressure", y = "Density") +
  theme_classic()

#The density curve for non-diabetic patients (in blue) is centered around
#a lower blood pressure value compared to the curve for diabetic patients 
#(in gray), which is more spread out and centered around a higher blood 
#pressure value. We can also see that the density of non-diabetic patients 
#blood pressure values is higher overall, which may indicate that non-diabetic 
#patients are more likely to have blood pressure values clustered around a specific 
#range.Also, we can see that the blood pressure levels in the dataset are mostly 
#concentrated around the 60-80 mmHg range, and the density of blood pressure 
#values for the non-diabetic group (blue) is slightly higher than the density for the
#diabetic group (gray). The density plot shows that the probability density of blood 
#pressure levels is highest around 70-75 mmHg and decreases as we move away from this
#value

#### add the other two charts without removing missing values!!!
ggplot(Diabetes, aes(x = SkinThickness)) +
  geom_density(fill='#00EEEE', alpha=0.6) +
  geom_histogram(aes(y=..density..), fill='#8EE5EE', color='#838B83', alpha=0.6) +
  labs(x ="Skin Thickness", y="Density") +
  theme_classic()

#From the chart, we can see that skin thickness is generally concentrated between
#20 and 35 mm, with a peak density around 30 mm. The histogram shows that the 
#distribution is slightly skewed to the right, which suggests that there may be some
#outliers or a long tail in the higher skin thickness values

ggplot(Diabetes, aes(x = SkinThickness, fill = factor(Outcome))) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("#00EEEE", "#838B83"), name = "Outcome") +
  labs(x = "Skin Thickness", y = "Density") +
  theme_classic()

#We can also see that the density curves for the two outcome variables overlap
#considerably, which suggests that skin thickness may not be a strong predictor
#of the outcome. However, we can see that there is a slightly higher density of
#skin thickness values around 30 mm for the outcome variable 0 (non-diabetic) than
#for the outcome variable 1 (diabetic)



ggplot(Diabetes, aes(x = Insulin, fill = factor(Outcome))) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("#A2CD5A", "#838B83"), name = "Outcome") +
  labs(x = "Insulin", y = "Density") +
  theme_classic()
#From the plot, we can see that the distribution of insulin levels in the Diabetes
#dataset differs between outcomes 0 and 1. The density plot shows that individuals 
#with an outcome of 0 have a slightly higher probability density of insulin levels 
#around 100-150 μIU/mL, whereas those with an outcome of 1 have a broader 
#distribution with a peak at around 150 μIU/mL. The histogram indicates that the 
#majority of individuals in the dataset have insulin levels in the range of 0-400 μIU/mL,
#with a long tail towards higher values. Overall, this plot suggests that insulin levels
#may be a useful predictor for diabetes outcomes, as individuals with different 
#outcomes have distinct distributions of insulin levels.


ggplot(Diabetes, aes(x = BMI)) +
  geom_density(fill='#00EEEE', alpha=0.6) +
  geom_histogram(aes(y=..density..), fill='#8EE5EE', color='#838B83', alpha=0.6) +
  labs(x ="BMI", y="Density") +
  theme_classic()
#we can see that the BMI values in the dataset are mostly distributed around the 20-40
#range. The density plot shows that the probability density of BMI values is highest
#around 30 and decreases as we move away from this value. The histogram shows that 
#the majority of the observations fall in the range of 20-50. Additionally, the shape
#of the distribution appears to be slightly skewed to the right, which suggests that
#there may be some outliers or a long tail in the higher BMI values.

ggplot(Diabetes, aes(x = BMI, fill = factor(Outcome))) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("#00EEEE", "#838B83"), name = "Outcome") +
  labs(x = "BMI", y = "Density") +
  theme_classic()

#The density plot suggests that the BMI distribution is bimodal, with one peak
#around 25 and another around 35. Additionally, the density of the BMI values is
#higher for Outcome 1 (positive diabetes diagnosis) than for Outcome 0 
#(negative diabetes diagnosis) in the higher BMI range, indicating that individuals
#with higher BMIs are more likely to be diagnosed with diabetes. The shape of the
#distribution also appears to be slightly skewed to the right, which suggests that
#there may be some outliers or a long tail in the higher BMI values. Overall, 
#this plot suggests that BMI is an important predictor of diabetes diagnosis, 
#with higher BMI values associated with a greater likelihood of being diagnosed 
#with diabetes

ggplot(Diabetes, aes(x = DiabetesPedigreeFunction)) +
  geom_density(fill='#00EEEE', alpha=0.6) +
  geom_histogram(aes(y=..density..), fill='#8EE5EE', color='#838B83', alpha=0.6) +
  labs(x ="DiabetesPedigreeFunction", y="Density") +
  theme_classic()

#From this plot, we can observe that the DiabetesPedigreeFunction variable is
#approximately normally distributed with a slight positive skew. The majority
#of the observations lie between 0 and 1, and there is a small bump in the density
#plot around the value of 0.4. The Diabetes Pedigree Function is a genetic score
#that estimates the risk of developing diabetes based on the family history of the
#patient. The bimodal shape of the density plot might indicate the presence of two
#sub-populations with different levels of genetic predisposition to diabetes. However,
#further analysis is required to confirm this interpretation.

ggplot(Diabetes, aes(x = DiabetesPedigreeFunction, fill = factor(Outcome))) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("#00EEEE", "#838B83"), name = "Outcome") +
  labs(x = "DiabetesPedigreeFunction", y = "Density") +
  theme_classic()

#By looking at this plot, we can see that the DiabetesPedigreeFunction values for
#Outcome 0(Non-Diabetes) are mostly concentrated around 0 to 1, while for Outcome 1(Diabetes),
#the values are more widely spread and skewed towards the right, with a long tail. This plot 
#can be useful in identifying the differences in the distribution of DiabetesPedigreeFunction
#between people with and without diabetes.we can see that the distribution of 
#DiabetesPedigreeFunction is shifted towards higher values in the group with diabetes. 


ggplot(Diabetes, aes(x = Age, fill = factor(Outcome))) +
  geom_bar() +
  scale_fill_manual(values = c("#76EEC6", "#838B83"), name = "Outcome") +
  labs(x = "Age", y = "Frequency")

#By looking at the plot, we can see that the frequency of individuals with diabetes
#increases with age, peaking in the age group of 50-59 years, and then gradually decreasing
#in older age groups. We can also see that the frequency of individuals without diabetes
#remains relatively stable across different age groups.


ggplot(Diabetes, aes(x = Age)) +
  geom_histogram(fill = '#EEC900', color = '#838B83', alpha = 0.6, 
                 binwidth = 1, position = "dodge") +
  facet_wrap(~Outcome, ncol = 2, scales = "free_y") +
  labs(x = "Age", y = "Frequency") +
  scale_fill_manual(values = c("#EEC900", "#838B83"), name = "Outcome") +
  theme_bw()

#The histogram plot shows the distribution of age for people with and without diabetes in
#the dataset. The plot suggests that people with diabetes tend to be older than those 
#without diabetes, with a peak in the 50-60 age range. This is consistent with the well-known
#fact that diabetes is more common in older age groups.The plot also shows that the age
#distribution for people without diabetes is roughly normal, with a peak in the 25-30 age
#range. This is consistent with the general population distribution of age.The differences
#in age distribution between people with and without diabetes are visually clear
#in the plot, which highlights the utility of this type of visualization for identifying
#potential risk factors for a given condition or disease.



ggplot(Diabetes, aes(x = Age, fill = factor(Outcome))) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("#EEC900", "#838B83"), name = "Outcome") +
  labs(x = "Age", y = "Density") +
  theme_classic()

#This plot shows the density distribution of age for individuals with and without diabetes.
#The plot suggests that the distribution of age for people with diabetes is slightly shifted
#to the right compared to those without diabetes. It also shows that the density of people
#with diabetes is higher in the age range of 40-60 years old, while the density of people
#without diabetes is higher in the age range of 20-30 years old.




table(Diabetes)
Diabetes %>%
  group_by(Outcome) %>%
  summarise(n = n()) %>%
  mutate(Percentage = round(n/sum(n)*100, 1)) %>%
  ggplot(aes(x="", y=n, fill = factor(Outcome))) +
  geom_bar(width = 1, color = "white", alpha = 0.5, stat = "identity") +
  coord_polar("y", start=0) +
  labs(fill ="Outcome", x="", y="") +
  theme_void() +
  geom_text(aes(y = n/1.3, label = paste0(Percentage, "%")), color = "white", size = 4)

# The plot shows the distribution of the Outcome variable in the Diabetes dataset.
#The Outcome variable has two levels, 0 for no diabetes and 1 for diabetes.
#The plot shows that out of the total 768 observations, 500 (65.1%) do not have diabetes,
#while 268 (34.9%) have diabetes.


#######################################################################################
# Let's check outliers for Diabetes Dataset
#######################################################################################
check_outlier = function(x) {
  q1 = quantile(x, 0.25)
  q3 = quantile(x, 0.75)
  iqr = q3 - q1
  lower = q1 - 1.5 * iqr
  upper = q3 + 1.5 * iqr
  outliers = x[x < lower | x > upper]
  if (length(outliers) > 0) {
    return(paste(':', TRUE,"| Number of outliers:", length(outliers)))
  } else {
    return(paste(':', FALSE,"| Number of outliers:", length(outliers)))
  }
}

for (col in names(Diabetes[-9])) {
  print(paste(col, check_outlier(Diabetes[[col]])))
}


par(mfrow=c(2,4))
for (i in c(1 , 3:8)) {
  boxplot(Diabetes[,i], main=names(Diabetes)[i] , col = '#76EEC6')
}


#Using Log-transformtion for Age variable
Diabetes$Age = log(Diabetes$Age)


# using Winsorize approach to handle outliers for Pregnancies and DiabetesPedigreeFunction

for (i in c(1 , 7)){
  pctiles = quantile(Diabetes[,i], probs = c(0.25, 0.75), na.rm = TRUE)
  Diabetes[,i] = Diabetes[,i]
  Diabetes[,i][Diabetes[,i] < pctiles[1]] = pctiles[1]
  Diabetes[,i][Diabetes[,i] > pctiles[2]] = pctiles[2]
}




#using IQR method to replace outliers.
quantiles <- c(0.25, 0.75)


# lets check which instances are outliers logically.
check_outlier2 = function(x) {
  q1 = quantile(x, 0.25)
  q3 = quantile(x, 0.75)
  iqr = q3 - q1
  lower = q1 - 1.5 * iqr
  upper = q3 + 1.5 * iqr
  outliers = x[x < lower | x > upper]
  if (length(outliers) > 0) {
    return (TRUE)
  } else {
    return(FALSE)
  }
}

for (col in c("Insulin","BloodPressure", "SkinThickness", "BMI")) {
  if (check_outlier2(Diabetes[[col]]) == TRUE){
    col_quantiles <- quantile(Diabetes[[col]], quantiles)
    iqr <- diff(col_quantiles)
    
    Diabetes[[col]][Diabetes[[col]] > col_quantiles[2]] <- col_quantiles[2] + 1.5 * iqr
    Diabetes[[col]][Diabetes[[col]] < col_quantiles[1]] <- col_quantiles[1] - 1.5 * iqr
  }
}

attach(Diabetes)
head(Diabetes)

par(mfrow=c(2,4))
for (i in c(1:8)) {
  boxplot(Diabetes[,i], main=names(Diabetes)[i] , col = '#76EEC6')
}


#Let's plot a heatmap in order to check the corrolations.
cor_matrix = cor(Diabetes)
# Reshape the correlation matrix into a dataframe
cor_df = as.data.frame(as.table(cor_matrix))
names(cor_df) = c("Var1", "Var2", "value")

# Add Pearson correlation coefficient to each cell
cor_df$corr_coef = round(cor_df$value, 2)

# Create a heatmap
ggplot(cor_df, aes(Var1, Var2, fill = value, label = corr_coef)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", mid = "white", high = "#79CDCD", midpoint = 0) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Correlation Matrix Heatmap", x = NULL, y = NULL) +
  geom_text(color = "black" , size = 2)
#################################################################################################
# Model
#################################################################################################
set.seed(123)
test_index = sample(nrow(Diabetes), 0.2 * nrow(Diabetes))
train = Diabetes[-test_index, ]
test = Diabetes[test_index, ]

# Fit a logistic regression model
model = glm(Outcome ~ ., data = train, family = "binomial")

# Train set performance
pred_train = predict(model, newdata = train, type = "response")
pred_class_train = ifelse(pred_train > 0.5, 1, 0)

# Calculate confusion matrix for train set
#at first lets explain some terms about the confusion matrix. we consider four values
# which are more important for us. first of all we consider true_positive
# which is the number of instances who have diabetes and are predicted as 
# diabetic people correctly.

# The second value is false_positive which is the number of instances who are
#Non-diabetic but are predicted as diabetes group.

# The third value is false_negative which is the number of instances who have diabetes
# but are predicted as non-diabetic.

# The last one is true_negative which is the number of instances who don't have diabete
# and are corretly predicted as non-diabetic


confusion_matrix_train = table(pred_class_train, train$Outcome)
true_positive_train = confusion_matrix_train[2,2]
false_positive_train = confusion_matrix_train[1,2]
false_negative_train = confusion_matrix_train[2,1]
true_negative_train = confusion_matrix_train[1,1]


#Now we have defined different values for our confusion matrix. now it's time
# to define different metrics.

# Calculate performance metrics for train set
recall_train = true_positive_train / (true_positive_train + false_negative_train)
precision_train = true_positive_train / (true_positive_train + false_positive_train)
f1_score_train = 2 * precision_train * recall_train / (precision_train + recall_train)
accuracy_train = (true_positive_train + true_negative_train) / sum(confusion_matrix_train)

# Test set performance
pred_test = predict(model, newdata = test, type = "response")
pred_class_test = ifelse(pred_test > 0.5, 1, 0)

# Calculate confusion matrix for test set
confusion_matrix_test = table(pred_class_test, test$Outcome)
true_positive_test = confusion_matrix_test[2,2]
false_positive_test = confusion_matrix_test[1,2]
false_negative_test = confusion_matrix_test[2,1]
true_negative_test = confusion_matrix_test[1,1]

# Calculate performance metrics for test set
recall_test = true_positive_test / (true_positive_test + false_negative_test)
precision_test = true_positive_test / (true_positive_test + false_positive_test)
f1_score_test = 2 * precision_test * recall_test / (precision_test + recall_test)
accuracy_test = (true_positive_test + true_negative_test) / sum(confusion_matrix_test)

# Output performance metrics
results_train = data.frame(
  Metric = c("Recall", "Precision","F1 Score" , "Accuracy"),
  Value = c(recall_train, precision_train,f1_score_train , accuracy_train),
  Set = "Train"
)

results_test = data.frame(
  Metric = c("Recall", "Precision","F1 Score" , "Accuracy"),
  Value = c( recall_test,precision_test, f1_score_test ,accuracy_test),
  Set = "Test"
)

print(results_train)
print(results_test)



##########################################################################################################
# Apply cross validation on Logistic Regression model.
#########################################################################################################
# Define the number of folds for cross-validation
k = 10

CV_test_index = sample(nrow(Diabetes), 0.2 * nrow(Diabetes))
CV_train = Diabetes[-CV_test_index, ]
CV_test = Diabetes[CV_test_index, ]

# Split the data into k folds
set.seed(123)
folds = cut(seq(1, nrow(CV_train)), breaks = k, labels = FALSE)

# Initialize vectors to store performance metrics
CV_accuracy = rep(0, k)
CV_precision = rep(0, k)
CV_recall = rep(0, k)
CV_f1_score = rep(0, k)

# Perform k-fold cross-validation
for (i in 1:k) {
  # Subset the data for the i-th fold
  CV_test_indices = which(folds == i)
  CV_test = CV_train[CV_test_indices, ]
  CV_train_subset = CV_train[-CV_test_indices, ]
  
  # Fit the logistic regression model on the training subset
  CV_model = glm(Outcome ~ ., data = CV_train_subset, family = "binomial")
  
  # Make predictions on the test subset
  CV_pred = predict(CV_model, newdata = CV_test, type = "response")
  CV_pred_class = ifelse(CV_pred > 0.5, 1, 0)
  
  # Calculate confusion matrix and performance metrics
  CV_confusion_matrix = table(CV_pred_class, CV_test$Outcome)
  CV_true_positive = CV_confusion_matrix[2,2]
  CV_false_positive = CV_confusion_matrix[1,2]
  CV_false_negative = CV_confusion_matrix[2,1]
  CV_true_negative = CV_confusion_matrix[1,1]
  

  CV_recall[i] = CV_true_positive / (CV_true_positive + CV_false_negative)
  CV_precision[i] = CV_true_positive / (CV_true_positive + CV_false_positive)
  CV_f1_score[i] = 2 *  CV_precision[i] * CV_recall[i] / ( CV_precision[i] + CV_recall[i])
  CV_accuracy[i] = (CV_true_positive + CV_true_negative) / sum(CV_confusion_matrix)
}

# Calculate average performance metrics across all folds
mean_accuracy = mean(CV_accuracy)
mean_precision = mean(CV_precision)
mean_recall = mean(CV_recall)
mean_f1_score = mean(CV_f1_score)

# Output performance metrics
CV_results = data.frame(
  Metric = c("CV Recall","CV Precision", "CV F1 Score","CV Accuracy"),
  Value = c(mean_recall,mean_precision, mean_f1_score, mean_accuracy)
)

print(CV_results)

##########################################################################################################
# Apply feature selection on Logistic model
##########################################################################################################
# Perform backward feature selection using step function
backward_model = step(model, direction = "backward")

# Train set performance
backward_pred_train = predict(backward_model, newdata = train, type = "response")
backward_pred_class_train = ifelse(backward_pred_train > 0.5, 1, 0)

# Calculate confusion matrix for train set
backward_confusion_matrix_train = table(backward_pred_class_train, train$Outcome)
backward_true_positive_train = backward_confusion_matrix_train[2,2]
backward_false_positive_train = backward_confusion_matrix_train[1,2]
backward_false_negative_train = backward_confusion_matrix_train[2,1]
backward_true_negative_train = backward_confusion_matrix_train[1,1]

# Calculate performance metrics for train set
backward_recall_train = backward_true_positive_train / (backward_true_positive_train + backward_false_negative_train)
backward_precision_train = backward_true_positive_train / (backward_true_positive_train + backward_false_positive_train)
backward_f1_score_train = 2 * backward_precision_train * backward_recall_train / (backward_precision_train + backward_recall_train)
backward_accuracy_train = (backward_true_positive_train + backward_true_negative_train) / sum(backward_confusion_matrix_train)


# Test set performance
backward_pred_test = predict(backward_model, newdata = test, type = "response")
backward_pred_class_test = ifelse(backward_pred_test > 0.5, 1, 0)

# Calculate confusion matrix for test set
backward_confusion_matrix_test = table(backward_pred_class_test, test$Outcome)
backward_true_positive_test = backward_confusion_matrix_test[2,2]
backward_false_positive_test = backward_confusion_matrix_test[1,2]
backward_false_negative_test = backward_confusion_matrix_test[2,1]
backward_true_negative_test = backward_confusion_matrix_test[1,1]

# Calculate performance metrics for test set
backward_recall_test = backward_true_positive_test / (backward_true_positive_test + backward_false_negative_test)
backward_precision_test = backward_true_positive_test / (backward_true_positive_test + backward_false_positive_test)
backward_f1_score_test = 2 * backward_precision_test * backward_recall_test / (backward_precision_test + backward_recall_test)
backward_accuracy_test = (backward_true_positive_test + backward_true_negative_test) / sum(backward_confusion_matrix_test)

# Output performance metrics
backward_results_train = data.frame(
  Metric = c("backward_Recall","backward_Precision", "backward_F1 Score", "backward_Accuracy"),
  Value = c( backward_recall_train, backward_precision_train, backward_f1_score_train, backward_accuracy_train),
  Set = "Train"
)

backward_results_test= data.frame(
  Metric = c("backward_Recall","backward_Precision", "backward_F1 Score", "backward_Accuracy"),
  Value = c(backward_recall_test, backward_precision_test, backward_f1_score_test, backward_accuracy_test),
  Set = "Test"
)

print(backward_results_train)
print(backward_results_test)

#######################################################################################################
# Now let's check which of these 3 models are the best one!
#######################################################################################################
Logistic_Regression_results = data.frame(Metric = c('Recall' , 'Precission' , 'F1-Score' , 'Accuracy'),
                                         Logistic_Regression = c( 0.75 , 0.50 ,0.60 ,0.74  ),
                                         Cross_Validation = c(0.68 , 0.58 , 0.62 , 0.76),
                                         Backward_Feature_Selection = c(0.73 , 0.50 , 0.60 , 0.73))


print(Logistic_Regression_results)


Confusion_Matrix = data.frame(class = c('Non-Diabetic' , 'Diabetic'), 'Log-Non-Diabetic' = c(84 , 10) , 'Log-Diabetic' = c(29 , 30),
                              'CV-Non-Diabetic' = c(35 , 4) , 'CV-Diabetic' = c(11 , 12),
                              'Bakward-Non-Diabetic' = c(83 , 11) , 'backward-Diabetic' = c(29 , 30))

print(Confusion_Matrix)

############################################################################################################################
# Logistic Regression ROC curve
############################################################################################################################
roc_data = data.frame(prob = backward_pred_test, label = test$Outcome)
CV_roc_data = data.frame(prob = CV_pred, label = CV_test$Outcome)
LR_roc_data = data.frame(prob = pred_test, label = test$Outcome)

# Sort the data frame by the predicted probabilities in descending order
roc_data = roc_data[order(-roc_data$prob), ]
CV_roc_data = CV_roc_data[order(-CV_roc_data$prob), ]
LR_roc_data = LR_roc_data[order(-LR_roc_data$prob), ]

# Calculate the true positive rate (sensitivity) and the false positive rate (1 - specificity)
tpr = cumsum(roc_data$label) / sum(roc_data$label)
fpr = cumsum(!roc_data$label) / sum(!roc_data$label)

CV_tpr = cumsum(CV_roc_data$label) / sum(CV_roc_data$label)
CV_fpr = cumsum(!CV_roc_data$label) / sum(!CV_roc_data$label)

LR_tpr = cumsum(LR_roc_data$label) / sum(LR_roc_data$label)
LR_fpr = cumsum(!LR_roc_data$label) / sum(!LR_roc_data$label)

# Plot the ROC curve
par(mfrow = c(1,1))
plot(fpr, tpr, type = "l", main = "ROC Curve", xlab = "False Positive Rate", ylab = "True Positive Rate" , col = '#76EEC6')
lines(CV_fpr, CV_tpr, type = "l", main = "ROC Curve", xlab = "False Positive Rate", ylab = "True Positive Rate" , col = 'blue')
lines(LR_fpr, LR_tpr, type = "l", main = "ROC Curve", xlab = "False Positive Rate", ylab = "True Positive Rate" , col = 'red')

# Add a diagonal line representing random guessing
abline(0, 1, col = "gray", lty = 2)

Backward_auc_roc = round(auc(fpr, tpr), 3)
CV_auc_roc = round(auc(CV_fpr, CV_tpr), 3)
LR_auc_roc = round(auc(LR_fpr, LR_tpr), 3)


legend_labels = c(paste("Backward_AUC =", Backward_auc_roc),
                   paste("CV_AUC =", CV_auc_roc),
                   paste("LR_AUC =", LR_auc_roc))

legend("bottomright", legend = legend_labels, bty = "n", col = c('#76EEC6', 'blue', 'red'), lty = 1)

############################################################################################################################
#Let's fit a KNN model to our dataset.
############################################################################################################################

# Fit a KNN model
k = 5
KNN_model = knn(train[, -9], test[, -9], train[, 9], k)

# Calculate confusion matrix for test set
KNN_confusion_matrix = table(KNN_model, test$Outcome)
KNN_true_positive = KNN_confusion_matrix[2,2]
KNN_false_positive = KNN_confusion_matrix[1,2]
KNN_false_negative = KNN_confusion_matrix[2,1]
KNN_true_negative = KNN_confusion_matrix[1,1]

# Calculate performance metrics for test set
KNN_accuracy = (KNN_true_positive + KNN_true_negative) / sum(KNN_confusion_matrix)
KNN_precision = KNN_true_positive / (KNN_true_positive + KNN_false_positive)
KNN_recall = KNN_true_positive / (KNN_true_positive + KNN_false_negative)
KNN_f1_score = 2 * KNN_precision * KNN_recall / (KNN_precision + KNN_recall)

# Output performance metrics
KNN_results = data.frame(
  Metric = c("Recall", "Precision", "F1 Score", "Accuracy"),
  Value = c(KNN_recall, KNN_precision, KNN_f1_score, KNN_accuracy),
  Set = "Test"
)

print(KNN_results)


###############################################################################################################
# Apply cross validation on KNN model.
###############################################################################################################
k = 10

# Split the data into training and test sets
k <- 10
KNN_CV_recall <- numeric(k)
KNN_CV_precision <- numeric(k)
KNN_CV_f1_score <- numeric(k)
KNN_CV_accuracy <- numeric(k)

for (i in 1:k) {
  # Create training and test sets for this fold
  KNN_test_indices <- which(KNN_folds == i, arr.ind = TRUE)
  KNN_test <- Diabetes[KNN_test_indices, ]
  KNN_train <- Diabetes[-KNN_test_indices, ]
  
  # Fit a KNN model
  KNN_CV_model <- knn(KNN_train[, -9], KNN_test[, -9], KNN_train[, 9], k = 5)
  
  # Calculate confusion matrix for test set
  KNN_CV_confusion_matrix <- table(KNN_CV_model, KNN_test$Outcome)
  KNN_CV_true_positive <- KNN_CV_confusion_matrix[2,2]
  KNN_CV_false_positive <- KNN_CV_confusion_matrix[1,2]
  KNN_CV_false_negative <- KNN_CV_confusion_matrix[2,1]
  KNN_CV_true_negative <- KNN_CV_confusion_matrix[1,1]
  
  # Calculate performance metrics for test set
  KNN_CV_accuracy[i] <- (KNN_CV_true_positive + KNN_CV_true_negative) / sum(KNN_CV_confusion_matrix)
  KNN_CV_precision[i] <- KNN_CV_true_positive / (KNN_CV_true_positive + KNN_CV_false_positive)
  KNN_CV_recall[i] <- KNN_CV_true_positive / (KNN_CV_true_positive + KNN_CV_false_negative)
  KNN_CV_f1_score[i] <- 2 * KNN_CV_precision[i] * KNN_CV_recall[i] / (KNN_CV_precision[i] + KNN_CV_recall[i])
}

# Output performance metrics
KNN_CV_results <- data.frame(
  Fold = 1:k,
  Recall = KNN_CV_recall,
  Precision = KNN_CV_precision,
  F1_Score = KNN_CV_f1_score,
  Accuracy = KNN_CV_accuracy
)

print(KNN_CV_results)



###########################################################################################################
#Let's fit a naive bayes on our dataset.
###########################################################################################################
# Fit Naive Bayes model
nb_model = naiveBayes(Outcome ~ ., data = train)

# Train set performance
nb_pred_train = predict(nb_model, newdata = train, type = 'class')



nb_confusion_matrix_train = table(nb_pred_train, train$Outcome)
nb_true_positive_train = nb_confusion_matrix_train[2,2]
nb_false_positive_train = nb_confusion_matrix_train[1,2]
nb_false_negative_train = nb_confusion_matrix_train[2,1]
nb_true_negative_train = nb_confusion_matrix_train[1,1]


#Now we have defined different values for our confusion matrix. now it's time
# to define different metrics.

# Calculate performance metrics for train set
nb_recall_train = nb_true_positive_train / (nb_true_positive_train + nb_false_negative_train)
nb_precision_train = nb_true_positive_train / (nb_true_positive_train + nb_false_positive_train)
nb_f1_score_train = 2 * nb_precision_train * nb_recall_train / (nb_precision_train + nb_recall_train)
nb_accuracy_train = (nb_true_positive_train + nb_true_negative_train) / sum(nb_confusion_matrix_train)

# Test set performance
nb_pred_test = predict(nb_model, newdata = test , type = 'class')

# Calculate confusion matrix for test set
nb_confusion_matrix_test = table(nb_pred_test, test$Outcome)
nb_true_positive_test = nb_confusion_matrix_test[2,2]
nb_false_positive_test = nb_confusion_matrix_test[1,2]
nb_false_negative_test = nb_confusion_matrix_test[2,1]
nb_true_negative_test = nb_confusion_matrix_test[1,1]

# Calculate performance metrics for test set
nb_recall_test = nb_true_positive_test / (nb_true_positive_test + nb_false_negative_test)
nb_precision_test = nb_true_positive_test / (nb_true_positive_test + nb_false_positive_test)
nb_f1_score_test = 2 * nb_precision_test * nb_recall_test / (nb_precision_test + nb_recall_test)
nb_accuracy_test = (nb_true_positive_test + nb_true_negative_test) / sum(nb_confusion_matrix_test)

# Output performance metrics
nb_results_train = data.frame(
  Metric = c("Recall", "Precision","F1 Score" , "Accuracy"),
  Value = c(nb_recall_train, nb_precision_train, nb_f1_score_train , nb_accuracy_train),
  Set = "Train"
)

nb_results_test = data.frame(
  Metric = c("Recall", "Precision","F1 Score" , "Accuracy"),
  Value = c( nb_recall_test, nb_precision_test, nb_f1_score_test , nb_accuracy_test),
  Set = "Test"
)

print(nb_results_train)
print(nb_results_test)

###############################################################################################################
# Apply cross validation on Naive Bayes model.
###############################################################################################################
# Define the number of folds for cross-validation
k = 10

nb_CV_test_index = sample(nrow(Diabetes), 0.2 * nrow(Diabetes))
nb_CV_train = Diabetes[-nb_CV_test_index, ]
nb_CV_test = Diabetes[nb_CV_test_index, ]

# Split the data into k folds
set.seed(123)
nb_folds = cut(seq(1, nrow(nb_CV_train)), breaks = k, labels = FALSE)

# Initialize vectors to store performance metrics
nb_CV_accuracy = rep(0, k)
nb_CV_precision = rep(0, k)
nb_CV_recall = rep(0, k)
nb_CV_f1_score = rep(0, k)

# Perform k-fold cross-validation
for (i in 1:k) {
  # Subset the data for the i-th fold
  nb_CV_test_indices = which(nb_folds == i)
  nb_CV_test = nb_CV_train[nb_CV_test_indices, ]
  nb_train_subset = nb_CV_train[-nb_CV_test_indices, ]
  
  # Fit the logistic regression model on the training subset
  nb_CV_model = naiveBayes(Outcome ~ ., data = nb_train_subset, family = "binomial")
  
  # Make predictions on the test subset
  nb_CV_pred = predict(nb_CV_model, newdata = nb_CV_test, type = "class")
  
  # Calculate confusion matrix and performance metrics
  nb_CV_confusion_matrix = table(nb_CV_pred, nb_CV_test$Outcome)
  nb_CV_true_positive = nb_CV_confusion_matrix[2,2]
  nb_CV_false_positive = nb_CV_confusion_matrix[1,2]
  nb_CV_false_negative = nb_CV_confusion_matrix[2,1]
  nb_CV_true_negative = nb_CV_confusion_matrix[1,1]
  
  
  nb_CV_recall[i] = nb_CV_true_positive / (nb_CV_true_positive + nb_CV_false_negative)
  nb_CV_precision[i] = nb_CV_true_positive / (nb_CV_true_positive + nb_CV_false_positive)
  nb_CV_f1_score[i] = 2 *  nb_CV_precision[i] * nb_CV_recall[i] / ( nb_CV_precision[i] + nb_CV_recall[i])
  nb_CV_accuracy[i] = (nb_CV_true_positive + nb_CV_true_negative) / sum(nb_CV_confusion_matrix)
}

# Calculate average performance metrics across all folds
mean_accuracy = mean(nb_CV_accuracy)
mean_precision = mean(nb_CV_precision)
mean_recall = mean(nb_CV_recall)
mean_f1_score = mean(nb_CV_f1_score)

# Output performance metrics
nb_CV_results = data.frame(
  Metric = c("nb_CV Recall","nb_CV Precision", "nb_CV F1 Score","nb_CV Accuracy"),
  Value = c(mean_recall,mean_precision, mean_f1_score, mean_accuracy)
)

print(nb_CV_results)
##############################################################################################################
# now let's check which of these 2 models are better!
##############################################################################################################
Naive_Bayes_results = data.frame(Metric = c('Recall' , 'Precission' , 'F1-Score' , 'Accuracy'),
                                         Naive_Bayes = c( 0.74 , 0.54 ,0.61 ,0.71),
                                         NB_Cross_Validation = c(0.63 , 0.64 , 0.63 , 0.75))


print(Naive_Bayes_results)


Confusion_Matrix_nb = data.frame(class = c('Non-Diabetic' , 'Diabetic'), 'NB-Non-Diabetic' = c(75 , 19) , 'NB-Diabetic' = c(24 , 35),
                              'NB_CV-Non-Diabetic' = c(35 , 5) , 'NB_CV-Diabetic' = c(8 , 14))

print(Confusion_Matrix_nb)

##############################################################################################################
# Naive Bayes model ROC Curve.
##############################################################################################################
NB_CV_roc_data = data.frame(prob = nb_CV_pred, label = nb_CV_test$Outcome)
NB_roc_data = data.frame(prob = nb_pred_test, label = test$Outcome)

# Sort the data frame by the predicted probabilities in descending order
NB_CV_roc_data = CV_roc_data[order(- NB_CV_roc_data$prob), ]
NB_roc_data = LR_roc_data[order(- NB_roc_data$prob), ]

# Calculate the true positive rate (sensitivity) and the false positive rate (1 - specificity)
NB_tpr = cumsum(NB_roc_data$label) / sum(NB_roc_data$label)
NB_fpr = cumsum(!NB_roc_data$label) / sum(!NB_roc_data$label)


NB_CV_tpr = cumsum(NB_CV_roc_data$label) / sum(NB_CV_roc_data$label)
NB_CV_fpr = cumsum(!NB_CV_roc_data$label) / sum(!NB_CV_roc_data$label)

# Plot the ROC curve
par(mfrow = c(1,1))
plot(NB_fpr, NB_tpr, type = "l", main = "ROC Curve", xlab = "False Positive Rate", ylab = "True Positive Rate" , col = 'red')
lines(NB_CV_fpr, NB_CV_tpr, type = "l", main = "ROC Curve", xlab = "False Positive Rate", ylab = "True Positive Rate" , col = 'blue')

# Add a diagonal line representing random guessing
abline(0, 1, col = "gray", lty = 2)


NB_CV_auc_roc = round(auc(NB_CV_fpr, NB_CV_tpr), 3)
NB_auc_roc = round(auc(NB_fpr, NB_tpr), 3)


legend_labels = c(paste("CV_AUC =", NB_CV_auc_roc),
                  paste("NB_AUC =", NB_auc_roc))

legend("bottomright", legend = legend_labels, bty = "n", col = c('red', 'blue'), lty = 1)
##############################################################################################################
# Let's fit the LDA model for our dataset.
##############################################################################################################
# Fit LDA model
LDA_model = lda(Outcome ~ ., data = train)

# Make predictions on test set
LDA_pred_train = predict( LDA_model, newdata = train)$class
LDA_pred_test = predict( LDA_model, newdata = test)$class

# Calculate confusion matrix
LDA_confusion_matrix_train = table( LDA_pred_train, train$Outcome)
LDA_true_positive_train = LDA_confusion_matrix_train[2,2]
LDA_false_positive_train = LDA_confusion_matrix_train[1,2]
LDA_false_negative_train = LDA_confusion_matrix_train[2,1]
LDA_true_negative_train = LDA_confusion_matrix_train[1,1]




LDA_confusion_matrix_test = table( LDA_pred_test, test$Outcome)
LDA_true_positive_test = LDA_confusion_matrix_test[2,2]
LDA_false_positive_test = LDA_confusion_matrix_test[1,2]
LDA_false_negative_test = LDA_confusion_matrix_test[2,1]
LDA_true_negative_test = LDA_confusion_matrix_test[1,1]

# Calculate performance metrics
LDA_accuracy_train = (LDA_true_positive_train + LDA_true_negative_train) / sum(LDA_confusion_matrix_train)
LDA_precision_train = LDA_true_positive_train / (LDA_true_positive_train + LDA_false_positive_train)
LDA_recall_train = LDA_true_positive_train / (LDA_true_positive_train + LDA_false_negative_train)
LDA_f1_score_train = 2 * LDA_precision_train * LDA_recall_train / (LDA_precision_train + LDA_recall_train)



LDA_accuracy_test = (LDA_true_positive_test + LDA_true_negative_test) / sum(LDA_confusion_matrix_test)
LDA_precision_test = LDA_true_positive_test / (LDA_true_positive_test + LDA_false_positive_test)
LDA_recall_test = LDA_true_positive_test / (LDA_true_positive_test + LDA_false_negative_test)
LDA_f1_score_test = 2 * LDA_precision_test * LDA_recall_test / (LDA_precision_test + LDA_recall_test)

LDA_results_train = data.frame(
  Metric = c("Recall","Precision", "F1 Score","Accuracy"),
  Value = c(LDA_recall_train,LDA_precision_train, LDA_f1_score_train, LDA_accuracy_train),
  set = 'train'
)

LDA_results_test = data.frame(
  Metric = c("Recall", "Precision","F1 Score","Accuracy"),
  Value = c(LDA_recall_test, LDA_precision_test,LDA_f1_score_test, LDA_accuracy_test),
  set = 'test'
)

print(LDA_results_train)
print(LDA_results_test)

###################################################################################################
# Apply cross validation on LDA model.
###################################################################################################
# Set the number of folds for cross-validation
k = 10

# Split the data into k folds
set.seed(123)
LDA_folds = cut(seq(1, nrow(train)), breaks = k, labels = FALSE)

LDA_CV_test_index = sample(nrow(Diabetes), 0.2 * nrow(Diabetes))
LDA_CV_train = Diabetes[-LDA_CV_test_index, ]
LDA_CV_test = Diabetes[LDA_CV_test_index, ]

LDA_CV_accuracy_test = rep(0, k)
LDA_CV_precision_test = rep(0, k)
LDA_CV_recall_test = rep(0, k)
LDA_CV_f1_score_test = rep(0, k)

# Perform k-fold cross-validation
for (i in 1:k) {
  # Subset the data for the i-th fold
  LDA_test_indices = which(LDA_folds == i)
  LDA_train_subset = LDA_CV_train[-LDA_test_indices, ]
  LDA_test_subset = LDA_CV_train[LDA_test_indices, ]
  
  # Fit the LDA model on the training subset
  LDA_CV_model = lda(Outcome ~ ., data = LDA_train_subset)
  
  
  # Make predictions on the test subset
  LDA_CV_pred_test = predict(LDA_CV_model, newdata = LDA_test_subset)$class
  
  # Calculate confusion matrix and performance metrics for test subset
  LDA_CV_confusion_matrix_test = table(LDA_CV_pred_test, LDA_test_subset$Outcome)
  LDA_CV_true_positive_test = LDA_CV_confusion_matrix_test[2, 2]
  LDA_CV_false_positive_test = LDA_CV_confusion_matrix_test[1, 2]
  LDA_CV_false_negative_test = LDA_CV_confusion_matrix_test[2, 1]
  LDA_CV_true_negative_test = LDA_CV_confusion_matrix_test[1, 1]
  
  
  LDA_CV_accuracy_test[i] = (LDA_CV_true_positive_test + LDA_CV_true_negative_test) / sum(LDA_CV_confusion_matrix_test)
  LDA_CV_precision_test[i] = LDA_CV_true_positive_test / (LDA_CV_true_positive_test + LDA_CV_false_positive_test)
  LDA_CV_recall_test[i] = LDA_CV_true_positive_test / (LDA_CV_true_positive_test + LDA_CV_false_negative_test)
  LDA_CV_f1_score_test[i] = 2 * LDA_CV_precision_test[i] * LDA_CV_recall_test[i] / (LDA_CV_precision_test[i] + LDA_CV_recall_test[i])
}

# Calculate average performance metrics across all folds
LDA_CV_mean_accuracy = mean(LDA_CV_accuracy_test)
LDA_CV_mean_precision = mean(LDA_CV_precision_test)
LDA_CV_mean_recall = mean(LDA_CV_recall_test)
LDA_CV_mean_f1_score = mean(LDA_CV_f1_score_test)

# Output performance metrics
LDA_CV_results = data.frame(
  Metric = c("CV Recall","CV Precision", "CV F1 Score","CV Accuracy"),
  Value = c(LDA_CV_mean_recall,LDA_CV_mean_precision,LDA_CV_mean_f1_score,LDA_CV_mean_accuracy),
  Set = "Test"
)

print(LDA_CV_results)

###############################################################################################################
#Now let's check which of these 2 models are the best one!
###############################################################################################################
LDA_results = data.frame(Metric = c('Recall' , 'Precission' , 'F1-Score' , 'Accuracy'),
                                         LDA = c( 0.75 , 0.56 ,0.61 ,0.76  ),
                                         LDA_Cross_Validation = c(0.68 , 0.56 , 0.61 , 0.76))


print(LDA_results)


Confusion_Matrix_LDA = data.frame(class = c('Non-Diabetic' , 'Diabetic'), 'LDA-Non-Diabetic' = c(43 , 10) , 'LDA-Diabetic' = c(28 , 31),
                              'LDA_CV-Non-Diabetic' = c(36 , 4) , 'LDA_CV-Diabetic' = c(8 , 14))

print(Confusion_Matrix_LDA)

##############################################################################################################
# LDA model PLOTS.
##############################################################################################################
plot(LDA_model)
plot(LDA_model, type="density")


######################################################################################################################
# Let's fit the QDA model for our dataset.
######################################################################################################################
# Fit QDA model
QDA_model = qda(Outcome ~ ., data = train)

# Make predictions on test set
QDA_pred_train = predict(QDA_model, newdata = train)$class
QDA_pred_test = predict(QDA_model, newdata = test)$class

# Calculate confusion matrix
QDA_confusion_matrix_train = table( QDA_pred_train, train$Outcome)
QDA_true_positive_train = QDA_confusion_matrix_train[2,2]
QDA_false_positive_train = QDA_confusion_matrix_train[1,2]
QDA_false_negative_train = QDA_confusion_matrix_train[2,1]
QDA_true_negative_train = QDA_confusion_matrix_train[1,1]




QDA_confusion_matrix_test = table( QDA_pred_test, test$Outcome)
QDA_true_positive_test = QDA_confusion_matrix_test[2,2]
QDA_false_positive_test = QDA_confusion_matrix_test[1,2]
QDA_false_negative_test = QDA_confusion_matrix_test[2,1]
QDA_true_negative_test = QDA_confusion_matrix_test[1,1]

# Calculate performance metrics
QDA_accuracy_train = (QDA_true_positive_train + QDA_true_negative_train) / sum(QDA_confusion_matrix_train)
QDA_precision_train = LDA_true_positive_train / (QDA_true_positive_train + QDA_false_positive_train)
QDA_recall_train = QDA_true_positive_train / (QDA_true_positive_train + QDA_false_negative_train)
QDA_f1_score_train = 2 * QDA_precision_train * QDA_recall_train / (QDA_precision_train + QDA_recall_train)



QDA_accuracy_test = (QDA_true_positive_test + QDA_true_negative_test) / sum(QDA_confusion_matrix_test)
QDA_precision_test = QDA_true_positive_test / (QDA_true_positive_test + QDA_false_positive_test)
QDA_recall_test = QDA_true_positive_test / (QDA_true_positive_test + QDA_false_negative_test)
QDA_f1_score_test = 2 * QDA_precision_test * QDA_recall_test / (QDA_precision_test + QDA_recall_test)

QDA_results_train = data.frame(
  Metric = c("Recall","Precision", "F1 Score","Accuracy"),
  Value = c(QDA_recall_train, QDA_precision_train, QDA_f1_score_train, QDA_accuracy_train),
  set = 'train'
)

QDA_results_test = data.frame(
  Metric = c("Recall", "Precision","F1 Score","Accuracy"),
  Value = c(QDA_recall_test, QDA_precision_test, QDA_f1_score_test, QDA_accuracy_test),
  set = 'test'
)

print(QDA_results_train)
print(QDA_results_test)


################################################################################################################
# Apply cross validation on QDA model.
################################################################################################################
# Set the number of folds for cross-validation
k = 10

# Split the data into k folds
set.seed(123)
QDA_folds = cut(seq(1, nrow(train)), breaks = k, labels = FALSE)

QDA_CV_test_index = sample(nrow(Diabetes), 0.2 * nrow(Diabetes))
QDA_CV_train = Diabetes[-QDA_CV_test_index, ]
QDA_CV_test = Diabetes[QDA_CV_test_index, ]

QDA_CV_accuracy_test = rep(0, k)
QDA_CV_precision_test = rep(0, k)
QDA_CV_recall_test = rep(0, k)
QDA_CV_f1_score_test = rep(0, k)

# Perform k-fold cross-validation
for (i in 1:k) {
  # Subset the data for the i-th fold
  QDA_test_indices = which(QDA_folds == i)
  QDA_train_subset = QDA_CV_train[-QDA_test_indices, ]
  QDA_test_subset = QDA_CV_train[QDA_test_indices, ]
  
  # Fit the LDA model on the training subset
  QDA_CV_model = qda(Outcome ~ ., data = QDA_train_subset)
  
  
  # Make predictions on the test subset
  QDA_CV_pred_test = predict(QDA_CV_model, newdata = QDA_test_subset)$class
  
  # Calculate confusion matrix and performance metrics for test subset
  QDA_CV_confusion_matrix_test = table(QDA_CV_pred_test, QDA_test_subset$Outcome)
  QDA_CV_true_positive_test = QDA_CV_confusion_matrix_test[2, 2]
  QDA_CV_false_positive_test = QDA_CV_confusion_matrix_test[1, 2]
  QDA_CV_false_negative_test = QDA_CV_confusion_matrix_test[2, 1]
  QDA_CV_true_negative_test = QDA_CV_confusion_matrix_test[1, 1]
  
  
  QDA_CV_accuracy_test[i] = (QDA_CV_true_positive_test + QDA_CV_true_negative_test) / sum(QDA_CV_confusion_matrix_test)
  QDA_CV_precision_test[i] = QDA_CV_true_positive_test / (QDA_CV_true_positive_test + QDA_CV_false_positive_test)
  QDA_CV_recall_test[i] = QDA_CV_true_positive_test / (QDA_CV_true_positive_test + QDA_CV_false_negative_test)
  QDA_CV_f1_score_test[i] = 2 * QDA_CV_precision_test[i] * QDA_CV_recall_test[i] / (QDA_CV_precision_test[i] + QDA_CV_recall_test[i])
}

# Calculate average performance metrics across all folds
# Calculate average performance metrics across all folds
QDA_CV_mean_accuracy = mean(QDA_CV_accuracy_test)
QDA_CV_mean_precision = mean(QDA_CV_precision_test)
QDA_CV_mean_recall = mean(QDA_CV_recall_test)
QDA_CV_mean_f1_score = mean(QDA_CV_f1_score_test)

# Output performance metrics
QDA_CV_results = data.frame(
  Metric = c("CV Recall","CV Precision", "CV F1 Score","CV Accuracy"),
  Value = c(QDA_CV_mean_recall,QDA_CV_mean_precision, QDA_CV_mean_f1_score, QDA_CV_mean_accuracy),
  Set = "Test"
)

print(QDA_CV_results)

#############################################################################################################
# Now let's check which of these 2 models are better.
#############################################################################################################
QDA_results = data.frame(Metric = c('Recall' , 'Precission' , 'F1-Score' , 'Accuracy'),
                         QDA = c( 0.72 , 0.52 ,0.6 ,0.73),
                         QDA_Cross_Validation = c(0.65 , 0.54 , 0.59 , 0.75))


print(QDA_results)


Confusion_Matrix_QDA = data.frame(class = c('Non-Diabetic' , 'Diabetic'), 'QDA-Non-Diabetic' = c(82 , 12) , 'QDA-Diabetic' = c(28 , 31),
                                  'QDA_CV-Non-Diabetic' = c(35 , 5) , 'CV-Diabetic' = c(9 , 13))

print(Confusion_Matrix_QDA)

#########################################################################################################################################
roc_obj = roc(test_data$Outcome, qda_pred_prob$Yes)
plot(roc_obj)



#########################################################################################################################################
# Comparing different models
#########################################################################################################################################
results_df <- data.frame(
  Model = c('Logistic Regression with cross validation', 'KNN', 'Naive Bayes with cross validation', 'LDA' , 'QDA'),
  Recall = c(0.68, 0.63, 0.63, 0.75, 0.72),
  F1_Score = c(0.58, 0.50, 0.64, 0.56, 0.52),
  Precision = c(0.62, 0.56, 0.63, 0.61, 0.6),
  Accuracy = c(0.76, 0.71, 0.75, 0.76, 0.73)
)

# Reshape data into long format
results_long <- reshape2::melt(results_df, id.vars = "Model", variable.name = "Metric")
my_colors <- c("#66CDAA", "#79CDCD", "#9AC0CD", "#A2B5CD")
# Plot the results
ggplot(results_long, aes(x = Model, y = value, fill = Metric)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(y = "Model", x = "Score", fill = "Metric") +
  ggtitle("Model Performance Comparison") +
  theme(plot.title = element_text(hjust = 0.5))+
  scale_fill_manual(values = my_colors)


#From the chart, we can see that Logistic Regression has the highest Accuracy, Recall,and
#F1 Score,while KNN and Naive Bayes have relatively lower Accuracy, Recall, and F1 Score. 
#However, KNN has the highest Precision.





