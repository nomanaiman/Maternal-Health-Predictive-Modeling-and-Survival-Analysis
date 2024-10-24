# Step 1: Prepare the dataset for SMOTE
wm_data_filtered_rf <- wm_data_filtered %>%
  select(
    ANC4_any,          # Outcome variable
    windex5,              # Wealth index quintiles
    education_level,      # Education level
    water_access,         # Access to water (binary)
    sanitation_access,    # Access to sanitation (binary)
    HH6,                  # Urban/Rural classification
    wmweight              # Sample weight
  ) %>%
  mutate(
    ANC4_any = as.factor(ANC4_any),  # Convert ANC4_any to factor for SMOTE
    windex5 = as.numeric(as.factor(windex5)),
    education_level = as.numeric(as.factor(education_level)),
    HH6 = as.numeric(as.factor(HH6)),
    water_access = as.numeric(water_access),
    sanitation_access = as.numeric(sanitation_access)
  )

# Step 2: Split the data into training and test sets
set.seed(123)
data_split <- initial_split(wm_data_filtered_rf, prop = 0.75)
train_data <- training(data_split)
test_data <- testing(data_split)

# Step 3: Apply SMOTE for data balancing
smote_recipe <- recipe(ANC4_any ~ ., data = train_data) %>%
  step_smote(ANC4_any, over_ratio = 1)

# Prepare the recipe
smote_prepped <- prep(smote_recipe)

# Apply SMOTE and get the balanced training data
train_data_smote <- bake(smote_prepped, new_data = NULL)

# Step 4: Fit the Random Forest Model after SMOTE
rf_model_smote <- randomForest(
  ANC4_any ~ . - wmweight,   # Exclude the sample weight as a predictor
  data = train_data_smote,
  ntree = 500,
  importance = TRUE
)

# Step 5: Predict and Evaluate the Model
predicted_prob_rf_smote <- predict(rf_model_smote, test_data, type = "prob")[, 2]
predicted_class_rf_smote <- ifelse(predicted_prob_rf_smote > 0.5, "Yes", "No")

# Confusion Matrix
conf_matrix_rf_smote <- confusionMatrix(as.factor(predicted_class_rf_smote), as.factor(test_data$ANC4_any))
print(conf_matrix_rf_smote)

# Step 6: ROC Curve and AUC
roc_rf_smote <- roc(test_data$ANC4_any, predicted_prob_rf_smote)
plot(roc_rf_smote, main = "ROC Curve for Random Forest (SMOTE)")
auc_rf_smote <- auc(roc_rf_smote)
cat("AUC for Random Forest (SMOTE): ", auc_rf_smote, "\n")

# Step 7: Feature Importance for the SMOTE-augmented model
importance_rf_smote <- importance(rf_model_smote)
varImpPlot(rf_model_smote, main = "Variable Importance for Random Forest Model (SMOTE)")

# Step 8: Performance Metrics
accuracy_rf_smote <- conf_matrix_rf_smote$overall['Accuracy']
precision_rf_smote <- conf_matrix_rf_smote$byClass['Pos Pred Value']  # Precision
recall_rf_smote <- conf_matrix_rf_smote$byClass['Sensitivity']        # Recall
f1_score_rf_smote <- 2 * ((precision_rf_smote * recall_rf_smote) / (precision_rf_smote + recall_rf_smote))  # F1 score

# Output performance metrics
cat("Random Forest Model (SMOTE) - Accuracy: ", accuracy_rf_smote, "\n")
cat("Random Forest Model (SMOTE) - Precision: ", precision_rf_smote, "\n")
cat("Random Forest Model (SMOTE) - Recall: ", recall_rf_smote, "\n")
cat("Random Forest Model (SMOTE) - F1 Score: ", f1_score_rf_smote, "\n")
