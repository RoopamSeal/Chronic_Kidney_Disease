install.packages("janitor")
install.packages("validate")
install.packages("pointblank")

library(tidyverse)  # For data manipulation
library(janitor)    # For cleaning column names
library(validate)   # For defining data quality rules
library(pointblank) # For creating data quality reports


clinical_headers <- names(read_csv("ckd-dataset-v2.csv", n_max = 0))

raw_ckd <- read_csv("ckd-dataset-v2.csv", 
                    skip = 3, 
                    col_names = clinical_headers) %>%
  clean_names()

clean_clinical_string <- function(x) {
  if (is.na(x) || x == "" || x == " ") return(NA)
  
  x <- gsub("â‰¥|≥|>", "", x)
  x <- gsub("<", "", x)
  
  if (grepl("-", x)) {
    nums <- suppressWarnings(as.numeric(unlist(strsplit(x, "-"))))
    
    if (any(is.na(nums))) return(NA) 
    
    return(mean(nums, na.rm = TRUE))
  }
  
  return(suppressWarnings(as.numeric(x)))
}
  

clean_ckd <- raw_ckd %>%
  mutate(across(c(sg, al, sod, su, bgr, bu, sc, pot, hemo, pcv, rbcc, wbcc, age, grf), 
                ~ sapply(.x, clean_clinical_string))) %>%
  mutate(affected = as.factor(affected),
         class = as.factor(class))

ckd_rules <- validator(
  valid_sod  = sod >= 100 & sod <= 160,    
  valid_hemo = hemo >= 5 & hemo <= 20,     
  valid_age  = age >= 0 & age <= 120,      
  not_null_class = !is.na(class)
)

integrity_check <- confront(clean_ckd, ckd_rules)
summary(integrity_check)


query_log <- violating(clean_ckd, integrity_check)

# Save the final cleaned dataset and the audit log
write_csv(clean_ckd, "CKD_Cleaned_Analysis_Data.csv")
write_csv(query_log, "CKD_Clinical_Query_Log.csv")

print("Pipeline execution complete. Cleaned data and Query Log generated.")


agent <- create_agent(tbl = clean_ckd, label = "Full CKD Study Data Integrity Audit") %>%
  col_is_numeric(
    columns = vars(bp_diastolic, bp_limit, sg, al, su, bgr, bu, sod, sc, pot, hemo, pcv, rbcc, wbcc, grf, age)
  ) %>%
  col_is_factor(
    columns = vars(class, rbc, pc, pcc, ba, htn, dm, cad, appet, pe, ane, stage, affected)
  ) %>%
  col_vals_not_null(
    columns = vars(class, age, affected, bp_diastolic)
  ) %>%
  col_vals_between(vars(sod), left = 100, right = 160, na_pass = TRUE) %>%
  col_vals_between(vars(hemo), left = 5, right = 20, na_pass = TRUE) %>%
  
  col_vals_between(vars(age), left = 0, right = 120, na_pass = TRUE) %>%
  
  col_vals_between(vars(sc), left = 0.1, right = 15, na_pass = TRUE) %>%
  
  col_vals_between(vars(pot), left = 2.0, right = 10.0, na_pass = TRUE) %>%
  
  col_vals_in_set(vars(class), set = c("ckd", "notckd")) %>%
  
  col_vals_in_set(vars(affected), set = c("0", "1")) %>%
  
  interrogate()

export_report(agent, filename = "Data_Quality_Report.html")


install.packages("caret")
install.packages("randomForest")
library(caret)
library(randomForest)


set.seed(123)
train_index <- createDataPartition(clean_ckd$class, p = 0.8, list = FALSE)
train_data <- clean_ckd[train_index, ]
test_data  <- clean_ckd[-train_index, ]

rf_model <- randomForest(class ~ hemo + sod + sc + age + bp_diastolic, 
                         data = train_data, 
                         na.action = na.roughfix) # Handles the 1% of NAs we identified

predictions <- predict(rf_model, test_data)
confusionMatrix(predictions, test_data$class)



importance_matrix <- importance(rf_model)

importance_df <- data.frame(
  Feature = rownames(importance_matrix),
  Importance = importance_matrix[, 1]
) %>%
  arrange(desc(Importance))

ggplot(importance_df, aes(x = reorder(Feature, Importance), y = Importance, fill = Importance)) +
  geom_bar(stat = "identity") +
  coord_flip() + # Flip for better readability of feature names
  scale_fill_gradient(low = "skyblue", high = "darkblue") +
  labs(
    title = "Clinical Feature Importance",
    subtitle = "Predicting CKD based on validated lab markers",
    x = "Clinical Marker",
    y = "Importance (Mean Decrease Gini)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")


saveRDS(rf_model, "ckd_model.rds")
