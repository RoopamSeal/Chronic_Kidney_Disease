# Clinical Data Integrity & Predictive Analytics Pipeline (CKD)

## Project Overview

This is an end-to-end clinical data analysis project, transforming "noisy" Electronic Health Record (EHR) data into a validated, analysis-ready dataset and a basic predictive machine learning tool for learning puposes only.

## Core Features

* **Automated ETL Pipeline**: Programmatically cleans clinical data containing range-based strings (e.g., "138-143"), encoding errors, and inequality markers (≥, <) using R and Regex.
* **Data Integrity Interrogation**: Implements a formal "Data Quality Gate" using the `pointblank` package to audit records against physiological guardrails (e.g., Sodium and Hemoglobin limits).
* **Predictive Modeling**: Utilizes a Random Forest classifier to predict Chronic Kidney Disease (CKD) with **92% Accuracy** and **96% Sensitivity** Further modifications can be done by hyperparameter tuning of the model.
* **Interactive Deployment**: A live **R Shiny** application allows clinicians to input patient lab results and receive real-time risk assessments.

## Technical Stack

* **Language**: R
* **Data Engineering**: `tidyverse`, `janitor`
* **Validation & Auditing**: `validate`, `pointblank`
* **Machine Learning**: `caret`, `randomForest`
* **Deployment**: `Shiny`, `shinyapps.io`
