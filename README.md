# H1B-Case-Status-Prediction
This repo consists of the scripts and report for the project 'H1B Case Status Prediction' which was a part of my coursework in DS 502: 'Statistical Methods for Data Science' at WPI.

# Getting Started
These instructions will get you a copy of the project up and running on your local machine for development and testing purposes.

## Prerequesites
This project has been developed in R using RStudio, and the following packages were used:
* readr - To read .csv and .xlsx files
* ggplot2 - For visualization and exploratory analysis
* dplyr - For exploratory analysis
* dummies - For one-hot encoding for KNN
* class - To import the KNN library
* randomForest - To perform Random Forest
* caret - For aesthetically appealing confusion matrices
* Matrix - To build sparse matrices for XGBoost
* xgboost - To perform XGBoost
* MASS - For scaling of data
* ROSE - To perform oversampling or undersampling to tackle class imbalance
* rpart - To perform recursive partitioning


## Installing
You can find the datasets required for the scripts in this repo over [here](https://drive.google.com/open?id=1PAzQtZJvMx3QbzURiCBhaOs409A_MTLN)
* H-1B_Disclosure_Data_FY17.xlsx contains the H1B case status data for the year 2017, the dataset that has been used for this project.
* cleaned2017.csv contains the cleaned version of the above dataset.
* train.csv, test.csv contain the training and testing datasets used for Random Forest.

The compiled code can be found in [Compiled_Code.r](https://github.com/mihinsumaria/H1B-Case-Status-Prediction/blob/master/Compiled_Code.r)
Download "H-1B_Disclosure_Data_FY17.xlsx" to your local system, and on line 2 change the path of read_csv to that of the file you just downloaded.

You can install any of the packages above in the following manner:
```
install.packages('PACKAGE_NAME_HERE')
```

## Built with
* R
* R Studio - IDE
* Excel - Exploratory Analysis, Basic Pre-Processing, Pivot Tables
* Turing Cluster at WPI - Used for high-performance computing required for Random Forest and KNN. More information can be found (here)[http://arc.wpi.edu/resources/hardware/hpc-clusters/]

## Project Report
A comprehensive report of what this project entails can be found (here)[https://github.com/mihinsumaria/H1B-Case-Status-Prediction/blob/master/DS%20502%20Report.pdf].

## Authors
* Mihin Sumaria
* Mihir Sawant
* Janvi Kothari
* Rushikesh Naidu
* Jinal Jain

## Who do I talk to?
For any questions send an email at mssumaria@wpi.edu



