# Examining-MLB-Statcast-Data-to-Identify-Undervalued-Players-Using-R
### Jordan Rivera
### Summer 2021

## Project Description:
##### Using R, analyzed Statcast data from the 2019 MLB season with the goal of identifying undervalued players who would be ideal, cost-effective, and low risk targets for acquisition
##### Performed cluster analysis (k-means, non-probabilistic) in R to group players into 4 clusters based on their Statcast metrics. Identified the cluster representing the top tier of hitters and filtered the players in the cluster to identify those with statistical profiles similar to star players yet were more inexpensive and underrated
##### Performed principal components analysis to reduce the number of variables from 41 variables to 10 principal components
##### Built a model using principal components regression that was used to predict WAR values for players based on their metrics
##### Developed a general statistical strategy for identifying undervalued players and used analysis to specifically name 12 undervalued players who could be smart, low risk, and high upside acquisitions


## Project Files:
### Rivera-8949.R
##### R script for project. Imports data set and performs cluster analysis, PC analysis, PC regression, MLE estimation
### Rivera-8949.csv
##### Data set for this project. Created manually by downloading and combining tables from Baseball-Reference and Baseball Savant
### Examining MLB Statcast Data to Identify Undervalued Players Using R
##### Research Paper detailing the project. Includes introduction, description of data set, analyses, results, conclusions. Identifies some MLB players who could be considered undervalued.
