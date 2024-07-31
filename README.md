# Examining the US Housing Market between 2003 and 2022
*Note:*
*If you are visiting from my CV, please reference `./classwork/Final-Report.Rmd` file to see my code and writing submissions or view the word document for the final report.*

*The report uses different data than what is listed in this repository. I have done my best to recreate/improve upon the orginal data set using Kaggle. I will be working on separating the code to make the overall project more digestible.*

*Apologies for the incomplete README, I am in the process of improving upon the work in this repository!*

<p align="center">
  <img src = "./assets/hpi-ffr.png">
</p>

## Table of Contents
1. [Introduction](#introduction)
2. [Data](#data)
3. [Exploration](#exploration)
5. [Modeling](#modeling)
6. [Dashboarding](#dashboarding)

## Introduction

The current U.S. housing market is valued at 47 trillion USD (Rosen, 2023). Home ownership is a widespread aspiration, contributing to the ongoing growth of this market. Investors and economists rely on the Housing Price Index (HPI) as a crucial metric for assessing economic conditions. The HPI is a comprehensive macroeconomic measure that carefully monitors the price fluctuations of single-family homes nationwide. It also serves as an analytical tool for approximating the changes in the rates of mortgage defaults, prepayments, and housing affordability (Liberto, 2023). The Federal Housing Finance Agency compiles this data by reviewing single-family housing mortgages purchased or securitized by Fannie Mac or Freddie Mac. In this repo, I attempt to recreate the data set I used for the original project, organize, remake, and improve my modeling and visualizations, and elaborate on my learnings below. 

## Data

To recreate the data set I used for my final project, I used two data sets of hpi influences from kaggle. By comparing the values of variables between the two data sets and looking at the documentation of each, I chose only the variables whose values I could verify as accurate. I left joined on the each individual csv file on `house_data.csv`, because it only went back until 2003. The St. Louis Federal Reserve only reports real GDP on a quarterly basis, meaning the GDP column had missing values for 8 out of the 12 months for each year. I decided to use seasonal decomposition to impute the intermediate values. In this method, the time series is decomposed into its trend, and seasonal components; then, the intermediate values are imputed using only the trend; finally, the seasonality is added back into the data. This allows for imputed values that smoothly follow the trend of the time series while also adhering to any seasonality present. After renaming the remaining variables for ease of use, I extracted the month and year from the date features to use in my analyses before saving the data frame to `./data/modelling/` as `modelling.csv`. 

## Exploration

The raw code for my data exploration can be found in `./eda/viz-raw.R`. This R file contains the code for all visualization I have made/will make. Eventually, I would like to compartmentalize the code into separate notebooks, which each follow a specific narrative to highlight a particular part of the data. One of the first visualizations I created is shown below:

<p align="center">
  <img src = "./assets/vars-by-year.png">
</p>

The above chart illustrates each numeric variable's trend over the dates in the data set. The next visualization I made was the following:

<p align="center">
  <img src = "./assets/gdp-imputed.png">
</p>

The above chart illustrates the quarterly gdp and 

## Modeling

## Dashboarding
<p align="center">
  <img src = "./assets/shiny.gif">
</p>
