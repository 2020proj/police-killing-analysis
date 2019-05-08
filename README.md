# Police Killing Analysis

This is the code for the project for analyzing Police Killing in US in 2015.

- `EDA.ipynb` and `EDA.r` are used for the Exploratory Data Analysis. The share the same content despite their different formats. 
- `main_code.rmd` file contains the models for our analysis
- `Summary.pdf` is the one-page summary for our project. 
- `Report.pdf` is the final report for the project, with a detailed description for our project. 


# Abstract
This project investigates the question of whether police exercise bias in their treatment of African Americans.
Specifcally, we attempt to answer the question of whether being an African American versus another race
makes you more likely to be killed by the police under identical scenarios. The primary challenge that we faced
and that historical analyses have faced is related to the available data, as the data that exists only captures
people that were killed, and not the people that were not killed under similar circumstances. Through
exploratory analyses and hypothesis testing, we convincingly exhibit how African Americans are killed at
greater rates than people of other races. However, when we employ more comprehensive Generalized Linear
Models and a Zero-Infated Poisson Model to account for all confounding factors, we are only able to fnd mild
evidence suggesting that explicitly being an African American makes you more likely to be killed than others.
