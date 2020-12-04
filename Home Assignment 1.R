##### Home assignment 1, 2, and 3 #####
# Name: Dhitya Prasetya Dian Nugraha
# Course: Advanced Scientific Methods in Psychology
# Teacher: Zoltan Kekecs

##### Essential R Packages for Home Assignment #####
# Activating R Packages before doing the analysis

install.packages("tidyverse")
library(tidyverse)

install.packages("psych")
library(psych)

install.packages("gridExtra")
library(gridExtra)

install.packages("lm.beta")
library(lm.beta)

install.packages("car")
library(car)

install.packages("lmtest")
library(lmtest)

install.packages("sandwich")
library(sandwich)

install.packages("boot")
library(boot)

install.packages("lmboot")
library(lmboot)

install.packages("cAIC4")
library(cAIC4)

install.packages("r2glmm")
library(r2glmm)

install.packages("lme4")
library(lme4)

install.packages("lmerTest")
library(lmerTest)

install.packages("MuMIn")
library(MuMIn)

install.packages("optimx")
library(optimx)

# Custom functions for creating the final table of Regression Coefficients

coef_table = function(model){	
  require(lm.beta)	
  mod_sum = summary(model)	
  mod_sum_p_values = as.character(round(mod_sum$coefficients[,4], 3))		
  mod_sum_p_values[mod_sum_p_values != "0" & mod_sum_p_values != "1"] = substr(mod_sum_p_values[mod_sum_p_values != "0" & mod_sum_p_values != "1"], 2, nchar(mod_sum_p_values[mod_sum_p_values != "0" & mod_sum_p_values != "1"]))		
  mod_sum_p_values[mod_sum_p_values == "0"] = "<.001"		
  
  
  mod_sum_table = cbind(as.data.frame(round(cbind(coef(model), confint(model), c(0, lm.beta(model)$standardized.coefficients[c(2:length(model$coefficients))])), 2)), mod_sum_p_values)		
  names(mod_sum_table) = c("b", "95%CI lb", "95%CI ub", "Std.Beta", "p-value")		
  mod_sum_table["(Intercept)","Std.Beta"] = "0"		
  return(mod_sum_table)	
}

# Custom function for checking the visualization error predictions

error_plotter <- function(mod, col = "black", x_var = NULL){	
  mod_vars = as.character(mod$call[2])	
  data = as.data.frame(eval(parse(text = as.character(mod$call[3]))))	
  y = substr(mod_vars, 1, as.numeric(gregexpr(pattern ='~',mod_vars))-2)	
  x = substr(mod_vars, as.numeric(gregexpr(pattern ='~',mod_vars))+2, nchar(mod_vars))	
  
  data$pred = predict(mod)	
  
  if(x == "1" & is.null(x_var)){x = "response_ID"	
  data$response_ID = 1:nrow(data)} else if(x == "1"){x = x_var}	
  
  plot(data[,y] ~ data[,x], ylab = y, xlab = x)	
  abline(mod)	
  
  for(i in 1:nrow(data)){	
    clip(min(data[,x]), max(data[,x]), min(data[i,c(y,"pred")]), max(data[i,c(y,"pred")]))	
    abline(v = data[i,x], lty = 2, col = col)	
  }	
  
}

# Customise function for Std.Beta coefficients from Linear Mixed Models

stdCoef.merMod <- function(object) {	
  sdy <- sd(getME(object,"y"))	
  sdx <- apply(getME(object,"X"), 2, sd)	
  sc <- fixef(object)*sdx/sdy	
  se.fixef <- coef(summary(object))[,"Std. Error"]	
  se <- se.fixef*sdx/sdy	
  return(data.frame(stdcoef=sc, stdse=se))	
}

##### Home Assignment 1 #####
# Step 1 Data Exploration

data_sample_1 = read.csv("https://tinyurl.com/ha-dataset1") # Original Home Assignment Dataset

View(data_sample_1) # View data_sample_1 into a new window

str(data_sample_1) # Checking the structure of the data_sample_1 dataset

# Dataset Variables

# Pain level variable using a numerical scale 0-10 (Interval)
# State Trait Anxiety Inventory (STAI) variable using a numerical scale 20-80 (Numerical)
# Pain Catastrophizing Scale variable using a numerical scale 0-52 (Interval)
# Mindfulness Attention Awareness Scale variable using a numerical scale 1-6 (Numerical)
# Cortisol variable contains from Blood serum (Numerical) and Saliva (Numerical) variables
# Participants Weight in Kg (Numerical)
# Participants IQ (Interval)
# Participants household income in USD (Interval)
# Participants Age (Interval)
# Participants Sex (Character)

##### Exploring and Checking the Original Dataset for Descriptive Statistics #####

summary(data_sample_1) # Checking the Min, Median, Mean, and Max for all variables in the original dataset 

describe(data_sample_1)

# Checking for potential errors in the dataset
# There are several variables (Age, STAI Trait, and Household Income) that have an extreme response and might be a potential error
# Therefore, I have decided to change the error values into NA values and I will remove it from the dataset

data_sample_1_corrected_Age_STAI_Household <- data_sample_1 %>% 
  mutate(age = replace(age, age == "444", NA), STAI_trait = replace(
    STAI_trait, STAI_trait == "3.9", NA), household_income = replace(
      household_income, household_income == "-3732", NA))

# Updating the Original Dataset by correcting the potential error

data_sample_1_Updated <- data_sample_1_corrected_Age_STAI_Household # Creating the extreme response from the Variables (Age, STAI Trait, and Household Income) into a Not Available (NA) cases for each variables

data_sample_1_Updated_No_NA <- na.omit(data_sample_1_Updated) # Removing the NA cases from the new object dataset

summary(data_sample_1_Updated_No_NA)

View(data_sample_1_Updated_No_NA) # View the updated version of the original dataset into a new window

# Before creating the regression model, I have decided to check and explore more descriptive statistics
# Reporting descriptive statistics between categorical variable (Sex) and all of the continuous variables

# Reporting descriptive statistics and histogram distribution between categorical variable (Sex) and continuous variable (Pain)

data_sample_1_Updated_No_NA %>% select(sex, pain) %>% group_by(sex) %>% summarize(mean = mean(pain),
                                                                                  sd = sd(pain)) # Mean Sex Female = 5.02 and SD = 1.49, and Mean Sex Male = 5.04 and SD = 1.67 

data_sample_1_Updated_No_NA %>% select(sex, pain) %>% ggplot() +
  aes(x = sex, y = pain, fill = sex) +
  geom_violin() +
  geom_jitter(width = 0.1) # Visualizing the mean distribution using the ggplot function and based on the figure there is no extreme cases 

# Reporting descriptive statistics and histogram distribution between categorical variable (Sex) and continuous variable (Age)

data_sample_1_Updated_No_NA %>% select(sex, age) %>% group_by(sex) %>% summarize(mean = mean(age),
                                                                                 sd = sd(age)) # Mean Sex Female = 40.4 and SD = 4.86, and Mean Sex Male = 39.5 and SD = 5.27 

data_sample_1_Updated_No_NA %>% select(sex, age) %>% ggplot() +
  aes(x = sex, y = age, fill = sex) +
  geom_violin() +
  geom_jitter(width = 0.1) # Visualizing the mean distribution using the ggplot function and based on the figure there is no extreme cases

# Reporting descriptive statistics and histogram distribution between categorical variable (Sex) and continuous variable (STAI Trait)

data_sample_1_Updated_No_NA %>% select(sex, STAI_trait) %>% group_by(sex) %>% summarize(mean = mean(STAI_trait),
                                                                                        sd = sd(STAI_trait)) # Mean Sex Female = 41.6 and SD = 4.49, and Mean Sex Male = 38.5 and SD = 5.37 

data_sample_1_Updated_No_NA %>% select(sex, STAI_trait) %>% ggplot() +
  aes(x = sex, y = STAI_trait, fill = sex) +
  geom_violin() +
  geom_jitter(width = 0.1) # Visualizing the mean distribution using the ggplot function and based on the figure there is no extreme cases

# Reporting descriptive statistics and histogram distribution between categorical variable (Sex) and continuous variable (Pain Catastrophizing)

data_sample_1_Updated_No_NA %>% select(sex, pain_cat) %>% group_by(sex) %>% summarize(mean = mean(pain_cat),
                                                                                      sd = sd(pain_cat)) # Mean Sex Female = 30.3 and SD = 4.55, and Mean Sex Male = 29.5 and SD = 5.59 

data_sample_1_Updated_No_NA %>% select(sex, pain_cat) %>% ggplot() +
  aes(x = sex, y = pain_cat, fill = sex) +
  geom_violin() +
  geom_jitter(width = 0.1) # Visualizing the mean distribution using the ggplot function and based on the figure there is no extreme cases

# Reporting descriptive statistics and histogram distribution between categorical variable (Sex) and continuous variable (Cortisol Serum)

data_sample_1_Updated_No_NA %>% select(sex, cortisol_serum) %>% group_by(sex) %>% summarize(mean = mean(cortisol_serum),
                                                                                            sd = sd(cortisol_serum)) # Mean Sex Female = 5.24 and SD = 0.900, and Mean Sex Male = 4.84 and SD = 1.13 

data_sample_1_Updated_No_NA %>% select(sex, cortisol_serum) %>% ggplot() +
  aes(x = sex, y = cortisol_serum, fill = sex) +
  geom_violin() +
  geom_jitter(width = 0.1) # Visualizing the mean distribution using the ggplot function and based on the figure there is no extreme cases

# Reporting descriptive statistics and histogram distribution between categorical variable (Sex) and continuous variable (Cortisol Saliva)

data_sample_1_Updated_No_NA %>% select(sex, cortisol_saliva) %>% group_by(sex) %>% summarize(mean = mean(cortisol_saliva),
                                                                                             sd = sd(cortisol_saliva)) # Mean Sex Female = 5.22 and SD = 0.908, and Mean Sex Male = 4.90 and SD = 1.11 
data_sample_1_Updated_No_NA %>% select(sex, cortisol_saliva) %>% ggplot() +
  aes(x = sex, y = cortisol_saliva, fill = sex) +
  geom_violin() +
  geom_jitter(width = 0.1) # Visualizing the mean distribution using the ggplot function and based on the figure there is no extreme cases

# Reporting descriptive statistics and histogram distribution between categorical variable (Sex) and continuous variable (Mindfulness)

data_sample_1_Updated_No_NA %>% select(sex, mindfulness) %>% group_by(sex) %>% summarize(mean = mean(mindfulness),
                                                                                         sd = sd(mindfulness)) # Mean Sex Female = 2.84 and SD = 0.833, and Mean Sex Male = 2.97 and SD = 1.02 
data_sample_1_Updated_No_NA %>% select(sex, mindfulness) %>% ggplot() +
  aes(x = sex, y = mindfulness, fill = sex) +
  geom_violin() +
  geom_jitter(width = 0.1) # Visualizing the mean distribution using the ggplot function and based on the figure there is no extreme cases

# Reporting descriptive statistics and histogram distribution between categorical variable (Sex) and continuous variable (Weight)

data_sample_1_Updated_No_NA %>% select(sex, weight) %>% group_by(sex) %>% summarize(mean = mean(weight),
                                                                                    sd = sd(weight)) # Mean Sex Female = 69.0 and SD = 9.55, and Mean Sex Male = 68.8 and SD = 11.5 
data_sample_1_Updated_No_NA %>% select(sex, weight) %>% ggplot() +
  aes(x = sex, y = weight, fill = sex) +
  geom_violin() +
  geom_jitter(width = 0.1) # Visualizing the mean distribution using the ggplot function and based on the figure there is no extreme cases

# Reporting descriptive statistics and histogram distribution between categorical variable (Sex) and continuous variable (IQ)

data_sample_1_Updated_No_NA %>% select(sex, IQ) %>% group_by(sex) %>% summarize(mean = mean(IQ),
                                                                                sd = sd(IQ)) # Mean Sex Female = 101 and SD = 16.0, and Mean Sex Male = 100 and SD = 16.6 
data_sample_1_Updated_No_NA %>% select(sex, IQ) %>% ggplot() +
  aes(x = sex, y = IQ, fill = sex) +
  geom_violin() +
  geom_jitter(width = 0.1) # Visualizing the mean distribution using the ggplot function and based on the figure there is no extreme cases

# Reporting descriptive statistics and histogram distribution between categorical variable (Sex) and continuous variable (Household Income)

data_sample_1_Updated_No_NA %>% select(sex, household_income) %>% group_by(sex) %>% summarize(mean = mean(household_income),
                                                                                              sd = sd(household_income)) # Mean Sex Female = 71191 and SD = 25237, and Mean Sex Male = 66913 and SD = 19434 
data_sample_1_Updated_No_NA %>% select(sex, household_income) %>% ggplot() +
  aes(x = sex, y =household_income, fill = sex) +
  geom_violin() +
  geom_jitter(width = 0.1) # Visualizing the mean distribution using the ggplot function and based on the figure there is no extreme cases

# Reporting the distribution of categorical variable (Sex) using bar chart

data_sample_1_Updated_No_NA %>% 
  ggplot() +
  aes(x = sex, fill = sex) +
  geom_bar() # Displaying the composition of Sex in the updated dataset using Bar Chart

# Reporting the distribution of all continuous variables (Pain, Age, STAI, Pain Catastrophizing, Mindfulness, Cortisol Blood serum, Cortisol Saliva, Weight, IQ, and Household income)

# Histogram distribution of continuous variable (Pain) = Approximately Normally Distributed

data_sample_1_Updated_No_NA %>% ggplot() +
  aes(x = pain) +
  geom_histogram(bins = 30) # There is an approximately normal distribution for Pain level variable

# Histogram distribution of continuous variable (Age) = Positive Skew Distributed

data_sample_1_Updated_No_NA %>% ggplot() +
  aes(x = age) +
  geom_histogram(bins = 30) # There is an approximately normal distribution for Age variable

# Histogram distribution of continuous variable (STAI_Trait) = Approximately Negative Skew Distributed

data_sample_1_Updated_No_NA %>% ggplot() +
  aes(x = STAI_trait) +
  geom_histogram(bins = 30) # There is an approximately normal distribution for STAI Trait variable

# Histogram distribution of continuous variable (Pain Catastrophizing) = Approximately Normally Distributed

data_sample_1_Updated_No_NA %>% ggplot() +
  aes(x = pain_cat) +
  geom_histogram(bins = 30) # There is an approximately normal distribution for Pain Catastrophizing variable

# Histogram distribution of continuous variable (Cortisol Serum) = Approximately Normally Distributed

data_sample_1_Updated_No_NA %>% ggplot() +
  aes(x = cortisol_serum) +
  geom_histogram(bins = 30) # There is an approximately normal distribution for Cortisol Serum variable

# Histogram distribution of continuous variable (Cortisol Saliva) = Approximately Normally Distributed

data_sample_1_Updated_No_NA %>% ggplot() +
  aes(x = cortisol_saliva) +
  geom_histogram(bins = 30) # There is an approximately normal distribution for Cortisol Saliva variable

# Histogram distribution of continuous variable (Mindfulness) = Approximately Normally Distributed

data_sample_1_Updated_No_NA %>% ggplot() +
  aes(x = mindfulness) +
  geom_histogram(bins = 30) # There is an approximately normal distribution for Mindfulness variable

# Histogram distribution of continuous variable (Weight) = Approximately Normally Distributed

data_sample_1_Updated_No_NA %>% ggplot() +
  aes(x = weight) +
  geom_histogram(bins = 30) # There is an approximately normal distribution for Weight variable

# Histogram distribution of continuous variable (IQ) = Approximately Normally Distributed

data_sample_1_Updated_No_NA %>% ggplot() +
  aes(x = IQ) +
  geom_histogram(bins = 30) # There is an approximately normal distribution for IQ variable

# Histogram Distribution of continuous variable (Household Income) = Approximately Normally Distributed

data_sample_1_Updated_No_NA %>% ggplot() +
  aes(x = household_income) +
  geom_histogram(bins = 30) # There is an approximately normal distribution for Household Income variable

# Checking for Outliers in the original dataset (Before removing the NA cases)

data_sample_1 %>% mutate(rownum = row.names(data_sample_1)) %>% ggplot() +
  aes(x = pain, y = age, label = rownum) +
  geom_label() # Checking for Outliers using ggplot between Pain and Age Variables and it seems that Participant ID 93 might be a potential outlier

data_sample_1 %>% mutate(rownum = row.names(data_sample_1)) %>% ggplot() +
  aes(x = pain, y = STAI_trait, label = rownum) +
  geom_label() # Checking for Outliers using ggplot between Pain and STAI Trait Variables and it seems that Participant ID 150 might be a potential outlier

data_sample_1 %>% mutate(rownum = row.names(data_sample_1)) %>% ggplot() +
  aes(x = pain, y = household_income, label = rownum) +
  geom_label() # Checking for Outliers using ggplot between Pain and Household Income Variables and it seems that Participant 109 might be a potential outlier

data_sample_1 %>% ggplot() +
  aes(x = pain, y = age) +
  geom_point() +
  geom_smooth(method = "lm") # Checking for Outliers using Scatterplot between Pain and Age Variables

data_sample_1 %>% ggplot() +
  aes(x = pain, y = STAI_trait) +
  geom_point() +
  geom_smooth(method = "lm") # Checking for Outliers using Scatterplot between Pain and STAI Trait Variables

data_sample_1 %>% ggplot() +
  aes(x = pain, y = household_income) +
  geom_point() +
  geom_smooth(method = "lm") # Checking for Outliers using Scatterplot between Pain and Household Income Variables

# Histogram distribution of continuous variables (Age), (STAI Trait), and (Household Income) old and corrected versions

data_sample_1_old_plot_Age <- data_sample_1 %>%
  ggplot() +
  aes(x = age) +
  geom_histogram()

data_sample_1_old_plot_Age # Histogram distribution before removing the error in Age variable

data_sample_1_corrected_Age_new_plot <- data_sample_1_corrected_Age_STAI %>% 
  ggplot() +
  aes(x = age) +
  geom_histogram()

data_sample_1_corrected_Age_new_plot # Histogram distribution after removing the error in Age variable

data_sample_1_old_plot_STAI_Trait <- data_sample_1 %>%
  ggplot() +
  aes(x = STAI_trait) +
  geom_histogram()

data_sample_1_old_plot_STAI_Trait # Histogram distribution before removing the error in STAI Trait variable

data_sample_1_Corrected_STAI_Trait_new_plot <- data_sample_1_corrected_Age_STAI %>% 
  ggplot() +
  aes(x = STAI_trait) +
  geom_histogram()

data_sample_1_Corrected_STAI_Trait_new_plot # Histogram distribution after removing the error in STAI Trait variable

data_sample_1_old_plot_household_income <- data_sample_1 %>% 
  ggplot() +
  aes(x = household_income) +
  geom_histogram()

data_sample_1_old_plot_household_income # Histogram distribution before removing the error in Household Income variable

data_sample_1_corrected_Age_STAI_Household_new_plot <- data_sample_1_corrected_Age_STAI_Household %>% 
  ggplot() +
  aes(x = household_income) +
  geom_histogram()

data_sample_1_corrected_Age_STAI_Household_new_plot # Histogram distribution after removing the error in Household income variable

# Comparing the old and new plot of Histogram distribution for Age, STAI Trait, and Household Income

grid.arrange(data_sample_1_old_plot_Age,
             data_sample_1_corrected_Age_new_plot, ncol = 2) # Comparing the histogram distribution (Old and New) in Age Variable

grid.arrange(data_sample_1_old_plot_STAI_Trait,
             data_sample_1_Corrected_STAI_Trait_new_plot, ncol = 2) # Comparing the histogram distribution (Old and New) in STAI Trait Variable

grid.arrange(data_sample_1_old_plot_household_income,
             data_sample_1_corrected_Age_STAI_Household_new_plot, ncol = 2) # Comparing the histogram distribution (Old and New) in Household Income Variable

# Reporting the Correlation for all continuous variables

# Reporting the correlation between variables (Pain Level) and (Age)

data_sample_1_Updated_No_NA %>% select(pain, age) %>% 
  drop_na() %>% cor() # Correlation = -0.254

# Reporting the correlation between variables (Pain Level) and (STAI Trait)

data_sample_1_Updated_No_NA %>% select(pain, STAI_trait) %>% 
  drop_na() %>% cor() # Correlation = 0.403

# Reporting the correlation between variables (Pain Level) and (Pain Catastrophizing)

data_sample_1_Updated_No_NA %>% select(pain, pain_cat) %>% 
  drop_na() %>% cor() # Correlation = 0.598

# Reporting the correlation between variables (Pain Level) and (Cortisol Serum)

data_sample_1_Updated_No_NA %>% select(pain, cortisol_serum) %>% 
  drop_na() %>% cor() # Correlation = 0.541

# Reporting the correlation between variables (Pain Level) and (Cortisol Saliva)

data_sample_1_Updated_No_NA %>% select(pain, cortisol_saliva) %>% 
  drop_na() %>% cor() # Correlation = 0.549

# Reporting the correlation between variables (Pain Level) and (Mindfulness)

data_sample_1_Updated_No_NA %>% select(pain, mindfulness) %>% 
  drop_na() %>% cor() # Correlation = -0.436

# Reporting the correlation between variables (Pain Level) and (Weight)

data_sample_1_Updated_No_NA %>% select(pain, weight) %>% 
  drop_na() %>% cor() # Correlation = 0.025

# Reporting the correlation between variables (Pain Level) and (IQ)

data_sample_1_Updated_No_NA %>% select(pain, IQ) %>% 
  drop_na() %>% cor() # Correlation = 0.025

# Reporting the correlation between variables (Pain Level) and (Household Income)

data_sample_1_Updated_No_NA %>% select(pain, household_income) %>% 
  drop_na() %>% cor() # Correlation = -0.100

# Reporting the scatter plot visualization between variables (Pain) and (Age)

data_sample_1_Updated_No_NA %>% select(pain, age) %>% 
  drop_na() %>% ggplot() +
  aes(x = pain, y = age) +
  geom_point() +
  geom_smooth()

# Reporting the scatter plot visualization between variables (Pain) and (STAI Trait)

data_sample_1_Updated_No_NA %>% select(pain, STAI_trait) %>% 
  drop_na() %>% ggplot() +
  aes(x = pain, y = STAI_trait) +
  geom_point() +
  geom_smooth()

# Reporting the scatter plot visualization between variables (Pain) and (Pain Catastrophizing)

data_sample_1_Updated_No_NA %>% select(pain, pain_cat) %>% 
  drop_na() %>% ggplot() +
  aes(x = pain, y = pain_cat) +
  geom_point() +
  geom_smooth()

# Reporting the scatter plot visualization between variables (Pain) and (Cortisol Serum)

data_sample_1_Updated_No_NA %>% select(pain, cortisol_serum) %>% 
  drop_na() %>% ggplot() +
  aes(x = pain, y = cortisol_serum) +
  geom_point() +
  geom_smooth()

# Reporting the scatter plot visualization between variables (Pain) and (Cortisol Saliva)

data_sample_1_Updated_No_NA %>% select(pain, cortisol_saliva) %>% 
  drop_na() %>% ggplot() +
  aes(x = pain, y = cortisol_saliva) +
  geom_point() +
  geom_smooth()

# Reporting the scatter plot visualization between variables (Pain) and (Mindfulness)

data_sample_1_Updated_No_NA %>% select(pain, mindfulness) %>% 
  drop_na() %>% ggplot() +
  aes(x = pain, y = mindfulness) +
  geom_point() +
  geom_smooth()

# Reporting the scatter plot visualization between variables (Pain) and (Weight)

data_sample_1_Updated_No_NA %>% select(pain, weight) %>% 
  drop_na() %>% ggplot() +
  aes(x = pain, y = weight) +
  geom_point() +
  geom_smooth()

# Reporting the scatter plot visualization between variables (Pain) and (IQ)

data_sample_1_Updated_No_NA %>% select(pain, IQ) %>% 
  drop_na() %>% ggplot() +
  aes(x = pain, y = IQ) +
  geom_point() +
  geom_smooth()

# Reporting the scatter plot visualization between variables (Pain) and (Household Income)

data_sample_1_Updated_No_NA %>% select(pain, household_income) %>% 
  drop_na() %>% ggplot() +
  aes(x = pain, y = household_income) +
  geom_point() +
  geom_smooth()

# Reporting Pearson correlation test for all continuous variables

# Reporting Pearson correlation test between (Pain) and (Age) variables

data_sample_1_Updated_No_NA %>% ggplot() +
  aes(x = pain, y = age) +
  geom_point() +
  geom_smooth(method = "lm")

correlation_test_Pain_Age = cor.test(data_sample_1_Updated_No_NA$pain,
                                     data_sample_1_Updated_No_NA$age)

correlation_test_Pain_Age # Pearson Correlation = -0.254, t = -3.277, df = 155, p = .001, 95% CI = Lower boundary (-0.395) and (-0.102) Upper boundary

# Reporting Pearson correlation test between (Pain) and (STAI Trait) variables

data_sample_1_Updated_No_NA %>% ggplot() +
  aes(x = pain, y = STAI_trait) +
  geom_point() +
  geom_smooth(method = "lm")

correlation_test_Pain_STAI = cor.test(data_sample_1_Updated_No_NA$pain,
                                      data_sample_1_Updated_No_NA$STAI_trait)

correlation_test_Pain_STAI # Pearson correlation = 0.403, t = 5.482, df = 155, p = 1.667, 95% CI = Lower boundary (0.262) and (0.526) Upper boundary

# Reporting Pearson correlation test between (Pain) and (Pain Catastrophizing) variables

data_sample_1_Updated_No_NA %>% ggplot() +
  aes(x = pain, y = pain_cat) +
  geom_point() +
  geom_smooth(method = "lm")

correlation_test_Pain_Pain_Catastrophizing = cor.test(data_sample_1_Updated_No_NA$pain,
                                                      data_sample_1_Updated_No_NA$pain_cat)

correlation_test_Pain_Pain_Catastrophizing # Pearson correlation = 0.598, t = 9.294, df = 155, p < .001, 95% CI = Lower boundary (0.487) and (0.690) Upper boundary

# Reporting Pearson correlation test between (Pain) and (Cortisol Serum) variables

data_sample_1_Updated_No_NA %>% ggplot() +
  aes(x = pain, y = cortisol_serum) +
  geom_point() +
  geom_smooth(method = "lm")

correlation_test_Pain_Cortisol_Serum = cor.test(data_sample_1_Updated_No_NA$pain,
                                                data_sample_1_Updated_No_NA$cortisol_serum)

correlation_test_Pain_Cortisol_Serum # Pearson correlation = 0.541, t = 8.022, df = 155, p = 2.379, 95% CI = Lower boundary (0.420) and (0.643) Upper boundary

# Reporting Pearson correlation test between (Pain) and (Cortisol Saliva) variables

data_sample_1_Updated_No_NA %>% ggplot() +
  aes(x = pain, y = cortisol_saliva) +
  geom_point() +
  geom_smooth(method = "lm")

correlation_test_Pain_Cortisol_Saliva = cor.test(data_sample_1_Updated_No_NA$pain,
                                                 data_sample_1_Updated_No_NA$cortisol_saliva)

correlation_test_Pain_Cortisol_Saliva # Pearson correlation = 0.549, t = 8.185, df = 155, p = 9.298, 95% CI = Lower boundary (0.429) and (0.650) Upper boundary

# Reporting Pearson correlation test between (Pain) and (Mindfulness) variables

data_sample_1_Updated_No_NA %>% ggplot() +
  aes(x = pain, y = mindfulness) +
  geom_point() +
  geom_smooth(method = "lm")

correlation_test_Pain_Mindfulness = cor.test(data_sample_1_Updated_No_NA$pain,
                                             data_sample_1_Updated_No_NA$mindfulness)

correlation_test_Pain_Mindfulness # Pearson correlation = -0.436, t = -6.039, df = 155, p = 1.102, 95% CI = Lower boundary (-0.555) and (-0.300) Upper boundary

# Reporting Pearson correlation test between (Pain) and (Weight) variables

data_sample_1_Updated_No_NA %>% ggplot() +
  aes(x = pain, y = weight) +
  geom_point() +
  geom_smooth(method = "lm")

correlation_test_Pain_Weight = cor.test(data_sample_1_Updated_No_NA$pain,
                                        data_sample_1_Updated_No_NA$weight)

correlation_test_Pain_Weight # Pearson correlation = 0.025, t = 0.322, df = 155, p = .747, 95% CI = Lower boundary (-0.131) and (0.181) Upper boundary

# Reporting Pearson correlation test between (Pain) and (IQ) variables

data_sample_1_Updated_No_NA %>% ggplot() +
  aes(x = pain, y = IQ) +
  geom_point() +
  geom_smooth(method = "lm")

correlation_test_Pain_IQ = cor.test(data_sample_1_Updated_No_NA$pain,
                                    data_sample_1_Updated_No_NA$IQ)

correlation_test_Pain_IQ # Pearson correlation = 0.025, t = 0.317, p = .751, 95% CI = Lower boundary (-0.131) and (0.181) Upper boundary

# Reporting Pearson correlation test between (Pain) and (Household Income) variables

data_sample_1_Updated_No_NA %>% ggplot() +
  aes(x = pain, y = household_income) +
  geom_point() +
  geom_smooth(method = "lm")

correlation_test_Pain_Household_Income = cor.test(data_sample_1_Updated_No_NA$pain,
                                                  data_sample_1_Updated_No_NA$household_income)

correlation_test_Pain_Household_Income # Pearson correlation = -0.100, t = -1.255, df = 155, p =.211, 95% CI = Lower boundary (-0.253) and (0.057) Upper boundary

##### Home Assignment 1 Part 1 #####
# Step 2: Building the Regression Model

Linear_M1_No_NA <- lm(pain ~ age + sex, data = data_sample_1_Updated_No_NA) # Creating Multiple Linear Regression Model 1 by using Age and Sex as predictors in the Original Dataset (Excluding NA cases)

Linear_M1_No_NA # Linear Regression Model 1

summary(Linear_M1_No_NA) # F(2, 154) = 5.358, p = .005, Adj.R^2 = 0.052

# Regression Equation for Multiple Linear Regression Model 1 X1 (Age) and X2 (Sex)

# Y = 8.231 + (-0.079)*(X1_age) + (-0.049)*(X2_sex)

AIC(Linear_M1_No_NA) # AIC = 582.931

lm.beta(Linear_M1_No_NA)

# Standardized Coefficients for Multiple Linear Regression Model 1 (Without Outliers)

# Intercept = 0.000
# Age = -0.255
# Sex = -0.015

coef_table(Linear_M1_No_NA) # Summary table for Linear regression model 1

# Beta Intercept = 8.23, 95% CI = Lower boundary (6.27) and (10.19) Upper boundary, Std.Beta = 0, p < .001
# Beta Age = -0.08, 95% CI = Lower boundary (-0.13) and (-0.03) Upper boundary, Std.Beta = -0.26, p = .001
# Beta Sex = -0.05, 95% CI = Lower boundary (-0.54) and (0.44) Upper boundary, Std.Beta = -0.02, p = .841

# Confidence Interval for Multiple Linear Regression Model 1

confint(Linear_M1_No_NA)

# Checking the Assumption for Linear Regression Model 1

# Checking the Normality of Residuals Assumption in Model 1

Linear_M1_No_NA %>% plot(which = 2) # Checking the Normality of Residuals Assumption

describe(residuals(Linear_M1_No_NA)) # Checking the assumption of normality of residuals with the describe function 

# Checking the Linearity Assumption in Model 1 Original Dataset

Linear_M1_No_NA %>% residualPlots() # Tukey Test and the predictor (Age) is not significant = p = .681. Therefore, there is no violation of Linearity Assumption

# Checking the Homoscedasticity Assumption in Model 1 Original Dataset

Linear_M1_No_NA %>% plot(which = 3)

Linear_M1_No_NA %>% ncvTest() # Non-Constant Variance Score Test indicates no violation of Homoscedasticity. Chisquare = 0.914, df = 1, p = .338

Linear_M1_No_NA %>% bptest() # Studentized Breusch-Pagan Test indicates no violation of assumption in Homoscedasticity. BP = 2.036, df = 2, p = .361

# Checking for Multicollinearity Assumption in Model 1 Original Dataset

Linear_M1_No_NA %>% vif() # There is no multicollinearity when there is outliers. Age = 1.007, Sex = 1.007

# Checking for Random Outliers

Linear_M1_No_NA %>% plot(which = 5) # Checking Residuals and Leverage Outliers in Model 1

Linear_M1_No_NA %>% plot(which = 4) # Checking Cook's Distance Outliers in Model 1

# Overall, the assumption of Linear regression model 1 has not been violated

# Step 3: Building the second linear regression model

Linear_M2_No_NA <- lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum + cortisol_saliva, data = data_sample_1_Updated_No_NA) # Creating Multiple Linear Regression Model 2 from Original Dataset (Excluding NA cases)

Linear_M2_No_NA # Linear Regression Model 2

summary(Linear_M2_No_NA) # F(7, 149) = 21.49, p < .001, Adj.R^2 = 0.479

# Regression Equation for  Linear Regression Model 2 X1(Age) and X2(Sex), X3(STAI Trait), X4(Pain Catastrophizing), X5(Mindfulness), X6(Cortisol Serum), X7(Cortisol Saliva)

# Y = 0.678 + -0.016(X1) + 0.241(X2) + -0.036(X3) + 0.128(X4) + -0.245(X5) + 0.232(X6) + 0.405(X7) 

AIC(Linear_M2_No_NA) # AIC = 493.914

lm.beta(Linear_M2_No_NA)

# Confidence Interval  Linear Regression Model 2

confint(Linear_M2_No_NA)

# Standardized Coefficients

# Intercept = 0.000
# Age = -0.054
# Sex = 0.076
# STAI Trait = -0.118
# Pain Catastrophizing = 0.413
# Mindfulness = -0.143
# Cortisol Serum = 0.151
# Cortisol Saliva = 0.261

# Summary Table for Linear Regression Model 2
coef_table(Linear_M2_No_NA)

# Checking the Assumption for Linear Regression Model 2

# Checking for Normality of Residuals Assumption in Model 2 Original Dataset

Linear_M2_No_NA %>% plot(which = 2) # Checking the normality of residuals using QQ plot

describe(residuals(Linear_M2_No_NA)) # Checking the assumption of normality of residuals with the describe function

# Checking for Linearity Assumption in Model 2

Linear_M2_No_NA %>% residualPlots() # Tukey Test and All of the remaining predictors indicate non-significant p values = 0.838

# Checking for Random Outliers

Linear_M2_No_NA %>% plot(which = 5) # Checking Residuals and Leverage Outliers in Model 2

Linear_M2_No_NA %>% plot(which = 4) # Checking Cook's Distance Outliers in Model 2

# Checking the Homoscedasticity Assumption in Model 2 Original Dataset

Linear_M2_No_NA %>% plot(which = 3)

Linear_M2_No_NA %>% ncvTest() # Non-Constant Variance Score Test indicates no violation of Homoscedasticity. Chisquare = 2.609, df = 1, p = .106

Linear_M2_No_NA %>% bptest() # Studentized Breusch-Pagan Test indicates no violation of assumption in Homoscedasticity. BP = 5.913, df = 7, p = .549

# Checking for Multicollinearity Assumption in Model 2 Original Dataset

Linear_M2_No_NA %>% vif() # There is multicollinearity between Cortisol Serum and Saliva. 6.692 (Cortisol Serum) and 7.495 (Cortisol Saliva)

# Dealing with Multicollinearity for Multiple Linear Regression Model 2
# I have decided to remove the Cortisol Saliva variable because the variable has a strong correlation with cortisol serum

Linear_M2_Exclude_Cortisol_Saliva <- lm(pain ~ age + sex + STAI_trait + pain_cat +
                                               mindfulness + cortisol_serum, data = data_sample_1_Updated_No_NA) # Excluding Cortisol Saliva variable

Linear_M2_Exclude_Cortisol_Saliva # Final model of linear regression model 2

# Running Model Diagnostic for the Final Model (Excluding Cortisol Saliva)

Linear_M2_Exclude_Cortisol_Saliva %>% plot(which = 2) # There is no violation of Normality of residuals

describe(residuals(Linear_M2_Exclude_Cortisol_Saliva))

Linear_M2_Exclude_Cortisol_Saliva %>% residualPlots() # Tukey test and all of the predictors indicate a non significant p value = .996

Linear_M2_Exclude_Cortisol_Saliva %>% plot(which = 5) 

Linear_M2_Exclude_Cortisol_Saliva %>% plot(which = 4)

Linear_M2_Exclude_Cortisol_Saliva %>% plot(which = 3)

Linear_M2_Exclude_Cortisol_Saliva %>% ncvTest() # Non-Constant Variance Score Test indicates no violation of Homoscedasticity. Chisquare = 2.684, df = 1, p = .101

Linear_M2_Exclude_Cortisol_Saliva %>% bptest() # Studentized Breusch-Pagan Test indicates no violation of assumption in Homoscedasticity. BP = 5.378, df = 6, p = .496 

Linear_M2_Exclude_Cortisol_Saliva %>% vif() # There is no violation of multicollinearity in all predictors

# Overall there is no violation of assumption in the final linear regression model (Excluding Cortisol Saliva)

summary(Linear_M2_Exclude_Cortisol_Saliva) # F(6, 150) = 24.33, p < .001, Adj.R^2 = 0.472

# Regression Equation for Multiple Linear Regression Model 2 Excluding Cortisol Serum Variable due to Multicollinearity Violation

# Y = 1.855 + (-0.038)*(X1_age) + 0.270*(X2_sex) + (-0.021)*(X3_Stai_trait) + 0.114*(X4_pain_cat) + (-0.282)*(X5_mindfulness) + 0.560*(X6_cortisol_serum)

AIC(Linear_M2_Exclude_Cortisol_Saliva) # AIC = 494.784

# Table format for final Linear Regression Model 2

coef_table(Linear_M2_Exclude_Cortisol_Saliva)

# Step 4: Comparing the Linear Regression Model 1 and Model 2
# Comparing Multiple Linear Regression Model 1 and Model 2

anova(Linear_M1_No_NA, Linear_M2_Exclude_Cortisol_Saliva) # F(4, 150) = 31.682, p < .001

AIC(Linear_M1_No_NA, Linear_M2_Exclude_Cortisol_Saliva)

# Model 2 is better compared to Model 1
# AIC Model 1 = 582.931
# AIC Model 2 = 495.784

##### Adjusted R^2 Comparison for Multiple Linear Regression Model 1 and Model 2 #####

summary(Linear_M1_No_NA)$adj.r.squared # Adj.R^2 = 0.052 (5.2%) Variance Explained
summary(Linear_M2_Exclude_Cortisol_Saliva)$adj.r.squared # Adj.R^2 = 0.472 (47.2%) Variance Explained

# In the first part of the home assignment, I have concluded that the second linear regression model is a better model than the first model
# The reason that I have chosen the final model is because the AIC from model 2 is lower than model 1 which indicate a good fit and the anova comparison shows a significant effect for model 2

##### Home Assignment 2 Original Dataset #####

Home_sample_1 <- read.csv("https://tinyurl.com/ha-dataset1")

View(Home_sample_1)

Home_sample_2 <- read.csv("https://tinyurl.com/ha-dataset2")

View(Home_sample_2)

##### Removing NA cases from the Original Dataset #####

Home_sample_1_Identifying_NA <- Home_sample_1 %>% 
  mutate(age = replace(age, age == "444", NA), STAI_trait = replace(
    STAI_trait, STAI_trait == "3.9", NA), household_income = replace(
      household_income, household_income == "-3732", NA))

Home_sample_1_Removing_NA <- na.omit(Home_sample_1_Identifying_NA)

View(Home_sample_1_Removing_NA)

##### Confirming the Researcher's Initial Model Claim using the following predictors #####

Researchers_Initial_Model <- lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness +
                         cortisol_serum + weight +
                         IQ + household_income, data = Home_sample_1_Removing_NA)
Researchers_Initial_Model

# Regression Equation for Confirming the Researcher's Claim

# Y = 2.221 + -3.866(X1) + 2.580(X2) + -1.657(X3) + 1.117(X4) + -2.807(X5) + 5.597(X6) + -4.722(X7) + 2.908(X8) + -6.070(X9)

summary(Researchers_Initial_Model) # F(9, 147) = 16.52, p < .001, Adj.R^2 = 0.472

AIC(Researchers_Initial_Model) # AIC = 497.754

confint(Researchers_Initial_Model) # Confidence Interval information in each predictors

lm.beta(Researchers_Initial_Model) # Standardized Coefficients information in each predictors

coef_table(Researchers_Initial_Model) # Summary table information for the Researcher Initial Model

# Checking for Outliers in the Researcher's Model

Researchers_Initial_Model %>% plot(which = 5) # Checking the Residuals and Leverage Outliers in Researcher's

Researchers_Initial_Model %>% plot(which = 4) # Checking Cook's Distance Outliers in 

# Checking Normality of Residuals in Backward Regression Model

Researchers_Initial_Model %>% plot(which = 2) # Checking the Normality of Residuals Assumption 

# Checking Linearity Assumption in Backward Model

Researchers_Initial_Model %>% residualPlots()

# Checking Homoscedasticity Assumption

Researchers_Initial_Model %>% plot(which = 3)

Researchers_Initial_Model %>% ncvTest() # Chi-Square = 2.498, df = 1, p = .113

Researchers_Initial_Model %>% bptest() # BP = 11.19, df = 9, p = .262

# Checking Multicollinearity Assumption

Researchers_Initial_Model %>% vif() # No violation of Multicollinearity

##### Reporting the Stepwise Backward Regression Model #####

Researchers_Backward_Model = step(Researchers_Initial_Model, direction = "backward") 
Researchers_Backward_Model

summary(Researchers_Backward_Model) # F(6, 150) = 24.98, p < .001, Adj R^2 = 0.479

# Regression Equation of the Researchers Backward Model

# Y = 1.951 + -4.178*(X1) + 2.828*(X2) + 1.068*(X3) + -2.617*(X4) + 5.203*(X5) + -6.432*(X6)

AIC(Researchers_Backward_Model) # AIC = 492.722
AIC(Researchers_Initial_Model) # AIC = 497.754

anova(Researchers_Backward_Model, Researchers_Initial_Model)

# When performing the Anova comparison between the Researcher Initial Model and Backward Regression Model, the Backward model shows no significant effect

confint(Researchers_Backward_Model)

lm.beta(Researchers_Backward_Model)

coef_table(Researchers_Backward_Model)

##### New Regression Model with the significant predictors #####

New_Regression_Backward_Model <- lm(pain ~ age + 
                             pain_cat + mindfulness + 
                             cortisol_serum, data = Home_sample_1)

summary(New_Regression_Backward_Model) # F(4, 155) = 33.33, p = < .001

AIC(New_Regression_Backward_Model) # AIC = 506.736

lm.beta(New_Regression_Backward_Model)

##### Table format of New Backward Regression Model #####

coef_table(New_Regression_Backward_Model)

##### Comparing the Theory Based Model from Home Assignment 1 and Final Backward Model #####

Theory_Based_Model_A1 <- lm(pain ~ age + sex + STAI_trait + pain_cat +
                                          mindfulness + cortisol_serum, data = data_sample_1_Updated_No_NA) # Excluding Cortisol Saliva variable
summary(Theory_Based_Model_A1)

New_Regression_Backward_Model <- lm(pain ~ age + 
                                      pain_cat + mindfulness + 
                                      cortisol_serum, data = Home_sample_1)
summary(New_Regression_Backward_Model)

AIC(Theory_Based_Model_A1) # AIC = 494.784
AIC(New_Regression_Backward_Model) # AIC = 506.736

# My Theory Based Model is a better fit than the Backward Model from the Researcher's Claim

# Creating predictions on Pain using the regression model of the Backward model and Theory Based Model

Theory_Based_Model_A1_Pred <- predict(Theory_Based_Model_A1, Home_sample_2) # It predicts the pain values for all of the participants from the second datafile 

Backward_Model_Predictions <- predict(New_Regression_Backward_Model, Home_sample_2)

# Predictions of Pain for Theory Based and Backward Models

Theory_Based_Model_A1_Pred

Backward_Model_Predictions

# Calculating the RSS for Theory Based and Backward Models

RSS_Theory_Based_Model = sum((Home_sample_2$pain - Theory_Based_Model_A1_Pred)^2)

RSS_Theory_Based_Model # RSS = 233.986

RSS_Backward_Model = sum((Home_sample_2$pain - Backward_Model_Predictions)^2)

RSS_Backward_Model # RSS = 241.327

# There is less error in the Theory Based Model than the backward model

# Summary Table for the Theory Based and Backward Models

coef_table(Theory_Based_Model_A1)

coef_table(New_Regression_Backward_Model)

##### Home Assignment 3 #####

data_file_3 <- read.csv("https://tinyurl.com/ha-dataset3")

View(data_file_3)

str(data_file_3)

summary(data_file_3)

describe(data_file_3)

data_file_4 <- read.csv("https://tinyurl.com/ha-dataset4")

View(data_file_4)

str(data_file_4)

summary(data_file_4)

describe(data_file_4)

##### Assigning Hospital variable into group factors #####

data_file_3 %>% 
  mutate(hospital = factor(hospital))

##### Checking and Removing Errors from data file 3 #####

data_file_3_No_NA <- data_file_3 %>%  
  mutate(household_income = replace(household_income,
                                                  household_income == "-6994", NA),
                                    sex = replace(sex, sex == "femlae", NA))

data_file_3_No_NA = na.omit(data_file_3_No_NA)

View(data_file_3_No_NA)

describe(data_file_3_No_NA$household_income)

##### Checking the Mean Distribution in Data File 3 #####

data_file_3_No_NA %>% select(sex, age) %>% ggplot() +
  aes(x = sex, y = age, fill = sex) +
  geom_violin() +
  geom_jitter(width = 0.1)

data_file_3_No_NA %>% select(sex, STAI_trait) %>% ggplot() +
  aes(x = sex, y = STAI_trait, fill = sex) +
  geom_violin() +
  geom_jitter(width = 0.1)

data_file_3_No_NA %>% select(sex, pain_cat) %>% ggplot() +
  aes(x = sex, y = pain_cat, fill = sex) +
  geom_violin() +
  geom_jitter(width = 0.1)

data_file_3_No_NA %>% select(sex, cortisol_serum) %>% ggplot() +
  aes(x = sex, y = cortisol_serum, fill = sex) +
  geom_violin() +
  geom_jitter(width = 0.1)

data_file_3_No_NA %>% select(sex, cortisol_saliva) %>% ggplot() +
  aes(x = sex, y = cortisol_saliva, fill = sex) +
  geom_violin() +
  geom_jitter(width = 0.1)

data_file_3_No_NA %>% select(sex, mindfulness) %>% ggplot() +
  aes(x = sex, y = mindfulness, fill = sex) +
  geom_violin() +
  geom_jitter(width = 0.1)

data_file_3_No_NA %>% select(sex, weight) %>% ggplot() +
  aes(x = sex, y = weight, fill = sex) +
  geom_violin() +
  geom_jitter(width = 0.1)

data_file_3_No_NA %>% select(sex, IQ) %>% ggplot() +
  aes(x = sex, y = IQ, fill = sex) +
  geom_violin() +
  geom_jitter(width = 0.1)

data_file_3_No_NA %>% select(sex, household_income) %>% ggplot() +
  aes(x = sex, y = household_income, fill = sex) +
  geom_violin() +
  geom_jitter(width = 0.1)

##### Checking and Removing Errors from data file 4 #####

data_file_4_No_NA <- data_file_4 %>%  
  mutate(household_income = replace(household_income,
                                    household_income == "-23482", NA),
         replace(mindfulness, mindfulness == "6.05", NA))

data_file_4_No_NA = na.omit(data_file_4_No_NA)

View(data_file_4_No_NA)

data_file_4_No_NA %>% 
  mutate(hospital = factor(hospital))

##### Checking the Mean Distribution in Data File 4 #####

data_file_4 %>% select(sex, age) %>% ggplot() +
  aes(x = sex, y = age, fill = sex) +
  geom_violin() +
  geom_jitter(width = 0.1)

data_file_4 %>% select(sex, STAI_trait) %>% ggplot() +
  aes(x = sex, y = STAI_trait, fill = sex) +
  geom_violin() +
  geom_jitter(width = 0.1)

data_file_4 %>% select(sex, cortisol_serum) %>% ggplot() +
  aes(x = sex, y = cortisol_serum, fill = sex) +
  geom_violin() +
  geom_jitter(width = 0.1)

data_file_4 %>% select(sex, cortisol_saliva) %>% ggplot() +
  aes(x = sex, y = cortisol_saliva, fill = sex) +
  geom_violin() +
  geom_jitter(width = 0.1)

data_file_4 %>% select(sex, mindfulness) %>% ggplot() +
  aes(x = sex, y = mindfulness, fill = sex) +
  geom_violin() +
  geom_jitter(width = 0.1)

data_file_4 %>% select(sex, weight) %>% ggplot() +
  aes(x = sex, y = weight, fill = sex) +
  geom_violin() +
  geom_jitter(width = 0.1)

data_file_4 %>% select(sex, IQ) %>% ggplot() +
  aes(x = sex, y = IQ, fill = sex) +
  geom_violin() +
  geom_jitter(width = 0.1)

data_file_4 %>% select(sex, household_income) %>% ggplot() +
  aes(x = sex, y = household_income, fill = sex) +
  geom_violin() +
  geom_jitter(width = 0.1)

##### Home Assignment 3 Part 1 #####
# Comparing the Random Intercept Model and the Final Regression Model in Assignment 1

Final_Linear_Model_A1 <- lm(pain ~ age + sex + STAI_trait + pain_cat +
                                             mindfulness + cortisol_serum,
                                           data = data_file_3_No_NA)
summary(Final_Linear_Model_A1)

confint(Final_Linear_Model_A1)

confint(Linear_Mixed_Model_Random_Intercept)

coef_table(Final_Linear_Model_A1)

##### Building the Random Intercept Model #####

Linear_Mixed_Model_Random_Intercept <- lmer(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + 
                                              cortisol_serum + (1|hospital),
                                            data = data_file_3_No_NA, REML = FALSE)
Linear_Mixed_Model_Random_Intercept

##### Regression Equation Random Intercept Model #####

# Y = 4.550 + -0.080(X1) + 0.306(X2) + 0.014(X3) + 0.042(X4) + -0.217(X5) + 0.513(X6)

summary(Linear_Mixed_Model_Random_Intercept)

confint(Linear_Mixed_Model_Random_Intercept)

##### Comparing Model Coefficients in Assignment 3 and Assignment 1 #####

coef_table(Final_Linear_Model_A1)
coef_table(Linear_M2_Exclude_Cortisol_Saliva)

##### Home Assignment 3 Part 2 #####

##### Computing the Variance Explained by the model on data file 3 #####

cAIC(Linear_Mixed_Model_Random_Intercept)
cAIC(Linear_Mixed_Model_Random_Intercept)$caic # cAIC = 646.753 

##### Comparing Marginal R^2 and Conditional R^2 for Random Intercept Models #####

r2beta(Linear_Mixed_Model_Random_Intercept, method = "nsj", data = data_file_3_No_NA)

# CI Model = Lower boundary = 0.294 and Upper boundary = 0.483
# CI Cortisol Serum = Lower boundary = 0.041 and Upper boundary = 0.198
# CI Age = Lower boundary = 0.021 and Upper boundary = 0.159
# CI Mindfulness = Lower boundary = 0.000 and Upper boundary = 0.071
# CI Sex = Lower boundary = 0.000 and Upper boundary = 0.062
# CI Pain Catastrophizing = 0.000 and Upper boundary = 0.060
# CI STAI Trait = Lower boundary 0.000 and Upper boundary = 0.033

r.squaredGLMM(Linear_Mixed_Model_Random_Intercept) # Marginal R^2 = 0.378 and Conditional R^2 = 0.486
# Conditional R^2 takes into account for the random effects and makes the model to be more concrete 

##### Predicting Pain from Random Intercept Model from data file 4 #####

Random_Intercept_Model_Pred <- predict(Linear_Mixed_Model_Random_Intercept, data_file_4_No_NA, allow.new.levels = TRUE)
Random_Intercept_Model_Pred

Linear_Mixed_Model_Mean <- lm(pain ~ 1, data = data_file_4_No_NA)
Linear_Mixed_Model_Mean

Random_Intercept_Model_Mean <- predict(Linear_Mixed_Model_Mean, data_file_4_No_NA, allow.new.levels = TRUE)
Random_Intercept_Model_Mean

##### Computing the RSS and TSS to obtain the Variance Explained #####

RSS_Linear_Mixed_Model = sum((data_file_4_No_NA$pain - Random_Intercept_Model_Pred)^2)
RSS_Linear_Mixed_Model # 339.114

TSS_Linear_Mixed_Model = sum((data_file_4_No_NA$pain - Random_Intercept_Model_Mean)^2)
TSS_Linear_Mixed_Model # 434.994

##### Variance Explained (R^2) of Pain from data file 4 #####

R.Squared_Linear_Mixed_Model = 1-(RSS_Linear_Mixed_Model/TSS_Linear_Mixed_Model)
R.Squared_Linear_Mixed_Model # 0.220 (22%)

##### Home Assignment 3 Part 3 #####

##### Creating a new linear Model with Random Slope and Intercept Models #####
summary(Linear_Mixed_Model_Random_Intercept) # Based on the summary function, the most influential predictor is the Cortisol Serum

Linear_Mixed_Slope = lmer(pain ~ cortisol_serum  + (cortisol_serum|hospital), data = data_file_3_No_NA) 
Linear_Mixed_Slope

##### Visualizing the Fitted Regression Lines for each Hospital Separately #####

data_file_3 = data_file_3_No_NA %>% 
  mutate(prediction_slope = predict(Linear_Mixed_Slope))

##### Building the Random Intercept and Slope Models Plot #####
Random_Intercept_Model_Plot = data_file_3 %>% ggplot() +
  aes(x = cortisol_serum, y = pain, group = hospital) +
  geom_point(aes(color = hospital), size = 4) +
  geom_line(color = "red", aes(x = cortisol_serum, y = prediction_slope)) +
  facet_wrap(~ hospital, ncol = 2)

Random_Intercept_Model_Plot # Final Model with separated fitted regression lines