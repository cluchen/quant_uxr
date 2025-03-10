# -*- coding: utf-8 -*-
"""chap5.ipynb

Automatically generated by Colab.

Original file is located at
    https://colab.research.google.com/drive/1n65Owyxnl8tFgnj5SXTcmIbQkfyw3Vn5

**Prompt:** imagine that a product manager (PM) presents you with a data set that includes observations of 50,000 users' behaviors for 20 product features in a mobile application. A subset of 2000 users have given a rating of their satisfaction with the product on a 5-point ordinal scale. Among the 20 feature usage observations, 18 of the variables are binary (usage of the feautre or not) while 2 are continuous (number of sessions and total time in the application by the user during the observation). Finally, there is a variable with assignment of users into three groups labeled "casual" users, "professiona" users, and "disinterested" users.

1. What else would you want to know about the dataset?
"""

# is this across all user groups (countries, etc.)?
# when was this collected?
# how was this collected? was this 2000 a random sample? in-app pop-up survey?
# are there any missing data?
# what are the 20 features being tracked? why those 20 features?

"""2. What steps would you take to determine the quality of the data?"""

# i would check for N/As
# i would check for duplicate records
# i would check for the data distribution/descriptive statistics

"""### Loading Dataset"""

#load the datasets statistics-dat1.csv and statistics-dat2.csv from https://quantuxbook.com/data

from google.colab import drive
drive.mount('/content/drive')

import pandas as pd
df_1 = pd.read_csv('/content/drive/MyDrive/Quant UXR/statistics-dat1.csv')
df_2 = pd.read_csv('/content/drive/MyDrive/Quant UXR/statistics-dat2.csv')

#check if mounted well

df_1.head(10)
df_2.head(10)

"""###Checking quality of data"""

#check for NAs

missing_values_1 = df_1.isnull().sum()
print(missing_values_1)
#no missing values for df1

missing_values_2 = df_2.isnull().sum()
print(missing_values_2)
#no missing values for df2

#check for duplicate rows

duplicate_rows_1 = df_1.duplicated().sum()
print(f"Number of duplicate rows: {duplicate_rows_1}")
#no duplicate rows for df1

duplicate_rows_2 = df_2.duplicated().sum()
print(f"Number of duplicate rows: {duplicate_rows_2}")
#no duplicate rows for df2

"""###In each dataset there are two variables, v1 and v2. As far you can tell from statistical analysis, are they good observations? Be sure to plot them."""

#checking the descriptive statistics

desc_stats_1 = df_1.describe()
print("Descriptive Statistics for df_1:")
print(desc_stats_1)

desc_stats_2 = df_2.describe()
print("\nDescriptive Statistics for df_2:")
print(desc_stats_2)

# plotting values

import matplotlib.pyplot as plt

# Create histograms for df_1
plt.figure(figsize=(12, 5))  # Adjust figure size if needed

plt.subplot(1, 2, 1)  # Create a subplot for v1
plt.hist(df_1['v1'], bins=20, edgecolor='black')  # Adjust bins as needed
plt.title('Distribution of v1 in df_1')
plt.xlabel('v1')
plt.ylabel('Frequency')

plt.subplot(1, 2, 2)  # Create a subplot for v2
plt.hist(df_1['v2'], bins=20, edgecolor='black')  # Adjust bins as needed
plt.title('Distribution of v2 in df_1')
plt.xlabel('v2')
plt.ylabel('Frequency')

plt.tight_layout()  # Adjust subplot spacing
plt.show()

# Create histograms for df_2
plt.figure(figsize=(12, 5))  # Adjust figure size if needed

plt.subplot(1, 2, 1)  # Create a subplot for v1
plt.hist(df_2['v1'], bins=20, edgecolor='black')  # Adjust bins as needed
plt.title('Distribution of v1 in df_1')
plt.xlabel('v1')
plt.ylabel('Frequency')

plt.subplot(1, 2, 2)  # Create a subplot for v2
plt.hist(df_2['v2'], bins=20, edgecolor='black')  # Adjust bins as needed
plt.title('Distribution of v2 in df_1')
plt.xlabel('v2')
plt.ylabel('Frequency')

plt.tight_layout()  # Adjust subplot spacing
plt.show()

# Create a scatter plot for df_1
plt.figure(figsize=(8, 6))  # Adjust figure size if needed
plt.scatter(df_1['v1'], df_1['v2'], alpha=0.5)  # alpha controls transparency
plt.title('Scatter Plot of v1 vs. v2 in df_1')
plt.xlabel('v1')
plt.ylabel('v2')
plt.grid(True)  # Add a grid for better visualization
plt.show()

# Create a scatter plot for df_2 (similar to df_1)
plt.figure(figsize=(8, 6))
plt.scatter(df_2['v1'], df_2['v2'], alpha=0.5)
plt.title('Scatter Plot of v1 vs. v2 in df_2')
plt.xlabel('v1')
plt.ylabel('v2')
plt.grid(True)
plt.show()

"""###In each dataset, is there a statistically significant difference between v1 and v2?


"""

#let's do a t-test because both variables are continous and we want to see the DIFFERENCE BETWEEN MEANS
#we're not looking at correlation - correlation would be pearson's correlation
#use a t-test when you want to determine if there is a statistically signficant difference in the means of two groups or variables
#use pearson's correlation when you want to measure the strength and direction of a linear relationship between two continous variables

import scipy.stats as stats

t_statistic_1, p_value_1 = stats.ttest_ind(df_1['v1'], df_1['v2'])
print("t-statistic for df_1:", t_statistic_1)
print("p-value for df_1:", p_value_1)

t_statistic_2, p_value_2 = stats.ttest_ind(df_2['v1'], df_2['v2'])
print("t-statistic for df_2:", t_statistic_2)
print("p-value for df_2:", p_value_2)

#interpretation
#null hypothesis (h0): there is no significant difference between v1 and v2
#alt hypothesis (h1): there is a significant difference between v1 and v2
#t-statistic: the difference between the means of v1 and v2, standardized by the variability within each group
#p-value: probability of observing the data if there was no actual difference in the means of v1 and v2

#t-statistic for df_1: 0.28186108625954037
#p-value for df_1: 0.77806461726755
#but p is above 0.05, so we fail to reject the null hypothesis (that there is no significant difference between the means of v1 and v2 in df_1).

#t-statistic for df_2: -0.7590422380383209
#p-value for df_2: 0.44787203798386155
#but p is above 0.05, so we fail to reject the null hypothesis (that there is no significant difference between the means of v1 and v2 in df_1).