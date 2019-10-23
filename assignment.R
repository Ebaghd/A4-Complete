# A4 Data Wrangling

# Loading and Exploring Data -------------------------------- (**28 points**)

# To begin, you'll need to download the Kickstarter Projects data from the
# Kaggle website: https://www.kaggle.com/kemical/kickstarter-projects
# Download the `ks-projects-201801.csv` file into a new folder called `data/`

# Load the `dplyr` package

# Load your data, making sure to not interpret strings as factors


# To start, write the code to get some basic information about the dataframe:
# - What are the column names?
# - How many rows is the data frame?
# - How many columns are in the data frame?

# Use the `summary` function to get some summary information


# Unfortunately, this doesn't give us a great set of insights. Let's write a
# few functions to try and do this better.
# First, let's write a function `get_col_info()` that takes as parameters a
# column name and a dataframe. If the values in the column are *numeric*,
# the function should return a list with the keys:
# - `min`: the minimum value of the column
# - `max`: the maximum value of the column
# - `mean`: the mean value of the column
# If the column is *not* numeric and there are fewer than 10 unique values in
# the column, you should return a list with the keys:
# - `n_values`: the number of unique values in the column
# - `unique_values`: a vector of each unique value in the column
# If the column is *not* numeric and there are 10 or *more* unique values in
# the column, you should return a list with the keys:
# - `n_values`: the number of unique values in the column
# - `sample_values`: a vector containing a random sample of 10 column values
# Hint: use `typeof()` to determine the column type


# Demonstrate that your function works by passing a column name of your choice
# and the kickstarter data to your function. Store the result in a variable
# with a meaningful name


# To take this one step further, write a function `get_summary_info()`,
# that takes in a data frame  and returns a *list* of information for each
# column (where the *keys* of the returned list are the column names, and the
# _values_ are the summary information returned by the `get_col_info()` function
# The suggested approach is to use the appropriate `*apply` method to
# do this, though you can write a loop


# Demonstrate that your function works by passing the kickstarter data
# into it and saving the result in a variable


# Take note of 3 observations that you find interesting from this summary
# information (and/or questions that arise that want to investigate further)
# YOUR COMMENTS HERE
# LIKELY ON MULTIPLE LINES

# Asking questions of the data ----------------------------- (**29 points**)

# Write the appropriate dplyr code to answer each one of the following questions
# Make sure to return (only) the desired value of interest (e.g., use `pull()`)
# Store the result of each question in a variable with a clear + expressive name
# If there are multiple observations that meet each condition, the results
# can be in a vector. Make sure to *handle NA values* throughout!
# You should answer each question using a single statement with multiple pipe
# operations!

# What was the name of the project(s) with the highest goal?


# What was the category of the project(s) with the lowest goal?


# How many projects had a deadline in 2018?


# What proportion or projects weren't successful? Your result can be a decimal


# What was the amount pledged for the project with the most backers?


# Of all of the projects that *failed*, what was the name of the project with
# the highest amount of money pledged?


# How much total money was pledged to projects that weren't successful?


# Performing analysis by *grouped* observations ----------------- (38 Points)

# Which category had the most money pledged (total)?


# Which country had the most backers?


# Which year had the most money pledged (hint: you may have to create a new
# column)?


# What were the top 3 main categories in 2018 (as ranked by number of backers)?


# What was the most common day of the week on which to launch a project?
# (return the name of the day, e.g. "Sunday", "Monday"....)


# What was the least successful day on which to launch a project? In other
# words, which day had the lowest success rate (lowest proportion of projects
# that were successful)? This might require some creative problem solving....

