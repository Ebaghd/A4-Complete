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
ks_data <- read.csv("ks-projects-201801.csv", stringsAsFactors = FALSE)
col_names <- colnames(ks_data, do.NULL = TRUE, prefix = "row")
col_num <- ncol(ks_data)
row_num <- nrow(ks_data)
# Use the `summary` function to get some summary information

ks_data_summary <- summary(ks_data)
print(ks_data_summary)
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
get_col_info <- function(col_name, df) {
  column_type <- typeof(df[[col_name]])
  col <- df[[col_name]]
  if (column_type == "character") {
    n_values <- length(unique(col, incomparables = FALSE))
    num_unique_values <- "Number of unique values in the column:"
    if (n_values > 10) {
      sample_values <- sample(unique(col, incomparables = FALSE),
                              10, replace = FALSE, prob = NULL)
      sample_value_desc <-  "Ten unique sample values:"
      col_list <- list(num_unique_values = n_values, sample_value_desc = sample_values)
    }
    else {
      unique_values <- unique(col, incomparables = FALSE)
      each_unique_value <- "Each unique value:"
      col_list <- list(num_unique_values = n_values, each_unique_value = unique_values)
    }
  }
  else if (column_type == ("double") | column_type == "integer"){
    min_value <- min(col, na.rm = TRUE)
    max_value <- max(col, na.rm = TRUE)
    mean_value <- mean(col, na.rm = TRUE)
    min_output <- "Minimum value:"
    mean_output <- "Mean value:"
    max__output <- "Maximum value:"
    col_list <- list(min_output = min_value, max_output = max_value, mean_output = mean_value)
  }
  col_list
}
# Demonstrate that your function works by passing a column name of your choice
# and the kickstarter data to your function. Store the result in a variable
# with a meaningful name

sample_unique_deadline_values <- get_col_info("deadline", ks_data)
# To take this one step further, write a function `get_summary_info()`,
# that takes in a data frame  and returns a *list* of information for each
# column (where the *keys* of the returned list are the column names, and the
# _values_ are the summary information returned by the `get_col_info()` function
# The suggested approach is to use the appropriate `*apply` method to
# do this, though you can write a loop
get_summary_info <- function(df) {
  values <- lapply(names(df), get_col_info, df)
  names(values) <- names(df)
}
# Demonstrate that your function works by passing the kickstarter data
# into it and saving the result in a variable
x <- get_summary_info(ks_data)
# Take note of 3 observations that you find interesting from this summary
# information (and/or questions that arise that want to investigate further)
# 1: The minimum goal is 0.01 USD, how is the project worth it if such little money is being made?
# 2: So many different terms for state, what are the difference between them?
# 3: A kickstarter project has 219,382 backers - very well supported
#
# Asking questions of the data ----------------------------- (**29 points**)

# Write the appropriate dplyr code to answer each one of the following questions
# Make sure to return (only) the desired value of interest (e.g., use `pull()`)
# Store the result of each question in a variable with a clear + expressive name
# If there are multiple observations that meet each condition, the results
# can be in a vector. Make sure to *handle NA values* throughout!
# You should answer each question using a single statement with multiple pipe
# operations!

# What was the name of the project(s) with the highest goal?
highest_goal_projects <- ks_data %>%
  select(name, goal) %>%
  filter(goal == max(goal)) %>%
  select(name)
# What was the category of the project(s) with the lowest goal?
lowest_goal_categories <- ks_data %>%
  select(category, goal) %>%
  filter(goal == min(goal)) %>%
  select(category)
# How many projects had a deadline in 2018?
projects_with_2018_deadline <- ks_data %>% 
  select(name, deadline) %>%
  filter(deadline >= 2018) %>%
  select(name) %>%
  lengths
# What proportion of projects weren't successful? Your result can be a decimal
unsuccessful_projects <- ks_data %>%
  select(state) %>%
  filter(state == "failed"| state == "canceled") %>%
  lengths / length(ks_data[["state"]])
# What was the amount pledged for the project with the most backers?
most_backers <- ks_data %>%
  select(usd_pledged_real, backers) %>%
  filter(backers == max(backers)) %>%
  select(usd_pledged_real)
# Of all of the projects that *failed*, what was the name of the project with
# the highest amount of money pledged?
max_failed <- ks_data %>%
  select(state, name, usd_pledged_real) %>%
  filter(state == "failed") %>%
  filter(usd_pledged_real == max(usd_pledged_real)) %>%
  select(name)
# How much total money was pledged to projects that weren't successful?
total_usd_failed_projects <- ks_data %>%
  select(usd_pledged_real, state) %>%
  filter(state == "failed" | state == "canceled") %>%
  select(usd_pledged_real) %>%
  sum
# Performing analysis by *grouped* observations ----------------- (38 Points)

# Which category had the most money pledged (total)?
richest_category <- ks_data %>%
  select(usd_pledged_real, category) %>%
  group_by(category, add = FALSE) %>%
  summarize(usd_pledged_real = sum(usd_pledged_real)) %>%
  filter(usd_pledged_real == max(usd_pledged_real)) %>%
  select(category)
# Which country had the most backers?
most_backers_country <- ks_data %>%
  select(country, backers) %>%
  group_by(country, add = FALSE) %>%
  summarize(backers = sum(backers)) %>%
  filter(backers == max(backers)) %>%
  select(country) 
# Which year had the most money pledged (hint: you may have to create a new
# column)?
richest_year <- ks_data%>% 
  select(usd_pledged_real, launched) %>%
  group_by(launched = substr(launched, 1, 4)) %>%
  summarize(usd_pledged_real = sum(usd_pledged_real)) %>%
  filter(usd_pledged_real == max(usd_pledged_real)) %>%
  select(launched)
# What were the top 3 main categories in 2018 (as ranked by number of backers)?
top_three_categories <- ks_data %>%
  select(category, backers, launched) %>%
  filter(launched >= 2018 ) %>%
  group_by(category) %>%
  summarize(backers = sum(backers)) %>%
  mutate(backers_order = dense_rank(backers)) %>%
  filter(backers_order >= 27) %>%
  select(category)
# What was the most common day of the week on which to launch a project?
# (return the name of the day, e.g. "Sunday", "Monday"....)
most_common_launch_day <- ks_data %>%
  select(launched) %>%
  mutate(time_launched = substr(launched, 1, 10)) %>%
  mutate(day = weekdays(as.Date(time_launched))) %>%
  mutate(time = "") %>%
  group_by(day, add = FALSE) %>%
  summarise(num = n()) %>%
  filter(most_common_launch_day[[2]] == max(most_common_launch_day[[2]])) %>%
  select(day)
# What was the least successful day on which to launch a project? In other
# words, which day had the lowest success rate (lowest proportion of projects
# that were successful)? This might require some creative problem solving....
least_successful_day <- ks_data %>%
  select(launched, state) %>%
  mutate(time_launched = substr(launched, 1, 10)) %>%
  mutate(day = weekdays(as.Date(time_launched))) %>%
  group_by(state) %>%
  mutate(num = summarise(num = n(), add = TRUE))
group_by(day, add = TRUE) %>%
  summarise(num = n()) %>%
  filter(most_common_launch_day[[2]] == min(most_common_launch_day[[2]]))