PUBH 7462 Homework 2
================
Jack Rabe
2/9/2022

-   [Problem 3. BRFSS SMART 2002-2010](#problem-3-brfss-smart-2002-2010)
    -   [3.1 Data Exploration and
        Cleaning](#31-data-exploration-and-cleaning)
    -   [3.2 Data Description](#32-data-description)

# Problem 3. BRFSS SMART 2002-2010

## 3.1 Data Exploration and Cleaning

``` r
#read data with relative path
brfss.df <- read.csv(file = "./data/brfss_smart_2010.csv", header = TRUE) %>% 
  as_tibble()
```

``` r
#tidy time
tidy.brfss.df <- brfss.df %>% 
  janitor::clean_names() %>%  #clean up variable names
  filter(topic == "Overall Health") %>% #only retain rows where topic is overall health
  separate(col = "locationdesc",
           sep = "-",
           into = c("state", "county")) %>% #separate column into two distinct variables, "state" and "county"
  select(year, state, county, response, sample_size, data_value) %>% #only retain some of the columns for working with data
  mutate(
    year = as.numeric(year),
    county = str_remove(county, "County"), #remove county from each data entry
    response = as.factor(response) %>% 
      fct_relevel("Poor", "Fair", "Good", "Very good"), #relevel from alphabetical to poor - excellent scale
  ) %>% 
  rename(health_cond = response,
         prop_response = data_value) %>% 
  arrange(year, state, county, health_cond)

#check that it all looks good
str(tidy.brfss.df)
```

## 3.2 Data Description

*Case definition*: Each observation in the retained **BRFSS** data set
represents the number of responses, within a specific United States
county and over a single year, that rated their health in a certain
category along a scale of “poor” to “excellent.” Each recorded response
comes from adults in the United States who were surveyed over the phone
by the Center for Disease Control and asked how they would rate their
general health condition.

There are 10625 observations in the filtered **BRFSS** data set with 6
variables.

The variables are as follows:

-   *year* of course refers to the year (2002 to 2010) of penguin being
    measured

-   *state* refers to the US state the surveys were conducted in for
    each observation

-   *county* refers to the county the surveys were conducted in

-   *health_cond* refers to the overall health condition (Poor, Fair,
    Good, Very good, Excellent) that people could respond with when
    asked how they would rate their general health

-   *sample_size* refers to the number of responses recorded for a
    specific rating/condition (e.g. excellent) for that US county during
    that year’s survey.

-   *prop_response* refers to the proportion, or more accurately,
    percent of responses that rated themselves as a specific condition
    out of the total number of survey responses.
