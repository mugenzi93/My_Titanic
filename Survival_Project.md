Titanic Project
================
Clement Mugenzi
9/20/2019

# Introduction

This titanic project is based on the infamous sinking of Titanic in
1912, a tragedy that led to 1,502 people dying out of 2,224 passengers.
Datasets provided include the train dataset with 891 passengers whose
survival fate is known and a test dataset with 418 passengers whose
survival fate is unknown. I will first start by loading both datasets
then combine them to do some feature engineering (data cleaning and data
manipulation).

## Loading the Dataset

``` r
# First, we will load the train dataset.
train = 
  read_csv("Data/train.csv") %>% 
  janitor::clean_names() 
```

    ## Parsed with column specification:
    ## cols(
    ##   PassengerId = col_double(),
    ##   Survived = col_double(),
    ##   Pclass = col_double(),
    ##   Name = col_character(),
    ##   Sex = col_character(),
    ##   Age = col_double(),
    ##   SibSp = col_double(),
    ##   Parch = col_double(),
    ##   Ticket = col_character(),
    ##   Fare = col_double(),
    ##   Cabin = col_character(),
    ##   Embarked = col_character()
    ## )

``` r
# Second, the test dataset is loaded
test = 
  read_csv("Data/test.csv") %>% 
  janitor::clean_names()
```

    ## Parsed with column specification:
    ## cols(
    ##   PassengerId = col_double(),
    ##   Pclass = col_double(),
    ##   Name = col_character(),
    ##   Sex = col_character(),
    ##   Age = col_double(),
    ##   SibSp = col_double(),
    ##   Parch = col_double(),
    ##   Ticket = col_character(),
    ##   Fare = col_double(),
    ##   Cabin = col_character(),
    ##   Embarked = col_character()
    ## )

``` r
# Then both the train and test datasets are combined into a single dataset.
Titanic = 
  bind_rows(train, test) %>% 
  rename(gender = "sex") %>% view()
```

After loading and combining both datasets, it is better to highlight
what kind of dataset I will be working with. A glimpse of the dataset is
in the next code chunk.

``` r
glimpse(Titanic)
```

    ## Observations: 1,309
    ## Variables: 12
    ## $ passenger_id <dbl> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, …
    ## $ survived     <dbl> 0, 1, 1, 1, 0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 1, 0, …
    ## $ pclass       <dbl> 3, 1, 3, 1, 3, 3, 1, 3, 3, 2, 3, 1, 3, 3, 3, 2, 3, …
    ## $ name         <chr> "Braund, Mr. Owen Harris", "Cumings, Mrs. John Brad…
    ## $ gender       <chr> "male", "female", "female", "female", "male", "male…
    ## $ age          <dbl> 22, 38, 26, 35, 35, NA, 54, 2, 27, 14, 4, 58, 20, 3…
    ## $ sib_sp       <dbl> 1, 1, 0, 1, 0, 0, 0, 3, 0, 1, 1, 0, 0, 1, 0, 0, 4, …
    ## $ parch        <dbl> 0, 0, 0, 0, 0, 0, 0, 1, 2, 0, 1, 0, 0, 5, 0, 0, 1, …
    ## $ ticket       <chr> "A/5 21171", "PC 17599", "STON/O2. 3101282", "11380…
    ## $ fare         <dbl> 7.2500, 71.2833, 7.9250, 53.1000, 8.0500, 8.4583, 5…
    ## $ cabin        <chr> NA, "C85", NA, "C123", NA, NA, "E46", NA, NA, NA, "…
    ## $ embarked     <chr> "S", "C", "S", "S", "S", "Q", "S", "S", "S", "C", "…

Some of the variables important to highlight include name, passengerID,
gender, age, and each individual’s survival status.

## Summary of missing values

The code chunk below summarises how many missing values we have per
column.

``` r
  Titanic %>%
    gather(key = "key", value = "val") %>%
    mutate(is.missing = is.na(val)) %>%
    group_by(key, is.missing) %>%
    summarise(num.missing = n()) %>%
    filter(is.missing == T) %>%
    select(-is.missing) %>%
    arrange(desc(num.missing)) %>%
    rename("Missing Values" = "num.missing", "Variable" = "key") %>% 
  knitr::kable()
```

| Variable | Missing Values |
| :------- | -------------: |
| cabin    |           1014 |
| survived |            418 |
| age      |            263 |
| embarked |              2 |
| fare     |              1 |

This is a dirty dataset and we either need to drop the rows with NaN
values or fill in the gaps by leveraging the data in the dataset to
estimate what those values could have been. We will choose the latter
and try to estimate those values and fill in the gaps rather than lose
observations.

## Cleaning Names

With the following code chunk, I will extract the name title such as
“Mr.” or “Miss.” everywhere such titles occurs within the name
variable.

``` r
Titanic %>%
  mutate(
    name = str_replace(name, "Mr.", ""),
    name = str_replace(name, "Mrs.", ""),
    name = str_replace(name, "Miss", ""),
    name = str_replace(name, "miss", ""),
    name = str_replace(name, "Master", ""),
    name = str_replace(name, ",.", ","),
    name = str_replace(name, ".,", "")) %>% view()
```
