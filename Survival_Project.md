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
manipulation) then use machine learning tools to predict what the
survival fate for the passengers in the test dataset would have been.

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

# Feature Engineering

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

## Defining factor variables

The code chunk below converts appropriate variables to factor variables.

``` r
titanic =
  Titanic %>% 
  mutate(
    survived = recode(survived, "0" = "Died", "1" = "Survived"),
    embarked = recode(embarked, "C" = "Cherbourg", "S" = "Southampton",
                      "Q" = "Queenstown"),
    pclass = recode(pclass, "1" = "1st", "2" = "2nd", "3" = "3rd"),
    gender = factor(gender, levels = c("male", "female")),
    survived = factor(survived, levels = c("Died", "Survived")),
    embarked = factor(embarked, levels = c("Cherbourg", "Southampton",
                                           "Queenstown")),
    pclass = factor(pclass, levels = c("1st", "2nd", "3rd")))
```

This is a dirty dataset and we either need to drop the rows with NaN
values or fill in the gaps by leveraging the data in the dataset to
estimate what those values could have been. We will choose the latter
and try to estimate those values and fill in the gaps rather than lose
observations.

## Cleaning Names

``` r
titanic %>% 
  mutate(
    name = str_replace(name, '(.*, )|(\\..*)', ''))
```

    ## # A tibble: 1,309 x 12
    ##    passenger_id survived pclass name  gender   age sib_sp parch ticket
    ##           <dbl> <fct>    <fct>  <chr> <fct>  <dbl>  <dbl> <dbl> <chr> 
    ##  1            1 Died     3rd    Mr. … male      22      1     0 A/5 2…
    ##  2            2 Survived 1st    Mrs.… female    38      1     0 PC 17…
    ##  3            3 Survived 3rd    Miss… female    26      0     0 STON/…
    ##  4            4 Survived 1st    Mrs.… female    35      1     0 113803
    ##  5            5 Died     3rd    Mr. … male      35      0     0 373450
    ##  6            6 Died     3rd    Mr. … male      NA      0     0 330877
    ##  7            7 Died     1st    Mr. … male      54      0     0 17463 
    ##  8            8 Died     3rd    Mast… male       2      3     1 349909
    ##  9            9 Survived 3rd    Mrs.… female    27      0     2 347742
    ## 10           10 Survived 2nd    Mrs.… female    14      1     0 237736
    ## # … with 1,299 more rows, and 3 more variables: fare <dbl>, cabin <chr>,
    ## #   embarked <fct>

With the following code chunk, we will determine what different name
titles we have and their distribution according to gender.

``` r
# I will extract titles from the name variable
Titanic$titles = gsub('(.*, )|(\\..*)', '', Titanic$name)
table(Titanic$gender, Titanic$titles)
```

    ##         
    ##          Capt Col Don Dona  Dr Jonkheer Lady Major Master Miss Mlle Mme
    ##   female    0   0   0    1   1        0    1     0      0  260    2   1
    ##   male      1   4   1    0   7        1    0     2     61    0    0   0
    ##         
    ##           Mr Mrs  Ms Rev Sir the Countess
    ##   female   0 197   2   0   0            1
    ##   male   757   0   0   8   1            0

Last names can help us identify passengers according to which families
they come from. Therefore, I will be cleaning the name variable by
removing different name titles such as “Mr”and “Mrs” that are built in
this name vector already. Also, the name variable seem to have “rare”
titles, so I will be replacing those with something ubiquitous.

With the following code chunk, I will extract the name title such as
“Mr.” or “Miss.” everywhere such titles occurs within the name
variable.

we see that we have 18 different titles but we will want to normalize
these a bit so that we can generalize a bit more.
