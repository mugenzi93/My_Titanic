My\_Titanic
================
Clement Mugenzi
9/20/2019

This dataset will be comprised of two separate .csv files, both of which
will be combined then used to predict who survives on the titanic. But
first, let’s load all packages that I will be using in this project.

# Import the Dataset

``` r
# First, we will load the train dataset.
train = read_csv("train.csv")
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
test = read_csv("test.csv")
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
full = bind_rows(train, test)
```
