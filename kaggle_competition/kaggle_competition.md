Kaggle Competition
================
Pascal Schmidt
10/02/2022

During my 4th year as an undergraduate student in statistics, I took a
Big Data class where we had to compete in a Kaggle competition.

This competition was about imputing missing values in a times series
weather data set, acquired from 76 different weather stations in the
United Kingdom. The data set consisted of 1,880,892 rows and 11 columns.
In total, there were 2,074,873 missing values where we only had to make
predictions for 29,000 of them. The final score was evaluated by
calculating the absolute mean error. Our approaches for missing data
imputations included linear interpolation, random forests, and linear
regressions. In the end, however, the crucial part for this module was
to figure out for which NA values we needed to use linear interpolation,
and which ones we needed to predict with modeling.

# Data Preparation

``` r
# reading in training set
train <- readr::read_csv(here::here("data/train.csv")) %>%

  # create new column with row number
  # important for looking up missing values
  # in the training set that are in the test set
  dplyr::mutate(ID = dplyr::row_number()) %>%

  # some values have abnormally large values. Hence,
  # we decided that we are going to impute NA for outliers
  dplyr::mutate_at(vars(WINDDIR, WINDSPEED, TEMPERATURE, DEWPOINT, PRESSURE),
    .funs = ~ base::ifelse((. > 4 | . < -4), NA, .)
  )

# reading in testing set
test <- readr::read_csv(here::here("data/test.csv"))
head(train)
```

    ## # A tibble: 6 × 12
    ##     USAF  YEAR MONTH DAY   HOUR  MINUTE WINDDIR WINDSPEED TEMPERATURE DEWPOINT
    ##    <dbl> <dbl> <chr> <chr> <chr> <chr>    <dbl>     <dbl>       <dbl>    <dbl>
    ## 1 992700  2008 01    00    00    00      NA        0.0555       0.441    0.874
    ## 2 992700  2008 01    00    00    00      -0.246   NA            0.441    0.760
    ## 3 992700  2008 01    00    00    00      -0.246   -0.0529      NA        0.760
    ## 4 992700  2008 01    00    00    00      -0.246    0.597        0.305   NA    
    ## 5 992700  2008 01    00    00    00      -0.142    0.489        0.305    0.532
    ## 6 992700  2008 01    00    00    00      -0.142    1.14         0.168    0.418
    ## # … with 2 more variables: PRESSURE <dbl>, ID <int>

``` r
head(test)
```

    ## # A tibble: 6 × 1
    ##   ID       
    ##   <chr>    
    ## 1 374076-5 
    ## 2 1327550-2
    ## 3 450121-4 
    ## 4 1697534-3
    ## 5 952016-5 
    ## 6 1772604-1

In the code above, we are replacing all outliers with NA values.

``` r
# interpolation beats any model for small gaps of NA values
# therefore, we are imputing values by interpolation for a gap of 3 or smaller
train %>%
  dplyr::mutate_at(vars(WINDDIR, WINDSPEED, TEMPERATURE, DEWPOINT, PRESSURE),
    .funs = ~ imputeTS::na_interpolation(., maxgap = 5)
  ) -> train
head(train)
```

    ## # A tibble: 6 × 12
    ##     USAF  YEAR MONTH DAY   HOUR  MINUTE WINDDIR WINDSPEED TEMPERATURE DEWPOINT
    ##    <dbl> <dbl> <chr> <chr> <chr> <chr>    <dbl>     <dbl>       <dbl>    <dbl>
    ## 1 992700  2008 01    00    00    00      -0.246   0.0555        0.441    0.874
    ## 2 992700  2008 01    00    00    00      -0.246   0.00131       0.441    0.760
    ## 3 992700  2008 01    00    00    00      -0.246  -0.0529        0.373    0.760
    ## 4 992700  2008 01    00    00    00      -0.246   0.597         0.305    0.646
    ## 5 992700  2008 01    00    00    00      -0.142   0.489         0.305    0.532
    ## 6 992700  2008 01    00    00    00      -0.142   1.14          0.168    0.418
    ## # … with 2 more variables: PRESSURE <dbl>, ID <int>

Next, we are doing linear interpolation when there are no more than 5
consecutive NA values.

# Building th Test Set

``` r
test %>%
  tidyr::separate(col = ID, into = c("row", "col"), sep = "-") %>%
  dplyr::mutate_all(.funs = as.numeric) %>%
  dplyr::mutate(column = dplyr::case_when(
    col == 1 ~ "WINDDIR",
    col == 2 ~ "WINDSPEED",
    col == 3 ~ "TEMPERATURE",
    col == 4 ~ "DEWPOINT",
    col == 5 ~ "PRESSURE"
  )) -> to_be_predicted

to_be_predicted %>%
  dplyr::mutate(value = NA) -> to_be_predicted
head(to_be_predicted)
```

    ## # A tibble: 6 × 4
    ##       row   col column      value
    ##     <dbl> <dbl> <chr>       <lgl>
    ## 1  374076     5 PRESSURE    NA   
    ## 2 1327550     2 WINDSPEED   NA   
    ## 3  450121     4 DEWPOINT    NA   
    ## 4 1697534     3 TEMPERATURE NA   
    ## 5  952016     5 PRESSURE    NA   
    ## 6 1772604     1 WINDDIR     NA

We separatd the ID column into two columns and also added the actual
column name to the data set.

Now we will be filling up the `to_b_predicted` data frame with the
linear interpolated values from previously. We aree using a for loop
where we are going over every row of thee `to_be_predicted` data frame
and then use the row and column values to get the values from the
`train` data set.

``` r
# fill out missing values in df to_be_predicted
# by the interpolated values in the training data set
for (i in 1:nrow(to_be_predicted)) {
  to_be_predicted$value[i] <- train[to_be_predicted$row[i], to_be_predicted$column[i]] %>%
    dplyr::pull()
}
head(to_be_predicted)
```

    ## # A tibble: 6 × 4
    ##       row   col column       value
    ##     <dbl> <dbl> <chr>        <dbl>
    ## 1  374076     5 PRESSURE     0.293
    ## 2 1327550     2 WINDSPEED   -0.775
    ## 3  450121     4 DEWPOINT     1.16 
    ## 4 1697534     3 TEMPERATURE  0.168
    ## 5  952016     5 PRESSURE     0.466
    ## 6 1772604     1 WINDDIR     -1.69

``` r
# missing values left
sum(is.na(to_be_predicted$value))
```

    ## [1] 3461

After getting the linear intterpolated values in the test data set, we
still have 3461 values we need to fill.

``` r
# breakdown of missing values
to_be_predicted %>%
  dplyr::filter(is.na(value)) %>%
  dplyr::pull(column) %>%
  table() 
```

    ## .
    ##    DEWPOINT    PRESSURE TEMPERATURE     WINDDIR   WINDSPEED 
    ##         517         692         868         595         789

Next, we will be creating two columns in the `to_be_prdicted` data set
where we are computing how many consecutive NA values there are above
the NA value that we need to predict and how many consecutive NA values
there are below the missing value we need to predict.

``` r
df <- to_be_predicted %>%
  dplyr::filter(is.na(value)) %>%
  dplyr::mutate(
    above = NA,
    below = NA
  )

### Functions Implemented in C++ to reduce run time by about 10min compared to R loops
Rcpp::sourceCpp(here::here("scripts/missing_gap.cpp"))

# count how many missing values there are above and below
# each target missing value in the test set
for (i in c("WINDDIR", "TEMPERATURE", "DEWPOINT", "PRESSURE", "WINDSPEED")) {
  missing_below(
    df$row,
    df$column,
    train[, i] %>%
      dplyr::pull(),
    df$below,
    i
  ) -> df$below
}

for (i in c("WINDDIR", "TEMPERATURE", "DEWPOINT", "PRESSURE", "WINDSPEED")) {
  missing_above(
    df$row,
    df$column,
    train[, i] %>%
      dplyr::pull(),
    df$above,
    i
  ) -> df$above
}

head(df)
```

    ## # A tibble: 6 × 6
    ##       row   col column      value above below
    ##     <dbl> <dbl> <chr>       <dbl> <dbl> <dbl>
    ## 1 1044015     2 WINDSPEED      NA   100   100
    ## 2  779897     5 PRESSURE       NA   100   100
    ## 3  123439     1 WINDDIR        NA   100   100
    ## 4 1124760     3 TEMPERATURE    NA   100   100
    ## 5 1240458     5 PRESSURE       NA   100   100
    ## 6  995863     1 WINDDIR        NA   100   100

If the column above OR below is less than 6, then we are doing linear
interpolation because the NA value is still very close in time to a
known value.

``` r
df %>%
  dplyr::filter(is.na(value)) %>%
  dplyr::filter(above > 6 & below > 6) -> df
nrow(df)
```

    ## [1] 3161

There are still 3161 missing values that we need to predict. And for th
other 308 values we can do linear interpolation. Firstly, we are doing
the regression and then the linear interpolation.

The linear regression will be a little bit tricky because there might be
multiple missing value within a row. So when we want to predict an NA
value for `WINDSPEED` for example and all the other four columns have NA
values in them then we won’t be able to do a regression because there
are no predictors.

Therefore, we need to identify which columns are missing in the row
where we need to predict a missing value.

The code below takes care of the it. We are fitting arround 3000
separate linear models.

-   First, we identify the row of the missing value we need to predict
-   Next, we look if the predictors also have NA values in that row. If
    it does, we are removing that predictor from the regression.
-   Now, if all predictors have NA values, then we are just using the
    data from the station and the same month from the NA value and then
    using mean imputation.
-   I decided to only use data in the regression from the same station.
    So next, I am checking the percentage of missing values within the
    column of each predictor. If the column has more than 40% missing
    values, then that column will be removed as a predicor because
    otherwise we would lose too many observattions for the regression.
-   If all of the predictor columns have more than 40% of missing
    values, then we are only using the mean value of the response column
    as predition.
-   Finally, if we are left with more than 500 observations, we are
    doing a linear regression.

The 500 observations and 40% missing values were randomly chosen.

``` r
###################################
### Fitting ~3000 Linear Models ###
###################################


# keep rows for which regression model failed
error_row <- c()
# keep r squared
r_squared <- c()

# for each target missing values, determine which other predictors in the row
# of the target missing values are missing as well so we can exclude
# these predictors from our regression and that also ensures we choose as many predictors as possible
for (i in 1:nrow(df)) {

  # pull out predictors which are not missing in response variable row
  train[df$row[[i]], ] %>%
    dplyr::select(WINDDIR:PRESSURE, -c(df$column[[i]], which(is.na(.)))) %>%
    names() -> names_pred

  # if all predictors in row are missing use mean imputation
  if (length(names_pred) == 0) {
    train %>%
      dplyr::filter(USAF == train[df$row[[i]], ]$USAF[[1]] &
        MONTH == train[df$row[[i]], ]$MONTH[[1]]) %>%
      {
        mean(.[, df$column[[i]]] %>%
          dplyr::pull(),
        na.rm = TRUE
        )
      } -> df[i, "value"]

    next
  }

  # check if predictors have more than 40% missing values in column
  # if they do, discard that particular predictor
  train %>%
    dplyr::filter(USAF == train$USAF[[df$row[[i]]]]) %>%
    dplyr::select(names_pred) %>%
    dplyr::summarise_all(.funs = ~ (sum(is.na(.)) / length(.))) %>%
    dplyr::distinct() %>%
    tidyr::gather() %>%
    dplyr::filter(value < 0.40) %>%
    dplyr::pull(key) %>%
    paste(collapse = " + ") -> predictor_vars


  # if there are no predictor_vars because there are more then 40%
  # of missing values in each predictor column, then just impute the
  # average of the response column by station
  if (predictor_vars == "") {
    train %>%
      dplyr::filter(USAF == train[df$row[[i]], ]$USAF[[1]] &
        MONTH == train[df$row[[i]], ]$MONTH[[1]]) %>%
      {
        mean(.[, df$column[[i]]] %>%
          dplyr::pull(),
        na.rm = TRUE
        )
      } -> df[i, "value"]

    next
  }


  # if there are enough observations (500 or more) for a particular station,
  # we do a linear regression by station
  if (train %>%
    dplyr::select(c(USAF, df$column[[i]], stringr::str_split(predictor_vars, pattern = "\\+") %>%
      unlist() %>%
      stringi::stri_trim_both())) %>%
    dplyr::filter(USAF == train$USAF[[df$row[[i]]]]) %>%
    na.omit() %>%
    nrow() > 500) {

    # linear regression formula
    formula <- paste0(df$column[[i]], " ~ MONTH + ", predictor_vars)
    formula <- as.formula(formula)

    # linear model
    lm(formula,
      data = train %>%
        dplyr::filter(USAF == train$USAF[[df$row[[i]]]])
    ) -> lm_model

    # try catch statement when factor MONTH in predict row does not appear in training data
    tryCatch(
      expr = {
        predicted_value <- predict(lm_model, newdata = train[df$row[[i]], ])
        df[i, "value"] <- predicted_value
      },
      error = function(error) {
        error_row <<- c(error_row, i)
        df[i, "value"] <<- NA
        message(paste("Caught an error in row", i))
      }
    )
  }

  # if there are not enough observations for a particular station,
  # we use the entire training data set with the station as a predictor
  else {
    formula <- paste0(df$column[[i]], " ~ MONTH + as.factor(USAF) + ", predictor_vars)
    formula <- as.formula(formula)

    lm(formula,
      data = train
    ) -> lm_model

    # try catch statement when factor MONTH in predict row does not appear in training data
    tryCatch(
      expr = {
        predicted_value <- predict(lm_model, newdata = train[df$row[[i]], ])
        df[i, "value"] <- predicted_value
      },
      error = function(error) {
        error_row <<- c(error_row, i)
        df[i, "value"] <<- NA
        message(paste("Caught an error in row", i))
      }
    )
  }

  # safe r squared in case we want to deal with missing values for
  # low r squared models sepeartely
  r_squared[[i]] <- summary(lm_model)$r.squared
  names(r_squared) <- i
}
```

``` r
train %>% 
  dplyr::select(WINDDIR:PRESSURE) %>% 
  na.omit() %>% 
  cor()
```

    ##                 WINDDIR   WINDSPEED TEMPERATURE    DEWPOINT    PRESSURE
    ## WINDDIR      1.00000000  0.07615423 -0.01390792 -0.03722569 -0.02001083
    ## WINDSPEED    0.07615423  1.00000000 -0.13940574 -0.09789144 -0.31549858
    ## TEMPERATURE -0.01390792 -0.13940574  1.00000000  0.89136749  0.05715878
    ## DEWPOINT    -0.03722569 -0.09789144  0.89136749  1.00000000  0.01802579
    ## PRESSURE    -0.02001083 -0.31549858  0.05715878  0.01802579  1.00000000

There is a strong linear correlation between `TEMPERATURE` and
`DEWPOINT`. Therefore, we are only fitting a linear regression for
thesee two columns where the previous model failed.

``` r
df[error_row, ] %>%
  dplyr::filter(column == "DEWPOINT" | column == "TEMPERATURE") -> temp_dew_df


r <- c()
for (i in 1:nrow(temp_dew_df)) {
  if (temp_dew_df$column[[i]] == "TEMPERATURE") {
    lm(TEMPERATURE ~ DEWPOINT + as.factor(MONTH),
      data = train %>%
        dplyr::filter(USAF == train[temp_dew_df$row[[i]], ]$USAF)
    ) -> lm_model
    predicted_value <- predict(lm_model, newdata = train[temp_dew_df$row[[i]], ])
    temp_dew_df[i, "value"] <- predicted_value
    r[[i]] <- summary(lm_model)$r.squared
  } else {
    lm(DEWPOINT ~ TEMPERATURE + as.factor(MONTH),
      data = train %>%
        dplyr::filter(USAF == train[temp_dew_df$row[[i]], ]$USAF)
    ) -> lm_model
    predicted_value <- predict(lm_model, newdata = train[temp_dew_df$row[[i]], ])
    temp_dew_df[i, "value"] <- predicted_value
    r[[i]] <- summary(lm_model)$r.squared
  }
}

# join imputed values with test data set
df %>%
  tidyr::unite(together, c("row", "column")) %>%
  dplyr::left_join(
    temp_dew_df %>%
      tidyr::unite(together, c("row", "column")) %>%
      select(together, value), by = "together"
  ) %>%
  dplyr::mutate(value.x = ifelse(!(is.na(value.y)), value.y, value.x)) %>%
  dplyr::rename(value = value.x) %>%
  tidyr::separate(together, into = c("row", "column"), remove = FALSE) %>%
  dplyr::select(-c(value.y, together)) -> df
```

``` r
to_be_predicted %>%
  tidyr::unite(together, c("row", "column")) %>%
  dplyr::left_join(df %>%
    tidyr::unite(together, c("row", "column")) %>%
    select(together, value), by = "together") %>%
  dplyr::mutate(value.x = ifelse(!(is.na(value.y)), value.y, value.x)) %>%
  dplyr::rename(value = value.x) %>%
  tidyr::separate(together, into = c("row", "column"), remove = FALSE) %>%
  dplyr::select(-c(value.y, together)) -> to_be_predicted

# interpolation for rest of missing NAs
train %>%
  dplyr::mutate_at(vars(WINDDIR, WINDSPEED, TEMPERATURE, DEWPOINT, PRESSURE),
    .funs = ~ imputeTS::na_interpolation(.)
  ) -> train

for (i in 1:nrow(to_be_predicted)) {
  if (is.na(to_be_predicted$value[[i]])) {
    to_be_predicted[i, "value"] <- train[to_be_predicted$row[i], to_be_predicted$column[i]] %>%
      dplyr::pull()
  }
}

to_be_predicted %>%
  dplyr::rename(ID = row, Value = value) %>%
  tidyr::unite(col = ID, ID, col, sep = "-", remove = FALSE) %>%
  dplyr::select(-column, -col) -> predicted_values

readr::write_csv(predicted_values, "predictions/interpolation_lm.csv")
```
