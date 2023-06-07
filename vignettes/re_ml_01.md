Report Exercice 9
================
Thaddäus Braun
2023-05-08

The topic of this exercice is a comparison of the a linear regression
and KNN models. In the following chunks, I am going to split the data in
a test and training data set to evaluate the model performances. Before
I am able to perform this step I have to pre-process the data. In a
first step, I load the required packages and read the data.
Additionally, data wrangling is performed.

``` r
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(readr)
library(lubridate)
```

    ## 
    ## Attaching package: 'lubridate'

    ## The following objects are masked from 'package:base':
    ## 
    ##     date, intersect, setdiff, union

``` r
daily_fluxes <- read_csv("/Users/ThaddaeusBraun/Desktop/AGDS I/R_Codes/AGDS_Report_Exercices/agds_report_thaddaeusbraun/vignettes/FLX_CH-Dav_FLUXNET2015_FULLSET_DD_1997-2014_1-3.csv") |>  
  
  # select only the variables we are interested in
  dplyr::select(TIMESTAMP,
                GPP_NT_VUT_REF,    # the target
                ends_with("_QC"),  # quality control info
                ends_with("_F"),   # includes all all meteorological covariates
                -contains("JSB")   # weird useless variable
                ) |>

  # convert to a nice date object
  dplyr::mutate(TIMESTAMP = ymd(TIMESTAMP)) |>

  # set all -9999 to NA
  mutate(across(where(is.numeric), ~na_if(., -9999))) |>
  
  # retain only data based on >=80% good-quality measurements
  # overwrite bad data with NA (not dropping rows)
  dplyr::mutate(GPP_NT_VUT_REF = ifelse(NEE_VUT_REF_QC < 0.8, NA, GPP_NT_VUT_REF),
                TA_F           = ifelse(TA_F_QC        < 0.8, NA, TA_F),
                SW_IN_F        = ifelse(SW_IN_F_QC     < 0.8, NA, SW_IN_F),
                LW_IN_F        = ifelse(LW_IN_F_QC     < 0.8, NA, LW_IN_F),
                VPD_F          = ifelse(VPD_F_QC       < 0.8, NA, VPD_F),
                PA_F           = ifelse(PA_F_QC        < 0.8, NA, PA_F),
                P_F            = ifelse(P_F_QC         < 0.8, NA, P_F),
                WS_F           = ifelse(WS_F_QC        < 0.8, NA, WS_F)) |> 

  # drop QC variables (no longer needed)
  dplyr::select(-ends_with("_QC"))
```

    ## Rows: 6574 Columns: 334

    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## dbl (334): TIMESTAMP, TA_F_MDS, TA_F_MDS_QC, TA_F_MDS_NIGHT, TA_F_MDS_NIGHT_...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

In order to specify to specify the target and predictor variables, I use
the formula notation.

``` r
#formula notation

lm(GPP_NT_VUT_REF ~ SW_IN_F + VPD_F + TA_F, data = daily_fluxes)
```

    ## 
    ## Call:
    ## lm(formula = GPP_NT_VUT_REF ~ SW_IN_F + VPD_F + TA_F, data = daily_fluxes)
    ## 
    ## Coefficients:
    ## (Intercept)      SW_IN_F        VPD_F         TA_F  
    ##     1.10021      0.01329     -0.33963      0.29928

In this chunk I use the function “train” from the package “caret” to
train the model. Here, I speficy the linear regression model.

``` r
library(caret)
```

    ## Loading required package: ggplot2

    ## Loading required package: lattice

``` r
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ forcats 1.0.0     ✔ tibble  3.2.1
    ## ✔ purrr   1.0.1     ✔ tidyr   1.3.0
    ## ✔ stringr 1.5.0

    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ✖ purrr::lift()   masks caret::lift()
    ## ℹ Use the ]8;;http://conflicted.r-lib.org/conflicted package]8;; to force all conflicts to become errors

``` r
caret::train(
  form = GPP_NT_VUT_REF ~ SW_IN_F + VPD_F + TA_F, 
  data = daily_fluxes |> drop_na(),  # drop missing values
  trControl = caret::trainControl(method = "none"),  # no resampling
  method = "lm"
)
```

    ## Linear Regression 
    ## 
    ## 2729 samples
    ##    3 predictor
    ## 
    ## No pre-processing
    ## Resampling: None

In this chunk I specify the knn model. Again with the train function.

``` r
library(caret)

caret::train(
  form = GPP_NT_VUT_REF ~ SW_IN_F + VPD_F + TA_F, 
  data = daily_fluxes |> drop_na(), 
  trControl = caret::trainControl(method = "none"),
  method = "knn"
)
```

    ## k-Nearest Neighbors 
    ## 
    ## 2729 samples
    ##    3 predictor
    ## 
    ## No pre-processing
    ## Resampling: None

Once the two models have been specified, the data is now split into a
test and a train data set. To effectively evaluate the generalization
error of a model, a crucial procedure involves segregating a portion of
the data during training and keeping it completely separate and
untouched for testing purposes. In this case, 70% of the data is used
for training. This is a typical split to ensure that there is enough
data for training that the test results are robust and also able to find
a relationship. In a second step the distribution of of values in the
training and test set is plotted.

``` r
set.seed(123)  # for reproducibility
split <- rsample::initial_split(daily_fluxes, prop = 0.7, strata = "VPD_F")
daily_fluxes_train <- rsample::training(split)
daily_fluxes_test <- rsample::testing(split)

plot_data <- daily_fluxes_train |> 
  dplyr::mutate(split = "train") |> 
  dplyr::bind_rows(daily_fluxes_test |> 
  dplyr::mutate(split = "test")) |> 
  tidyr::pivot_longer(cols = 2:9, names_to = "variable", values_to = "value")

plot_data |> 
  ggplot(aes(x = value, y = ..density.., color = split)) +
  geom_density() +
  facet_wrap(~variable, scales = "free")
```

    ## Warning: The dot-dot notation (`..density..`) was deprecated in ggplot2 3.4.0.
    ## ℹ Please use `after_stat(density)` instead.
    ## This warning is displayed once every 8 hours.
    ## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
    ## generated.

    ## Warning: Removed 5451 rows containing non-finite values (`stat_density()`).

![](re_ml_01_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

The purpose of data pre-processing is twofold: first, it involves
preparing the data for a particular model fitting process, and second,
it aims to enhance the efficacy of model training. The {recipes} package
offers a more robust approach for simultaneously specifying the formula
and pre-processing steps. It seamlessly integrates with the {caret}
package’s train() function. To illustrate, considering the same formula
mentioned earlier, and assuming we want to normalize (center and scale)
the data in daily_fluxes_train, we can define a “recipe” using the pipe
operator as follows:

``` r
library(recipes)
```

    ## 
    ## Attaching package: 'recipes'

    ## The following object is masked from 'package:stringr':
    ## 
    ##     fixed

    ## The following object is masked from 'package:stats':
    ## 
    ##     step

``` r
pp <- recipes::recipe(GPP_NT_VUT_REF ~ SW_IN_F + VPD_F + TA_F, data = daily_fluxes_train) |> 
  recipes::step_center(all_numeric(), -all_outcomes()) |>
  recipes::step_scale(all_numeric(), -all_outcomes())

caret::train(
  pp, 
  data = daily_fluxes_train, 
  method = "knn",
  trControl = caret::trainControl(method = "none")
)
```

    ## k-Nearest Neighbors 
    ## 
    ## 4600 samples
    ##    8 predictor
    ## 
    ## Recipe steps: center, scale 
    ## Resampling: None

The following chunk aims to standardize the values of the data. Some
algorithms explicitly necessitate the standardization of data to ensure
that the values of all predictors fall within a comparable range.

``` r
daily_fluxes |> 
  summarise(across(where(is.numeric), ~quantile(.x, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE))) |> 
  t() |> 
  as_tibble(rownames = "variable") |> 
  setNames(c("variable", "min", "q25", "q50", "q75", "max"))
```

    ## Warning: Returning more (or less) than 1 row per `summarise()` group was deprecated in
    ## dplyr 1.1.0.
    ## ℹ Please use `reframe()` instead.
    ## ℹ When switching from `summarise()` to `reframe()`, remember that `reframe()`
    ##   always returns an ungrouped data frame and adjust accordingly.
    ## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
    ## generated.

    ## Warning: The `x` argument of `as_tibble.matrix()` must have unique column names if
    ## `.name_repair` is omitted as of tibble 2.0.0.
    ## ℹ Using compatibility `.name_repair`.
    ## This warning is displayed once every 8 hours.
    ## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
    ## generated.

    ## # A tibble: 8 × 6
    ##   variable           min     q25    q50    q75    max
    ##   <chr>            <dbl>   <dbl>  <dbl>  <dbl>  <dbl>
    ## 1 GPP_NT_VUT_REF  -4.23    0.773   2.87   5.45  12.3 
    ## 2 TA_F           -21.9    -1.47    3.51   8.72  20.7 
    ## 3 SW_IN_F          3.30   77.8   135.   214.   366.  
    ## 4 LW_IN_F        138.    243.    279.   308.   365.  
    ## 5 VPD_F            0.001   0.959   2.23   4.06  16.6 
    ## 6 PA_F            80.4    83.2    83.7   84.1   85.6 
    ## 7 P_F              0       0       0      1.6   92.1 
    ## 8 WS_F             0.405   1.56    1.93   2.34   6.54

Furthermore, there are more steps required to actually transform
“daily_fluxes_train”.

``` r
pp_prep <- recipes::prep(pp, training = daily_fluxes_train) 

daily_fluxes_juiced <- recipes::juice(pp_prep)

daily_fluxes_baked <- recipes::bake(pp_prep, new_data = daily_fluxes_train)

# confirm that juice and bake return identical objects when given the same data
all_equal(daily_fluxes_juiced, daily_fluxes_baked)
```

    ## Warning: `all_equal()` was deprecated in dplyr 1.1.0.
    ## ℹ Please use `all.equal()` instead.
    ## ℹ And manually order the rows/cols as needed
    ## This warning is displayed once every 8 hours.
    ## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
    ## generated.

    ## [1] TRUE

``` r
daily_fluxes_baked <- recipes::bake(pp_prep, new_data = daily_fluxes_train)

# confirm that juice and bake return identical objects when given the same data
all_equal(daily_fluxes_juiced, daily_fluxes_baked)
```

    ## [1] TRUE

This part of the code replaces missing values with the best guess. This
step is called imputation. Here, I use KNN with five neighbors.

``` r
pp |> 
  step_impute_median(all_predictors())
```

    ## 

    ## ── Recipe ──────────────────────────────────────────────────────────────────────

    ## 

    ## ── Inputs

    ## Number of variables by role

    ## outcome:   1
    ## predictor: 3

    ## 

    ## ── Operations

    ## • Centering for: all_numeric(), -all_outcomes()

    ## • Scaling for: all_numeric(), -all_outcomes()

    ## • Median imputation for: all_predictors()

``` r
pp |> 
  step_impute_knn(all_predictors(), neighbors = 5)
```

    ## 

    ## ── Recipe ──────────────────────────────────────────────────────────────────────

    ## 

    ## ── Inputs

    ## Number of variables by role

    ## outcome:   1
    ## predictor: 3

    ## 

    ## ── Operations

    ## • Centering for: all_numeric(), -all_outcomes()

    ## • Scaling for: all_numeric(), -all_outcomes()

    ## • K-nearest neighbor imputation for: all_predictors()

Next, I fit the linear regression model “mod_lm”. As a metric I use the
Root Mean Square Error (RMSE). it is a commonly used evaluation metric
for regression tasks. RMSE measures the average deviation between the
predicted values and the actual values, giving higher weight to larger
errors. It provides a comprehensive understanding of the model’s
performance by considering both the magnitude and direction of errors.

``` r
# Fit linear regression model
mod_lm <- caret::train(
  pp, 
  data = daily_fluxes_train |> drop_na(), 
  method = "lm",
  trControl = caret::trainControl(method = "none"),
  metric = "RMSE"
)
```

In this chunk I fit the KNN model “mod_knn”. However, it is important to
consider the number of neighbors. The selection of the number of
neighbors in a k-NN model involves a trade-off between bias and
variance. A smaller value of k, such as 1, can lead to a more flexible
and low-bias model but may also make it more sensitive to noise and
outliers. On the other hand, a larger value of k, such as 8, can help
reduce the impact of noise and outliers, leading to a more robust and
stable model with potentially lower variance.

``` r
mod_knn <- caret::train(
  pp, 
  data = daily_fluxes_train |> drop_na(), 
  method = "knn",
  trControl = caret::trainControl(method = "none"),
  tuneGrid = data.frame(k = 8),
  metric = "RMSE"
)
```

The model objects mod_lm and mod_knn can be utilized to incorporate the
fitted values into both the training and test data using the predict()
function, along with the newdata argument. The following code snippet
demonstrates the prediction step, the evaluation of prediction accuracy,
and the visualization of predicted versus observed values on both the
test and training sets. These functionalities are encapsulated within a
single function named eval_model(), which can be reused for each fitted
model object.

``` r
# make model evaluation into a function to reuse code
eval_model <- function(mod, df_train, df_test){
  
  # add predictions to the data frames
  df_train <- df_train |> 
    drop_na()
  df_train$fitted <- predict(mod, newdata = df_train)
  
  df_test <- df_test |> 
    drop_na()
  df_test$fitted <- predict(mod, newdata = df_test)
  
  # get metrics tables
  metrics_train <- df_train |> 
    yardstick::metrics(GPP_NT_VUT_REF, fitted)
  
  metrics_test <- df_test |> 
    yardstick::metrics(GPP_NT_VUT_REF, fitted)
  
  # extract values from metrics tables
  rmse_train <- metrics_train |> 
    filter(.metric == "rmse") |> 
    pull(.estimate)
  rsq_train <- metrics_train |> 
    filter(.metric == "rsq") |> 
    pull(.estimate)
  
  rmse_test <- metrics_test |> 
    filter(.metric == "rmse") |> 
    pull(.estimate)
  rsq_test <- metrics_test |> 
    filter(.metric == "rsq") |> 
    pull(.estimate)
  
  # visualise as a scatterplot
  # adding information of metrics as sub-titles
  plot_1 <- ggplot(data = df_train, aes(GPP_NT_VUT_REF, fitted)) +
    geom_point(alpha = 0.3) +
    geom_smooth(method = "lm", se = FALSE, color = "red") +
    geom_abline(slope = 1, intercept = 0, linetype = "dotted") +
    labs(subtitle = bquote( italic(R)^2 == .(format(rsq_train, digits = 2)) ~~
                            RMSE == .(format(rmse_train, digits = 3))),
         title = "Training set") +
    theme_classic()
  
  plot_2 <- ggplot(data = df_test, aes(GPP_NT_VUT_REF, fitted)) +
    geom_point(alpha = 0.3) +
    geom_smooth(method = "lm", se = FALSE, color = "red") +
    geom_abline(slope = 1, intercept = 0, linetype = "dotted") +
    labs(subtitle = bquote( italic(R)^2 == .(format(rsq_test, digits = 2)) ~~
                            RMSE == .(format(rmse_test, digits = 3))),
         title = "Test set") +
    theme_classic()
  
  out <- cowplot::plot_grid(plot_1, plot_2)
  
  return(out)
}

# linear regression model
eval_model(mod = mod_lm, df_train = daily_fluxes_train, df_test = daily_fluxes_test)
```

    ## `geom_smooth()` using formula = 'y ~ x'
    ## `geom_smooth()` using formula = 'y ~ x'

![](re_ml_01_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

``` r
# KNN
eval_model(mod = mod_knn, df_train = daily_fluxes_train, df_test = daily_fluxes_test)
```

    ## `geom_smooth()` using formula = 'y ~ x'
    ## `geom_smooth()` using formula = 'y ~ x'

![](re_ml_01_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

Interpretation of the results: Larger Difference in Evaluation on
Training and Test Set for KNN: The larger difference between the
evaluation on the training and test sets for the KNN model compared to
the linear regression model suggests that the KNN model has higher
variance. The KNN model tends to be more flexible and less biased,
allowing it to closely fit the training data. However, this flexibility
can also make it more susceptible to overfitting, resulting in a larger
discrepancy between the model’s performance on the training and test
sets.

Better Model Performance on Test Set for KNN: The evaluation on the test
set indicating a better model performance for the KNN model compared to
the linear regression model suggests that the KNN model has lower bias.
The KNN model’s ability to capture complex relationships and adapt to
the data patterns allows it to generalize well to unseen data, resulting
in improved performance on the test set. In contrast, the linear
regression model may have a higher bias, meaning it makes stronger
assumptions about the data and may not capture all the nuances present.

Positioning on the Bias-Variance Trade-Off Spectrum: Considering the
bias-variance trade-off, the KNN model is positioned towards the lower
bias and higher variance end of the spectrum. It exhibits lower bias as
it can flexibly capture complex relationships, but it has higher
variance due to its sensitivity to the local structure of the training
data. On the other hand, the linear regression model is positioned
towards the higher bias and lower variance end. It makes stronger
assumptions and has a simpler structure, resulting in higher bias but
lower variance compared to the KNN model.

In a last step, I provide a visualization of the two models

Noch nicht alles visualisiert -\> wie bringe ich test und training in
einen plot?

``` r
library(dplyr)
library(ggplot2)
library(hrbrthemes)
```

    ## NOTE: Either Arial Narrow or Roboto Condensed fonts are required to use these themes.

    ##       Please use hrbrthemes::import_roboto_condensed() to install Roboto Condensed and

    ##       if Arial Narrow is not on your system, please see https://bit.ly/arialnarrow

``` r
visualitation_test <- ggplot(daily_fluxes_test, aes(x=TIMESTAMP, y=GPP_NT_VUT_REF)) +
  geom_line( color="#69b3a2") + 
  xlab("") +
  theme_ipsum() +
  theme(axis.text.x=element_text(angle=60, hjust=1)) 
visualitation_test
```

![](re_ml_01_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

``` r
visualitation_train <- ggplot(daily_fluxes_train, aes(x=TIMESTAMP, y=GPP_NT_VUT_REF)) +
  geom_line( color="#69b3a2") + 
  xlab("") +
  theme_ipsum() +
  theme(axis.text.x=element_text(angle=60, hjust=1)) 
visualitation_train
```

    ## Warning: Removed 2 rows containing missing values (`geom_line()`).

![](re_ml_01_files/figure-gfm/unnamed-chunk-14-2.png)<!-- -->

Exercice 2:

The In this 2nd subtask, the role of k is to be considered. First
question: Hypothesis: As the number of neighbors (k) in a KNN model
approaches 1, it is expected that the R2 (coefficient of determination)
and MAE (Mean Absolute Error) evaluated on both the test and training
sets would decrease. Conversely, as k approaches N (the number of
observations in the data), it is expected that the R2 and MAE would
increase. In summary, as k approaches 1, the KNN model becomes more
overfit to the training data, leading to high training set performance
but poor generalization. As k approaches N, the model becomes less
flexible but more robust, resulting in lower training set performance
but improved generalization to unseen data.

In the second question of this exercice I put the hypothesis to the
test:

To test the hypothesis, I use the data splitted into a training and test
set above. For this I use a for loop to evaluate the variations in the
different k’s. To store the values of the k’s, I define a list
(list_values). The number of values is specified at 80. Such a high
value was chosen to show the development of the Mae in detail. In a
first step, I make a model evaluation (eval_model2) into a function to
reuse code.

``` r
# make model evaluation into a function to reuse code
eval_model2 <- function(mod, df_train, df_test){
  
  # add predictions to the data frames
  df_train <- df_train |> 
    drop_na()
  df_train$fitted <- predict(mod, newdata = df_train)
  
  df_test <- df_test |> 
    drop_na()
  df_test$fitted <- predict(mod, newdata = df_test)
  
  # get metrics tables
  metrics_train <- df_train |> 
    yardstick::metrics(GPP_NT_VUT_REF, fitted)
  
  metrics_test <- df_test |> 
    yardstick::metrics(GPP_NT_VUT_REF, fitted)
  
  # extract values from metrics tables
  rmse_train <- metrics_train |> 
    filter(.metric == "rmse") |> 
    pull(.estimate)
  rsq_train <- metrics_train |> 
    filter(.metric == "rsq") |> 
    pull(.estimate)
  
  rmse_test <- metrics_test |> 
    filter(.metric == "rmse") |> 
    pull(.estimate)
  rsq_test <- metrics_test |> 
    filter(.metric == "rsq") |> 
    pull(.estimate)
  
  return(rmse_test)
}

# linear regression model
eval_model(mod = mod_lm, df_train = daily_fluxes_train, df_test = daily_fluxes_test)
```

    ## `geom_smooth()` using formula = 'y ~ x'
    ## `geom_smooth()` using formula = 'y ~ x'

![](re_ml_01_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->

This chunk

``` r
my_array <- seq(from = 1, to = 80)

list_values <- list()

library(tidyverse)
library(recipes)

for(k in 1:length(my_array)) {
  model_temp <- caret::train(
  pp, 
  data = daily_fluxes_train |> drop_na(), 
  method = "knn",
  trControl = caret::trainControl(method = "none"),
  tuneGrid = data.frame(k = k),
  metric = "MAE"
  )
  #Eval model
  list_values[k] <- eval_model2(model_temp, daily_fluxes_train, daily_fluxes_test)
}
```

It appears that the optimal value of k in the k-nearest neighbors (KNN)
model is determined to be 26, as it yields the smallest error with a
value of 1.53. This result suggests that when considering the model
complexity, a moderate level of nearest neighbors yields the best
performance in terms of minimizing the Mean Absolute Error (MAE) metric.
The fact that the MAE reaches its minimum at k = 26 indicates that
increasing the complexity of the model up to this point leads to
improved predictions on the unseen test set. However, the larger the k
the better the prediction in the training dataset and the worse the
flexibility when I apply the model to data that is independent of the
original data.

``` r
# Create dataframe from list_values
results_df <- data.frame(k = my_array, metric_value = unlist(list_values))

# Plotting
ggplot(results_df, aes(x = k, y = metric_value)) +
  geom_line() +
  geom_point() +
  labs(x = "Model Complexity (k)", y = "Evaluation Metric") +
  ggtitle("Model Generalizability as a Function of Model Complexity")
```

![](re_ml_01_files/figure-gfm/unnamed-chunk-17-1.png)<!-- -->

In the visual representation of model generalizability as a function of
model complexity, the regions of overfitting and underfitting can be
identified by observing the behavior of the evaluation metric (e.g.,
MAE) as the model complexity increases. Overfitting region: In the case
of overfitting, the model complexity is high, which means the model is
capturing noise or random variations in the training data rather than
the underlying patterns. In the plot, you may notice that as the model
complexity increases, the evaluation metric on the training set
decreases (e.g., MAE decreases). However, at the same time, the
evaluation metric on the test/validation set may start to increase or
plateau, or the rate of improvement becomes slower. This indicates that
the model is becoming too specific to the training data and is failing
to generalize well to new, unseen data. The overfitting region is
characterized by a large gap between the evaluation metric values on the
training set and the test/validation set. Underfitting region: On the
other hand, in the case of underfitting, the model complexity is too low
to capture the underlying patterns in the data. In the plot, you may
observe that the evaluation metric on both the training set and the
test/validation set remains relatively high and does not show
significant improvement as the model complexity increases. This
indicates that the model is not able to adequately learn from the
training data and is failing to capture the underlying relationships or
patterns. The underfitting region is characterized by high evaluation
metric values on both the training set and the test/validation set.
