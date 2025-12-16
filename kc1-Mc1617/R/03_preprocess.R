source("config.R")
source("R/lib_helpers.R")
library(recipes)
library(dplyr)

set.seed(CFG$seed)

train <- readRDS("data/processed/train.rds")

rec <- recipe(defects ~ ., data = train) %>%
  step_zv(all_predictors()) %>%
  step_impute_median(all_numeric_predictors()) %>%
  step_normalize(all_numeric_predictors())  # scaling: glmnet/xgb için şart; ağaçlara zarar vermez

prep_rec <- prep(rec, training = train, verbose = FALSE)

saveRDS(prep_rec, "outputs/models/prep_recipe.rds")
cat("Preprocess saved: outputs/models/prep_recipe.rds\n")
