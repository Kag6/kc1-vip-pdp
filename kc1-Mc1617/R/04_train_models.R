source("config.R")
source("R/lib_helpers.R")

library(dplyr)
library(recipes)
library(ranger)
library(e1071)

set.seed(CFG$seed)

train <- readRDS("data/processed/train.rds")
prep_rec <- readRDS("outputs/models/prep_recipe.rds")

# preprocess uygulanmış data frame
train_b <- bake(prep_rec, new_data = train)

y_train <- train_b$defects
X_train_df <- train_b %>% select(-defects)

pos_class <- "true"
saveRDS(pos_class, "outputs/models/pos_class.rds")

# 1) Logistic Regression
fit_glm <- glm(defects ~ ., data = train_b, family = binomial())
saveRDS(fit_glm, "outputs/models/model_glm.rds")

# 2) Random Forest (ranger)
mtry_use <- CFG$rf_mtry
if (is.null(mtry_use)) mtry_use <- floor(sqrt(ncol(X_train_df)))

fit_rf <- ranger(
  x = X_train_df,
  y = y_train,
  num.trees = CFG$rf_trees,
  mtry = mtry_use,
  probability = TRUE,
  importance = "none",
  seed = CFG$seed
)
saveRDS(fit_rf, "outputs/models/model_rf.rds")

# 3) SVM (e1071) - probability şart
if (is.null(CFG$svm_gamma)) {
  fit_svm <- svm(
    x = X_train_df,
    y = y_train,
    kernel = CFG$svm_kernel,
    cost = CFG$svm_cost,
    probability = TRUE,
    scale = FALSE
  )
} else {
  fit_svm <- svm(
    x = X_train_df,
    y = y_train,
    kernel = CFG$svm_kernel,
    cost = CFG$svm_cost,
    gamma = CFG$svm_gamma,
    probability = TRUE,
    scale = FALSE
  )
}
saveRDS(fit_svm, "outputs/models/model_svm.rds")


cat("Models trained & saved: GLM, RF, SVM\n")
