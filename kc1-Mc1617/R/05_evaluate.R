#!/usr/bin/env Rscript
options(stringsAsFactors = FALSE)

suppressPackageStartupMessages({
  library(dplyr)
  library(recipes)
  library(ranger)
  library(e1071)
})

# --- packages for AUC / PR-AUC ---
pkgs <- c("pROC", "PRROC")
for (p in pkgs) {
  if (!requireNamespace(p, quietly = TRUE)) {
    install.packages(p, repos = "https://cloud.r-project.org")
  }
}
suppressPackageStartupMessages({
  library(pROC)
  library(PRROC)
})

# -------------------------
# PATHS
# -------------------------
PATH_TEST     <- file.path("data", "processed", "test.rds")
PATH_RECIPE   <- file.path("outputs", "models", "prep_recipe.rds")
PATH_GLM      <- file.path("outputs", "models", "model_glm.rds")
PATH_RF       <- file.path("outputs", "models", "model_rf.rds")
PATH_SVM      <- file.path("outputs", "models", "model_svm.rds")
PATH_POSCLASS <- file.path("outputs", "models", "pos_class.rds")

stop_if_missing <- function(p) if (!file.exists(p)) stop("Missing file: ", p, call. = FALSE)
stop_if_missing(PATH_TEST)
stop_if_missing(PATH_RECIPE)
stop_if_missing(PATH_GLM)
stop_if_missing(PATH_RF)
stop_if_missing(PATH_SVM)
stop_if_missing(PATH_POSCLASS)

dir.create("outputs/tables", recursive = TRUE, showWarnings = FALSE)

# -------------------------
# LOAD
# -------------------------
test     <- readRDS(PATH_TEST)
prep_rec <- readRDS(PATH_RECIPE)

fit_glm   <- readRDS(PATH_GLM)
fit_rf    <- readRDS(PATH_RF)
fit_svm   <- readRDS(PATH_SVM)
pos_class <- as.character(readRDS(PATH_POSCLASS))

# -------------------------
# BAKE + y_test_num
# -------------------------
test_b <- bake(prep_rec, new_data = test)

if (!"defects" %in% names(test_b)) stop("Target column 'defects' not found after bake().", call. = FALSE)

X_test_df <- test_b %>% dplyr::select(-defects)

def_raw <- test_b$defects
def_chr <- if (is.logical(def_raw)) ifelse(def_raw, "true", "false") else as.character(def_raw)

# y=1 pozitif sınıf = pos_class
y_test_num <- ifelse(def_chr == pos_class, 1L, 0L)

cat("\n=== VALIDATION CHECKS ===\n")
cat("Test positive rate (mean(y==1)) = ", round(mean(y_test_num == 1), 3),
    " | n=", length(y_test_num), " | pos_class=", pos_class, "\n", sep="")

# -------------------------
# Predict wrappers
# -------------------------
get_prob <- function(model, newdata, pos_class) {

  if (inherits(model, "glm")) {
    return(as.numeric(stats::predict(model, newdata = newdata, type = "response")))
  }

  if (inherits(model, "ranger")) {
    pr <- predict(model, data = newdata)$predictions
    pr <- as.data.frame(pr)
    if (!pos_class %in% names(pr)) stop("pos_class not in ranger probs: ", paste(names(pr), collapse=", "), call. = FALSE)
    return(as.numeric(pr[[pos_class]]))
  }

  if (inherits(model, "svm")) {
    pr <- predict(model, newdata = newdata, probability = TRUE)
    probs <- as.data.frame(attr(pr, "probabilities"))
    if (!pos_class %in% names(probs)) stop("pos_class not in svm probs: ", paste(names(probs), collapse=", "), call. = FALSE)
    return(as.numeric(probs[[pos_class]]))
  }

  stop("Unsupported model class: ", paste(class(model), collapse=", "), call. = FALSE)
}

clean_prob <- function(p) {
  p <- as.numeric(p)
  p[!is.finite(p)] <- NA_real_
  p <- pmax(pmin(p, 1), 0)
  p
}

p_glm <- clean_prob(get_prob(fit_glm, X_test_df, pos_class))
p_rf  <- clean_prob(get_prob(fit_rf,  X_test_df, pos_class))
p_svm <- clean_prob(get_prob(fit_svm, X_test_df, pos_class))

# -------------------------
# Metrics helpers
# -------------------------
clf_metrics <- function(y, p, thr = 0.50) {
  ok <- !is.na(p)
  y  <- y[ok]
  p  <- p[ok]
  pred <- as.integer(p >= thr)

  tp <- sum(pred == 1 & y == 1)
  tn <- sum(pred == 0 & y == 0)
  fp <- sum(pred == 1 & y == 0)
  fn <- sum(pred == 0 & y == 1)

  accuracy    <- (tp + tn) / (tp + tn + fp + fn)
  recall      <- if ((tp + fn) == 0) NA_real_ else tp / (tp + fn)
  specificity <- if ((tn + fp) == 0) NA_real_ else tn / (tn + fp)
  precision   <- if ((tp + fp) == 0) NA_real_ else tp / (tp + fp)
  f1          <- if (is.na(precision) || is.na(recall) || (precision + recall) == 0) NA_real_
                 else 2 * precision * recall / (precision + recall)

  tibble(tp=tp, tn=tn, fp=fp, fn=fn,
         accuracy=accuracy, recall=recall, specificity=specificity,
         precision=precision, f1=f1,
         n=length(y), pos_rate=mean(y==1))
}

roc_auc <- function(y, p) {
  ok <- !is.na(p)
  y <- y[ok]; p <- p[ok]
  r <- pROC::roc(response = factor(y, levels = c(0,1)),
                 predictor = p,
                 levels = c("0","1"),
                 direction = "<",
                 quiet = TRUE)
  as.numeric(pROC::auc(r))
}

pr_auc <- function(y, p) {
  ok <- !is.na(p)
  y <- y[ok]; p <- p[ok]
  if (sum(y==1) == 0 || sum(y==0) == 0) return(NA_real_)
  PRROC::pr.curve(scores.class0 = p[y==1],
                  scores.class1 = p[y==0],
                  curve = FALSE)$auc.integral
}

# -------------------------
# Compute + Save
# -------------------------
metrics_tbl <- bind_rows(
  clf_metrics(y_test_num, p_glm, 0.50) %>% mutate(model="Logistic Regression",
    roc_auc=roc_auc(y_test_num, p_glm), pr_auc=pr_auc(y_test_num, p_glm)),
  clf_metrics(y_test_num, p_rf,  0.50) %>% mutate(model="Random Forest",
    roc_auc=roc_auc(y_test_num, p_rf),  pr_auc=pr_auc(y_test_num, p_rf)),
  clf_metrics(y_test_num, p_svm, 0.50) %>% mutate(model="SVM",
    roc_auc=roc_auc(y_test_num, p_svm), pr_auc=pr_auc(y_test_num, p_svm))
) %>%
  select(model, roc_auc, pr_auc, accuracy, recall, specificity, precision, f1,
         tp, fp, tn, fn, n, pos_rate)

cat("\n=== TEST METRICS (threshold=0.50) ===\n")
print(metrics_tbl)

out_path <- file.path("outputs", "tables", "metrics_test.csv")
write.csv(metrics_tbl, out_path, row.names = FALSE)
cat("\nSaved metrics to: ", out_path, "\n", sep="")
