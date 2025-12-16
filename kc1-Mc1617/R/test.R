# --- AUC / PR-AUC + temel metrikler (05_evaluate.R) ---

if (!requireNamespace("pROC", quietly = TRUE)) install.packages("pROC")
if (!requireNamespace("PRROC", quietly = TRUE)) install.packages("PRROC")

library(pROC)
library(PRROC)
library(dplyr)

clf_metrics <- function(y, p, thr = 0.50) {
  pred <- as.integer(p >= thr)
  tp <- sum(pred == 1 & y == 1)
  tn <- sum(pred == 0 & y == 0)
  fp <- sum(pred == 1 & y == 0)
  fn <- sum(pred == 0 & y == 1)

  accuracy  <- (tp + tn) / (tp + tn + fp + fn)
  recall    <- if ((tp + fn) == 0) NA_real_ else tp / (tp + fn)          # sensitivity
  specificity <- if ((tn + fp) == 0) NA_real_ else tn / (tn + fp)
  precision <- if ((tp + fp) == 0) NA_real_ else tp / (tp + fp)
  f1        <- if (is.na(precision) || is.na(recall) || (precision + recall) == 0) NA_real_
               else 2 * precision * recall / (precision + recall)

  tibble(tp=tp, tn=tn, fp=fp, fn=fn,
         accuracy=accuracy, recall=recall, specificity=specificity,
         precision=precision, f1=f1)
}

roc_auc <- function(y, p) {
  as.numeric(pROC::auc(pROC::roc(response = y, predictor = p, quiet = TRUE)))
}

pr_auc <- function(y, p) {
  # PRROC: scores.class0 = positive class scores, scores.class1 = negative class scores
  PRROC::pr.curve(scores.class0 = p[y == 1], scores.class1 = p[y == 0], curve = FALSE)$auc.integral
}

metrics_tbl <- bind_rows(
  clf_metrics(y_test_num, p_glm, 0.50) %>% mutate(model="Logistic Regression",
    roc_auc=roc_auc(y_test_num, p_glm), pr_auc=pr_auc(y_test_num, p_glm)),
  clf_metrics(y_test_num, p_rf,  0.50) %>% mutate(model="Random Forest",
    roc_auc=roc_auc(y_test_num, p_rf),  pr_auc=pr_auc(y_test_num, p_rf)),
  clf_metrics(y_test_num, p_svm, 0.50) %>% mutate(model="SVM",
    roc_auc=roc_auc(y_test_num, p_svm), pr_auc=pr_auc(y_test_num, p_svm))
) %>% select(model, roc_auc, pr_auc, accuracy, recall, specificity, precision, f1, tp, fp, tn, fn)

cat("\n=== TEST METRICS (threshold=0.50) ===\n")
print(metrics_tbl)

dir.create("outputs/tables", recursive = TRUE, showWarnings = FALSE)
write.csv(metrics_tbl, "outputs/tables/metrics_test.csv", row.names = FALSE)
