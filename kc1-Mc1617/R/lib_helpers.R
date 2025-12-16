assert_file <- function(path) {
  if (!file.exists(path)) stop(paste0("Dosya yok: ", path, "\n→ kc1.arff dosyasını data/raw/kc1.arff içine koy."), call. = FALSE)
}

make_dirs <- function() {
  dir.create("data/raw", recursive = TRUE, showWarnings = FALSE)
  dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)
  dir.create("outputs/models", recursive = TRUE, showWarnings = FALSE)
  dir.create("outputs/tables", recursive = TRUE, showWarnings = FALSE)
  dir.create("outputs/figures", recursive = TRUE, showWarnings = FALSE)
}

# Basit metrikler + AUC
calc_metrics <- function(y_true, prob_pos, threshold = 0.5, pos_label = "true") {
  # y_true: factor with levels c("false","true")
  y_true <- droplevels(y_true)
  pred <- ifelse(prob_pos >= threshold, pos_label, setdiff(levels(y_true), pos_label)[1])
  pred <- factor(pred, levels = levels(y_true))

  tp <- sum(pred == pos_label & y_true == pos_label)
  tn <- sum(pred != pos_label & y_true != pos_label)
  fp <- sum(pred == pos_label & y_true != pos_label)
  fn <- sum(pred != pos_label & y_true == pos_label)

  acc <- (tp + tn) / (tp + tn + fp + fn)
  sens <- if ((tp + fn) == 0) NA_real_ else tp / (tp + fn)
  spec <- if ((tn + fp) == 0) NA_real_ else tn / (tn + fp)
  prec <- if ((tp + fp) == 0) NA_real_ else tp / (tp + fp)
  f1 <- if (is.na(prec) || is.na(sens) || (prec + sens) == 0) NA_real_ else 2 * prec * sens / (prec + sens)

  # AUC
  auc <- tryCatch({
    pROC::auc(pROC::roc(response = y_true, predictor = prob_pos, levels = rev(levels(y_true)), quiet = TRUE)) |> as.numeric()
  }, error = function(e) NA_real_)

  data.frame(
    accuracy = acc,
    sensitivity = sens,
    specificity = spec,
    precision = prec,
    f1 = f1,
    auc = auc
  )
}
