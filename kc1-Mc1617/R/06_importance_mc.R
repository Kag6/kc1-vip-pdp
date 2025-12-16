#!/usr/bin/env Rscript
# ============================================================
# 06_importance_MC.R — Variable importance (VIP) for 3 models
# Models: Logistic Regression, Random Forest, SVM
# Output:
#   - outputs/models/vip_*.rds
#   - outputs/figures/vip_3models_full.png
#   - outputs/figures/vip_3models_top5.png
#   - outputs/tables/top5_*.csv
# ============================================================

source("config.R")
source("R/lib_helpers.R")

make_dirs()
set.seed(CFG$seed)

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(patchwork)
  library(recipes)
  library(DALEX)
  library(ingredients)
  library(ranger)
  library(e1071)
})

# -------------------------
# Robust CFG fallback
# -------------------------
vip_top_k <- if (!is.null(CFG$vip_top))  CFG$vip_top  else 5
vip_B     <- if (!is.null(CFG$vip_B))    CFG$vip_B    else 50
vip_N     <- if (!is.null(CFG$vip_N))    CFG$vip_N    else 1000
vip_seed  <- if (!is.null(CFG$vip_seed)) CFG$vip_seed else 1980

# -------------------------
# Paths
# -------------------------
dir.create("outputs/models",  recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/figures", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/tables",  recursive = TRUE, showWarnings = FALSE)

# -------------------------
# LOAD
# -------------------------
test      <- readRDS("data/processed/test.rds")
prep_rec  <- readRDS("outputs/models/prep_recipe.rds")

fit_glm   <- readRDS("outputs/models/model_glm.rds")
fit_rf    <- readRDS("outputs/models/model_rf.rds")
fit_svm   <- readRDS("outputs/models/model_svm.rds")
pos_class <- readRDS("outputs/models/pos_class.rds")

# -------------------------
# BAKE ONCE (models baked data ile eğitildi)
# -------------------------
test_b    <- bake(prep_rec, new_data = test)
X_test_df <- test_b %>% dplyr::select(-defects)
y_test_num <- ifelse(test_b$defects == pos_class, 1, 0)

# -------------------------
# PREDICT WRAPPERS (NO bake here)
# -------------------------
pred_glm <- function(m, newdata) {
  as.numeric(stats::predict(m, newdata = newdata, type = "response"))
}

pred_rf <- function(m, newdata) {
  pr <- predict(m, data = newdata)$predictions
  pr <- as.data.frame(pr)
  as.numeric(pr[[pos_class]])
}

pred_svm <- function(m, newdata) {
  pr <- predict(m, newdata = newdata, probability = TRUE)
  probs <- attr(pr, "probabilities")
  probs <- as.data.frame(probs)
  as.numeric(probs[[pos_class]])
}

# -------------------------
# EXPLAINERS
# -------------------------
expl_glm <- DALEX::explain(
  model = fit_glm, data = X_test_df, y = y_test_num,
  predict_function = pred_glm, label = "Logistic Regression",
  verbose = FALSE
)

expl_rf <- DALEX::explain(
  model = fit_rf, data = X_test_df, y = y_test_num,
  predict_function = pred_rf, label = "Random Forest",
  verbose = FALSE
)

expl_svm <- DALEX::explain(
  model = fit_svm, data = X_test_df, y = y_test_num,
  predict_function = pred_svm, label = "SVM",
  verbose = FALSE
)

vars_fixed <- colnames(X_test_df)
N_use <- min(vip_N, nrow(X_test_df))

# -------------------------
# VIP: LOAD if exists ELSE COMPUTE
# -------------------------
vip_files_ok <- file.exists("outputs/models/vip_logistic.rds") &&
  file.exists("outputs/models/vip_rf.rds") &&
  file.exists("outputs/models/vip_svm.rds")

if (vip_files_ok) {
  vip_glm <- readRDS("outputs/models/vip_logistic.rds")
  vip_rf  <- readRDS("outputs/models/vip_rf.rds")
  vip_svm <- readRDS("outputs/models/vip_svm.rds")
  cat("Loaded VIP objects from outputs/models/*.rds\n")
} else {
  cat("VIP rds not found -> computing via model_parts...\n")

  set.seed(vip_seed)
  vip_glm <- ingredients::model_parts(
    expl_glm,
    loss_function = loss_one_minus_auc,
    B = vip_B, type = "difference",
    variables = vars_fixed, N = N_use
  )

  set.seed(vip_seed)
  vip_rf <- ingredients::model_parts(
    expl_rf,
    loss_function = loss_one_minus_auc,
    B = vip_B, type = "difference",
    variables = vars_fixed, N = N_use
  )

  set.seed(vip_seed)
  vip_svm <- ingredients::model_parts(
    expl_svm,
    loss_function = loss_one_minus_auc,
    B = vip_B, type = "difference",
    variables = vars_fixed, N = N_use
  )

  saveRDS(vip_glm, "outputs/models/vip_logistic.rds")
  saveRDS(vip_rf,  "outputs/models/vip_rf.rds")
  saveRDS(vip_svm, "outputs/models/vip_svm.rds")
  cat("Computed + saved VIP objects.\n")
}

# =========================
# FIGURE 1: FULL VIP (all variables)
# =========================
p_full <- (plot(vip_glm) + theme_bw()) /
          (plot(vip_rf)  + theme_bw()) /
          (plot(vip_svm) + theme_bw())

ggsave("outputs/figures/vip_3models_full.png", p_full, width = 12, height = 8, dpi = 170)
cat("Saved FULL VIP plot: outputs/figures/vip_3models_full.png\n")

# =========================
# FIGURE 2: TOP-k VIP summary + CSV outputs
# =========================
vip_summary <- function(vip_obj) {
  df <- as.data.frame(vip_obj) %>%
    dplyr::filter(!variable %in% c("_full_model_", "_baseline_"))

  # most common column name in model_parts output
  if ("dropout_loss" %in% names(df)) {
    df %>%
      dplyr::group_by(label, variable) %>%
      dplyr::summarise(
        mean_dropout_loss = mean(dropout_loss),
        q10 = unname(stats::quantile(dropout_loss, 0.10)),
        q90 = unname(stats::quantile(dropout_loss, 0.90)),
        .groups = "drop"
      )
  } else if ("mean_dropout_loss" %in% names(df)) {
    df %>%
      dplyr::group_by(label, variable) %>%
      dplyr::summarise(
        mean_dropout_loss = first(mean_dropout_loss),
        q10 = NA_real_, q90 = NA_real_,
        .groups = "drop"
      )
  } else {
    stop("Unexpected model_parts columns: ", paste(names(df), collapse = ", "))
  }
}

top_n_tbl <- function(s, n) {
  s %>% dplyr::arrange(dplyr::desc(mean_dropout_loss)) %>% dplyr::slice_head(n = n)
}

s_glm <- vip_summary(vip_glm)
s_rf  <- vip_summary(vip_rf)
s_svm <- vip_summary(vip_svm)

t5_glm <- top_n_tbl(s_glm, vip_top_k)
t5_rf  <- top_n_tbl(s_rf,  vip_top_k)
t5_svm <- top_n_tbl(s_svm, vip_top_k)

write.csv(t5_glm, "outputs/tables/top5_logistic.csv", row.names = FALSE)
write.csv(t5_rf,  "outputs/tables/top5_rf.csv",       row.names = FALSE)
write.csv(t5_svm, "outputs/tables/top5_svm.csv",      row.names = FALSE)
cat("Saved Top-5 tables to outputs/tables/top5_*.csv\n")

plot_vip_top5 <- function(t5, ttl) {
  t5 <- t5 %>%
    dplyr::mutate(
      mean_p = pmax(0, mean_dropout_loss),
      q10_p  = ifelse(is.na(q10), NA_real_, pmax(0, q10)),
      q90_p  = ifelse(is.na(q90), NA_real_, pmax(0, q90))
    ) %>%
    dplyr::arrange(mean_p) %>%
    dplyr::mutate(variable = factor(variable, levels = variable))

  g <- ggplot(t5, aes(x = mean_p, y = variable)) +
    geom_col() +
    geom_vline(xintercept = 0, linetype = "dashed") +
    labs(
      title = ttl,
      x = "One minus AUC loss after permutations (mean, clipped at 0)",
      y = NULL
    ) +
    theme_bw(base_size = 12) +
    coord_cartesian(xlim = c(0, max(t5$mean_p, na.rm = TRUE) * 1.10))

  # whiskers if available
  if (!all(is.na(t5$q10_p)) && !all(is.na(t5$q90_p))) {
    if ("geom_errorbarh" %in% getNamespaceExports("ggplot2")) {
      g <- g + geom_errorbarh(aes(xmin = q10_p, xmax = q90_p), height = 0.2)
    } else {
      g <- g + geom_segment(aes(x = q10_p, xend = q90_p, y = variable, yend = variable), linewidth = 0.6)
    }
  }
  g
}

p_top5 <- plot_vip_top5(t5_glm, sprintf("Logistic Regression (Top-%d)", vip_top_k)) /
          plot_vip_top5(t5_rf,  sprintf("Random Forest (Top-%d)", vip_top_k)) /
          plot_vip_top5(t5_svm, sprintf("SVM (Top-%d)", vip_top_k))

ggsave("outputs/figures/vip_3models_top5.png", p_top5, width = 12, height = 8, dpi = 170)
cat("Saved TOP-k VIP plot: outputs/figures/vip_3models_top5.png\n")