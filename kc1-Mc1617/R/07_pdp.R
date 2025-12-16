#!/usr/bin/env Rscript
# ============================================================
# 07_pdp.R — Partial Dependence Plots (3 models, KC1)
# - White background outputs (theme + ggsave(bg="white"))
# - Robust across DrWhy versions (model_profile export issues)
# - Outputs:
#   1) outputs/figures/pdp_<var>.png  (overlay 3 models)
#   2) outputs/figures/pdp_panel.png  (stacked panel)
#   3) outputs/figures/pdp_aggregated_profiles.png (all vars, 1 figure)
#   4) outputs/figures/fig17_1_like_cp_pd_rf_<var>.png (CP vs CP+PD, RF, 25 obs)
# Optional:
#   - CP+PD profiles per variable (MAKE_CP_PROFILES)
#   - Grouped PDP (MAKE_GROUPED_PD)
# ============================================================

options(stringsAsFactors = FALSE)

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
# CONFIG
# -------------------------
SEED <- 2025
PDP_N           <- 400
PDP_GRID_POINTS <- 101

# EMA-like extras
MAKE_CP_PROFILES <- FALSE
MAKE_GROUPED_PD  <- FALSE
GROUP_VAR        <- "loc_bin"
GROUP_K          <- 6
GROUPED_MODEL    <- "Random Forest" # "Logistic Regression" | "Random Forest" | "SVM"

# Fig 17.1-like (RF) settings
CP_N_EXAMPLE <- 25  # 25 random observations

# -------------------------
# PATHS (consistent with 06)
# -------------------------
PATH_TEST     <- file.path("data", "processed", "test.rds")
PATH_RECIPE   <- file.path("outputs", "models", "prep_recipe.rds")
PATH_GLM      <- file.path("outputs", "models", "model_glm.rds")
PATH_RF       <- file.path("outputs", "models", "model_rf.rds")
PATH_SVM      <- file.path("outputs", "models", "model_svm.rds")
PATH_POSCLASS <- file.path("outputs", "models", "pos_class.rds")

PATH_TOP5_DIR <- file.path("outputs", "tables")
PATH_FIG_DIR  <- file.path("outputs", "figures")

dir_create <- function(path) if (!dir.exists(path)) dir.create(path, recursive = TRUE, showWarnings = FALSE)
stop_if_missing <- function(p) if (!file.exists(p)) stop("Missing file: ", p, call. = FALSE)

dir_create(PATH_FIG_DIR)

stop_if_missing(PATH_TEST)
stop_if_missing(PATH_RECIPE)
stop_if_missing(PATH_GLM)
stop_if_missing(PATH_RF)
stop_if_missing(PATH_SVM)
stop_if_missing(PATH_POSCLASS)

set.seed(SEED)

# -------------------------
# WHITE THEME (2nd photo vibe: clean, light grid, white bg)
# -------------------------
white_theme <- function(base_size = 16) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.background  = element_rect(fill = "white", colour = NA),
      panel.background = element_rect(fill = "white", colour = NA),
      legend.position  = "top",
      plot.title       = element_text(face = "bold"),
      strip.text       = element_text(face = "bold")
    )
}

apply_white <- function(p, base_size = 16) p + white_theme(base_size)

# -------------------------
# Resolve model_profile / variable_profile robustly (export differences)
# -------------------------
resolve_profile_fun <- function() {
  ing_exports   <- try(getNamespaceExports("ingredients"), silent = TRUE)
  dalex_exports <- try(getNamespaceExports("DALEX"), silent = TRUE)

  # Prefer model_profile
  if (!inherits(ing_exports, "try-error") && "model_profile" %in% ing_exports)
    return(get("model_profile", envir = asNamespace("ingredients")))
  if (!inherits(dalex_exports, "try-error") && "model_profile" %in% dalex_exports)
    return(get("model_profile", envir = asNamespace("DALEX")))

  # Not exported but exists inside namespace
  if (exists("model_profile", envir = asNamespace("ingredients"), inherits = FALSE))
    return(get("model_profile", envir = asNamespace("ingredients")))
  if (exists("model_profile", envir = asNamespace("DALEX"), inherits = FALSE))
    return(get("model_profile", envir = asNamespace("DALEX")))

  # Older name: variable_profile
  if (!inherits(ing_exports, "try-error") && "variable_profile" %in% ing_exports)
    return(get("variable_profile", envir = asNamespace("ingredients")))
  if (!inherits(dalex_exports, "try-error") && "variable_profile" %in% dalex_exports)
    return(get("variable_profile", envir = asNamespace("DALEX")))

  if (exists("variable_profile", envir = asNamespace("ingredients"), inherits = FALSE))
    return(get("variable_profile", envir = asNamespace("ingredients")))
  if (exists("variable_profile", envir = asNamespace("DALEX"), inherits = FALSE))
    return(get("variable_profile", envir = asNamespace("DALEX")))

  stop("Could not find model_profile/variable_profile in ingredients/DALEX. Update packages.", call. = FALSE)
}

PROFILE_FUN <- resolve_profile_fun()

call_profile <- function(explainer, var, type = "partial", N = NULL, grid_points = 101, groups = NULL, k = NULL) {
  # Try modern signature first
  res <- try(
    PROFILE_FUN(
      explainer   = explainer,
      variables   = var,
      type        = type,
      N           = N,
      grid_points = grid_points,
      groups      = groups,
      k           = k
    ),
    silent = TRUE
  )
  if (!inherits(res, "try-error")) return(res)

  # Retry without grid_points/k (older versions)
  res <- try(
    PROFILE_FUN(
      explainer = explainer,
      variables = var,
      type      = type,
      N         = N,
      groups    = groups
    ),
    silent = TRUE
  )
  if (!inherits(res, "try-error")) return(res)

  # Minimal fallback
  res <- try(
    PROFILE_FUN(
      explainer = explainer,
      variables = var
    ),
    silent = TRUE
  )
  if (!inherits(res, "try-error")) return(res)

  stop("Profile call failed for variable: ", var, "\nLast error:\n", as.character(res), call. = FALSE)
}

# -------------------------
# Resolve ceteris_paribus robustly (for Fig 17.1-like)
# -------------------------
resolve_cp_fun <- function() {
  ing_exports <- try(getNamespaceExports("ingredients"), silent = TRUE)
  if (!inherits(ing_exports, "try-error") && "ceteris_paribus" %in% ing_exports)
    return(get("ceteris_paribus", envir = asNamespace("ingredients")))
  if (exists("ceteris_paribus", envir = asNamespace("ingredients"), inherits = FALSE))
    return(get("ceteris_paribus", envir = asNamespace("ingredients")))
  # Older/alternate: predict_profile (rare)
  if (!inherits(ing_exports, "try-error") && "predict_profile" %in% ing_exports)
    return(get("predict_profile", envir = asNamespace("ingredients")))
  if (exists("predict_profile", envir = asNamespace("ingredients"), inherits = FALSE))
    return(get("predict_profile", envir = asNamespace("ingredients")))

  stop("Could not find ceteris_paribus (or predict_profile) in ingredients. Update packages.", call. = FALSE)
}

CP_FUN <- resolve_cp_fun()

call_cp <- function(explainer, obs_df, var) {
  # Most versions accept observation=<data.frame> and variables=<char vec>
  res <- try(
    CP_FUN(
      explainer   = explainer,
      observation = obs_df,
      variables   = var
    ),
    silent = TRUE
  )
  if (!inherits(res, "try-error")) return(res)

  # Retry minimal (some variants only need explainer + observation)
  res <- try(
    CP_FUN(
      explainer   = explainer,
      observation = obs_df
    ),
    silent = TRUE
  )
  if (!inherits(res, "try-error")) return(res)

  stop("CP call failed for variable: ", var, "\nLast error:\n", as.character(res), call. = FALSE)
}

# -------------------------
# LOAD + BAKE ONCE
# -------------------------
test     <- readRDS(PATH_TEST)
prep_rec <- readRDS(PATH_RECIPE)

fit_glm   <- readRDS(PATH_GLM)
fit_rf    <- readRDS(PATH_RF)
fit_svm   <- readRDS(PATH_SVM)
pos_class <- as.character(readRDS(PATH_POSCLASS))

test_b <- bake(prep_rec, new_data = test)
if (!"defects" %in% names(test_b)) stop("Target column 'defects' not found after bake().", call. = FALSE)

X_test_df <- test_b %>% dplyr::select(-defects)

def_raw <- test_b$defects
def_chr <- if (is.logical(def_raw)) ifelse(def_raw, "true", "false") else as.character(def_raw)
y_test_num <- ifelse(def_chr == pos_class, 1L, 0L)

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

# -------------------------
# PROB DIAGNOSTICS (your control item)
# -------------------------
prob_diag <- function(name, p) {
  qs <- as.numeric(stats::quantile(p, probs = c(0, .01, .05, .5, .95, .99, 1), na.rm = TRUE))
  tibble(
    model = name,
    q0 = qs[1], q01 = qs[2], q05 = qs[3], q50 = qs[4], q95 = qs[5], q99 = qs[6], q100 = qs[7],
    p_lt_0.01 = mean(p < 0.01, na.rm = TRUE),
    p_gt_0.99 = mean(p > 0.99, na.rm = TRUE)
  )
}

p_glm <- get_prob(fit_glm, X_test_df, pos_class)
p_rf  <- get_prob(fit_rf,  X_test_df, pos_class)
p_svm <- get_prob(fit_svm, X_test_df, pos_class)

diag_tbl <- bind_rows(
  prob_diag("Logistic Regression", p_glm),
  prob_diag("Random Forest",       p_rf),
  prob_diag("SVM",                 p_svm)
)

cat("\n=== Probability diagnostics (pos_class=", pos_class, ") ===\n", sep = "")
print(diag_tbl)
# -------------------------
# VALIDATION CHECKS (report)
# -------------------------
cat("\n=== VALIDATION CHECKS ===\n")

# 1) Class ratio
class_ratio <- mean(y_test_num == 1, na.rm = TRUE)
cat("Test positive rate (mean(y==1)) = ", round(class_ratio, 4), " | n=", length(y_test_num), "\n", sep="")

# 2) Confusion matrices @0.50 threshold (mapping sanity)
pred_label <- function(p, thr=0.5) ifelse(p >= thr, 1L, 0L)

cm_glm <- table(pred = pred_label(p_glm, 0.5), true = y_test_num)
cm_rf  <- table(pred = pred_label(p_rf,  0.5), true = y_test_num)
cm_svm <- table(pred = pred_label(p_svm, 0.5), true = y_test_num)

cat("\nConfusion Matrix @0.50 (GLM):\n"); print(cm_glm)
cat("\nConfusion Matrix @0.50 (RF):\n");  print(cm_rf)
cat("\nConfusion Matrix @0.50 (SVM):\n"); print(cm_svm)

# 3) Show 5 example rows (try to include both classes)
set.seed(SEED)
idx_pos <- which(y_test_num == 1)
idx_neg <- which(y_test_num == 0)

pick <- c(
  if (length(idx_pos) > 0) sample(idx_pos, min(3, length(idx_pos))) else integer(0),
  if (length(idx_neg) > 0) sample(idx_neg, min(2, length(idx_neg))) else integer(0)
)
if (length(pick) < 5) {
  pick <- unique(c(pick, sample(seq_along(y_test_num), 5 - length(pick))))
}

ex_tbl <- dplyr::tibble(
  row_id = pick,
  y_true = y_test_num[pick],
  p_glm  = round(p_glm[pick], 4),
  p_rf   = round(p_rf[pick], 4),
  p_svm  = round(p_svm[pick], 4)
) %>%
  dplyr::mutate(
    pred_glm_05 = pred_label(p_glm, 0.5),
    pred_rf_05  = pred_label(p_rf,  0.5),
    pred_svm_05 = pred_label(p_svm, 0.5)
  )

cat("\n5 example rows (true label + 3 model probabilities):\n")
print(ex_tbl)

# 4) LR overconfidence indicator (already in diag_tbl)
lr_pgt099 <- diag_tbl %>% dplyr::filter(model == "Logistic Regression") %>% dplyr::pull(p_gt_0.99)
cat("\nLR p(p>0.99) = ", round(lr_pgt099, 4), "\n", sep="")

# -------------------------
# Explain
# -------------------------
expl_glm <- DALEX::explain(
  model = fit_glm, data = X_test_df, y = y_test_num,
  predict_function = function(m, newdata) get_prob(m, newdata, pos_class),
  label = "Logistic Regression", verbose = FALSE
)

expl_rf <- DALEX::explain(
  model = fit_rf, data = X_test_df, y = y_test_num,
  predict_function = function(m, newdata) get_prob(m, newdata, pos_class),
  label = "Random Forest", verbose = FALSE
)

expl_svm <- DALEX::explain(
  model = fit_svm, data = X_test_df, y = y_test_num,
  predict_function = function(m, newdata) get_prob(m, newdata, pos_class),
  label = "SVM", verbose = FALSE
)

# -------------------------
# Top-5 union -> numeric only
# -------------------------
top5_files <- list.files(PATH_TOP5_DIR, pattern = "^top5_.*\\.csv$", full.names = TRUE)
if (length(top5_files) == 0) {
  stop("No top5 CSV files found in: ", PATH_TOP5_DIR,
       "\nExpected: outputs/tables/top5_logistic.csv, top5_rf.csv, top5_svm.csv",
       call. = FALSE)
}

read_top5_vars <- function(fp) {
  df <- read.csv(fp, stringsAsFactors = FALSE)
  if (!"variable" %in% names(df)) stop("top5 file missing 'variable' col: ", fp, call. = FALSE)
  df$variable
}

top_vars <- unique(unlist(lapply(top5_files, read_top5_vars)))
top_vars <- top_vars[top_vars %in% names(X_test_df)]

cont_vars <- top_vars[vapply(X_test_df[top_vars], is.numeric, logical(1))]
if (length(cont_vars) == 0) stop("No numeric variables found among Top-5 union.", call. = FALSE)

cat("\nPDP variables (numeric union of Top-5): ", paste(cont_vars, collapse = ", "), "\n", sep = "")

safe_name <- function(x) gsub("[^A-Za-z0-9_]+", "_", x)

# -------------------------
# Optional grouping
# -------------------------
make_groups <- function(df, group_var = GROUP_VAR, k = GROUP_K) {
  if (group_var %in% names(df)) return(df)
  if (group_var == "loc_bin" && "loc" %in% names(df)) {
    df$loc_bin <- ggplot2::cut_number(df$loc, n = k) %>% as.factor()
    return(df)
  }
  num_cols <- names(df)[vapply(df, is.numeric, logical(1))]
  if (length(num_cols) > 0) {
    x <- num_cols[1]
    df[[group_var]] <- ggplot2::cut_number(df[[x]], n = k) %>% as.factor()
    return(df)
  }
  stop("Cannot create GROUP_VAR='", group_var, "'.", call. = FALSE)
}

if (MAKE_GROUPED_PD) {
  X_test_df <- make_groups(X_test_df, GROUP_VAR, GROUP_K)
  expl_glm$data <- X_test_df
  expl_rf$data  <- X_test_df
  expl_svm$data <- X_test_df
}

# -------------------------
# PDP loops (per variable overlay)
# -------------------------
N_use <- if (is.null(PDP_N)) NULL else min(PDP_N, nrow(X_test_df))
out_panel <- list()

for (var in cont_vars) {

  cat("Computing PDP for: ", var, "\n", sep = "")

  pdp_glm <- call_profile(expl_glm, var, type = "partial", N = N_use, grid_points = PDP_GRID_POINTS)
  pdp_rf  <- call_profile(expl_rf,  var, type = "partial", N = N_use, grid_points = PDP_GRID_POINTS)
  pdp_svm <- call_profile(expl_svm, var, type = "partial", N = N_use, grid_points = PDP_GRID_POINTS)

  g <- plot(pdp_glm, pdp_rf, pdp_svm) +
    ggtitle(paste0("PDP: ", var, " (3 models)")) +
    ylab(paste0("Partial Dependence (predicted P(defects=", pos_class, "))"))

  g <- apply_white(g, base_size = 16)

  out_path <- file.path(PATH_FIG_DIR, paste0("pdp_", safe_name(var), ".png"))
  ggsave(out_path, g, width = 10, height = 5, dpi = 160, bg = "white")

  out_panel[[var]] <- g

  # CP+PD profiles per variable (optional)
  if (MAKE_CP_PROFILES) {
    prof_dir <- file.path(PATH_FIG_DIR, "pdp_profiles")
    dir_create(prof_dir)

    g_glm <- apply_white(plot(pdp_glm, geom = "profiles") +
                           ggtitle(paste0("CP+PD: ", var, " | Logistic Regression")), 16)
    g_rf  <- apply_white(plot(pdp_rf,  geom = "profiles") +
                           ggtitle(paste0("CP+PD: ", var, " | Random Forest")), 16)
    g_svm <- apply_white(plot(pdp_svm, geom = "profiles") +
                           ggtitle(paste0("CP+PD: ", var, " | SVM")), 16)

    ggsave(file.path(prof_dir, paste0("cp_pd_glm_", safe_name(var), ".png")), g_glm, width = 10, height = 5, dpi = 160, bg = "white")
    ggsave(file.path(prof_dir, paste0("cp_pd_rf_",  safe_name(var), ".png")), g_rf,  width = 10, height = 5, dpi = 160, bg = "white")
    ggsave(file.path(prof_dir, paste0("cp_pd_svm_", safe_name(var), ".png")), g_svm, width = 10, height = 5, dpi = 160, bg = "white")
  }

  # Grouped PD (optional)
  if (MAKE_GROUPED_PD) {
    grp_dir <- file.path(PATH_FIG_DIR, "pdp_grouped")
    dir_create(grp_dir)

    expl_map <- list(
      "Logistic Regression" = expl_glm,
      "Random Forest"       = expl_rf,
      "SVM"                 = expl_svm
    )
    expl_g <- expl_map[[GROUPED_MODEL]]
    if (is.null(expl_g)) stop("Invalid GROUPED_MODEL: ", GROUPED_MODEL, call. = FALSE)

    pdp_g <- call_profile(expl_g, var, type = "partial", N = N_use, grid_points = PDP_GRID_POINTS, groups = GROUP_VAR)

    gg <- plot(pdp_g) +
      ggtitle(paste0("Grouped PDP: ", var, " | ", GROUPED_MODEL, " | groups=", GROUP_VAR)) +
      ylab(paste0("Partial Dependence (predicted P(defects=", pos_class, "))"))

    gg <- apply_white(gg, 16)

    outg <- file.path(grp_dir, paste0("grouped_", safe_name(GROUPED_MODEL), "_", safe_name(var), "_by_", safe_name(GROUP_VAR), ".png"))
    ggsave(outg, gg, width = 11, height = 6, dpi = 160, bg = "white")
  }
}

# -------------------------
# Panel (stacked)
# -------------------------
panel_path <- file.path(PATH_FIG_DIR, "pdp_panel.png")
panel_plot <- patchwork::wrap_plots(out_panel, ncol = 1) + plot_annotation(title = "PDP Panel (Top-5 union, 3 models)")
panel_plot <- apply_white(panel_plot, 16)
ggsave(panel_path, panel_plot, width = 8, height = 3.2 * length(out_panel), dpi = 160, bg = "white")

# -------------------------
# Aggregated Profiles (single figure like your 2nd screenshot)
# -------------------------
cat("\nBuilding aggregated profiles figure...\n")

pd_glm_all <- call_profile(expl_glm, cont_vars, type = "partial", N = N_use, grid_points = PDP_GRID_POINTS)
pd_rf_all  <- call_profile(expl_rf,  cont_vars, type = "partial", N = N_use, grid_points = PDP_GRID_POINTS)
pd_svm_all <- call_profile(expl_svm, cont_vars, type = "partial", N = N_use, grid_points = PDP_GRID_POINTS)

g_agg <- plot(pd_glm_all, pd_rf_all, pd_svm_all) +
  ggtitle("Aggregated Profiles (Top-5 union, 3 models)") +
  ylab(paste0("prediction = P(defects=", pos_class, ")"))

g_agg <- apply_white(g_agg, 18)
ggsave(file.path(PATH_FIG_DIR, "pdp_aggregated_profiles.png"), g_agg, width = 12, height = 6, dpi = 170, bg = "white")

# -------------------------
# Fig 17.1-like output (RF): CP vs CP+PD for 25 observations
# -------------------------
# -------------------------
# Fig 17.1-like output (RF): CP vs CP+PD for 25 observations
# (NO ceteris_paribus dependency; use model_profile result)
# -------------------------
cat("\nBuilding Fig 17.1-like CP/PD example (Random Forest)...\n")

pick_rf_var <- function() {
  rf_fp <- file.path(PATH_TOP5_DIR, "top5_rf.csv")
  if (file.exists(rf_fp)) {
    df <- read.csv(rf_fp, stringsAsFactors = FALSE)
    if ("variable" %in% names(df)) {
      cand <- df$variable[df$variable %in% names(X_test_df)]
      cand_num <- cand[vapply(X_test_df[cand], is.numeric, logical(1))]
      if (length(cand_num) > 0) return(cand_num[1])
    }
  }
  cont_vars[1]
}

var_ex <- pick_rf_var()

set.seed(SEED)
idx <- sample(seq_len(nrow(X_test_df)), size = min(CP_N_EXAMPLE, nrow(X_test_df)))
obs25 <- X_test_df[idx, , drop = FALSE]

# PD + CP profiles together (from model_profile)
pdp_rf_25 <- call_profile(
  expl_rf, var_ex,
  type = "partial",
  N = min(CP_N_EXAMPLE, nrow(X_test_df)),
  grid_points = PDP_GRID_POINTS
)

res <- pdp_rf_25$result

# Robust column picks (ingredients versions differ)
vcol  <- if ("_vname_" %in% names(res)) "_vname_" else NULL
xcol  <- if ("_x_"     %in% names(res)) "_x_"     else var_ex
ycol  <- if ("_yhat_"  %in% names(res)) "_yhat_"  else "prediction"
idcol <- if ("_ids_"   %in% names(res)) "_ids_"   else NULL
labcol <- if ("_label_" %in% names(res)) "_label_" else NULL

# Filter to our variable if available
df_var <- res
if (!is.null(vcol)) df_var <- df_var[df_var[[vcol]] == var_ex, , drop = FALSE]

# CP rows: those with ids present (best guess across versions)
df_cp <- df_var
if (!is.null(idcol)) df_cp <- df_var[!is.na(df_var[[idcol]]), , drop = FALSE]

# Points: original observation value + prediction
p0 <- get_prob(fit_rf, obs25, pos_class)
x0 <- obs25[[var_ex]]
df_pts <- data.frame(x = x0, y = p0)

# Left panel: CP-only + blue dots
g_cp <- ggplot(df_cp, aes(x = .data[[xcol]], y = .data[[ycol]])) +
  geom_line(aes(group = .data[[idcol]]), linewidth = 0.4, alpha = 0.35) +
  geom_point(data = df_pts, aes(x = x, y = y), inherit.aes = FALSE, size = 1.8) +
  ggtitle(paste0("CP profiles (", CP_N_EXAMPLE, " obs) | RF | ", var_ex)) +
  ylab(paste0("prediction = P(defects=", pos_class, ")")) +
  xlab(var_ex)

g_cp <- apply_white(g_cp, 16)

# Right panel: CP (grey) + PD (blue) via built-in plot
g_cp_pd <- plot(pdp_rf_25, geom = "profiles") +
  ggtitle(paste0("CP (grey) + PD (blue) | RF | ", var_ex)) +
  ylab(paste0("prediction = P(defects=", pos_class, ")"))

g_cp_pd <- apply_white(g_cp_pd, 16)

g_fig17 <- g_cp + g_cp_pd + plot_annotation(
  title = paste0("Fig 17.1-like: CP and PD profiles (Random Forest, ", CP_N_EXAMPLE, " observations)")
)
g_fig17 <- apply_white(g_fig17, 16)

fig17_path <- file.path(PATH_FIG_DIR, paste0("fig17_1_like_cp_pd_rf_", safe_name(var_ex), ".png"))
ggsave(fig17_path, g_fig17, width = 14, height = 6, dpi = 170, bg = "white")


cat("\nDone. Figures in: ", PATH_FIG_DIR, "\n", sep = "")
