source("config.R")
source("R/lib_helpers.R")

make_dirs()
set.seed(CFG$seed)

pkgs <- c(
  "dplyr","tibble","ggplot2","patchwork",
  "farff",
  "recipes",
  "glmnet",
  "ranger",
  "e1071",
  "xgboost",
  "DALEX","ingredients",
  "pROC"
)

to_install <- pkgs[!pkgs %in% rownames(installed.packages())]
options(repos = c(CRAN = "https://cran.r-project.org/"))
if (length(to_install) > 0) install.packages(to_install)

invisible(lapply(pkgs, library, character.only = TRUE))

assert_file(CFG$arff_path)

cat("Setup OK. File found:", CFG$arff_path, "\n")
