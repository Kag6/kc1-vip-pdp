source("config.R")
source("R/lib_helpers.R")
library(dplyr)

set.seed(CFG$seed)
assert_file(CFG$arff_path)

df <- farff::readARFF(CFG$arff_path) |> as_tibble()

# Target kontrol
if (!"defects" %in% names(df)) stop("Beklenen target yok: defects", call. = FALSE)

# Defects seviyelerini sabitle (false/true)
df <- df %>%
  mutate(defects = as.factor(defects)) %>%
  mutate(defects = factor(defects, levels = c("false","true")))

# Hızlı sanity check
cat("Rows:", nrow(df), "Cols:", ncol(df), "\n")
cat("Target balance:\n")
print(prop.table(table(df$defects)))

# Missing var mı?
na_count <- sum(is.na(df))
cat("Total NA:", na_count, "\n")

saveRDS(df, "data/processed/kc1_full.rds")
cat("Saved: data/processed/kc1_full.rds\n")
