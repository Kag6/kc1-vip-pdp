CFG <- list(
  seed = 20251214,
  arff_path = "data/raw/kc1.arff",
  train_frac = 0.80,

  vip_top  = 5,
  vip_B    = 50,
  vip_N    = 1000,
  vip_seed = 1980,

  rf_trees = 800,
  rf_mtry  = NULL,

  svm_kernel = "radial",
  svm_cost   = 1,
  svm_gamma  = NULL,

  pdp_N     = 300,   # PDP’de kullanılacak gözlem sayısı (EMA default gibi)
  pdp_grid  = 50     # her değişken için grid sayısı
)
