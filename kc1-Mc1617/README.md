# KC1 Veri Setinde Permütasyon VIP ve PDP Analizi

KC1 yazılım metrikleri veri setinde (`defects` ikili sınıflandırma) üç model ailesi ile:
- Permütasyon tabanlı değişken önemliliği (VIP)
- Partial Dependence Plot (PDP) karşılaştırması
üretilmiştir.
## Veri Kaynağı (KC1)
Bu proje KC1 yazılım metrikleri veri setini kullanır.

- OpenML: https://www.openml.org/search?type=data&status=active&id=1067  (KC1)

### Ham veri dosyası
Scriptler varsayılan olarak şu konumu bekler:
- `data/raw/kc1.arff`

Dosyayı OpenML sayfasından indirip bu klasöre koyun.

## Modeller
- Logistic Regression (GLM)
- Random Forest (ranger)
- SVM (RBF, e1071)

## Çalıştırma Sırası
```bash
Rscript R/00_setup.R
Rscript R/01_load_data.R
Rscript R/02_split.R
Rscript R/03_preprocess.R
Rscript R/04_train_models.R
Rscript R/05_evaluate.R
Rscript R/06_importance_mc.R
Rscript R/07_pdp.R
