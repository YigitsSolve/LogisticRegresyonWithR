# Gerekli paketleri yükleyin
install.packages(c("readr", "dplyr", "ggplot2", "caret", "randomForest", "e1071", "MASS", "xgboost", "gridExtra"))

# Gerekli kütüphaneleri yükleyin
library(readr)       # Veri okuma
library(dplyr)       # Veri işleme
library(ggplot2)     # Veri görselleştirme
library(caret)       # Makine öğrenimi için yardımcı fonksiyonlar
library(randomForest) # Rastgele orman modeli
library(e1071)       # Destek vektör makineleri için
library(MASS)        # İstatistiksel analiz için
library(xgboost)     # Gradient boosting modeli
library(gridExtra)   # Grafik düzenleme

# Veri setini oku ve kontrol et
loan_data_set <- read_csv("C:/.../Desktop/loan_data_set.csv")
str(loan_data_set)   # Veri setinin yapısını kontrol et

# Kategorik değişkenlerin dağılımını görselleştir
categorical_vars <- c("Gender", "Married", "Dependents", "Education", "Self_Employed", "Credit_History", "Property_Area", "Loan_Status")
plots <- list()

for (var in categorical_vars) {
  p <- ggplot(loan_data_set, aes_string(x = var)) +
    geom_bar(fill = "skyblue") +
    labs(title = paste(var, "Distribution")) +
    theme_minimal()
  
  plots[[length(plots) + 1]] <- p
}

# Tüm grafikleri bir araya getirip görselleştirme
grid.arrange(grobs = plots, ncol = 2)

# Aykırı gözlemleri kaldır
loan_data_set <- loan_data_set %>%
  filter(ApplicantIncome <= quantile(ApplicantIncome, 0.99, na.rm = TRUE),
         CoapplicantIncome <= quantile(CoapplicantIncome, 0.99, na.rm = TRUE),
         LoanAmount <= quantile(LoanAmount, 0.99, na.rm = TRUE))

# Eksik verileri doldur
loan_data_set <- loan_data_set %>%
  mutate(Gender = if_else(is.na(Gender), "Male", Gender),           # Cinsiyet eksik değerleri dolduruluyor
         Married = if_else(is.na(Married), "Yes", Married),         # Evlilik durumu eksik değerleri dolduruluyor
         Dependents = if_else(is.na(Dependents), "0", Dependents), # Bağımlılar eksik değerleri dolduruluyor
         Self_Employed = if_else(is.na(Self_Employed), "No", Self_Employed), # Kendi işinde çalışma durumu eksik değerleri dolduruluyor
         Credit_History = if_else(is.na(Credit_History), 1, Credit_History), # Kredi geçmişi eksik değerleri dolduruluyor
         Loan_Amount_Term = if_else(is.na(Loan_Amount_Term), 360, Loan_Amount_Term), # Kredi vadesi eksik değerleri dolduruluyor
         LoanAmount = if_else(is.na(LoanAmount), mean(LoanAmount, na.rm = TRUE), LoanAmount)) # Kredi miktarı eksik değerleri dolduruluyor

# İşlenmiş Verinin Kategorik değişkenlerin dağılımını görselleştir
categorical_vars <- c("Gender", "Married", "Dependents", "Education", "Self_Employed", "Credit_History", "Property_Area", "Loan_Status")
plots <- list()

for (var in categorical_vars) {
  p <- ggplot(loan_data_set, aes_string(x = var)) +
    geom_bar(fill = "skyblue") +
    labs(title = paste(var, "Distribution")) +
    theme_minimal()
  
  plots[[length(plots) + 1]] <- p
}

# İşlenmiş Verinin Tüm grafikleri bir araya getirip görselleştirme
grid.arrange(grobs = plots, ncol = 2)

# Kategorik değişkenleri ikili dönüşümlere dönüştür
loan_data_set[categorical_vars] <- lapply(loan_data_set[categorical_vars], as.factor)

# Aykırı gözlemleri kaldır ve dönüşüm yap
loan_data_set$ApplicantIncome <- sqrt(loan_data_set$ApplicantIncome)       # Başvuru sahibinin geliri karekök ile dönüştürülüyor
loan_data_set$CoapplicantIncome <- sqrt(loan_data_set$CoapplicantIncome) # Ortak başvuru sahibinin geliri karekök ile dönüştürülüyor
loan_data_set$LoanAmount <- sqrt(loan_data_set$LoanAmount)               # Kredi miktarı karekök ile dönüştürülüyor

# Eğitim ve test setlerini ayır
set.seed(123)
train_index <- sample(1:nrow(loan_data_set), 0.8 * nrow(loan_data_set))
X_train <- loan_data_set[train_index, -which(names(loan_data_set) == "Loan_Status")]
y_train <- loan_data_set$Loan_Status[train_index]
X_test <- loan_data_set[-train_index, -which(names(loan_data_set) == "Loan_Status")]
y_test <- loan_data_set$Loan_Status[-train_index]

# Lojistik regresyon modelini oluştur
LRclassifier <- glm(y_train ~ ., data = X_train, family = "binomial", maxit = 100000) # Lojistik regresyon modeli oluşturuluyor

# Tahminler yap
y_pred <- predict(LRclassifier, newdata = X_test, type = "response") # Test seti için tahminler yapılıyor

# Karar sınırını belirle
y_pred_class <- ifelse(y_pred > 0.5, "Y", "N") # Tahminler için karar sınırı belirleniyor

# Karışıklık matrisi oluştur
confusion_matrix <- confusionMatrix(as.factor(y_pred_class), as.factor(y_test)) # Karışıklık matrisi oluşturuluyor

# Sonuçları görüntüle
print(confusion_matrix) # Karışıklık matrisi görüntüleniyor
