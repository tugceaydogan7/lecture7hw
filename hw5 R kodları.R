### 1th QUESTION ### 

install.packages("openxlsx")
library(openxlsx)
data <- read.xlsx("C:/Users/suuser/Desktop/co2gdpdata.xlsx")
# Install necessary packages
install.packages("plm")
install.packages("lmtest")
install.packages("pcse")
install.packages("fixest")
install.packages("prais")
install.packages("stats")

# Load necessary libraries
library(plm)
library(lmtest)
library(pcse)
library(fixest)
library(prais)
library(stats)
library(plm)
library(pcse)
panel_data <- pdata.frame(data)
# Impute missing values in GDP_percapita with the mean of the available values
panel_data$GDP_percapita[is.na(panel_data$GDP_percapita)] <- mean(panel_data$GDP_percapita, na.rm = TRUE)
# Pooled OLS regression for CO2 emissions per capita as a function of GDP per capita growth rate
pooled_ols <- plm(CO2_percapita ~ GDP_percapita, data = panel_data, model = "pooling")
summary(pooled_ols)
# Perform Pesaran's (2004) test for cross-sectional dependence
pesaran_test <- pcdtest(pooled_ols)
print(pesaran_test)
install.packages("fixest")
library(fixest)
# Apply CCEMG estimator using Pesaran (2006) method
ccemg_model <- feols(CO2_percapita ~ GDP_percapita | Country.Name + Year, data = panel_data, cluster = "Country.Name")
# Display the model summary
summary(ccemg_model)
# Extract residuals from the CCEMG model
residuals_ccemg <- residuals(ccemg_model)
# Create a pdata.frame from your original panel data (using the same indexing)
panel_data_ccemg <- pdata.frame(panel_data)
# Add the residuals as a new variable in the panel data (so you can use it for testing)
panel_data_ccemg$residuals <- residuals_ccemg
# Load the plm package to access pcdtest
library(plm)
# Perform Pesaran's (2004) test for cross-sectional dependence on the residuals of CCEMG model
pesaran_test_ccemg <- pcdtest(residuals ~ 1, data = panel_data_ccemg)
# Print the test results
print(pesaran_test_ccemg)


### 2nd QUESTION ###


install.packages("openxlsx")
library(openxlsx)
solowdata= read.xlsx("C:/Users/suuser/Desktop/france_solow.xlsx")
# Install necessary packages if not already installed
install.packages("plm")      # For panel data analysis
install.packages("kernlab")  # For Kernel-based methods (K-RLS)
# Load libraries
library(plm)
library(kernlab)
# Output per Worker ve Capital per Worker calculation
solowdata$ln_rgdpo <- log(solowdata$rgdpo / solowdata$emp)  # Log(real GDP / Labor Force)
solowdata$ln_capital <- log(solowdata$cn / solowdata$emp)  # Log(Capital / Labor Force)
# log(A) (ctfp variable considers technology)
solowdata$ln_ctfp <- log(solowdata$ctfp)
#OLS modelsummary()
solow_model <- lm(ln_rgdpo ~ ln_capital + ln_ctfp, data = solowdata)
View(solow_model)
summary(solow_model)
install.packages("kernlab")
library(kernlab)
krls_model <- ksvm(ln_rgdpo ~ ln_capital + ln_ctfp, data = solowdata, kernel = "rbfdot", C = 10)
coef(krls_model)
#Comparision
X=solowdata[, c("ln_capital", "ln_ctfp")]
krls_preds <- predict(krls_model, X)
ols_preds <- predict(solow_model)
# Actual values
y <- solowdata$ln_rgdpo
# Root Mean Squared Error(RMSE) of OLS model
rmse_ols <- sqrt(mean((y - ols_preds)^2))
rmse_ols
# Root Mean Squared Error(RMSE) of Kernel Ridge Regression model
rmse_krls <- sqrt(mean((y - krls_preds)^2))
rmse_krls
#R-squared for OLS model
r_squared_ols <- summary(solow_model)$r.squared
# R-squared for Kernel Ridge Regression (via predictions)
rss_krls <- sum((y - krls_preds)^2)  # Residual Sum of Squares
tss_krls <- sum((y - mean(y))^2)     # Total Sum of Squares
r_squared_krls <- 1 - (rss_krls / tss_krls)
r_squared_ols
r_squared_krls
# Mean Absolute Error(MAE) for OLS model
mae_ols <- mean(abs(y - ols_preds))
# Mean Absolute Error(MAE) for Kernel Ridge Regression
mae_krls <- mean(abs(y - krls_preds))
mae_krls
mae_ols

### 3rd QUESTION ###

install.packages("openxlsx")
library(openxlsx)
combined_data <- read.xlsx("C:/Users/suuser/Desktop/combineddata.xlsx")
r_dates <- as.Date(combined_data$Date, origin = "1899-12-30")
combined_data$Date=r_dates
combined_data$CPI[is.na(combined_data$CPI)] <- mean(combined_data$CPI, na.rm = TRUE)
combined_data$std_dev_xr[is.na(combined_data$std_dev_xr)] <- mean(combined_data$std_dev_xr, na.rm = TRUE)
sum(is.na(combined_data))
install.packages("dplyr")
library(dplyr)
install.packages("WaveletComp")
library(WaveletComp)
install.packages("wavelets")
library(wavelets)
wavelet_results <- analyze.coherency(turkeydata, dt=1, dj=0.1, lowerPeriod = 2, upperPeriod=128)

# 1. std_dev_xr i??in dalgac??k d??n??????m??
wt_xr <- dwt(turkeydata$V1, filter = "haar", n.levels = 6)

# 2. CPI i??in dalgac??k d??n??????m??
wt_inf <- dwt(turkeydata$V2, filter = "haar", n.levels = 6)

# 3. G???? spektrumlar??n?? hesaplama (her seviyenin karesini alma)

# std_dev_xr i??in g???? spektrumu hesaplama
power_xr <- list(
  power_W1 = abs(wt_xr@W$W1)^2,
  power_W2 = abs(wt_xr@W$W2)^2,
  power_W3 = abs(wt_xr@W$W3)^2,
  power_W4 = abs(wt_xr@W$W4)^2,
  power_W5 = abs(wt_xr@W$W5)^2,
  power_W6 = abs(wt_xr@W$W6)^2,
  power_V1 = abs(wt_xr@V$V1)^2,
  power_V2 = abs(wt_xr@V$V2)^2,
  power_V3 = abs(wt_xr@V$V3)^2,
  power_V4 = abs(wt_xr@V$V4)^2,
  power_V5 = abs(wt_xr@V$V5)^2,
  power_V6 = abs(wt_xr@V$V6)^2
)

# CPI i??in g???? spektrumu hesaplama
power_inf <- list(
  power_W1 = abs(wt_inf@W$W1)^2,
  power_W2 = abs(wt_inf@W$W2)^2,
  power_W3 = abs(wt_inf@W$W3)^2,
  power_W4 = abs(wt_inf@W$W4)^2,
  power_W5 = abs(wt_inf@W$W5)^2,
  power_W6 = abs(wt_inf@W$W6)^2,
  power_V1 = abs(wt_inf@V$V1)^2,
  power_V2 = abs(wt_inf@V$V2)^2,
  power_V3 = abs(wt_inf@V$V3)^2,
  power_V4 = abs(wt_inf@V$V4)^2,
  power_V5 = abs(wt_inf@V$V5)^2,
  power_V6 = abs(wt_inf@V$V6)^2
)

# 4. G???? spektrumlar??n?? g??rselle??tirme

# Grafik d??zenini ayarlama (3 sat??r ve 4 s??tun ??eklinde)
par(mfrow = c(3, 4))

# std_dev_xr i??in grafikler
for (i in 1:6) {
  plot(power_xr[[paste0("power_W", i)]], type = "l", col = "blue", 
       main = paste("Power W", i, "(xr)"), ylab = "Power", xlab = "Time")
}

for (i in 1:6) {
  plot(power_xr[[paste0("power_V", i)]], type = "l", col = "red", 
       main = paste("Power V", i, "(xr)"), ylab = "Power", xlab = "Time")
}

# CPI i??in grafikler
for (i in 1:6) {
  plot(power_inf[[paste0("power_W", i)]], type = "l", col = "blue", 
       main = paste("Power W", i, "(inf)"), ylab = "Power", xlab = "Time")
}

for (i in 1:6) {
  plot(power_inf[[paste0("power_V", i)]], type = "l", col = "red", 
       main = paste("Power V", i, "(inf)"), ylab = "Power", xlab = "Time")
}

# Faz sonu??lar??n??n g??rselle??tirilmesi
image(wavelet_results$Phase.x, main="Phase (x)", xlab="Time", ylab="Frequency")
image(wavelet_results$Phase.y, main="Phase (y)", xlab="Time", ylab="Frequency")
# Sim??lasyon p-de??eri (istatistiksel anlaml??l??k) g??rselle??tirmesi
image(wavelet_results$Power.xy.pval, main="Power XY p-values", xlab="Time", ylab="Frequency")


# Install and load nArdl package if not installed
install.packages("nardl")
library(nardl)

# Fit the NARDL model (INF = f(XR))
model_nardl <- nardl(V1 ~ V2, data = turkeydata)

# Summary of the model
summary(model_nardl)

# Install and load the segmented package
install.packages("segmented")
library(segmented)

# Rename the variables in your data frame
colnames(turkeydata)[colnames(turkeydata) == "V1"] <- "y"
colnames(turkeydata)[colnames(turkeydata) == "V2"] <- "x"
# Fit the linear model
lm_model <- lm(y ~ x, data = turkeydata)
range(turkeydata$x)
# Choose a valid psi value within the range (e.g., 5)
psi_value <- 5
# Fit the segmented model
seg_model <- segmented(lm_model, seg.Z = ~x, psi = psi_value)
# Summary of the segmented model
summary(seg_model)
# Plotting the segmented regression model
library(ggplot2)

# Create a new data frame with predicted values from the segmented model
turkeydata$predicted <- predict(seg_model, type = "response")

# Plot the data points and the regression line
ggplot(turkeydata, aes(x = x, y = y)) +
  geom_point(color = "blue") +
  geom_line(aes(x = x, y = predicted), color = "red") +
  geom_vline(xintercept = 1.262, linetype = "dashed", color = "black") +
  theme_minimal() +
  labs(title = "Segmented Regression Model", x = "x", y = "y") +
  theme(plot.title = element_text(hjust = 0.5))


# Install the thr package
install.packages("thr")
library(thr)
# NA de??erleri olan sat??rlar?? ????karma
turkeydata <- na.omit(turkeydata)
# Eksik de??erleri medyan ile doldurma
turkeydata$x[is.na(turkeydata$x)] <- median(turkeydata$x, na.rm = TRUE)
# Threshold de??erini belirleyelim (??rne??in, theta = 5)
theta <- 5
# Threshold etkisini hesaplama
turkeydata$threshold_effect <- (turkeydata$x - theta) * (turkeydata$x > theta)
# Yeni de??i??keni kontrol etme
head(turkeydata$threshold_effect)
# Threshold regresyon modelini fit edelim
model <- lm(y ~ x + threshold_effect, data = turkeydata)
# Modelin ??zetini g??relim
summary(model)