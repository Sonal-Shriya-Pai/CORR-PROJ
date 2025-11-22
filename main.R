

# !!! please read Code Help.Rmd file BEFORE executing the code !!! 


cat("\n===== Data Input and pre-processing =====\n")
# --------------Data Input and pre-processing---------------
library(readxl)
library(dplyr)
library(caret)
library(randomForest)
library(ggplot2)
library(xgboost)
library(openxlsx)
df = read.xlsx("Data/Corrosion_of_New_Copper_Surfaces_in_Drinking_Water.xlsx", sheet="Figure 1 f5 and D5")
df
str(df)
head(df)
tail(df)
summary(df)
which(is.na(df)) #no missing values, all are read
class(df)
colnames(df)

# grouping data into condition basis 

library(dplyr)

condA <- df %>% 
  dplyr::select(1:3) %>%
  dplyr::rename(Time_min =1 ,f5_Hz =2 ,D5 = 3) %>%
  na.omit()
condA

condB <- df %>% 
  dplyr::select(4:6) %>%
  dplyr::rename(Time_min =1 ,f5_Hz =2 ,D5 = 3) %>%
  na.omit()
condB

condC <- df %>% 
  dplyr::select(7:9) %>%
  dplyr::rename(Time_min =1 ,f5_Hz =2 ,D5 = 3) %>%
  na.omit()
condC

condD <- df %>% 
  dplyr::select(10:12) %>%
  dplyr::rename(Time_min =1 ,f5_Hz =2 ,D5 = 3) %>%
  na.omit()
condD

condE <- df %>% 
  dplyr::select(13:15) %>%
  dplyr::rename(Time_min =1 ,f5_Hz =2 ,D5 = 3) %>%
  na.omit()
condE

# making baseline

condA <- condA %>% 
  mutate(
    delta_f5 = f5_Hz - first(f5_Hz), delta_D5 = D5 - first(D5)
  )
condA

condB <- condB %>% 
  mutate(
    delta_f5 = f5_Hz - first(f5_Hz), delta_D5 = D5 - first(D5)
  )
condB

condC <- condC %>% 
  mutate(
    delta_f5 = f5_Hz - first(f5_Hz), delta_D5 = D5 - first(D5)
  )
condC

condD <- condD %>% 
  mutate(
    delta_f5 = f5_Hz - first(f5_Hz), delta_D5 = D5 - first(D5)
  )
condD

condE <- condE %>% 
  mutate(
    delta_f5 = f5_Hz - first(f5_Hz), delta_D5 = D5 - first(D5)
  )
condE

# Sauerbrey rule, delta_m = -(17.7* delta_f5)/ 5 , in ng/ cm2

C = 17.7
n = 5

condA <- condA %>%
  mutate(delta_m = -(C * delta_f5)/ n)
condA

condB <- condB %>%
  mutate(delta_m = -(C * delta_f5)/ n)
condB

condC <- condC %>%
  mutate(delta_m = -(C * delta_f5)/ n)
condC

condD <- condD %>%
  mutate(delta_m = -(C * delta_f5)/ n)
condD

condE <- condE %>%
  mutate(delta_m = -(C * delta_f5)/ n)
condE


# Converting mass into thickness, density is rho= 8.96e9 ng/cm3

rho <- 8.96e9

condA <- condA %>%
  mutate(thickness_nm = (delta_m/ rho)* 1e7)  #1cm = 1e7 nm
condA

condB <- condB %>%
  mutate(thickness_nm = (delta_m/ rho)* 1e7)  #1cm = 1e7 nm
condB

condC <- condC %>%
  mutate(thickness_nm = (delta_m/ rho)* 1e7)  #1cm = 1e7 nm
condC

condD <- condD %>%
  mutate(thickness_nm = (delta_m/ rho)* 1e7)  #1cm = 1e7 nm
condD

condE <- condE %>%
  mutate(thickness_nm = (delta_m/ rho)* 1e7)  #1cm = 1e7 nm
condE


# extra element- smoothen the data, to eliminate noise, via savitsky-golay filter

help("sgolayfilt")

library(signal)
condA$delta_m_smooth <- sgolayfilt(condA$delta_m, p=2, n=5)
condA

condB$delta_m_smooth <- sgolayfilt(condB$delta_m, p=2, n=5)
condB

condC$delta_m_smooth <- sgolayfilt(condC$delta_m, p=2, n=5)
condC

condD$delta_m_smooth <- sgolayfilt(condD$delta_m, p=2, n=5)
condD

condE$delta_m_smooth <- sgolayfilt(condE$delta_m, p=2, n=5)
condE

cat("\n===== CONDITION A =====\n")
# --------------Condition A---------------

#plotting
# Compute basic statistics
Q1 <- quantile(condA$delta_m_smooth, 0.25)
Q3 <- quantile(condA$delta_m_smooth, 0.75)
median_val <- median(condA$delta_m_smooth)
IQR_val <- IQR(condA$delta_m_smooth)

# Compute whiskers (standard boxplot convention)
lower_whisker <- max(min(condA$delta_m_smooth), Q1 - 1.5 * IQR_val)
upper_whisker <- min(max(condA$delta_m_smooth), Q3 + 1.5 * IQR_val)

# Plot the boxplot
boxplot(condA$delta_m_smooth,
        main = "Δm₅ (Condition A) — Boxplot with IQR",
        ylab = "Δm₅ (ng/cm²)",
        col = "lightblue",
        outline = TRUE)  # show extreme outliers

# Add reference lines for statistics
abline(h = median_val, col = "red", lty = 2)           # median
abline(h = c(Q1, Q3), col = "darkgreen", lty = 2)     # IQR
abline(h = c(lower_whisker, upper_whisker), col = "purple", lty = 3) # whiskers

# Optional: add text labels for clarity
text(x = 1.2, y = median_val, labels = "Median", col = "red", pos = 4)
text(x = 1.2, y = Q1, labels = "Q1", col = "darkgreen", pos = 4)
text(x = 1.2, y = Q3, labels = "Q3", col = "darkgreen", pos = 4)
text(x = 1.2, y = lower_whisker, labels = "Lower Whisker", col = "purple", pos = 4)
text(x = 1.2, y = upper_whisker, labels = "Upper Whisker", col = "purple", pos = 4)

IQR_val
lower_whisker
upper_whisker
Q1 - 1.5 * IQR_val
Q3 + 1.5 * IQR_val
median_val

chatterjee_xi <- function(x, y) {
  # Chatterjee rank correlation implementation
  n <- length(x)
  r <- rank(y, ties.method = "average")
  
  # Order r by sorted x
  o <- order(x)
  r <- r[o]
  
  # numerator: sum of absolute rank differences
  num <- 0
  for (i in 1:(n-1)) {
    num <- num + abs(r[i+1] - r[i])
  }
  
  # denominator: n^2 - 1
  xi <- 1 - (3 * num) / (n^2 - 1)
  return(xi)
}

summary(condA)
sum(is.na(condA))
any(diff(condA$Time_min) <= 0)
plot(condA$Time_min, condA$delta_m_smooth, pch=16, main="Raw data check")

condA <- condA[order(condA$Time_min), ]
condA <- condA[complete.cases(condA$delta_m_smooth), ]
plot(condA$Time_min, condA$delta_m_smooth, type="p")
condA$sm <- stats::filter(condA$delta_m_smooth, rep(1/5,5), sides=2)
condA$sm 
library(zoo)
condA$delta_m_smooth <- na.approx(condA$delta_m_smooth)
condA$delta_m_smooth
sum(is.na(condA))
sum(is.na(condA$delta_m_smooth))
colSums(is.na(condA))
condA <- condA[, colSums(is.na(condA)) == 0]
condA

library(mgcv)
library(randomForest)
library(ggplot2)

### 1) Prepare data ###
df <- condA[order(condA$Time_min), ]
df <- df[complete.cases(df$Time_min, df$delta_m_smooth), ]

# Train-test split (time-based)
split <- floor(0.7 * nrow(df))
train <- df[1:split, ]
test  <- df[(split + 1):nrow(df), ]

### 2) Fit Models ###

# Linear model
model_lm <- lm(delta_m_smooth ~ Time_min, data=train)

# Polynomial (degree = 3)
model_poly <- lm(delta_m_smooth ~ stats::poly(Time_min, 3, raw=TRUE), data=train)

# GAM spline
model_gam <- gam(delta_m_smooth ~ s(Time_min, k=10), data=train)

# Random Forest
model_rf <- randomForest(delta_m_smooth ~ Time_min, data=train, ntree=500)

### 3) Predictions ###

test$pred_lm   <- predict(model_lm, newdata=test)
test$pred_poly <- predict(model_poly, newdata=test)
test$pred_gam  <- predict(model_gam, newdata=test)
test$pred_rf   <- predict(model_rf, newdata=test)


### 4) Compute Metrics ###

RMSE <- function(actual, pred) {
  sqrt(mean((actual - pred)^2))
}

# Time-series R² (uses Sum of Squares Residual / Total Sum of Squares)
R2_ts <- function(actual, pred) {
  ss_res <- sum((actual - pred)^2)          # SSR = sum of squared residuals
  ss_tot <- sum((actual - mean(actual))^2)  # SST = total variance
  1 - (ss_res / ss_tot)
}

results <- data.frame(
  Model = c("Linear", "Polynomial(3)", "GAM", "Random Forest"),
  RMSE  = c(
    RMSE(test$delta_m_smooth, test$pred_lm),
    RMSE(test$delta_m_smooth, test$pred_poly),
    RMSE(test$delta_m_smooth, test$pred_gam),
    RMSE(test$delta_m_smooth, test$pred_rf)
  ),
  R2_ts = c(
    R2_ts(test$delta_m_smooth, test$pred_lm),
    R2_ts(test$delta_m_smooth, test$pred_poly),
    R2_ts(test$delta_m_smooth, test$pred_gam),
    R2_ts(test$delta_m_smooth, test$pred_rf)
  )
)

print(results)

xi_lin  <- chatterjee_xi(test$delta_m_smooth, test$pred_lin)
xi_poly <- chatterjee_xi(test$delta_m_smooth, test$pred_poly)
xi_rf   <- chatterjee_xi(test$delta_m_smooth, test$pred_rf)
xi_gam  <- chatterjee_xi(test$delta_m_smooth, test$pred_gam)

cat("\nChatterjee ξ Values:\n")
cat("Time vs Δm:", round(xi_time, 4), "\n")
cat("Linear:", round(xi_lin, 4), "\n")
cat("Poly3:", round(xi_poly, 4), "\n")
cat("Random Forest:", round(xi_rf, 4), "\n")
cat("GAM:", round(xi_gam, 4), "\n")

### 5) Plot Comparison ###

ggplot() +
  geom_point(data = df, aes(Time_min, delta_m_smooth), color="black", alpha=0.6) +
  geom_line(data = test, aes(Time_min, pred_lm), color="blue", size=1.2) +
  geom_line(data = test, aes(Time_min, pred_poly), color="purple", size=1.2) +
  geom_line(data = test, aes(Time_min, pred_gam), color="red", size=1.6) +
  geom_line(data = test, aes(Time_min, pred_rf), color="green4", size=1.2) +
  labs(
    title="Model Comparison on Condition A",
    subtitle="Black = Actual | Blue = Linear | Purple = Poly3 | Red = GAM | Green = RF",
    x="Time (min)", y="Δm"
  ) +
  theme_minimal()

library(mgcv)
library(ggplot2)

# --------Data Prep--------
df <- condA
df <- df[complete.cases(df$Time_min, df$delta_m_smooth), ]
df <- df[order(df$Time_min), ]

# Time split (70% train)
split <- floor(0.7 * nrow(df))
train <- df[1:split, ]
test  <- df[(split+1):nrow(df), ]

# ---- GAM Model ----
gam_model <- gam(delta_m_smooth ~ s(Time_min, k = 10), data=train)

# Prediction on test set
test_pred <- predict(gam_model, newdata=test, se.fit=TRUE)
test$pred <- test_pred$fit



# Chatterjee Rank Correlation (ξ)
xi_time <- chatterjee_xi(df$Time_min, df$delta_m_smooth)
xi_gam  <- chatterjee_xi(test$delta_m_smooth, test$pred)

cat("Chatterjee ξ (Time vs Δm):", round(xi_time, 4), "\n")
cat("Chatterjee ξ (Actual vs GAM):", round(xi_gam, 4), "\n")


# -------------Metrics---------------
RMSE <- function(a,p) sqrt(mean((a - p)^2))
R2_ts <- function(a,p) {
  ssr <- sum((a - p)^2)
  sst <- sum((a - mean(a))^2)
  1 - ssr/sst
}

rmse <- RMSE(test$delta_m_smooth, test$pred)
r2   <- R2_ts(test$delta_m_smooth, test$pred)

cat("RMSE =", rmse, "\n")
cat("R2_ts =", r2, "\n")

# ---- Forecast 60 minutes ahead ----
future <- data.frame(Time_min = seq(max(df$Time_min), max(df$Time_min)+60, by=1))
future_pred <- predict(gam_model, newdata=future, se.fit=TRUE)
future$fit  <- future_pred$fit
future$upper <- future$fit + 1.96 * future_pred$se.fit
future$lower <- future$fit - 1.96 * future_pred$se.fit

# CI on full curve for plotting
full_pred <- predict(gam_model, newdata=df, se.fit=TRUE)
df$fit   <- full_pred$fit
df$upper <- df$fit + 1.96 * full_pred$se.fit
df$lower <- df$fit - 1.96 * full_pred$se.fit

# ---- Plot ----
ggplot() +
  geom_point(data=df, aes(Time_min, delta_m_smooth), color="black", alpha=0.6) +
  geom_ribbon(data=df, aes(Time_min, ymin=lower, ymax=upper), fill="skyblue2", alpha=0.25) +
  geom_line(data=df, aes(Time_min, fit), color="red", size=1.3) +
  geom_line(data=test, aes(Time_min, pred), color="darkred", linetype="dashed", size=1.2) +
  geom_ribbon(data=future, aes(Time_min, ymin=lower, ymax=upper), fill="lightblue", alpha=0.35) +
  geom_line(data=future, aes(Time_min, fit), color="blue", size=1.3) +
  labs(
    title = "GAM Fit and Forecast - Condition A",
    subtitle = paste("RMSE =", round(rmse,2), "| R² =", round(r2,3)),
    x = "Time (min)",
    y = "Δm (ng/cm²)"
  ) +
  theme_minimal(base_size = 14)

library(mgcv)
library(minpack.lm)
library(ggplot2)

# -------- Data prep --------
df <- condA
df <- df[complete.cases(df$Time_min, df$delta_m_smooth), ]
df <- df[order(df$Time_min), ]

# Train-test split (time-based)
split <- floor(0.7 * nrow(df))
train <- df[1:split, ]
test  <- df[(split+1):nrow(df), ]

# -------- GAM model --------
gam_model <- gam(delta_m_smooth ~ s(Time_min, k=10), data = train)
test$pred_gam <- predict(gam_model, newdata=test)

# -------- Weibull model --------
# Model: m(t) = A - B * exp(-(t/tau)^k)

wb_start <- list(
  A = max(train$delta_m_smooth),
  B = abs(min(train$delta_m_smooth)),
  tau = median(train$Time_min),
  k = 1.2
)

weibull_model <- nlsLM(
  delta_m_smooth ~ A - B * exp(-(Time_min/tau)^k),
  data=train,
  start=wb_start,
  control=nls.lm.control(maxiter=500)
)

test$pred_wb <- predict(weibull_model, newdata=test)

# -------- Metrics --------
RMSE <- function(a,p) sqrt(mean((a - p)^2))
R2_ts <- function(a,p) {
  ssr <- sum((a - p)^2)
  sst <- sum((a - mean(a))^2)
  1 - ssr/sst
}

rmse_gam <- RMSE(test$delta_m_smooth, test$pred_gam)
r2_gam   <- R2_ts(test$delta_m_smooth, test$pred_gam)

rmse_wb <- RMSE(test$delta_m_smooth, test$pred_wb)
r2_wb   <- R2_ts(test$delta_m_smooth, test$pred_wb)

cat("\nGAM Model:\n", "RMSE =", rmse_gam, "R2 =", r2_gam, "\n")
cat("Weibull Model:\n", "RMSE =", rmse_wb, "R2 =", r2_wb, "\n")

# -------- Plot --------
ggplot() +
  geom_point(data=df, aes(Time_min, delta_m_smooth), color="black", alpha=0.6) +
  geom_line(data=test, aes(Time_min, pred_gam), color="red", size=1.3) +
  geom_line(data=test, aes(Time_min, pred_wb), color="blue", size=1.3, linetype="dashed") +
  labs(
    title="Condition A: GAM vs Weibull Fit",
    subtitle=paste("GAM (red): RMSE =", round(rmse_gam,2), 
                   "| Weibull (blue): RMSE =", round(rmse_wb,2)),
    x="Time (min)",
    y="Δm (ng/cm²)"
  ) +
  theme_minimal(base_size = 14) +
  scale_color_manual(values=c("red","blue"))

cat("\n===== CONDITION B =====\n")
#----------------- Condition B---------------------
#plotting
# Compute basic statistics
Q1 <- quantile(condB$delta_m_smooth, 0.25)
Q3 <- quantile(condB$delta_m_smooth, 0.75)
median_val <- median(condB$delta_m_smooth)
IQR_val <- IQR(condB$delta_m_smooth)

# Compute whiskers (standard boxplot convention)
lower_whisker <- max(min(condB$delta_m_smooth), Q1 - 1.5 * IQR_val)
upper_whisker <- min(max(condB$delta_m_smooth), Q3 + 1.5 * IQR_val)

# Plot the boxplot
boxplot(condB$delta_m_smooth,
        main = "Δm₅ (Condition B) — Boxplot with IQR",
        ylab = "Δm₅ (ng/cm²)",
        col = "lightblue",
        outline = TRUE)  # show extreme outliers

# Add reference lines for statistics
abline(h = median_val, col = "red", lty = 2)           # median
abline(h = c(Q1, Q3), col = "darkgreen", lty = 2)     # IQR
abline(h = c(lower_whisker, upper_whisker), col = "purple", lty = 3) # whiskers

# Optional: add text labels for clarity
text(x = 1.2, y = median_val, labels = "Median", col = "red", pos = 4)
text(x = 1.2, y = Q1, labels = "Q1", col = "darkgreen", pos = 4)
text(x = 1.2, y = Q3, labels = "Q3", col = "darkgreen", pos = 4)
text(x = 1.2, y = lower_whisker, labels = "Lower Whisker", col = "purple", pos = 4)
text(x = 1.2, y = upper_whisker, labels = "Upper Whisker", col = "purple", pos = 4)

IQR_val
lower_whisker
upper_whisker
Q1 - 1.5 * IQR_val
Q3 + 1.5 * IQR_val
median_val

chatterjee_xi <- function(x, y) {
  # Chatterjee rank correlation implementation
  n <- length(x)
  r <- rank(y, ties.method = "average")
  
  # Order r by sorted x
  o <- order(x)
  r <- r[o]
  
  # numerator: sum of absolute rank differences
  num <- 0
  for (i in 1:(n-1)) {
    num <- num + abs(r[i+1] - r[i])
  }
  
  # denominator: n^2 - 1
  xi <- 1 - (3 * num) / (n^2 - 1)
  return(xi)
}

summary(condB)
sum(is.na(condB))
any(diff(condB$Time_min) <= 0)
plot(condB$Time_min, condB$delta_m_smooth, pch=16, main="Raw data check")

condB <- condB[order(condB$Time_min), ]
condB <- condB[complete.cases(condB$delta_m_smooth), ]
plot(condB$Time_min, condB$delta_m_smooth, type="p")
condB$sm <- stats::filter(condB$delta_m_smooth, rep(1/5,5), sides=2)
condB$sm 
library(zoo)
condB$delta_m_smooth <- na.approx(condB$delta_m_smooth)
condB$delta_m_smooth
sum(is.na(condB))
sum(is.na(condB$delta_m_smooth))
colSums(is.na(condB))
condB <- condB[, colSums(is.na(condB)) == 0]
condB

library(mgcv)
library(randomForest)
library(ggplot2)

### 1) Prepare data ###
df <- condB[order(condB$Time_min), ]
df <- df[complete.cases(df$Time_min, df$delta_m_smooth), ]

# Train-test split (time-based)
split <- floor(0.7 * nrow(df))
train <- df[1:split, ]
test  <- df[(split + 1):nrow(df), ]

### 2) Fit Models ###

# Linear model
model_lm <- lm(delta_m_smooth ~ Time_min, data=train)

# Polynomial (degree = 3)
model_poly <- lm(delta_m_smooth ~ stats::poly(Time_min, 3, raw=TRUE), data=train)

# GAM spline
model_gam <- gam(delta_m_smooth ~ s(Time_min, k=10), data=train)

# Random Forest
model_rf <- randomForest(delta_m_smooth ~ Time_min, data=train, ntree=500)

### 3) Predictions ###

test$pred_lm   <- predict(model_lm, newdata=test)
test$pred_poly <- predict(model_poly, newdata=test)
test$pred_gam  <- predict(model_gam, newdata=test)
test$pred_rf   <- predict(model_rf, newdata=test)

### 4) Compute Metrics ###

### 4) Compute Metrics ###

RMSE <- function(actual, pred) {
  sqrt(mean((actual - pred)^2))
}

# Time-series R² (uses Sum of Squares Residual / Total Sum of Squares)
R2_ts <- function(actual, pred) {
  ss_res <- sum((actual - pred)^2)          # SSR = sum of squared residuals
  ss_tot <- sum((actual - mean(actual))^2)  # SST = total variance
  1 - (ss_res / ss_tot)
}

results <- data.frame(
  Model = c("Linear", "Polynomial(3)", "GAM", "Random Forest"),
  RMSE  = c(
    RMSE(test$delta_m_smooth, test$pred_lm),
    RMSE(test$delta_m_smooth, test$pred_poly),
    RMSE(test$delta_m_smooth, test$pred_gam),
    RMSE(test$delta_m_smooth, test$pred_rf)
  ),
  R2_ts = c(
    R2_ts(test$delta_m_smooth, test$pred_lm),
    R2_ts(test$delta_m_smooth, test$pred_poly),
    R2_ts(test$delta_m_smooth, test$pred_gam),
    R2_ts(test$delta_m_smooth, test$pred_rf)
  )
)

print(results)

xi_lin  <- chatterjee_xi(test$delta_m_smooth, test$pred_lin)
xi_poly <- chatterjee_xi(test$delta_m_smooth, test$pred_poly)
xi_rf   <- chatterjee_xi(test$delta_m_smooth, test$pred_rf)
xi_gam  <- chatterjee_xi(test$delta_m_smooth, test$pred_gam)

cat("\nChatterjee ξ Values:\n")
cat("Time vs Δm:", round(xi_time, 4), "\n")
cat("Linear:", round(xi_lin, 4), "\n")
cat("Poly3:", round(xi_poly, 4), "\n")
cat("Random Forest:", round(xi_rf, 4), "\n")
cat("GAM:", round(xi_gam, 4), "\n")

### 5) Plot Comparison ###

ggplot() +
  geom_point(data = df, aes(Time_min, delta_m_smooth), color="black", alpha=0.6) +
  geom_line(data = test, aes(Time_min, pred_lm), color="blue", size=1.2) +
  geom_line(data = test, aes(Time_min, pred_poly), color="purple", size=1.2) +
  geom_line(data = test, aes(Time_min, pred_gam), color="red", size=1.6) +
  geom_line(data = test, aes(Time_min, pred_rf), color="green4", size=1.2) +
  labs(
    title="Model Comparison on Condition B",
    subtitle="Black = Actual | Blue = Linear | Purple = Poly3 | Red = GAM | Green = RF",
    x="Time (min)", y="Δm"
  ) +
  theme_minimal()

library(mgcv)
library(ggplot2)

# ---- Data prep ----
df <- condB
df <- df[complete.cases(df$Time_min, df$delta_m_smooth), ]
df <- df[order(df$Time_min), ]

# Time split (70% train)
split <- floor(0.7 * nrow(df))
train <- df[1:split, ]
test  <- df[(split+1):nrow(df), ]

# ---- GAM Model ----
gam_model <- gam(delta_m_smooth ~ s(Time_min, k = 10), data=train)

# Prediction on test set
test_pred <- predict(gam_model, newdata=test, se.fit=TRUE)
test$pred <- test_pred$fit


# Chatterjee Rank Correlation (ξ)
xi_time <- chatterjee_xi(df$Time_min, df$delta_m_smooth)
xi_gam  <- chatterjee_xi(test$delta_m_smooth, test$pred)

cat("Chatterjee ξ (Time vs Δm):", round(xi_time, 4), "\n")
cat("Chatterjee ξ (Actual vs GAM):", round(xi_gam, 4), "\n")


# ---- Metrics ----
RMSE <- function(a,p) sqrt(mean((a - p)^2))
R2_ts <- function(a,p) {
  ssr <- sum((a - p)^2)
  sst <- sum((a - mean(a))^2)
  1 - ssr/sst
}

rmse <- RMSE(test$delta_m_smooth, test$pred)
r2   <- R2_ts(test$delta_m_smooth, test$pred)

cat("RMSE =", rmse, "\n")
cat("R2_ts =", r2, "\n")

# ---- Forecast 60 minutes ahead ----
future <- data.frame(Time_min = seq(max(df$Time_min), max(df$Time_min)+60, by=1))
future_pred <- predict(gam_model, newdata=future, se.fit=TRUE)
future$fit  <- future_pred$fit
future$upper <- future$fit + 1.96 * future_pred$se.fit
future$lower <- future$fit - 1.96 * future_pred$se.fit

# CI on full curve for plotting
full_pred <- predict(gam_model, newdata=df, se.fit=TRUE)
df$fit   <- full_pred$fit
df$upper <- df$fit + 1.96 * full_pred$se.fit
df$lower <- df$fit - 1.96 * full_pred$se.fit

# ---- Plot ----
ggplot() +
  geom_point(data=df, aes(Time_min, delta_m_smooth), color="black", alpha=0.6) +
  geom_ribbon(data=df, aes(Time_min, ymin=lower, ymax=upper), fill="skyblue2", alpha=0.25) +
  geom_line(data=df, aes(Time_min, fit), color="red", size=1.3) +
  geom_line(data=test, aes(Time_min, pred), color="darkred", linetype="dashed", size=1.2) +
  geom_ribbon(data=future, aes(Time_min, ymin=lower, ymax=upper), fill="lightblue", alpha=0.35) +
  geom_line(data=future, aes(Time_min, fit), color="blue", size=1.3) +
  labs(
    title = "GAM Fit and Forecast - Condition B",
    subtitle = paste("RMSE =", round(rmse,2), "| R² =", round(r2,3)),
    x = "Time (min)",
    y = "Δm (ng/cm²)"
  ) +
  theme_minimal(base_size = 14)

library(mgcv)
library(minpack.lm)
library(ggplot2)

# -------- Data prep --------
df <- condB
df <- df[complete.cases(df$Time_min, df$delta_m_smooth), ]
df <- df[order(df$Time_min), ]

# Train-test split (time-based)
split <- floor(0.7 * nrow(df))
train <- df[1:split, ]
test  <- df[(split+1):nrow(df), ]

# -------- GAM model --------
gam_model <- gam(delta_m_smooth ~ s(Time_min, k=10), data = train)
test$pred_gam <- predict(gam_model, newdata=test)

# -------- Weibull model --------
# Model: m(t) = A - B * exp(-(t/tau)^k)

wb_start <- list(
  A = max(train$delta_m_smooth),
  B = abs(min(train$delta_m_smooth)),
  tau = median(train$Time_min),
  k = 1.2
)

weibull_model <- nlsLM(
  delta_m_smooth ~ A - B * exp(-(Time_min/tau)^k),
  data=train,
  start=wb_start,
  control=nls.lm.control(maxiter=500)
)

test$pred_wb <- predict(weibull_model, newdata=test)

# -------- Metrics --------
RMSE <- function(a,p) sqrt(mean((a - p)^2))
R2_ts <- function(a,p) {
  ssr <- sum((a - p)^2)
  sst <- sum((a - mean(a))^2)
  1 - ssr/sst
}

rmse_gam <- RMSE(test$delta_m_smooth, test$pred_gam)
r2_gam   <- R2_ts(test$delta_m_smooth, test$pred_gam)

rmse_wb <- RMSE(test$delta_m_smooth, test$pred_wb)
r2_wb   <- R2_ts(test$delta_m_smooth, test$pred_wb)

cat("\nGAM Model:\n", "RMSE =", rmse_gam, "R2 =", r2_gam, "\n")
cat("Weibull Model:\n", "RMSE =", rmse_wb, "R2 =", r2_wb, "\n")

# -------- Plot --------
ggplot() +
  geom_point(data=df, aes(Time_min, delta_m_smooth), color="black", alpha=0.6) +
  geom_line(data=test, aes(Time_min, pred_gam), color="red", size=1.3) +
  geom_line(data=test, aes(Time_min, pred_wb), color="blue", size=1.3, linetype="dashed") +
  labs(
    title="Condition B: GAM vs Weibull Fit",
    subtitle=paste("GAM (red): RMSE =", round(rmse_gam,2), 
                   "| Weibull (blue): RMSE =", round(rmse_wb,2)),
    x="Time (min)",
    y="Δm (ng/cm²)"
  ) +
  theme_minimal(base_size = 14) +
  scale_color_manual(values=c("red","blue"))

cat("\n===== CONDITION C =====\n")
#----------------Condition C------------------
#plotting
# Compute basic statistics
Q1 <- quantile(condC$delta_m_smooth, 0.25)
Q3 <- quantile(condC$delta_m_smooth, 0.75)
median_val <- median(condC$delta_m_smooth)
IQR_val <- IQR(condC$delta_m_smooth)

# Compute whiskers (standard boxplot convention)
lower_whisker <- max(min(condC$delta_m_smooth), Q1 - 1.5 * IQR_val)
upper_whisker <- min(max(condC$delta_m_smooth), Q3 + 1.5 * IQR_val)

# Plot the boxplot
boxplot(condC$delta_m_smooth,
        main = "Δm₅ (Condition C) — Boxplot with IQR",
        ylab = "Δm₅ (ng/cm²)",
        col = "lightblue",
        outline = TRUE)  # show extreme outliers

# Add reference lines for statistics
abline(h = median_val, col = "red", lty = 2)           # median
abline(h = c(Q1, Q3), col = "darkgreen", lty = 2)     # IQR
abline(h = c(lower_whisker, upper_whisker), col = "purple", lty = 3) # whiskers

# Optional: add text labels for clarity
text(x = 1.2, y = median_val, labels = "Median", col = "red", pos = 4)
text(x = 1.2, y = Q1, labels = "Q1", col = "darkgreen", pos = 4)
text(x = 1.2, y = Q3, labels = "Q3", col = "darkgreen", pos = 4)
text(x = 1.2, y = lower_whisker, labels = "Lower Whisker", col = "purple", pos = 4)
text(x = 1.2, y = upper_whisker, labels = "Upper Whisker", col = "purple", pos = 4)

IQR_val
lower_whisker
upper_whisker
Q1 - 1.5 * IQR_val
Q3 + 1.5 * IQR_val
median_val


chatterjee_xi <- function(x, y) {
  # Chatterjee rank correlation implementation
  n <- length(x)
  r <- rank(y, ties.method = "average")
  
  # Order r by sorted x
  o <- order(x)
  r <- r[o]
  
  # numerator: sum of absolute rank differences
  num <- 0
  for (i in 1:(n-1)) {
    num <- num + abs(r[i+1] - r[i])
  }
  
  # denominator: n^2 - 1
  xi <- 1 - (3 * num) / (n^2 - 1)
  return(xi)
}

summary(condC)
sum(is.na(condC))
any(diff(condC$Time_min) <= 0)
plot(condC$Time_min, condC$delta_m_smooth, pch=16, main="Raw data check")

condC <- condC[order(condC$Time_min), ]
condC <- condC[complete.cases(condC$delta_m_smooth), ]
plot(condC$Time_min, condC$delta_m_smooth, type="p")
condC$sm <- stats::filter(condC$delta_m_smooth, rep(1/5,5), sides=2)
condC$sm 
library(zoo)
condC$delta_m_smooth <- na.approx(condC$delta_m_smooth)
condC$delta_m_smooth
sum(is.na(condC))
sum(is.na(condC$delta_m_smooth))
colSums(is.na(condC))
condC <- condC[, colSums(is.na(condC)) == 0]
condC

library(mgcv)
library(randomForest)
library(ggplot2)

### 1) Prepare data ###
df <- condC[order(condC$Time_min), ]
df <- df[complete.cases(df$Time_min, df$delta_m_smooth), ]

# Train-test split (time-based)
split <- floor(0.7 * nrow(df))
train <- df[1:split, ]
test  <- df[(split + 1):nrow(df), ]

### 2) Fit Models ###

# Linear model
model_lm <- lm(delta_m_smooth ~ Time_min, data=train)

# Polynomial (degree = 3)
model_poly <- lm(delta_m_smooth ~ stats::poly(Time_min, 3, raw=TRUE), data=train)

# GAM spline
model_gam <- gam(delta_m_smooth ~ s(Time_min, k=10), data=train)

# Random Forest
model_rf <- randomForest(delta_m_smooth ~ Time_min, data=train, ntree=500)

### 3) Predictions ###

test$pred_lm   <- predict(model_lm, newdata=test)
test$pred_poly <- predict(model_poly, newdata=test)
test$pred_gam  <- predict(model_gam, newdata=test)
test$pred_rf   <- predict(model_rf, newdata=test)

### 4) Compute Metrics ###

### 4) Compute Metrics ###

RMSE <- function(actual, pred) {
  sqrt(mean((actual - pred)^2))
}

# Time-series R² (uses Sum of Squares Residual / Total Sum of Squares)
R2_ts <- function(actual, pred) {
  ss_res <- sum((actual - pred)^2)          # SSR = sum of squared residuals
  ss_tot <- sum((actual - mean(actual))^2)  # SST = total variance
  1 - (ss_res / ss_tot)
}

results <- data.frame(
  Model = c("Linear", "Polynomial(3)", "GAM", "Random Forest"),
  RMSE  = c(
    RMSE(test$delta_m_smooth, test$pred_lm),
    RMSE(test$delta_m_smooth, test$pred_poly),
    RMSE(test$delta_m_smooth, test$pred_gam),
    RMSE(test$delta_m_smooth, test$pred_rf)
  ),
  R2_ts = c(
    R2_ts(test$delta_m_smooth, test$pred_lm),
    R2_ts(test$delta_m_smooth, test$pred_poly),
    R2_ts(test$delta_m_smooth, test$pred_gam),
    R2_ts(test$delta_m_smooth, test$pred_rf)
  )
)

print(results)

xi_lin  <- chatterjee_xi(test$delta_m_smooth, test$pred_lin)
xi_poly <- chatterjee_xi(test$delta_m_smooth, test$pred_poly)
xi_rf   <- chatterjee_xi(test$delta_m_smooth, test$pred_rf)
xi_gam  <- chatterjee_xi(test$delta_m_smooth, test$pred_gam)

cat("\nChatterjee ξ Values:\n")
cat("Time vs Δm:", round(xi_time, 4), "\n")
cat("Linear:", round(xi_lin, 4), "\n")
cat("Poly3:", round(xi_poly, 4), "\n")
cat("Random Forest:", round(xi_rf, 4), "\n")
cat("GAM:", round(xi_gam, 4), "\n")

### 5) Plot Comparison ###

ggplot() +
  geom_point(data = df, aes(Time_min, delta_m_smooth), color="black", alpha=0.6) +
  geom_line(data = test, aes(Time_min, pred_lm), color="blue", size=1.2) +
  geom_line(data = test, aes(Time_min, pred_poly), color="purple", size=1.2) +
  geom_line(data = test, aes(Time_min, pred_gam), color="red", size=1.6) +
  geom_line(data = test, aes(Time_min, pred_rf), color="green4", size=1.2) +
  labs(
    title="Model Comparison on Condition C",
    subtitle="Black = Actual | Blue = Linear | Purple = Poly3 | Red = GAM | Green = RF",
    x="Time (min)", y="Δm"
  ) +
  theme_minimal()

library(mgcv)
library(ggplot2)

# ---- Data prep ----
df <- condC
df <- df[complete.cases(df$Time_min, df$delta_m_smooth), ]
df <- df[order(df$Time_min), ]

# Time split (70% train)
split <- floor(0.7 * nrow(df))
train <- df[1:split, ]
test  <- df[(split+1):nrow(df), ]

# ---- GAM Model ----
gam_model <- gam(delta_m_smooth ~ s(Time_min, k = 10), data=train)

# Prediction on test set
test_pred <- predict(gam_model, newdata=test, se.fit=TRUE)
test$pred <- test_pred$fit

# Chatterjee Rank Correlation (ξ)
xi_time <- chatterjee_xi(df$Time_min, df$delta_m_smooth)
xi_gam  <- chatterjee_xi(test$delta_m_smooth, test$pred)

cat("Chatterjee ξ (Time vs Δm):", round(xi_time, 4), "\n")
cat("Chatterjee ξ (Actual vs GAM):", round(xi_gam, 4), "\n")

# ---- Metrics ----
RMSE <- function(a,p) sqrt(mean((a - p)^2))
R2_ts <- function(a,p) {
  ssr <- sum((a - p)^2)
  sst <- sum((a - mean(a))^2)
  1 - ssr/sst
}

rmse <- RMSE(test$delta_m_smooth, test$pred)
r2   <- R2_ts(test$delta_m_smooth, test$pred)

cat("RMSE =", rmse, "\n")
cat("R2_ts =", r2, "\n")

# ---- Forecast 60 minutes ahead ----
future <- data.frame(Time_min = seq(max(df$Time_min), max(df$Time_min)+60, by=1))
future_pred <- predict(gam_model, newdata=future, se.fit=TRUE)
future$fit  <- future_pred$fit
future$upper <- future$fit + 1.96 * future_pred$se.fit
future$lower <- future$fit - 1.96 * future_pred$se.fit

# CI on full curve for plotting
full_pred <- predict(gam_model, newdata=df, se.fit=TRUE)
df$fit   <- full_pred$fit
df$upper <- df$fit + 1.96 * full_pred$se.fit
df$lower <- df$fit - 1.96 * full_pred$se.fit

# ---- Plot ----
ggplot() +
  geom_point(data=df, aes(Time_min, delta_m_smooth), color="black", alpha=0.6) +
  geom_ribbon(data=df, aes(Time_min, ymin=lower, ymax=upper), fill="skyblue2", alpha=0.25) +
  geom_line(data=df, aes(Time_min, fit), color="red", size=1.3) +
  geom_line(data=test, aes(Time_min, pred), color="darkred", linetype="dashed", size=1.2) +
  geom_ribbon(data=future, aes(Time_min, ymin=lower, ymax=upper), fill="lightblue", alpha=0.35) +
  geom_line(data=future, aes(Time_min, fit), color="blue", size=1.3) +
  labs(
    title = "GAM Fit and Forecast - Condition C",
    subtitle = paste("RMSE =", round(rmse,2), "| R² =", round(r2,3)),
    x = "Time (min)",
    y = "Δm (ng/cm²)"
  ) +
  theme_minimal(base_size = 14)

library(mgcv)
library(minpack.lm)
library(ggplot2)

# -------- Data prep --------
df <- condC
df <- df[complete.cases(df$Time_min, df$delta_m_smooth), ]
df <- df[order(df$Time_min), ]

# Train-test split (time-based)
split <- floor(0.7 * nrow(df))
train <- df[1:split, ]
test  <- df[(split+1):nrow(df), ]

# -------- GAM model --------
gam_model <- gam(delta_m_smooth ~ s(Time_min, k=10), data = train)
test$pred_gam <- predict(gam_model, newdata=test)

# -------- Weibull model --------
# Model: m(t) = A - B * exp(-(t/tau)^k)

wb_start <- list(
  A = max(train$delta_m_smooth),
  B = abs(min(train$delta_m_smooth)),
  tau = median(train$Time_min),
  k = 1.2
)

weibull_model <- nlsLM(
  delta_m_smooth ~ A - B * exp(-(Time_min/tau)^k),
  data=train,
  start=wb_start,
  control=nls.lm.control(maxiter=500)
)

test$pred_wb <- predict(weibull_model, newdata=test)

# -------- Metrics --------
RMSE <- function(a,p) sqrt(mean((a - p)^2))
R2_ts <- function(a,p) {
  ssr <- sum((a - p)^2)
  sst <- sum((a - mean(a))^2)
  1 - ssr/sst
}

rmse_gam <- RMSE(test$delta_m_smooth, test$pred_gam)
r2_gam   <- R2_ts(test$delta_m_smooth, test$pred_gam)

rmse_wb <- RMSE(test$delta_m_smooth, test$pred_wb)
r2_wb   <- R2_ts(test$delta_m_smooth, test$pred_wb)

cat("\nGAM Model:\n", "RMSE =", rmse_gam, "R2 =", r2_gam, "\n")
cat("Weibull Model:\n", "RMSE =", rmse_wb, "R2 =", r2_wb, "\n")

# -------- Plot --------
ggplot() +
  geom_point(data=df, aes(Time_min, delta_m_smooth), color="black", alpha=0.6) +
  geom_line(data=test, aes(Time_min, pred_gam), color="red", size=1.3) +
  geom_line(data=test, aes(Time_min, pred_wb), color="blue", size=1.3, linetype="dashed") +
  labs(
    title="Condition C: GAM vs Weibull Fit",
    subtitle=paste("GAM (red): RMSE =", round(rmse_gam,2), 
                   "| Weibull (blue): RMSE =", round(rmse_wb,2)),
    x="Time (min)",
    y="Δm (ng/cm²)"
  ) +
  theme_minimal(base_size = 14) +
  scale_color_manual(values=c("red","blue"))

cat("\n===== CONDITION D =====\n")
#----------------------Condition D---------------------------
# Compute basic statistics
Q1 <- quantile(condD$delta_m_smooth, 0.25)
Q3 <- quantile(condD$delta_m_smooth, 0.75)
median_val <- median(condD$delta_m_smooth)
IQR_val <- IQR(condD$delta_m_smooth)

# Compute whiskers (standard boxplot convention)
lower_whisker <- max(min(condD$delta_m_smooth), Q1 - 1.5 * IQR_val)
upper_whisker <- min(max(condD$delta_m_smooth), Q3 + 1.5 * IQR_val)

# Plot the boxplot
boxplot(condD$delta_m_smooth,
        main = "Δm₅ (Condition D) — Boxplot with IQR",
        ylab = "Δm₅ (ng/cm²)",
        col = "lightblue",
        outline = TRUE)  # show extreme outliers

# Add reference lines for statistics
abline(h = median_val, col = "red", lty = 2)           # median
abline(h = c(Q1, Q3), col = "darkgreen", lty = 2)     # IQR
abline(h = c(lower_whisker, upper_whisker), col = "purple", lty = 3) # whiskers

# Optional: add text labels for clarity
text(x = 1.2, y = median_val, labels = "Median", col = "red", pos = 4)
text(x = 1.2, y = Q1, labels = "Q1", col = "darkgreen", pos = 4)
text(x = 1.2, y = Q3, labels = "Q3", col = "darkgreen", pos = 4)
text(x = 1.2, y = lower_whisker, labels = "Lower Whisker", col = "purple", pos = 4)
text(x = 1.2, y = upper_whisker, labels = "Upper Whisker", col = "purple", pos = 4)

IQR_val
lower_whisker
upper_whisker
Q1 - 1.5 * IQR_val
Q3 + 1.5 * IQR_val
median_val


chatterjee_xi <- function(x, y) {
  # Chatterjee rank correlation implementation
  n <- length(x)
  r <- rank(y, ties.method = "average")
  
  # Order r by sorted x
  o <- order(x)
  r <- r[o]
  
  # numerator: sum of absolute rank differences
  num <- 0
  for (i in 1:(n-1)) {
    num <- num + abs(r[i+1] - r[i])
  }
  
  # denominator: n^2 - 1
  xi <- 1 - (3 * num) / (n^2 - 1)
  return(xi)
}

summary(condD)
sum(is.na(condD))
any(diff(condD$Time_min) <= 0)
plot(condD$Time_min, condD$delta_m_smooth, pch=16, main="Raw data check")

condD <- condD[order(condD$Time_min), ]
condD <- condD[complete.cases(condD$delta_m_smooth), ]
plot(condD$Time_min, condD$delta_m_smooth, type="p")
condD$sm <- stats::filter(condD$delta_m_smooth, rep(1/5,5), sides=2)
condD$sm 
library(zoo)
condD$delta_m_smooth <- na.approx(condD$delta_m_smooth)
condD$delta_m_smooth
sum(is.na(condD))
sum(is.na(condD$delta_m_smooth))
colSums(is.na(condD))
condD <- condD[, colSums(is.na(condD)) == 0]
condD

library(mgcv)
library(randomForest)
library(ggplot2)

### 1) Prepare data ###
df <- condD[order(condD$Time_min), ]
df <- df[complete.cases(df$Time_min, df$delta_m_smooth), ]

# Train-test split (time-based)
split <- floor(0.7 * nrow(df))
train <- df[1:split, ]
test  <- df[(split + 1):nrow(df), ]

### 2) Fit Models ###

# Linear model
model_lm <- lm(delta_m_smooth ~ Time_min, data=train)

# Polynomial (degree = 3)
model_poly <- lm(delta_m_smooth ~ stats::poly(Time_min, 3, raw=TRUE), data=train)

# GAM spline
model_gam <- gam(delta_m_smooth ~ s(Time_min, k=10), data=train)

# Random Forest
model_rf <- randomForest(delta_m_smooth ~ Time_min, data=train, ntree=500)

### 3) Predictions ###

test$pred_lm   <- predict(model_lm, newdata=test)
test$pred_poly <- predict(model_poly, newdata=test)
test$pred_gam  <- predict(model_gam, newdata=test)
test$pred_rf   <- predict(model_rf, newdata=test)

### 4) Compute Metrics ###

### 4) Compute Metrics ###

RMSE <- function(actual, pred) {
  sqrt(mean((actual - pred)^2))
}

# Time-series R² (uses Sum of Squares Residual / Total Sum of Squares)
R2_ts <- function(actual, pred) {
  ss_res <- sum((actual - pred)^2)          # SSR = sum of squared residuals
  ss_tot <- sum((actual - mean(actual))^2)  # SST = total variance
  1 - (ss_res / ss_tot)
}

results <- data.frame(
  Model = c("Linear", "Polynomial(3)", "GAM", "Random Forest"),
  RMSE  = c(
    RMSE(test$delta_m_smooth, test$pred_lm),
    RMSE(test$delta_m_smooth, test$pred_poly),
    RMSE(test$delta_m_smooth, test$pred_gam),
    RMSE(test$delta_m_smooth, test$pred_rf)
  ),
  R2_ts = c(
    R2_ts(test$delta_m_smooth, test$pred_lm),
    R2_ts(test$delta_m_smooth, test$pred_poly),
    R2_ts(test$delta_m_smooth, test$pred_gam),
    R2_ts(test$delta_m_smooth, test$pred_rf)
  )
)

print(results)

xi_lin  <- chatterjee_xi(test$delta_m_smooth, test$pred_lin)
xi_poly <- chatterjee_xi(test$delta_m_smooth, test$pred_poly)
xi_rf   <- chatterjee_xi(test$delta_m_smooth, test$pred_rf)
xi_gam  <- chatterjee_xi(test$delta_m_smooth, test$pred_gam)

cat("\nChatterjee ξ Values:\n")
cat("Time vs Δm:", round(xi_time, 4), "\n")
cat("Linear:", round(xi_lin, 4), "\n")
cat("Poly3:", round(xi_poly, 4), "\n")
cat("Random Forest:", round(xi_rf, 4), "\n")
cat("GAM:", round(xi_gam, 4), "\n")

### 5) Plot Comparison ###

ggplot() +
  geom_point(data = df, aes(Time_min, delta_m_smooth), color="black", alpha=0.6) +
  geom_line(data = test, aes(Time_min, pred_lm), color="blue", size=1.2) +
  geom_line(data = test, aes(Time_min, pred_poly), color="purple", size=1.2) +
  geom_line(data = test, aes(Time_min, pred_gam), color="red", size=1.6) +
  geom_line(data = test, aes(Time_min, pred_rf), color="green4", size=1.2) +
  labs(
    title="Model Comparison on Condition D",
    subtitle="Black = Actual | Blue = Linear | Purple = Poly3 | Red = GAM | Green = RF",
    x="Time (min)", y="Δm"
  ) +
  theme_minimal()

library(mgcv)
library(ggplot2)

# ---- Data prep ----
df <- condD
df <- df[complete.cases(df$Time_min, df$delta_m_smooth), ]
df <- df[order(df$Time_min), ]

# Time split (70% train)
split <- floor(0.7 * nrow(df))
train <- df[1:split, ]
test  <- df[(split+1):nrow(df), ]

# ---- GAM Model ----
gam_model <- gam(delta_m_smooth ~ s(Time_min, k = 10), data=train)

# Prediction on test set
test_pred <- predict(gam_model, newdata=test, se.fit=TRUE)
test$pred <- test_pred$fit

# Chatterjee Rank Correlation (ξ)
xi_time <- chatterjee_xi(df$Time_min, df$delta_m_smooth)
xi_gam  <- chatterjee_xi(test$delta_m_smooth, test$pred)

cat("Chatterjee ξ (Time vs Δm):", round(xi_time, 4), "\n")
cat("Chatterjee ξ (Actual vs GAM):", round(xi_gam, 4), "\n")

# ---- Metrics ----
RMSE <- function(a,p) sqrt(mean((a - p)^2))
R2_ts <- function(a,p) {
  ssr <- sum((a - p)^2)
  sst <- sum((a - mean(a))^2)
  1 - ssr/sst
}

rmse <- RMSE(test$delta_m_smooth, test$pred)
r2   <- R2_ts(test$delta_m_smooth, test$pred)

cat("RMSE =", rmse, "\n")
cat("R2_ts =", r2, "\n")

# ---- Forecast 60 minutes ahead ----
future <- data.frame(Time_min = seq(max(df$Time_min), max(df$Time_min)+60, by=1))
future_pred <- predict(gam_model, newdata=future, se.fit=TRUE)
future$fit  <- future_pred$fit
future$upper <- future$fit + 1.96 * future_pred$se.fit
future$lower <- future$fit - 1.96 * future_pred$se.fit

# CI on full curve for plotting
full_pred <- predict(gam_model, newdata=df, se.fit=TRUE)
df$fit   <- full_pred$fit
df$upper <- df$fit + 1.96 * full_pred$se.fit
df$lower <- df$fit - 1.96 * full_pred$se.fit

# ---- Plot ----
ggplot() +
  geom_point(data=df, aes(Time_min, delta_m_smooth), color="black", alpha=0.6) +
  geom_ribbon(data=df, aes(Time_min, ymin=lower, ymax=upper), fill="skyblue2", alpha=0.25) +
  geom_line(data=df, aes(Time_min, fit), color="red", size=1.3) +
  geom_line(data=test, aes(Time_min, pred), color="darkred", linetype="dashed", size=1.2) +
  geom_ribbon(data=future, aes(Time_min, ymin=lower, ymax=upper), fill="lightblue", alpha=0.35) +
  geom_line(data=future, aes(Time_min, fit), color="blue", size=1.3) +
  labs(
    title = "GAM Fit and Forecast - Condition D",
    subtitle = paste("RMSE =", round(rmse,2), "| R² =", round(r2,3)),
    x = "Time (min)",
    y = "Δm (ng/cm²)"
  ) +
  theme_minimal(base_size = 14)

library(mgcv)
library(minpack.lm)
library(ggplot2)

# -------- Data prep --------
df <- condD
df <- df[complete.cases(df$Time_min, df$delta_m_smooth), ]
df <- df[order(df$Time_min), ]

# Train-test split (time-based)
split <- floor(0.7 * nrow(df))
train <- df[1:split, ]
test  <- df[(split+1):nrow(df), ]

# -------- GAM model --------
gam_model <- gam(delta_m_smooth ~ s(Time_min, k=10), data = train)
test$pred_gam <- predict(gam_model, newdata=test)

# -------- Weibull model --------
# Model: m(t) = A - B * exp(-(t/tau)^k)

wb_start <- list(
  A = max(train$delta_m_smooth),
  B = abs(min(train$delta_m_smooth)),
  tau = median(train$Time_min),
  k = 1.2
)

weibull_model <- nlsLM(
  delta_m_smooth ~ A - B * exp(-(Time_min/tau)^k),
  data=train,
  start=wb_start,
  control=nls.lm.control(maxiter=500)
)

test$pred_wb <- predict(weibull_model, newdata=test)

# -------- Metrics --------
RMSE <- function(a,p) sqrt(mean((a - p)^2))
R2_ts <- function(a,p) {
  ssr <- sum((a - p)^2)
  sst <- sum((a - mean(a))^2)
  1 - ssr/sst
}

rmse_gam <- RMSE(test$delta_m_smooth, test$pred_gam)
r2_gam   <- R2_ts(test$delta_m_smooth, test$pred_gam)

rmse_wb <- RMSE(test$delta_m_smooth, test$pred_wb)
r2_wb   <- R2_ts(test$delta_m_smooth, test$pred_wb)

cat("\nGAM Model:\n", "RMSE =", rmse_gam, "R2 =", r2_gam, "\n")
cat("Weibull Model:\n", "RMSE =", rmse_wb, "R2 =", r2_wb, "\n")

# -------- Plot --------
ggplot() +
  geom_point(data=df, aes(Time_min, delta_m_smooth), color="black", alpha=0.6) +
  geom_line(data=test, aes(Time_min, pred_gam), color="red", size=1.3) +
  geom_line(data=test, aes(Time_min, pred_wb), color="blue", size=1.3, linetype="dashed") +
  labs(
    title="Condition D: GAM vs Weibull Fit",
    subtitle=paste("GAM (red): RMSE =", round(rmse_gam,2), 
                   "| Weibull (blue): RMSE =", round(rmse_wb,2)),
    x="Time (min)",
    y="Δm (ng/cm²)"
  ) +
  theme_minimal(base_size = 14) +
  scale_color_manual(values=c("red","blue"))

cat("\n===== CONDITION E =====\n")
#-----------------------------Condition E----------------------------------
# Compute basic statistics
Q1 <- quantile(condE$delta_m_smooth, 0.25)
Q3 <- quantile(condE$delta_m_smooth, 0.75)
median_val <- median(condE$delta_m_smooth)
IQR_val <- IQR(condE$delta_m_smooth)

# Compute whiskers (standard boxplot convention)
lower_whisker <- max(min(condE$delta_m_smooth), Q1 - 1.5 * IQR_val)
upper_whisker <- min(max(condE$delta_m_smooth), Q3 + 1.5 * IQR_val)

# Plot the boxplot
boxplot(condE$delta_m_smooth,
        main = "Δm₅ (Condition E) — Boxplot with IQR",
        ylab = "Δm₅ (ng/cm²)",
        col = "lightblue",
        outline = TRUE)  # show extreme outliers

# Add reference lines for statistics
abline(h = median_val, col = "red", lty = 2)           # median
abline(h = c(Q1, Q3), col = "darkgreen", lty = 2)     # IQR
abline(h = c(lower_whisker, upper_whisker), col = "purple", lty = 3) # whiskers

# Optional: add text labels for clarity
text(x = 1.2, y = median_val, labels = "Median", col = "red", pos = 4)
text(x = 1.2, y = Q1, labels = "Q1", col = "darkgreen", pos = 4)
text(x = 1.2, y = Q3, labels = "Q3", col = "darkgreen", pos = 4)
text(x = 1.2, y = lower_whisker, labels = "Lower Whisker", col = "purple", pos = 4)
text(x = 1.2, y = upper_whisker, labels = "Upper Whisker", col = "purple", pos = 4)

IQR_val
lower_whisker
upper_whisker
Q1 - 1.5 * IQR_val
Q3 + 1.5 * IQR_val
median_val

chatterjee_xi <- function(x, y) {
  # Chatterjee rank correlation implementation
  n <- length(x)
  r <- rank(y, ties.method = "average")
  
  # Order r by sorted x
  o <- order(x)
  r <- r[o]
  
  # numerator: sum of absolute rank differences
  num <- 0
  for (i in 1:(n-1)) {
    num <- num + abs(r[i+1] - r[i])
  }
  
  # denominator: n^2 - 1
  xi <- 1 - (3 * num) / (n^2 - 1)
  return(xi)
}


summary(condE)
sum(is.na(condE))
any(diff(condE$Time_min) <= 0)
plot(condE$Time_min, condE$delta_m_smooth, pch=16, main="Raw data check")

condE <- condE[order(condE$Time_min), ]
condE <- condE[complete.cases(condE$delta_m_smooth), ]
plot(condE$Time_min, condE$delta_m_smooth, type="p")
condE$sm <- stats::filter(condE$delta_m_smooth, rep(1/5,5), sides=2)
condE$sm 
library(zoo)
condE$delta_m_smooth <- na.approx(condE$delta_m_smooth)
condE$delta_m_smooth
sum(is.na(condE))
sum(is.na(condE$delta_m_smooth))
colSums(is.na(condE))
condE <- condE[, colSums(is.na(condE)) == 0]
condE

library(mgcv)
library(randomForest)
library(ggplot2)

### 1) Prepare data ###
df <- condE[order(condE$Time_min), ]
df <- df[complete.cases(df$Time_min, df$delta_m_smooth), ]

# Train-test split (time-based)
split <- floor(0.7 * nrow(df))
train <- df[1:split, ]
test  <- df[(split + 1):nrow(df), ]

### 2) Fit Models ###

# Linear model
model_lm <- lm(delta_m_smooth ~ Time_min, data=train)

# Polynomial (degree = 3)
model_poly <- lm(delta_m_smooth ~ stats::poly(Time_min, 3, raw=TRUE), data=train)

# GAM spline
model_gam <- gam(delta_m_smooth ~ s(Time_min, k=10), data=train)

# Random Forest
model_rf <- randomForest(delta_m_smooth ~ Time_min, data=train, ntree=500)

### 3) Predictions ###

test$pred_lm   <- predict(model_lm, newdata=test)
test$pred_poly <- predict(model_poly, newdata=test)
test$pred_gam  <- predict(model_gam, newdata=test)
test$pred_rf   <- predict(model_rf, newdata=test)

### 4) Compute Metrics ###

### 4) Compute Metrics ###

RMSE <- function(actual, pred) {
  sqrt(mean((actual - pred)^2))
}

# Time-series R² (uses Sum of Squares Residual / Total Sum of Squares)
R2_ts <- function(actual, pred) {
  ss_res <- sum((actual - pred)^2)          # SSR = sum of squared residuals
  ss_tot <- sum((actual - mean(actual))^2)  # SST = total variance
  1 - (ss_res / ss_tot)
}

results <- data.frame(
  Model = c("Linear", "Polynomial(3)", "GAM", "Random Forest"),
  RMSE  = c(
    RMSE(test$delta_m_smooth, test$pred_lm),
    RMSE(test$delta_m_smooth, test$pred_poly),
    RMSE(test$delta_m_smooth, test$pred_gam),
    RMSE(test$delta_m_smooth, test$pred_rf)
  ),
  R2_ts = c(
    R2_ts(test$delta_m_smooth, test$pred_lm),
    R2_ts(test$delta_m_smooth, test$pred_poly),
    R2_ts(test$delta_m_smooth, test$pred_gam),
    R2_ts(test$delta_m_smooth, test$pred_rf)
  )
)

print(results)

xi_lin  <- chatterjee_xi(test$delta_m_smooth, test$pred_lin)
xi_poly <- chatterjee_xi(test$delta_m_smooth, test$pred_poly)
xi_rf   <- chatterjee_xi(test$delta_m_smooth, test$pred_rf)
xi_gam  <- chatterjee_xi(test$delta_m_smooth, test$pred_gam)

cat("\nChatterjee ξ Values:\n")
cat("Time vs Δm:", round(xi_time, 4), "\n")
cat("Linear:", round(xi_lin, 4), "\n")
cat("Poly3:", round(xi_poly, 4), "\n")
cat("Random Forest:", round(xi_rf, 4), "\n")
cat("GAM:", round(xi_gam, 4), "\n")

### 5) Plot Comparison ###

ggplot() +
  geom_point(data = df, aes(Time_min, delta_m_smooth), color="black", alpha=0.6) +
  geom_line(data = test, aes(Time_min, pred_lm), color="blue", size=1.2) +
  geom_line(data = test, aes(Time_min, pred_poly), color="purple", size=1.2) +
  geom_line(data = test, aes(Time_min, pred_gam), color="red", size=1.6) +
  geom_line(data = test, aes(Time_min, pred_rf), color="green4", size=1.2) +
  labs(
    title="Model Comparison on Condition E",
    subtitle="Black = Actual | Blue = Linear | Purple = Poly3 | Red = GAM | Green = RF",
    x="Time (min)", y="Δm"
  ) +
  theme_minimal()

library(mgcv)
library(ggplot2)

# ---- Data prep ----
df <- condE
df <- df[complete.cases(df$Time_min, df$delta_m_smooth), ]
df <- df[order(df$Time_min), ]

# Time split (70% train)
split <- floor(0.7 * nrow(df))
train <- df[1:split, ]
test  <- df[(split+1):nrow(df), ]

# ---- GAM Model ----
gam_model <- gam(delta_m_smooth ~ s(Time_min, k = 10), data=train)

# Prediction on test set
test_pred <- predict(gam_model, newdata=test, se.fit=TRUE)
test$pred <- test_pred$fit

# Chatterjee Rank Correlation (ξ)
xi_time <- chatterjee_xi(df$Time_min, df$delta_m_smooth)
xi_gam  <- chatterjee_xi(test$delta_m_smooth, test$pred)

cat("Chatterjee ξ (Time vs Δm):", round(xi_time, 4), "\n")
cat("Chatterjee ξ (Actual vs GAM):", round(xi_gam, 4), "\n")

# ---- Metrics ----
RMSE <- function(a,p) sqrt(mean((a - p)^2))
R2_ts <- function(a,p) {
  ssr <- sum((a - p)^2)
  sst <- sum((a - mean(a))^2)
  1 - ssr/sst
}

rmse <- RMSE(test$delta_m_smooth, test$pred)
r2   <- R2_ts(test$delta_m_smooth, test$pred)

cat("RMSE =", rmse, "\n")
cat("R2_ts =", r2, "\n")

# ---- Forecast 60 minutes ahead ----
future <- data.frame(Time_min = seq(max(df$Time_min), max(df$Time_min)+60, by=1))
future_pred <- predict(gam_model, newdata=future, se.fit=TRUE)
future$fit  <- future_pred$fit
future$upper <- future$fit + 1.96 * future_pred$se.fit
future$lower <- future$fit - 1.96 * future_pred$se.fit

# CI on full curve for plotting
full_pred <- predict(gam_model, newdata=df, se.fit=TRUE)
df$fit   <- full_pred$fit
df$upper <- df$fit + 1.96 * full_pred$se.fit
df$lower <- df$fit - 1.96 * full_pred$se.fit

# ---- Plot ----
ggplot() +
  geom_point(data=df, aes(Time_min, delta_m_smooth), color="black", alpha=0.6) +
  geom_ribbon(data=df, aes(Time_min, ymin=lower, ymax=upper), fill="skyblue2", alpha=0.25) +
  geom_line(data=df, aes(Time_min, fit), color="red", size=1.3) +
  geom_line(data=test, aes(Time_min, pred), color="darkred", linetype="dashed", size=1.2) +
  geom_ribbon(data=future, aes(Time_min, ymin=lower, ymax=upper), fill="lightblue", alpha=0.35) +
  geom_line(data=future, aes(Time_min, fit), color="blue", size=1.3) +
  labs(
    title = "GAM Fit and Forecast - Condition E",
    subtitle = paste("RMSE =", round(rmse,2), "| R² =", round(r2,3)),
    x = "Time (min)",
    y = "Δm (ng/cm²)"
  ) +
  theme_minimal(base_size = 14)

library(mgcv)
library(minpack.lm)
library(ggplot2)

# -------- Data prep --------
df <- condE
df <- df[complete.cases(df$Time_min, df$delta_m_smooth), ]
df <- df[order(df$Time_min), ]

# Train-test split (time-based)
split <- floor(0.7 * nrow(df))
train <- df[1:split, ]
test  <- df[(split+1):nrow(df), ]

# -------- GAM model --------
gam_model <- gam(delta_m_smooth ~ s(Time_min, k=10), data = train)
test$pred_gam <- predict(gam_model, newdata=test)

# -------- Weibull model --------
# Model: m(t) = A - B * exp(-(t/tau)^k)

wb_start <- list(
  A = max(train$delta_m_smooth),
  B = abs(min(train$delta_m_smooth)),
  tau = median(train$Time_min),
  k = 1.2
)

weibull_model <- nlsLM(
  delta_m_smooth ~ A - B * exp(-(Time_min/tau)^k),
  data=train,
  start=wb_start,
  control=nls.lm.control(maxiter=500)
)

test$pred_wb <- predict(weibull_model, newdata=test)

# -------- Metrics --------
RMSE <- function(a,p) sqrt(mean((a - p)^2))
R2_ts <- function(a,p) {
  ssr <- sum((a - p)^2)
  sst <- sum((a - mean(a))^2)
  1 - ssr/sst
}

rmse_gam <- RMSE(test$delta_m_smooth, test$pred_gam)
r2_gam   <- R2_ts(test$delta_m_smooth, test$pred_gam)

rmse_wb <- RMSE(test$delta_m_smooth, test$pred_wb)
r2_wb   <- R2_ts(test$delta_m_smooth, test$pred_wb)

cat("\nGAM Model:\n", "RMSE =", rmse_gam, "R2 =", r2_gam, "\n")
cat("Weibull Model:\n", "RMSE =", rmse_wb, "R2 =", r2_wb, "\n")

# -------- Plot --------
ggplot() +
  geom_point(data=df, aes(Time_min, delta_m_smooth), color="black", alpha=0.6) +
  geom_line(data=test, aes(Time_min, pred_gam), color="red", size=1.3) +
  geom_line(data=test, aes(Time_min, pred_wb), color="blue", size=1.3, linetype="dashed") +
  labs(
    title="Condition E: GAM vs Weibull Fit",
    subtitle=paste("GAM (red): RMSE =", round(rmse_gam,2), 
                   "| Weibull (blue): RMSE =", round(rmse_wb,2)),
    x="Time (min)",
    y="Δm (ng/cm²)"
  ) +
  theme_minimal(base_size = 14) +
  scale_color_manual(values=c("red","blue"))