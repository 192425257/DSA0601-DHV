# DSA06 Data Handling and Visualization - Class Activity 3
# Student: MOTHUKURI MARUTHI | R.No. 22 | Reg. No. 192425257
# Slot C | Time Series in R and Tableau
# Run this entire script in RStudio. It creates ts_data.csv and all R charts/results.

set.seed(40374)
branch <- c("T Nagar", "Tambaram", "Anna Nagar")
n <- 36
month_no <- 1:n
date <- seq(as.Date("2023-01-01"), by = "month", length.out = n)
base <- c(130, 60, 110)
growth <- c(1.2, 0.8, 2)
amp <- c(6, 3, 5)
peak_month <- 3

sales <- sapply(1:3, function(i) {
  round(base[i] + growth[i] * month_no +
          amp[i] * cos(2 * pi * (month_no - peak_month) / 12) +
          rnorm(n, 0, 2), 1)
})

df <- data.frame(
  Month = rep(date, 3),
  Month_no = rep(month_no, 3),
  Branch = rep(branch, each = n),
  Sales = as.vector(sales)
)
write.csv(df, "ts_data.csv", row.names = FALSE)

# Arrange the matrix explicitly by branch, then month.
sales_mat <- sapply(1:3, function(i) sales[, i])
colnames(sales_mat) <- branch

# R1: Multiple time-series chart and linear slopes
png("R1_Multiple_Time_Series.png", width = 1400, height = 800, res = 130)
matplot(date, sales_mat, type = "l", lty = 1, lwd = 2,
        col = c("#D55E00", "#0072B2", "#009E73"),
        xlab = "Month", ylab = "Sales (Rs lakh)",
        main = "Monthly Sales by Branch (Jan 2023–Dec 2025)")
last_x <- date[n]
text(last_x, sales_mat[n, ], labels = branch, pos = 4,
     col = c("#D55E00", "#0072B2", "#009E73"), cex = 0.9)
dev.off()

slopes <- sapply(1:3, function(i) coef(lm(sales[, i] ~ month_no))[2])
names(slopes) <- branch
cat("\nR1: Linear slopes (Rs lakh/month)\n")
print(round(slopes, 4))
cat("Fastest-growing branch:", names(which.max(slopes)), "\n")

# R2: STL decomposition for T Nagar
x <- ts(sales[, 1], frequency = 12, start = c(2023, 1))
fit_stl <- stl(x, s.window = "periodic")
png("R2_T_Nagar_STL.png", width = 1400, height = 900, res = 130)
plot(fit_stl, main = "T Nagar STL Decomposition (Periodic Seasonality)")
dev.off()

seasonal <- fit_stl$time.series[, "seasonal"]
remainder <- fit_stl$time.series[, "remainder"]
seasonal_month_means <- tapply(as.numeric(seasonal),
                               cycle(x), mean)
peak_num <- as.integer(names(which.max(seasonal_month_means)))
trough_num <- as.integer(names(which.min(seasonal_month_means)))
month_names <- month.abb
Fs <- max(0, 1 - var(as.numeric(remainder)) /
            var(as.numeric(seasonal + remainder)))
cat("\nR2: Seasonal peak month:", month_names[peak_num], "\n")
cat("R2: Seasonal trough month:", month_names[trough_num], "\n")
cat("R2: Seasonal strength Fs:", round(Fs, 4), "\n")

# R3: Total monthly sales and trailing moving averages
total_sales <- rowSums(sales_mat)
ma3 <- stats::filter(total_sales, rep(1/3, 3), sides = 1)
ma12 <- stats::filter(total_sales, rep(1/12, 12), sides = 1)

png("R3_Total_Sales_Moving_Averages.png", width = 1400, height = 800, res = 130)
plot(date, total_sales, type = "l", lwd = 1.5, col = "grey35",
     xlab = "Month", ylab = "Total Sales (Rs lakh)",
     main = "Total Sales with Trailing Moving Averages")
lines(date, ma3, col = "#0072B2", lwd = 2)
lines(date, ma12, col = "#D55E00", lwd = 2)
legend("topleft", legend = c("Total sales", "3-month MA", "12-month MA"),
       col = c("grey35", "#0072B2", "#D55E00"), lty = 1, lwd = c(1.5, 2, 2),
       bty = "n")
dev.off()
cat("\nR3: Total sales at month 24:", total_sales[24], "\n")
cat("R3: 3-month trailing MA at month 24:", round(ma3[24], 4), "\n")
cat("R3: 12-month trailing MA at month 24:", round(ma12[24], 4), "\n")
cat("R3: The 12-month MA lags more because it averages a longer history.\n")

# R4: Tambaram detrended residuals and ACF
tambaram_lm <- lm(sales[, 2] ~ month_no)
tambaram_resid <- residuals(tambaram_lm)
png("R4_Tambaram_ACF.png", width = 1200, height = 800, res = 130)
acf(tambaram_resid, lag.max = 14,
    main = "ACF of Detrended Tambaram Sales")
dev.off()
acf_obj <- acf(tambaram_resid, lag.max = 14, plot = FALSE)
acf12 <- as.numeric(acf_obj$acf[13]) # lag 12 is index 12 + lag-zero entry
bound <- 1.96 / sqrt(36)
cat("\nR4: ACF at lag 12:", round(acf12, 4), "\n")
cat("R4: Significance bound +/-", round(bound, 4), "\n")
cat("R4: Yearly-cycle indication:",
    if (abs(acf12) > bound) "Yes, lag-12 autocorrelation exceeds the bound."
    else "No, lag-12 autocorrelation does not exceed the bound.", "\n")

# R5: Holt-Winters fitted on months 1–30; forecast months 31–36
train <- window(x, end = c(2025, 6))
test <- window(x, start = c(2025, 7))
hw_fit <- HoltWinters(train)
hw_pred <- predict(hw_fit, n.ahead = 6, prediction.interval = TRUE,
                   level = 0.95)
forecast_mean <- as.numeric(hw_pred[, "fit"])
lower <- as.numeric(hw_pred[, "lwr"])
upper <- as.numeric(hw_pred[, "upr"])
actual <- as.numeric(test)
mape <- mean(abs((actual - forecast_mean) / actual)) * 100
width_h1 <- upper[1] - lower[1]
width_h6 <- upper[6] - lower[6]

png("R5_HoltWinters_Forecast.png", width = 1400, height = 800, res = 130)
plot(x, xlab = "Year", ylab = "Sales (Rs lakh)",
     main = "T Nagar Holt-Winters Forecast: Jul–Dec 2025",
     xlim = c(2023, 2026), ylim = range(c(x, lower, upper)))
lines(ts(forecast_mean, start = c(2025, 7), frequency = 12),
      col = "#D55E00", lwd = 2)
lines(ts(lower, start = c(2025, 7), frequency = 12),
      col = "#0072B2", lty = 2, lwd = 1.5)
lines(ts(upper, start = c(2025, 7), frequency = 12),
      col = "#0072B2", lty = 2, lwd = 1.5)
lines(test, col = "black", lwd = 2)
legend("topleft", legend = c("History", "Forecast", "95% interval limits", "Actual test data"),
       col = c("black", "#D55E00", "#0072B2", "black"),
       lty = c(1, 1, 2, 1), lwd = c(1.5, 2, 1.5, 2), bty = "n")
dev.off()

cat("\nR5: Forecasts for months 31–36:\n")
print(round(forecast_mean, 3))
cat("R5: Actual values for months 31–36:\n")
print(actual)
cat("R5: MAPE (%):", round(mape, 3), "\n")
cat("R5: 95% band width at h=1:", round(width_h1, 3), "\n")
cat("R5: 95% band width at h=6:", round(width_h6, 3), "\n")

# Export a compact results file for checking/recording answers
results <- data.frame(
  Item = c("R1 slope T Nagar", "R1 slope Tambaram", "R1 slope Anna Nagar",
           "R2 seasonal peak month", "R2 seasonal trough month", "R2 Fs",
           "R3 total month 24", "R3 MA3 month 24", "R3 MA12 month 24",
           "R4 ACF lag 12", "R4 bound", "R5 MAPE percent",
           "R5 band width h1", "R5 band width h6"),
  Value = c(unname(slopes[1]), unname(slopes[2]), unname(slopes[3]),
            month_names[peak_num], month_names[trough_num], Fs,
            total_sales[24], ma3[24], ma12[24], acf12, bound, mape,
            width_h1, width_h6)
)
write.csv(results, "R_Results.csv", row.names = FALSE)
cat("\nCreated ts_data.csv, R_Results.csv, and R1–R5 PNG charts in the working directory.\n")
