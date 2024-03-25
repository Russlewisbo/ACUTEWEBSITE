
## a four-parameter logistic regression model is fit to ceftazidime concentrations to estimated PD parameters
library (readxl)
library(drda)
library (ggplot2)
library (broom)

caz_avi_7a<- read_excel("datasets_single/kpatcc_caz_avi_powder_4-1.xlsx")
df <- data.frame(caz_avi_7a)
# Fit the model (replace this with your actual model fitting code)
fit_atcc <- drda(tpos ~ ctz_s, data=caz_avi_7a, mean_function = "loglogistic4", max_iter = 1000)
plot(fit_atcc, xlab = "CAZ/AVI serum conc. (mg/L)", ylab = "Tpos (hr)")

# Generate a new data frame with predicted values
df_pred <- data.frame(
  ctz_s = seq(min(df$ctz_s), max(df$ctz_s), length.out = 100)
)
df_pred$tpos_pred <- predict(fit, newdata = df_pred)

# Compute the confidence intervals
conf_int <- predict(fit, newdata = df_pred, interval = "confidence")
df_pred$ymin <- conf_int[, "lower"]
df_pred$ymax <- conf_int[, "upper"]

# Plot the results
ggplot(df, aes(x = x, y = y)) +
  geom_point() +
  geom_line(data = df_pred, aes(y = y_pred), color = "red") +
  geom_ribbon(data = df_pred, aes(ymin = ymin, ymax = ymax), alpha = 0.2)
