## a four-parameter logistic regression model is fit to ceftazidime concentrations to estimated PD parameters
library (readxl)
library(drda)
caz_avi_kfab <- read_excel("datasets_single/kp_FAB_caz_avi_powder4-1.xlsx")
fitkfab <- drda(tpos ~ ctz_s, data=caz_avi_kfab, mean_function = "loglogistic4", max_iter = 1000)
plot(fitkfab, xlab = "CAZ/AVI serum conc. (mg/L)", ylab = "Tpos (hr)")
tidy (fitkfab,conf.int = TRUE, conf.level = 0.95)
library (tidy)
library (broom)
tidy(fitkfab)
tid