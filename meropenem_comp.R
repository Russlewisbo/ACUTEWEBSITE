
library (readxl)
library(drda)
mero <- read_excel("datasets_single/ATCC_meropenem.xlsx")
fit11 <- drda(tpos ~ mero_s, data=mero, mean_function = "loglogistic4", max_iter = 1000)
plot(fit11, xlab = "Meropenem serum conc. (mg/L)", ylab = "Tpos (hr)", xlim =c(0,100))

mero2 <- read_excel("datasets_single/KPCB_meropenem.xlsx")
fit12 <- drda(tpos ~ mero_s, data=mero2, mean_function = "loglogistic4", max_iter = 1000)
plot(fit12, xlab = "Meropenem serum conc. (mg/L)", ylab = "Tpos (hr)", xlim =c(0,100))
