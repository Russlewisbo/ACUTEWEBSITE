## Comparison of meropenem curves for ATCC and KPC_B strains adjusted for MIC

library (readxl)
library(drda)
mero <- read_excel("datasets_single/ATCC_meropenem.xlsx")
mero2 <- read_excel("datasets_single/kpcb_meropenem.xlsx")
fit1 <- drda(tpos ~ mero_mic, data=mero, mean_function = "loglogistic4", max_iter = 1000)
fit2 <- drda(tpos ~ mero_mic2, data=mero2, mean_function = "loglogistic4", max_iter = 1000)
plot(fit1, fit2, xlab = "Meropenem conc. (mg/L)/MIC", base=10, ylab = "Tpos (hr)", xlim =c(0,1000))

## Comparison of ceftazidime-avibactam curves for KPC-A and KPC_B strains adjusted for MIC

library (readxl)
library(drda)
ctz1 <- read_excel("datasets_single/kpca_caz_avi_powder_4-1.xlsx")
ctz2 <- read_excel("datasets_single/kpcb_caz_avi_powder_4-1.xlsx")
ctz3 <- read_excel("datasets_single/atcc_cazavi.xlsx")
fit3 <- drda(tpos ~ ctz_mic, data=ctz1, mean_function = "loglogistic4", max_iter = 1000)
fit4 <- drda(tpos ~ ctz_mic, data=ctz2, mean_function = "loglogistic4", max_iter = 1000)
fit5 <- drda(tpos ~ ctz_mic, data=ctz3, mean_function = "loglogistic4", max_iter = 1000)
p1<-plot(fit3, fit4, fit5, xlab = "Ceftazidime conc. (mg/L)/MIC", base=10, ylab = "Tpos (hr)", xlim =c(0,1000))
plot(fit4, xlab = "Ceftazidime conc. (mg/L)/MIC", col="blue", base=10, ylab = "Tpos (hr)", xlim =c(0,100))

# Comparison of tigecycline curves for KPC-A and KPC_B strains adjusted for MIC

tig1 <- read_excel("datasets_single/kpca_tig.xlsx")
tig2 <- read_excel("datasets_single/kpcb_tig.xlsx")
fittig1 <- drda(tpos ~ tig_mic, data=tig1, mean_function = "loglogistic4", max_iter = 1000)
fittig2 <- drda(tpos ~ tig_mic, data=tig2, mean_function = "loglogistic4", max_iter = 1000)
p2<-plot(fittig1, fittig2, xlab = "Tigecycline conc. (mg/L)/MIC", base=10, ylab = "Tpos (hr)", xlim =c(0,1000))


# Comparison of gentamicin curves for KPC-A and KPC_B strains adjusted for MIC

gent1 <- read_excel("datasets_single/kpca_gent.xlsx")
gent2 <- read_excel("datasets_single/kpcb_gent.xlsx")
fitgent1 <- drda(tpos ~ gent_mic, data=gent1, mean_function = "loglogistic4", max_iter = 1000)
fitgent2 <- drda(tpos ~ gent_mic, data=gent2, mean_function = "loglogistic4", max_iter = 1000)
p3<-plot(fitgent1, fitgent2, xlab = "gentamcin conc. (mg/L)/MIC", base=10, ylab = "Tpos (hr)", xlim =c(0,1000))

# Colistin adjusted for MIC

col1 <- read_excel("datasets_single/kpcb_coli.xlsx")
fitco1 <- drda(tpos ~ col_mic, data=col1, mean_function = "loglogistic4", max_iter = 1000)
p4<-plot(fitco1, xlab = "colistin conc. (mg/L)/MIC", base=10, ylab = "Tpos (hr)", xlim =c(0,1000))

# plot all data

plot (fit1, fit2, fit3, fit4, fit5, xlab = "antibiotic conc. (mg/L)/MIC", base=10, ylab = "Tpos (hr)", xlim =c(0,1000))
plot (fittig1, fittig2, fitgent1, fitgent2, fitco1, xlab = "antibiotic conc. (mg/L)/MIC", base=10, ylab = "Tpos (hr)", xlim =c(0,1000))
plot (fit1, fit2, fit3, fit4, fit5, fittig1, fittig2, fitgent1, fitgent2, fitco1, xlab = "antibiotic conc. (mg/L)/MIC", base=10, ylab = "Tpos (hr)", xlim =c(0,1000))
## plot raw data as x-y graph Tpos graph vs. drug concentrations stratified by dilution matrix, method="lm" is the method for linear regression

library (readxl)
library(drda)
library(broom)
library(kableExtra)
ed<-effective_dose(fitcol1, y = c(0.10,0.25,0.50,0.75,0.90,0.95))
kbl(ed)%>%
  kable_paper("hover", full_width = F, position = "left")
