library (ggplot2)
library(scales)
library(forestmangr)
library(kableExtra)
theme_set(theme_bw())
## Figure1a
## import raw data from .csv file
wp1 <- read.csv("~/Desktop/ACUTEWEBSITE/wp1a.csv")
## plot raw data as x-y graph Tpos graph vs. drug concentrations stratified by dilution matrix, method="lm" is the method for linear regression
library (ggplot2)
library(scales)
theme_set(theme_bw())
## import raw data from .csv file
wp1 <- read.csv("~/Desktop/ACUTEWEBSITE/wp1a.csv")
## plot raw data as x-y graph Tpos graph vs. drug concentrations stratified by dilution matrix, method="lm" is the method for linear regression
fig1 <-ggplot(wp1, aes(x=inoculum, y=tpos, color=isolates, shape=diluent, fill=isolates)) + geom_point(size=4, alpha = 0.5) + 
  scale_y_continuous(name="Tpos (hr)", limits=c(4,12)) +
  theme(legend.text=element_text(size=12)) +
  geom_smooth(aes(linetype=diluent), method=lm , color="black", fill="#69b3a2", se=TRUE, inherit.aes = TRUE )
fig1 + theme_bw(base_size = 14)+ scale_x_log10(name="Inoculum CFU/mL", breaks = trans_breaks("log10",n=7, function(x) 10^x),labels = trans_format("log10", math_format(10^.x)))

wp1 <- read.csv("~/Desktop/ACUTEWEBSITE/wp1a.csv")
df <- data.frame (wp1)
table1<-lm_table(df, log(inoculum) ~ tpos, "diluent")
kbl(table1)%>%
  kable_paper("hover", full_width = F, position="left")



## Figure1b
library (readxl)
theme_set(theme_bw())
wp2 <- read_excel("datasets_single/ecoliatcc_inoculum.xlsx")
## plot raw data as x-y graph Tpos graph vs. drug concentrations stratified by dilution matrix, method="lm" is the method for linear regression
fig1bb <-ggplot(wp2, aes(x=inoculum, y=tpos, color=isolates, fill=isolates)) + geom_point(size=2, alpha = 0.5) + 
  scale_y_continuous(name="Tpos (hr)", limits=c(4,12)) +
  theme(legend.text=element_text(size=10)) +
  geom_smooth(aes(linetype=diluent), method=lm , color="black", fill="#69b3a2", se=TRUE, inherit.aes = TRUE )
fig1b<-fig1bb + theme_bw(base_size = 10)+ scale_x_log10(name="Inoculum CFU/mL", breaks = trans_breaks("log10",n=7, function(x) 10^x),labels = trans_format("log10", math_format(10^.x)))

wp2 <- read.csv("~/Desktop/ACUTEWEBSITE/datasets_single/fig1akp.csv")
df <- data.frame (wp2)
table1<-lm_table(df, log(inoculum) ~ tpos)
kbl(table1)%>%
  kable_paper("hover", full_width = F, position="left")


## Figure 1C
library (ggplot2)
library(scales)
theme_set(theme_bw())
## import raw data from .csv file
library (readxl)
abaumani <- read_excel("abaum_pbs.xlsx")
## plot raw data as x-y graph Tpos graph vs. drug concentrations stratified by dilution matrix, method="lm" is the method for linear regression
fig1ab <-ggplot(abaumani, aes(x=inoculum, y=tpos)) + geom_point(size=2, alpha = 0.5) + 
  scale_y_continuous(name="Tpos (hr)", limits=c(4,12)) +
  theme(legend.text=element_text(size=10)) +
  geom_smooth(aes(linetype=diluent), method=lm , color="black", fill="#69b3a2", se=TRUE, inherit.aes = TRUE )
fig1c<-fig1ab + theme_bw(base_size = 10)+ scale_x_log10(name="Inoculum CFU/mL", breaks = trans_breaks("log10",n=7, function(x) 10^x),labels = trans_format("log10", math_format(10^.x)))


## Figure 1D
library (ggplot2)
library(scales)
theme_set(theme_bw())
## import raw data from .csv file
library (readxl)
pseudo <- read_excel("pseudo_pbs.xlsx")
## plot raw data as x-y graph Tpos graph vs. drug concentrations stratified by dilution matrix, method="lm" is the method for linear regression
fig2d <-ggplot(pseudo, aes(x=inoculum, y=tpos, color=isolates, fill=isolates)) + geom_point(size=2, alpha = 0.5) + 
  scale_y_continuous(name="Tpos (hr)", limits=c(4,12)) +
  theme(legend.text=element_text(size=10)) +
  geom_smooth(aes(linetype=diluent), method=lm , color="black", fill="#69b3a2", se=TRUE, inherit.aes = TRUE )
fig1d<-fig2d + theme_bw(base_size = 10)+ scale_x_log10(name="Inoculum CFU/mL", breaks = trans_breaks("log10",n=7, function(x) 10^x),labels = trans_format("log10", math_format(10^.x)))

library (patchwork)

fig1a + fig1b + fig1c + fig1d

## Figure 2

#| label: fig-compkp
#| fig-cap: "Pharmacodynamic relationship of Tpos to ceftazidime/avibactam concentrations"

## a four-parameter logistic regression model is fit to ceftazidime concentrations to estimated PD parameters
library (readxl)
library(drda)
kpca <- read_excel("datasets_single/kpca_caz_avi_powder_4-1.xlsx")
kpcb <- read_excel("datasets_single/kpcb_caz_avi_powder_4-1.xlsx")
kpccatania <- read_excel("datasets_single/kpccatania_caz_avi_powder_4-1.xlsx")
caz_avi_kfab <- read_excel("datasets_single/kp_FAB_caz_avi_powder4-1.xlsx")
kpwt <- read_excel("datasets_single/kpwt_caz_avi_powder_4-1.xlsx")
caz_avi_kprad <- read_excel("datasets_single/kp_PRAD_caz_avi_powder4-1.xlsx")
kpatcc <- read_excel("datasets_single/kpatcc_caz_avi_powder_4-1.xlsx")

## fit models for each of the isolates
library (broom)
fitkpca <- drda(tpos ~ ctz_s, kpca, mean_function = "loglogistic4", max_iter = 1000)
fitkpcatcc <- drda(tpos ~ ctz_s, kpatcc, mean_function = "loglogistic4", max_iter = 1000)
fitkpcb <- drda(tpos ~ ctz_s, kpcb, mean_function = "loglogistic4", max_iter = 1000)
fitkpccatania <- drda(tpos ~ ctz_s, kpccatania, mean_function = "loglogistic4", max_iter = 1000)
fitkpwt <- drda(tpos ~ ctz_s, kpwt, mean_function = "loglogistic4", max_iter = 1000)
fitkfab <- drda(tpos ~ ctz_s, data=caz_avi_kfab, mean_function = "loglogistic4", max_iter = 1000)
fitkprad <- drda(tpos ~ ctz_s, data=caz_avi_kprad, mean_function = "loglogistic4", max_iter = 1000)

#tidymodels
library (drc)
library (tidydrc)


## plot all of the isolates together
fitkpca <- drda(tpos ~ ctz_s, kpca, mean_function = "loglogistic4", max_iter = 1000)
fitkpcatcc <- drda(tpos ~ ctz_s, kpatcc, mean_function = "loglogistic4", max_iter = 1000)
fitkpcb <- drda(tpos ~ ctz_s, kpcb, mean_function = "loglogistic4", max_iter = 1000)
fitkpccatania <- drda(tpos ~ ctz_s, kpccatania, mean_function = "loglogistic4", max_iter = 1000)
fitkpwt <- drda(tpos ~ ctz_s, kpwt, mean_function = "loglogistic4", max_iter = 1000)
fitkfab <- drda(tpos ~ ctz_s, data=caz_avi_kfab, mean_function = "loglogistic4", max_iter = 1000)
fitkprad <- drda(tpos ~ ctz_s, data=caz_avi_kprad, mean_function = "loglogistic4", max_iter = 1000)
## plot all of the isolates together

