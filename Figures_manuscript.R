# Load necessary libraries
library (readxl)
library(drc)
library(ggplot2)
library(ggthemes)
library (ggsci)
library (devtools)
library (drda)
library (envalysis)
library (scales)
library(dplyr)


# FIGURE 1A #


###########################KPC_B##################################################################


# Assuming df is your data frame and x and y are your variables
kpc_b <- read_excel("/Users/russelllewis/Desktop/ACUTEWEBSITE/KPCB.xlsx")
df3 <- kpc_b
df3 <- na.omit(df3)



# Fit the 4PL model- ATCC isolate
fit3 <- drm( KPCB~ Conc, data = df3, fct = LL.4())

# Create a new data frame for predictions
newdata3 <- data.frame(Conc = seq(min(df3$Conc), max(df3$Conc), length.out = 100))

# Predict with 95% CI
pred3 <- predict(fit3, newdata = newdata3, interval = "confidence")
df_pred3 <- data.frame(pred3)

# Bind the predictions to the new data frame
newdata3 <- cbind(newdata3, pred3)


# Plot with ggplot2

ggplot(newdata3, aes(x = Conc, y = Prediction )) + 
  geom_line(aes(y = Prediction)) +  geom_point(data = df3, aes(x = Conc, y = KPCB), shape = 2, size = 4) +
  geom_ribbon(aes(ymin = Lower, ymax = Upper), alpha = 0.2) +
  labs(x = "Serum Ceftazidime-Avibactam [4:1] mg/L", y = "Time to positivity (Tpos) hours", title = "4-Parameter Logistic Regression with 95% CI") + theme_classic ()

###############################KPWT##############################################################
# Assuming df is your data frame and x and y are your variables
KPWT <- read_excel("/Users/russelllewis/Desktop/ACUTEWEBSITE/KPWT.xlsx") 

df1 <- KPWT
df1 <- na.omit(df1)


# Fit the 4PL model- ATCC isolate
fit1 <- drm(KPWT ~ Conc, data = df1, fct = LL.4())

# Create a new data frame for predictions
newdata1 <- data.frame(Conc = seq(min(df1$Conc), max(df1$Conc), length.out = 100))

# Predict with 95% CI
pred1 <- predict(fit1, newdata = newdata1, interval = "confidence")
df_pred1 <- data.frame(pred1)

# Bind the predictions to the new data frame
newdata1 <- cbind(newdata1, pred1)


# Plot with ggplot2

p1 <- ggplot() + 
  geom_line(data = newdata1, aes(x = Conc, y = Prediction)) +  
  geom_point(data = df1, aes(x = Conc, y = KPWT), shape = 1, size = 4) +
  geom_ribbon(data = newdata1, aes(x= Conc, ymin = Lower, ymax = Upper), alpha = 0.2) +
  geom_line(data = newdata3, aes(x = Conc, y = Prediction)) +  
  geom_point(data = df3, aes(x = Conc, y = KPC_B), shape = 2, size = 4) +
  geom_ribbon(data = newdata3, aes(x = Conc, ymin = Lower, ymax = Upper), alpha = 0.2) +
  labs(x = "Serum Ceftazidime-Avibactam [4:1] mg/L", y = "Time to positivity (Tpos) hours", title = "4-Parameter Logistic Regression with 95% CI") +
  scale_x_log10() + theme_classic ()


############KPC_A#################################################################################

KPCA <- read_excel("/Users/russelllewis/Desktop/ACUTEWEBSITE/KPCA.xlsx") 

df2 <- KPCA
df2 <- na.omit(df2)


# Fit the 4PL model- ATCC isolate
fit2 <- drm(KPCA ~ Conc, data = df2, fct = LL.4())

# Create a new data frame for predictions
newdata2 <- data.frame(Conc = seq(min(df2$Conc), max(df2$Conc), length.out = 100))

# Predict with 95% CI
pred2 <- predict(fit2, newdata = newdata2, interval = "confidence")
df_pred2 <- data.frame(pred2)

# Bind the predictions to the new data frame
newdata2 <- cbind(newdata2, pred2)


# Plot with ggplot2

ggplot() + 
  geom_line(data = newdata1, aes(x = Conc, y = Prediction)) +  
  geom_point(data = df1, aes(x = Conc, y = KPWT), shape = 1, size = 4) +
  geom_ribbon(data = newdata1, aes(x= Conc, ymin = Lower, ymax = Upper), alpha = 0.2) +
  geom_line(data = newdata2, aes(x = Conc, y = Prediction)) +  
  geom_point(data = df2, aes(x = Conc, y = KPCA), shape = 2, size = 4) +
  geom_ribbon(data = newdata2, aes(x= Conc, ymin = Lower, ymax = Upper), alpha = 0.2) +
  geom_line(data = newdata3, aes(x = Conc, y = Prediction)) +
  geom_point(data = df3, aes(x = Conc, y = KPCB), shape = 3, size = 4) +
  geom_ribbon(data = newdata3, aes(x = Conc, ymin = Lower, ymax = Upper), alpha = 0.2) +
  labs(x = "Serum Ceftazidime-Avibactam [4:1] mg/L", y = "Time to positivity (Tpos) hours", title = "4-Parameter Logistic Regression with 95% CI") +
  scale_x_log10() + theme_classic ()

############KPRAD#################################################################################

KPRAD <- read_excel("/Users/russelllewis/Desktop/ACUTEWEBSITE/KPRAD.xlsx") 

df4 <- KPRAD
df4 <- na.omit(df4)


# Fit the 4PL model- ATCC isolate
fit4 <- drm(KPRAD ~ Conc, data = df4, fct = LL.4())

# Create a new data frame for predictions
newdata4 <- data.frame(Conc = seq(min(df4$Conc), max(df4$Conc), length.out = 100))

# Predict with 95% CI
pred4 <- predict(fit4, newdata = newdata4, interval = "confidence")
df_pred4 <- data.frame(pred4)

# Bind the predictions to the new data frame
newdata4 <- cbind(newdata4, pred4)

ggplot() + 
  geom_line(data = newdata1, aes(x = Conc, y = Prediction)) +  
  geom_point(data = df1, aes(x = Conc, y = KPWT), shape = 1, size = 4) +
  geom_ribbon(data = newdata1, aes(x= Conc, ymin = Lower, ymax = Upper), alpha = 0.2) +
  geom_line(data = newdata2, aes(x = Conc, y = Prediction)) +  
  geom_point(data = df2, aes(x = Conc, y = KPCA), shape = 2, size = 4) +
  geom_ribbon(data = newdata2, aes(x= Conc, ymin = Lower, ymax = Upper), alpha = 0.2) +
  geom_line(data = newdata3, aes(x = Conc, y = Prediction)) +
  geom_point(data = df3, aes(x = Conc, y = KPCB), shape = 3, size = 4) +
  geom_ribbon(data = newdata3, aes(x = Conc, ymin = Lower, ymax = Upper), alpha = 0.2) +
  geom_line(data = newdata4, aes(x = Conc, y = Prediction)) +
  geom_point(data = df4, aes(x = Conc, y = KPRAD), shape = 5, size = 4) +
  geom_ribbon(data = newdata4, aes(x = Conc, ymin = Lower, ymax = Upper), alpha = 0.2) +
  labs(x = "Spiked serum ceftazidime-avibactam [4:1] mg/L", y = "Time to positivity (Tpos) hours") +
  scale_x_log10() + theme_base()

############KFAB#################################################################################

KFAB <- read_excel("/Users/russelllewis/Desktop/ACUTEWEBSITE/KFAB.xlsx") 

df5 <- KFAB
df5 <- na.omit(df5)


# Fit the 4PL model- ATCC isolate
fit5 <- drm(KPFAB ~ Conc, data = df5, fct = LL.4())

# Create a new data frame for predictions
newdata5 <- data.frame(Conc = seq(min(df5$Conc), max(df5$Conc), length.out = 100))

# Predict with 95% CI
pred5 <- predict(fit5, newdata = newdata5, interval = "confidence")
df_pred5 <- data.frame(pred5)

# Bind the predictions to the new data frame
newdata5 <- cbind(newdata5, pred5)

ggplot() + 
  geom_line(data = newdata1, aes(x = Conc, y = Prediction)) +  
  geom_point(data = df1, aes(x = Conc, y = KPWT), shape = 1, size = 4) +
  geom_ribbon(data = newdata1, aes(x= Conc, ymin = Lower, ymax = Upper), alpha = 0.2) +
  geom_line(data = newdata2, aes(x = Conc, y = Prediction)) +  
  geom_point(data = df2, aes(x = Conc, y = KPCA), shape = 2, size = 4) +
  geom_ribbon(data = newdata2, aes(x= Conc, ymin = Lower, ymax = Upper), alpha = 0.2) +
  geom_line(data = newdata3, aes(x = Conc, y = Prediction)) +
  geom_point(data = df3, aes(x = Conc, y = KPCB), shape = 8, size = 4) +
  geom_ribbon(data = newdata3, aes(x = Conc, ymin = Lower, ymax = Upper), alpha = 0.2) +
  geom_line(data = newdata4, aes(x = Conc, y = Prediction)) +
  geom_point(data = df4, aes(x = Conc, y = KPRAD), shape = 5, size = 4) +
  geom_ribbon(data = newdata4, aes(x = Conc, ymin = Lower, ymax = Upper), alpha = 0.2) +
  geom_line(data = newdata5, aes(x = Conc, y = Prediction)) +
  geom_point(data = df5, aes(x = Conc, y = KPFAB), shape = 6, size = 4) +
  geom_ribbon(data = newdata5, aes(x = Conc, ymin = Lower, ymax = Upper), alpha = 0.2) +
  labs(x = "Spiked serum ceftazidime-avibactam [4:1] mg/L", y = "Time to positivity (Tpos) hours") +
  scale_x_log10(labels = label_log(digits = 1)) + theme_base()

############KCAT#################################################################################

KPCAT <- read_excel("/Users/russelllewis/Desktop/ACUTEWEBSITE/KPCAT.xlsx") 

df6 <- KPCAT
df6 <- na.omit(df6)


# Fit the 4PL model- ATCC isolate
fit6 <- drm(KPCAT ~ Conc, data = df6, fct = LL.4())

# Create a new data frame for predictions
newdata6 <- data.frame(Conc = seq(min(df6$Conc), max(df6$Conc), length.out = 100))

# Predict with 95% CI
pred6 <- predict(fit6, newdata = newdata6, interval = "confidence")
df_pred6 <- data.frame(pred6)

# Bind the predictions to the new data frame
newdata6 <- cbind(newdata6, pred6)

ggplot() + 
  geom_line(data = newdata1, aes(x = Conc, y = Prediction)) +  
  geom_point(data = df1, aes(x = Conc, y = KPWT), shape = 1, size = 4) +
  geom_ribbon(data = newdata1, aes(x= Conc, ymin = Lower, ymax = Upper), alpha = 0.2) +
  geom_line(data = newdata2, aes(x = Conc, y = Prediction)) +  
  geom_point(data = df2, aes(x = Conc, y = KPCA), shape = 2, size = 4) +
  geom_ribbon(data = newdata2, aes(x= Conc, ymin = Lower, ymax = Upper), alpha = 0.2) +
  geom_line(data = newdata3, aes(x = Conc, y = Prediction)) +
  geom_point(data = df3, aes(x = Conc, y = KPCB), shape = 8, size = 4) +
  geom_ribbon(data = newdata3, aes(x = Conc, ymin = Lower, ymax = Upper), alpha = 0.2) +
  geom_line(data = newdata4, aes(x = Conc, y = Prediction)) +
  geom_point(data = df4, aes(x = Conc, y = KPRAD), shape = 5, size = 4) +
  geom_ribbon(data = newdata4, aes(x = Conc, ymin = Lower, ymax = Upper), alpha = 0.2) +
  geom_line(data = newdata5, aes(x = Conc, y = Prediction)) +
  geom_point(data = df5, aes(x = Conc, y = KPFAB), shape = 6, size = 4) +
  geom_ribbon(data = newdata5, aes(x = Conc, ymin = Lower, ymax = Upper), alpha = 0.2) +
  geom_line(data = newdata6, aes(x = Conc, y = Prediction)) +
  geom_point(data = df6, aes(x = Conc, y = KPCAT), shape = 0, size = 4) +
  geom_ribbon(data = newdata6, aes(x = Conc, ymin = Lower, ymax = Upper), alpha = 0.2) +
  labs(x = "Spiked serum ceftazidime-avibactam [4:1] mg/L", y = "Time to positivity (Tpos) hours") +
  scale_x_log10(labels = label_log(digits = 1)) + theme_base()

###############################_KPATCC_##############################################################
 
KPATCC <- read_excel("/Users/russelllewis/Desktop/ACUTEWEBSITE/KPATCC.xlsx") 

df7 <- KPATCC
df7 <- na.omit(df7)


# Fit the 4PL model- ATCC isolate
fit7 <- drm(KPATCC ~ Conc, data = df7, fct = LL.4())

# Create a new data frame for predictions
newdata7 <- data.frame(Conc = seq(min(df7$Conc), max(df7$Conc), length.out = 100))

# Predict with 95% CI
pred7 <- predict(fit7, newdata = newdata7, interval = "confidence")
df_pred7 <- data.frame(pred7)

# Bind the predictions to the new data frame
newdata7 <- cbind(newdata7, pred7)

ggplot() + 
  geom_line(data = newdata1, aes(x = Conc, y = Prediction, color="K. pneumoniae WT")) +  
  geom_point(data = df1, aes(x = Conc, y = KPWT, color="K. pneumoniae WT"), shape = 0, size = 4) +
  geom_ribbon(data = newdata1, aes(x= Conc, ymin = Lower, ymax = Upper, fill="K. pneumoniae WT"), alpha = 0.2) +
  geom_line(data = newdata2, aes(x = Conc, y = Prediction, color="K. pneumoniae WT")) +  
  geom_point(data = df2, aes(x = Conc, y = KPCA, color="K. pneumoniae KPC_A"), shape = 16, size = 4) +
  geom_ribbon(data = newdata2, aes(x= Conc, ymin = Lower, ymax = Upper, fill="K. pneumoniae KPC_A"), alpha = 0.2) +
  geom_line(data = newdata3, aes(x = Conc, y = Prediction, color="K. pneumoniae KPC_A")) +
  geom_point(data = df3, aes(x = Conc, y = KPCB, color="K. pneumoniae KPC_B"), shape = 17, size = 4) +
  geom_ribbon(data = newdata3, aes(x = Conc, ymin = Lower, ymax = Upper, fill="K. pneumoniae KPC_B"), alpha = 0.2) +
  geom_line(data = newdata4, aes(x = Conc, y = Prediction, color="K. pneumoniae KPC_B")) +
  geom_point(data = df4, aes(x = Conc, y = KPRAD, color="K. pneumoniae PRAD"), shape = 18, size = 4) +
  geom_ribbon(data = newdata4, aes(x = Conc, ymin = Lower, ymax = Upper, fill="K.pneumoniae PRAD"), alpha = 0.2) +
  geom_line(data = newdata5, aes(x = Conc, y = Prediction, color="K. pneumoniae FAB")) +
  geom_point(data = df5, aes(x = Conc, y = KPFAB, color="K. pneumoniae FAB"), shape = 15, size = 4) +
  geom_ribbon(data = newdata5, aes(x = Conc, ymin = Lower, ymax = Upper, fill = "K. pneumoniae FAB"), alpha = 0.2) +
  geom_line(data = newdata6, aes(x = Conc, y = Prediction, color ="K. pneumoniae Cat.")) +
  geom_point(data = df6, aes(x = Conc, y = KPCAT, color = "K. pneumoniae Cat."), shape = 8, size = 4) +
  geom_ribbon(data = newdata6, aes(x = Conc, ymin = Lower, ymax = Upper, fill="K. pneumoniae Cat."), alpha = 0.2) +
  geom_line(data = newdata7, aes(x = Conc, y = Prediction, color= "K. pneumoniae ATCC 700603")) +
  geom_point(data = df7, aes(x = Conc, y = KPATCC, color="K. pneumoniae ATCC 700603"), shape = 1, size = 4) +
  geom_ribbon(data = newdata7, aes(x = Conc, ymin = Lower, ymax = Upper, fill= "K. pneumoniae ATCC 700603"), alpha = 0.2) +
  labs(x = "Spiked serum ceftazidime-avibactam [4:1] mg/L", y = "Time to positivity (Tpos) hours") +
  scale_x_log10(labels = label_log(digits = 1)) + theme_base() +  scale_color_lancet() +  scale_fill_lancet()


# FIGURE 1B #

EC50 <- read_excel("/Users/russelllewis/Desktop/ACUTEWEBSITE/EC50.xlsx")
df <-data.frame(EC50)

ggplot(EC50, aes(x = MIC, y = EC50)) +
  geom_point(aes(color = isolates, shape = isolates), size = 4, alpha = 1.0) +
  scale_shape_manual(values = c(8,16,17,18,0,1,15)) +
  theme(legend.text = element_text(size = 12)) +
  geom_smooth(method = "lm", color = "black", fill = "#69b3a2", se = TRUE) + theme_base() +  scale_color_lancet() +  scale_fill_lancet()


## Figure 2

## gentamicin

gent<- read_excel("/Users/russelllewis/Desktop/ACUTEWEBSITE/gent.xlsx")
gent<-data.frame(gent)
gent <- na.omit(gent)

mero<- read_excel("/Users/russelllewis/Desktop/ACUTEWEBSITE/mero.xlsx")
mero<-data.frame(mero)
mero <- na.omit(mero)

tige <-  read_excel("/Users/russelllewis/Desktop/ACUTEWEBSITE/tigecycline.xlsx")
tige<-data.frame(tige)
tige <- na.omit(tige)

cipro <-  read_excel("/Users/russelllewis/Desktop/ACUTEWEBSITE/cipro.xlsx")
cipro<-data.frame(cipro)
cipro <- na.omit(cipro)

cefavi<-read_excel("/Users/russelllewis/Desktop/ACUTEWEBSITE/ceftaz-avi.xlsx")
cefavi<-data.frame(cefavi)
cefavi <- na.omit(cefavi)

ceftaz<-read_excel("/Users/russelllewis/Desktop/ACUTEWEBSITE/ceftaz.xlsx")
ceftaz<-data.frame(ceftaz)
ceftaz <- na.omit(ceftaz)

coli<-read_excel("/Users/russelllewis/Desktop/ACUTEWEBSITE/colistin.xlsx")
coli<-data.frame(coli)
coli <- na.omit(coli)


# Fit the 4PL models- KPC_B
fit_gent <- drm(Tpos ~ Conc, data = gent, fct = LL.4())
fit_mero <- drm(Tpos ~ Conc, data = mero, fct = LL.4())
fit_tige <- drm(Tpos ~ Conc, data = tige, fct = LL.4())
fit_cipro <- drm(Tpos ~ Conc, data = cipro, fct = LL.4())
fit_cefavi <- drm(Tpos ~ Conc, data = cefavi, fct = LL.4())
fit_ceftaz <- drm(Tpos ~ Conc, data = ceftaz, fct = LL.4())
fit_coli <- drm(Tpos ~ Conc, data = coli, fct = LL.4())

# Create a new data frame for predictions
newdata8 <- data.frame(Conc = seq(min(gent$Conc), max(gent$Conc), length.out = 100))
newdata9 <- data.frame(Conc = seq(min(mero$Conc), max(mero$Conc), length.out = 100))
newdata10 <- data.frame(Conc = seq(min(tige$Conc), max(tige$Conc), length.out = 100))
newdata11 <- data.frame(Conc = seq(min(cipro$Conc), max(cipro$Conc), length.out = 100))
newdata12 <- data.frame(Conc = seq(min(cefavi$Conc), max(cefavi$Conc), length.out = 100))
newdata13 <- data.frame(Conc = seq(min(ceftaz$Conc), max(ceftaz$Conc), length.out = 100))
newdata14 <- data.frame(Conc = seq(min(coli$Conc), max(coli$Conc), length.out = 100))

# Predict with 95% CI
pred8 <- predict(fit_gent, newdata = newdata8, interval = "confidence")
df_pred8 <- data.frame(pred8)
pred9 <- predict(fit_mero, newdata = newdata9, interval = "confidence")
df_pred9 <- data.frame(pred9)
pred10 <- predict(fit_tige, newdata = newdata10, interval = "confidence")
df_pred10 <- data.frame(pred10)
pred11 <- predict(fit_cipro, newdata = newdata11, interval = "confidence")
df_pred11 <- data.frame(pred11)
pred12 <- predict(fit_cefavi, newdata = newdata12, interval = "confidence")
df_pred12 <- data.frame(pred12)
pred13 <- predict(fit_ceftaz, newdata = newdata13, interval = "confidence")
df_pred13 <- data.frame(pred13)
pred14 <- predict(fit_coli, newdata = newdata14, interval = "confidence")
df_pred14 <- data.frame(pred14)

# Bind the predictions to the new data frame
newdata8 <- cbind(newdata8, pred8)
newdata9 <- cbind(newdata9, pred9)
newdata10 <- cbind(newdata10, pred10)
newdata11 <- cbind(newdata11, pred11)
newdata12 <- cbind(newdata12, pred12)
newdata13 <- cbind(newdata13, pred13)
newdata14 <- cbind(newdata14, pred14)




## plot of all dose response curves versus Conc-MIC

ggplot() + 
  geom_line(data = newdata8, aes(x = Conc, y = Prediction, color="Gentamicin")) +  
  geom_point(data = gent, aes(x = Conc, y = Tpos, color="Gentacin"), shape = 2, size = 4) +
  geom_ribbon(data = newdata8, aes(x= Conc, ymin = Lower, ymax = Upper, fill="Gentamcin"), alpha = 0.2) +
##  meropenem
  geom_line(data = newdata9, aes(x = Conc, y = Prediction, color="Meropenem")) +  
  geom_point(data = mero, aes(x = Conc, y = Tpos, color="Meropenem"), shape = 16, size = 4) +
  geom_ribbon(data = newdata9, aes(x= Conc, ymin = Lower, ymax = Upper, fill="Meropenem"), alpha = 0.2) +
## tigecycline
  geom_line(data = newdata10, aes(x = Conc, y = Prediction, color="Tigecycline")) +  
  geom_point(data = tige, aes(x = Conc, y = Tpos, color="Tigecycline"), shape = 17, size = 4) +
  geom_ribbon(data = newdata10, aes(x= Conc, ymin = Lower, ymax = Upper, fill="Tigecycline"), alpha = 0.2) +
## ciprofloxacin
  geom_line(data = newdata11, aes(x = Conc, y = Prediction, color="Ciprofloxacin")) +  
  geom_point(data = cipro, aes(x = Conc, y = Tpos, color="Ciprofloxacin"), shape = 18, size = 4) +
  geom_ribbon(data = newdata11, aes(x= Conc, ymin = Lower, ymax = Upper, fill="Ciprofloxacin"), alpha = 0.2) +
## ceftazidime-avibactam
  geom_line(data = newdata12, aes(x = Conc, y = Prediction, color="Ceftazidime-avibactam [4:1]")) +  
  geom_point(data = cefavi, aes(x = Conc, y = Tpos, color="Ceftazidime-avibactam [4:1]"), shape = 8, size = 4) +
  geom_ribbon(data = newdata12, aes(x= Conc, ymin = Lower, ymax = Upper, fill="Ceftazidime-avibactam [4:1]"), alpha = 0.2) +
## ceftazidime
  geom_line(data = newdata13, aes(x = Conc, y = Prediction, color="Ceftazidime")) +  
  geom_point(data = ceftaz, aes(x = Conc, y = Tpos, color="Ceftazidime"), shape = 0, size = 4) +
  geom_ribbon(data = newdata13, aes(x= Conc, ymin = Lower, ymax = Upper, fill="Ceftazidime"), alpha = 0.2) +
##  colistin
  geom_line(data = newdata14, aes(x = Conc, y = Prediction, color="Colistin")) +  
  geom_point(data = coli, aes(x = Conc, y = Tpos, color="Colistin"), shape = 1, size = 4) +
  geom_ribbon(data = newdata14, aes(x= Conc, ymin = Lower, ymax = Upper, fill="Colistin"), alpha = 0.2) +
 ## labels 
  labs(x = "Log10 Antibiotic concentration/ MIC ratio", y = "Tpos (hours)") +
  scale_x_log10(labels = label_log(digits = 1)) + theme_base() +  scale_color_lancet() +  scale_fill_lancet()


