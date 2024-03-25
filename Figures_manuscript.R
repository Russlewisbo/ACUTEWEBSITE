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


###########################KPC_B##################################################################


# Assuming df is your data frame and x and y are your variables
kpc_b <- read_excel("C:/Users/Russell Lewis/OneDrive/Desktop/kpc_b.xlsx", 
                    +     col_types = c("numeric", "numeric"))
df3 <- kpc_b
df3 <- na.omit(df3)


# Fit the 4PL model- ATCC isolate
fit3 <- drm(KPC_B ~ Conc, data = df3, fct = LL.4())

# Create a new data frame for predictions
newdata3 <- data.frame(Conc = seq(min(df3$Conc), max(df3$Conc), length.out = 100))

# Predict with 95% CI
pred3 <- predict(fit3, newdata = newdata3, interval = "confidence")
df_pred3 <- data.frame(pred3)

# Bind the predictions to the new data frame
newdata3 <- cbind(newdata3, pred3)


# Plot with ggplot2

p3<- ggplot(newdata3, aes(x = Conc, y = Prediction )) + 
  geom_line(aes(y = Prediction)) +  geom_point(data = df3, aes(x = Conc, y = KPC_B), shape = 2, size = 4) +
  geom_ribbon(aes(ymin = Lower, ymax = Upper), alpha = 0.2) +
  labs(x = "Serum Ceftazidime-Avibactam [4:1] mg/L", y = "Time to positivity (Tpos) hours", title = "4-Parameter Logistic Regression with 95% CI") 
p3 + theme_classic ()

###############################KPWT##############################################################
# Assuming df is your data frame and x and y are your variables
KPWT <- read_excel("C:/Users/Russell Lewis/OneDrive/Desktop/KPWT.xlsx") 

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

KPCA <- read_excel("C:/Users/Russell Lewis/OneDrive/Desktop/KPCA.xlsx") 

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
  geom_point(data = df3, aes(x = Conc, y = KPC_B), shape = 3, size = 4) +
  geom_ribbon(data = newdata3, aes(x = Conc, ymin = Lower, ymax = Upper), alpha = 0.2) +
  labs(x = "Serum Ceftazidime-Avibactam [4:1] mg/L", y = "Time to positivity (Tpos) hours", title = "4-Parameter Logistic Regression with 95% CI") +
  scale_x_log10() + theme_classic ()

############KPRAD#################################################################################

KPRAD <- read_excel("C:/Users/Russell Lewis/OneDrive/Desktop/KPRAD.xlsx") 

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
  geom_point(data = df3, aes(x = Conc, y = KPC_B), shape = 3, size = 4) +
  geom_ribbon(data = newdata3, aes(x = Conc, ymin = Lower, ymax = Upper), alpha = 0.2) +
  geom_line(data = newdata4, aes(x = Conc, y = Prediction)) +
  geom_point(data = df4, aes(x = Conc, y = KPRAD), shape = 5, size = 4) +
  geom_ribbon(data = newdata4, aes(x = Conc, ymin = Lower, ymax = Upper), alpha = 0.2) +
  labs(x = "Spiked serum ceftazidime-avibactam [4:1] mg/L", y = "Time to positivity (Tpos) hours") +
  scale_x_log10() + theme_base()

############KFAB#################################################################################

KFAB <- read_excel("C:/Users/Russell Lewis/OneDrive/Desktop/KFAB.xlsx") 

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
  geom_point(data = df3, aes(x = Conc, y = KPC_B), shape = 8, size = 4) +
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

KPCAT <- read_excel("C:/Users/Russell Lewis/OneDrive/Desktop/KPCAT.xlsx") 

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
  geom_point(data = df3, aes(x = Conc, y = KPC_B), shape = 8, size = 4) +
  geom_ribbon(data = newdata3, aes(x = Conc, ymin = Lower, ymax = Upper), alpha = 0.2) +
  geom_line(data = newdata4, aes(x = Conc, y = Prediction)) +
  geom_point(data = df4, aes(x = Conc, y = KPRAD), shape = 5, size = 4) +
  geom_ribbon(data = newdata4, aes(x = Conc, ymin = Lower, ymax = Upper), alpha = 0.2) +
  geom_line(data = newdata5, aes(x = Conc, y = Prediction)) +
  geom_point(data = df5, aes(x = Conc, y = KPFAB), shape = 6, size = 4) +
  geom_ribbon(data = newdata5, aes(x = Conc, ymin = Lower, ymax = Upper), alpha = 0.2) +
  geom_line(data = newdata6, aes(x = Conc, y = Prediction)) +
  geom_point(data = df6, aes(x = Conc, y = KPCAT), shape = 15, size = 4) +
  geom_ribbon(data = newdata6, aes(x = Conc, ymin = Lower, ymax = Upper), alpha = 0.2) +
  labs(x = "Spiked serum ceftazidime-avibactam [4:1] mg/L", y = "Time to positivity (Tpos) hours") +
  scale_x_log10(labels = label_log(digits = 1)) + theme_base()
 