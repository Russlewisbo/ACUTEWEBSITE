library(BIGL)
library(knitr)
library(rgl)
library(ggplot2)
library(broom)
library(kableExtra)
set.seed(12345)
cutoff <- 0.95
caz_gent_kpca2 <- read.csv("~/Desktop/ACUTEWEBSITE/datasets_combo/kpca_gent_caz.csv")
## Fitting marginal models, maximal fixed at 24 hours, minimal at 9.5 hours
marginalFit2 <- fitMarginals(caz_gent_kpca2, method = "optim",                 
                             fixed = c("m1" = 24, "m2" = 24, "b" = 9.5),
                             names = c("CTZ/AVI", "GENT"))
summary(marginalFit2)
## Plotting marginal model for ceftazidime and gentamicin, minimum constrained at 9 hrs and maximum constrained at 24 hours
plot(marginalFit2) + ggtitle(paste("Ceftazidime/Avibactam + Gentamicin"))
rs2 <- fitSurface(caz_gent_kpca2, marginalFit2,
                  null_model = "bliss",
                  B.CP = 50, statistic = "none", parallel = FALSE)
summary(rs2)
plot(rs2, legend = FALSE, main = "")
view3d(0, -75)
rglwidget()


## mean R statistic evaluates how the test evaluates how the predicted response surface based on a specified null model differs from the observed one. If the null hypothesis is rejected, this test suggests that at least some dose combinations may exhibit synergistic or antagonistic behaviour. The meanR test is not designed to pinpoint which combinations produce these effects nor what type of deviating effect is present.
meanR_N2 <-fitSurface(caz_gent_kpca2, marginalFit2, statistic="meanR",CP=rs2$CP, B.B=20, parallel = FALSE)

# maxR test allows to evaluate presence of synergistic/antagonistic effects for each dose combination and as such provides a point-by-point classification.
maxR_N2 <- fitSurface(caz_gent_kpca2, marginalFit2,
                      statistic = "maxR", CP = rs2$CP, B.B = 20,
                      parallel = FALSE)
maxR_B2 <- fitSurface(caz_gent_kpca2, marginalFit2,
                      statistic = "maxR", CP = rs2$CP, B.B = 20,
                      parallel = FALSE)
maxR_both2 <- rbind(summary(maxR_N2$maxR)$totals,
                    summary(maxR_B2$maxR)$totals)

contour(maxR_B2,
        ## colorPalette = c("blue", "black", "black", "red"),
        main = paste0(" Ceftazidime/Avibactam + Gentamicin"),
        scientific = TRUE, digits = 3, cutoff = cutoff)

plotConfInt(maxR_B2, color ="effect-size")

rsh2 <- fitSurface(caz_gent_kpca2, marginalFit2,
                   null_model = "hsa",
                   B.CP = 50, statistic = "both", parallel = FALSE)
summary(rsh2)

rsb2 <- fitSurface(caz_gent_kpca2, marginalFit2, 
                   null_model = "bliss",
                   B.CP = 50, statistic = "both", parallel = FALSE)
summary(rsb2)

nullModels <- c("loewe", "loewe2", "bliss", "hsa")
rs_list <- Map(fitSurface, null_model = nullModels, MoreArgs = list(
  data = caz_gent_kpca2, fitResult = marginalFit2, 
  B.CP = 50, statistic = "none", parallel = FALSE)
)

synergy_plot_bycomp(rs_list, ylab = "Response", plotBy = "Drug A", color = TRUE)


library(BIGL)
library(knitr)
library(rgl)
library(ggplot2)
library(broom)
library(kableExtra)
set.seed(12345)
cutoff <- 0.95
caz_azt <- read.csv("~/Desktop/ACUTEWEBSITE/datasets_combo/ndm_cazavi_azt.csv")
## Fitting marginal models, maximal fixed at 24 hours, minimal at 9.5 hours
marginalFit3cat <- fitMarginals(caz_azt, method = "optim",                 
                                fixed = c("m1" = 24, "m2" = 24, "b" = 9.5),
                                names = c("CTZ/AVI", "AZT"))
summary(marginalFit3cat)
## Plotting marginal model for ceftazidime/avibactam and aztroenam, minimum constrained at 9 hrs and maximum constrained at 24 hours
plot(marginalFit3cat) + ggtitle(paste("Ceftazidime/Avibactam + Aztreonam"))
rs3cat <- fitSurface(caz_azt, marginalFit3cat,
                     null_model = "bliss",
                     B.CP = 50, statistic = "none", parallel = FALSE)
summary(rs3cat)
plot(rs3cat, legend = FALSE, main = "")
view3d(0, -75)
rglwidget()


rsh3cat <- fitSurface(caz_azt, marginalFit3cat,
                      null_model = "hsa",
                      B.CP = 50, statistic = "both", parallel = FALSE)
summary(rsh3cat)

rsb3cat <- fitSurface(caz_azt, marginalFit3cat,
                      null_model = "bliss",
                      B.CP = 50, statistic = "both", parallel = FALSE)
summary(rsb3cat)
