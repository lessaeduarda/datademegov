
# Load database:
load("mariaeduarda-lessa-ad-ufpe-2019.RData")

# Require or install packages: 
require(ggforce)
require(ggplot2)
require(ggrepel)
require(stargazer)
require(coefplot)
require(lmtest)
require(MASS)
require(magrittr)
require(dplyr)
require(car)

---

# Set levels for categorical variables:
baseDemEgov$Continent <- factor(baseDemEgov$Continent, levels = c("Africa",
                                                                  "America",
                                                                  "Asia",
                                                                  "Europe",
                                                                  "Oceania"))

baseDemEgov$EGDILEVEL <- factor(baseDemEgov$EGDILEVEL, levels = c("Very High EGDI",
                                                                  "High EGDI",
                                                                  "Middle EGDI",
                                                                  "Low EGDI"))



# Plot Countries and EGDI Levels (FIGURE 1):
plot1 <- ggplot(data = baseDemEgov, aes(y = EGDI, x = DemIndex, color = EGDILEVEL, 
                                        label = Country)) +
  geom_point() +
  geom_text_repel(aes(x = DemIndex, y = EGDI,label = Country))+
  theme_replace()
plot1 + scale_color_manual(values = c("#4682B4", "#3CB371", "#DAA520", "#E9967A"))

# Pearson's correlation coefficient between variables:
cor.test(baseDemEgov$DemIndex, baseDemEgov$EGDI)

# Bivariate Linear Regression, DV = EGDI, IV = Democracy Index:
mod <- lm(EGDI ~ DemIndex, data = baseDemEgov)
summary(mod)

# Regression Table:
stargazer(mod, type = "text", title="Regression Results", align=TRUE, 
          dep.var.labels=c("E-Government Development Index"), 
          covariate.labels=c("Democracy Index"), omit.stat=c("LL","ser","f"), 
          no.space=TRUE)

# Plot bivariate linear regression (FIGURE2):
plot2 <- ggplot(data = baseDemEgov, aes(y = EGDI, x = DemIndex, 
                                        color = EGDILEVEL)) +
  ggtitle(label = "FIGURE 2", subtitle = "Bivariate Linear Regression Plot" )+
  geom_point() +
  theme_replace() +
  geom_smooth(method = "lm", color = "black", se = T)
plot2 + scale_color_manual(values = c("#4682B4", "#3CB371", "#DAA520", "#E9967A"))

# Plot residual analysis:
residreg <- resid(mod)
ggplot(data = baseDemEgov, aes(y = residreg, x = DemIndex)) +
  geom_point(color = "#4682B4") +
  theme_replace() +
  geom_smooth(method="lm", color = "black", se = F) +
  labs(title = "Residual Analysis", y = "Residuals", x = "DemIndex")

# Check for outliers (or extrem values) in the bivariate regression:
which(rstudent(mod) > 2)
# Bahrain, Belarus, Kazakhstan, Russian Federation, Saudi Arabia and United Arab
# Emirates.


---
  
  
# Analyze the variance of the DemIndex effect over EGDI within the continents:
res.aov <- aov(EGDI ~ DemIndex:Continent, data = baseDemEgov)

# Check ANOVA:
summary(res.aov)
# The variance of the effect of the democracy index on the e-government 
# development index for the different continents is significant.

# Density of the independent variable per capita GDP, before and after its 
# transformation in log:
hist(baseDemEgov$GDP, freq = F, xlab = "GDP PPC", main = "Distribuition GDP PPC")
lines(density(baseDemEgov$GDP), col = "#4682B4", lwd = 4)

hist(baseDemEgov$GDPlog, freq = F, xlab = "GDP PPC log", 
     main = "Distribuition GDP PPC log")
lines(density(baseDemEgov$GDPlog), col = "#4682B4", lwd = 4)

# Multiple Linear Regression, DV = EGDI, IVs = interaction term Continent x 
# Democracy Index and GDPlog:

## Model 1:
baseDemEgov$Continent <- relevel(baseDemEgov$Continent, ref = "Africa")
mod1 <- lm(EGDI ~ DemIndex*Continent + GDPlog, data = baseDemEgov)
summary(mod1)

## Model 2:
baseDemEgov$Continent <- relevel(baseDemEgov$Continent, ref = "America")
mod2 <- lm(EGDI ~ DemIndex*Continent + GDPlog, data = baseDemEgov)
summary(mod2)

## Model 3:
baseDemEgov$Continent <- relevel(baseDemEgov$Continent, ref = "Asia")
mod3 <- lm(EGDI ~ DemIndex*Continent + GDPlog, data = baseDemEgov)
summary(mod3)

## Model 4:
baseDemEgov$Continent <- relevel(baseDemEgov$Continent, ref = "Europe")
mod4 <- lm(EGDI ~ DemIndex*Continent + GDPlog, data = baseDemEgov)
summary(mod4)

## Model 5:
baseDemEgov$Continent <- relevel(baseDemEgov$Continent, ref = "Oceania")
mod5 <- lm(EGDI ~ DemIndex*Continent + GDPlog, data = baseDemEgov)
summary(mod5)

# Check coefficients confidence intervals:
confint(mod1)
confint(mod2)
confint(mod3)
confint(mod4)
confint(mod5)

# Plot coefficients without the intercept:
coefplot.lm(mod1, col="#4682B4",intercept=F)
coefplot.lm(mod2, col="#4682B4",intercept=F)
coefplot.lm(mod3, col="#4682B4",intercept=F)
coefplot.lm(mod4, col="#4682B4",intercept=F)
coefplot.lm(mod5, col="#4682B4",intercept=F)

# Plot residual analysis:
residmod1 <- resid(mod1)
ggplot(data = baseDemEgov, aes(y = residmod1, x = DemIndex:Continent + GDPlog)) +
  geom_point(color = "#4682B4") +
  theme_replace() +
  geom_smooth(method="lm", color = "black", se = F) +
  labs(title = "Residual Analysis", y = "Residuals", x = "GDP, DemIndex, Continent")

# Check residuals normality (Shapiro Test):
shapiro.test(mod1$residuals)
# The test does not indicate the rejection of the null hypothesis of residual
# normality.

# Plot residual distribution:
sresidmod1 <- studres(mod1) 
hist(sresidmod1, freq=FALSE, 
     main="Distribution of Studentized Residuals")
xfit1 <- seq(min(sresidmod1),max(sresidmod1),length=40) 
yfit1 <- dnorm(xfit1) 
lines(xfit1, yfit1, col = "#4682B4", lwd = 4)

# Check for outliers:
which(rstudent(mod1) > 2)
# Burundi, Russia, Rwanda & Uzbekistan.

# Check for multicollinearity between IVs:

## Model 1:
vif(mod1)
sqrt(vif(mod1)) > 2

## Model 2:
vif(mod2)
sqrt(vif(mod2)) > 2

## Model 3:
vif(mod1)
sqrt(vif(mod1)) > 2

## Model 4: 
vif(mod4)
sqrt(vif(mod4)) > 2

## Model 5:
vif(mod5)
sqrt(vif(mod5)) > 2

# Check model homocedasticity:
bptest(baseDemEgov$DemIndex ~ baseDemEgov$EGDI + baseDemEgov$GDPlog)

# Plot Multiple Linear Regression (FIGURE 3):
plot3 <- ggplot(data = baseDemEgov, aes(y = EGDI, x = DemIndex:Continent + 
                                    GDPlog, color = EGDILEVEL)) +
  geom_point() +
  theme_replace() +
  geom_smooth(method = "lm", color = "black", se = T)
plot3 + scale_color_manual(values = c("#4682B4", "#3CB371", "#DAA520", "#E9967A"))

# Plot Multiple Linear Regression faceted by continent without Oceania
# (FIGURE 4):
plot4 <- ggplot(data = baseDemEgov, aes(y = EGDI, x = DemIndex:Continent + 
                                             GDPlog, color = EGDILEVEL)) +
  geom_point() +
  theme_replace() +
  geom_smooth(method = "lm", color = "black", se = T) +
  facet_wrap_paginate(~Continent, ncol = 2, nrow = 2, page = 1)
plot4 + scale_color_manual(values = c("#4682B4", "#3CB371", "#DAA520", "#E9967A"))

# Check percentage of democratic countries in each continent:
target <- c("Full democracy", "Flawed democracy")

## Africa:
ContAfrica <- filter(baseDemEgov, Continent == "Africa") #48
ContAfrica <- filter(ContAfrica, DemLevel %in% target) #9 
9/48
# 18.75%

## America:
ContAmerica <- filter(baseDemEgov, Continent == "America") #24
ContAmerica <- filter(ContAmerica, DemLevel %in% target) #18
18/24
# 75%

## Asia:
ContAsia <- filter(baseDemEgov, Continent == "Asia") #43
ContAsia <- filter(ContAsia, DemLevel %in% target) #12
12/43
# 27.91%

## Europe
ContEurope <- filter(baseDemEgov, Continent == "Europe") #38
ContEurope <- filter(ContEurope, DemLevel %in% target) #31
31/38
# 81.58%

## Oceania:
ContOceania <- filter(baseDemEgov, Continent == "Oceania") #4
ContOceania <- filter(ContOceania, DemLevel %in% target) #3
3/4
# 75%


---
  

# Scatterplot variation of DemIndex x Variation of EGDI: 
plot5 <- ggplot(data = baseDemEgov, aes(y = DeltaEGDI, x = DeltaDemIndex,
                                        color = EGDILEVEL, label = Country)) +
geom_point() +
geom_text_repel(aes(x = DeltaDemIndex, y = DeltaEGDI,label = Country))+
theme_replace()
plot5 + scale_color_manual(values = c("#4682B4", "#3CB371", "#DAA520", "#E9967A"))

# Multiple Linear Regression, DV =  EGDI variation between 2008 and 2018 
# (DeltaEGDI), IVs = Democracy Index variation between 2008 and 2018 
# (DeltaDemIndex) + GDP per capita (log) variation within the same period:
reg3 <- lm(DeltaEGDI ~ DeltaDemIndex + DeltaGDPlog, data = baseDemEgov)
summary(reg3)

stargazer(reg3, type = "text", title="Regression Results", align=TRUE,
          dep.var.labels=c("E-Government Development Index Delta"),
          covariate.labels=c("Democracy Index Delta", "GDP Delta (log)"), 
          omit.stat=c("LL","ser","f"),
          no.space=TRUE)
---

# Check how many countries had negative variation in the Democracy Index:
NegativeDemVar <- baseDemEgov %>% filter(DeltaDemIndex < 0)
NegativeDemVar$Country

# Check how many countries had negative variation in the EGDI:
NegativeEGDIVar <- baseDemEgov %>% filter(DeltaEGDI < 0)
NegativeEGDIVar$Country

# Check authoritarian regimes with very high EGDI level:
AuthoVeryHigh <- filter(baseDemEgov, EGDILEVEL == "Very High EGDI")
AuthoVeryHigh <- filter(AuthoVeryHigh, DemLevel == "Authoritarian")
AuthoVeryHigh$Country

# Check authoritarian regimes with high EGDI level:
AuthoHigh <- filter(baseDemEgov, EGDILEVEL == "High EGDI")
AuthoHigh <- filter(AuthoHigh, DemLevel == "Authoritarian")
AuthoHigh$Country
