
# !! Set your own working directory !!
setwd()

# Load database:
load("mariaeduarda-lessa-ad-ufpe-2019.RData")

# Require or install packages: 
if(require(ggforce)==F)install.packages('ggforce');require(ggforce)
if(require(ggplot2)==F)install.packages('ggplot2');require(ggplot2)
if(require(ggrepel)==F)install.packages('ggrepel');require(ggrepel)
if(require(stargazer)==F)install.packages('stargazer');require(stargazer)
if(require(coefplot)==F)install.packages('coefplot');require(coefplot)
if(require(lmtest)==F)install.packages('lmtest');require(lmtest)
if(require(MASS)==F)install.packages('MASS');require(MASS)
if(require(magrittr)==F)install.packages('magrittr');require(magrittr)
if(require(dplyr)==F)install.packages('dplyr');require(dplyr)
if(require(car)==F)install.packages('car');require(car)

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

baseDemEgov$DemLevel <- factor(baseDemEgov$DemLevel, levels = c("Full democracy",
                                                                "Flawed democracy",
                                                                "Hybrid regime",
                                                                "Authoritarian"))
---

## FIGURE 1:
  
# Select countries to be shown in the scatterplot (FIGURE 1):
selectedcountries <- c("Niger", "Denmark", "Iceland", "Uruguay", 
                       "Russian Federation", "China", "United Arab Emirates",
                       "Bahrain", "Chad", "Estonia", "Costa Rica", "Mauritius", 
                       "Belarus", "Kazakhstan")

# Plot Countries and continents:
plot1 <- ggplot(data = baseDemEgov, aes(y = EGDI, x = DemIndex, color = Continent, 
                                        label = Country)) +
  ggtitle(label = "FIGURE 1", subtitle = "DemIndex & EGDI Scatterplot")+
  geom_point() +
  geom_text_repel(data = subset(baseDemEgov, Country %in% selectedcountries),
                  aes(label = Country), show.legend = F)+
  theme_replace()
plot1 + scale_color_manual(values = c("#7fcdbb", "#41b6c4", "#1d91c0", "#225ea8", "#0c2c84"))

---
  
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

---
  
## FIGURE 2:

# Plot bivariate linear regression:
plot2 <- ggplot(data = baseDemEgov, aes(y = EGDI, x = DemIndex, 
                                        color = Continent)) +
  ggtitle(label = "FIGURE 2", subtitle = "Bivariate Linear Regression Plot" )+
  geom_point() +
  theme_replace() +
  geom_smooth(method = "lm", color = "black", se = T)
plot2 + scale_color_manual(values = c("#7fcdbb", "#41b6c4", "#1d91c0",
                                      "#225ea8", "#0c2c84"))

# Plot residual analysis:
residreg <- resid(mod)
ggplot(data = baseDemEgov, aes(y = residreg, x = DemIndex)) +
  geom_point(color = "#4682B4") +
  theme_replace() +
  geom_smooth(method="lm", color = "black", se = F) +
  labs(title = "Residual Analysis", y = "Residuals", x = "DemIndex")

---
  
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

# Before:
hist(baseDemEgov$GDP, freq = F, xlab = "GDP PC", main = "Distribution GDP PC")
lines(density(baseDemEgov$GDP), col = "#225ea8", lwd = 4)

# After:
hist(baseDemEgov$GDPlog, freq = F, xlab = "GDP PPC log", 
     main = "Distribuition GDP PPC log")
lines(density(baseDemEgov$GDPlog), col = "#225ea8", lwd = 4)


---
  
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

---
  
## FIGURE 3:
  
# Plot coefficients without the intercept:
multicoefplot <-  multiplot(mod1, mod2, mod3, mod4, 
                            title = "FIGURE 3 \n Coefficient Plot",
                            intercept= F, plot.shapes = T, lwdOuter = 0.6, 
                            pointSize = 2, names = c("Model 1", "Model 2", 
                                                     "Model 3", "Model 4"), 
                            coefficients = c("DemIndex", 
                                             "DemIndex:ContinentAfrica",
                                             "DemIndex:ContinentAmerica",
                                             "DemIndex:ContinentAsia",
                                             "DemIndex:ContinentEurope"))
multicoefplot + scale_color_manual(values = c("#7fcdbb", "#41b6c4", "#1d91c0", 
                                              "#225ea8"))+
  annotate("text", x = 0.06, y = 0.7, label = "bold(R) ^ 2 == 0.856",parse = TRUE)

---
  
# Plot residual analysis:
residmod1 <- resid(mod1)
ggplot(data = baseDemEgov, aes(y = residmod1, x = DemIndex:Continent + GDPlog)) +
  geom_point(color = "#225ea8") +
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
lines(xfit1, yfit1, col = "#225ea8", lwd = 4)

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

---

## FIGURE 4:

# Set levels:
baseDemEgov$Continent <- factor(baseDemEgov$Continent, levels = c("Africa",
                                                                    "America",
                                                                    "Asia",
                                                                    "Europe",
                                                                    "Oceania"))  
# Plot Multiple Linear Regression (FIGURE 3):
plot3 <- ggplot(data = baseDemEgov, aes(y = EGDI, x = DemIndex:Continent + 
                                          GDPlog, color = Continent)) +
  ggtitle(label = "FIGURE 4", subtitle = "Multiple Linear Regression Plot") +
  geom_point() +
  theme_replace() +
  geom_smooth(method = "lm", color = "black", se = T)
plot3 + scale_color_manual(values = c("#7fcdbb", "#41b6c4", "#1d91c0", 
                                      "#225ea8", "#0c2c84"))

---

## FIGURE 5:
  
# Plot Multiple Linear Regression faceted by continent without Oceania:
  plot4 <- ggplot(data = baseDemEgov, aes(y = EGDI, x = DemIndex:Continent + 
                                            GDPlog, color = DemLevel)) +
  ggtitle(label = "FIGURE 5", 
          subtitle = "Multiple Linear Regression Plot Faceted by Continent") +
  geom_point() +
  theme_replace() +
  geom_smooth(method = "lm", color = "black", se = T) +
  facet_wrap_paginate(~Continent, ncol = 2, nrow = 2, page = 1)
plot4 + scale_color_manual(values = c("#225ea8", "#1d91c0", "#41b6c4", "#7fcdbb"))

---
  
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

## FIGURE 6:  

# Scatterplot variation of DemIndex x Variation of EGDI: 
  selectedcountries2 <- c("Tunisia", "Russian Federation", "Sweden", "Ghana", 
                          "Uruguay", "Kazakhstan", "Myanmar", "Norway", 
                          "Equatorial Guinea", "Nicaragua", "Lesotho", "Turkey",
                          "Montenegro")
plot5 <- ggplot(data = baseDemEgov, aes(y = DeltaEGDI, x = DeltaDemIndex + DeltaGDPlog,
                                        color = Continent, label = Country)) +
  ggtitle(label = "FIGURE 6", subtitle = "DemIndex & EGDI Variation Scatterplot")+
  geom_point() +
  geom_text_repel(data = subset(baseDemEgov, Country %in% selectedcountries2),
                  aes(label = Country), show.legend = F)+
  theme_replace()
plot5 + scale_color_manual(values = c("#7fcdbb", "#41b6c4", "#1d91c0", 
                                      "#225ea8", "#0c2c84"))

---
  
# Multiple Linear Regression, DV =  EGDI variation between 2008 and 2018 
# (DeltaEGDI), IVs = Democracy Index variation between 2008 and 2018 
# (DeltaDemIndex) + GDP per capita (log) variation within the same period:
mod6 <- lm(DeltaEGDI ~ DeltaDemIndex + DeltaGDPlog, data = baseDemEgov)
summary(mod6)

stargazer(mod6, type = "text", title="Regression Results", align=TRUE,
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

