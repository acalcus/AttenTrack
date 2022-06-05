require(nlme)
require(pastecs)
require(lme4)
require(lsmeans)
require(MuMIn)
require(ggplot2)
require(readxl)
require(lmerTest)
require(tidyverse)
require(magrittr)
require(varhandle)
require(emmeans)
require(grid)
require(gridExtra)
require(tibble)
require(segmented)
require(effectsize)
require(car)

# May 2022
# clear workspace
rm(list = ls())

# Template
theme_template<-theme(plot.title = element_text(size = 20, family = "Helvetica", face = "bold", hjust = 0.5), 
                      text = element_text(size = 20, family = "Helvetica", face = "bold"), 
                      axis.title = element_text(face = "bold"), 
                      axis.text.x = element_text(size = 20, face = "bold"), 
                      panel.border = element_blank(), legend.title = element_blank(), legend.position = "none")

# Are there outliers at the group level ?
setwd("~/Dropbox/Dossier de l'équipe InMignonetteWeTrust/ULB/Projects/19_AttenTrack/data/")
d <- read_xlsx("19_AttenTracking.xlsx")
outliers <- boxplot(baseline ~ audiometrie, data = d, range = 3)$out
outliers <- boxplot(TwoVoices ~ audiometrie, data = d, range = 3)$out
outliers <- boxplot(SiQ ~ audiometrie, data = d, range = 3)$out
outliers <- boxplot(SSN ~ audiometrie, data = d, range = 3)$out
outliers <- boxplot(Babble ~ audiometrie, data = d, range = 3)$out

# Outliers were manually removed and stored in a new Excel spreadsheet
d <- read_xlsx("19_AttenTracking_outliersRemoved.xlsx")
d <- subset(d, Music == "Nonmusician")
d <- subset(d, Age >= 8)
d <- subset(d, Age <= 22.9)

# Split into three age bands  
d <- d %>%
  mutate(Age_band = case_when(
    Age >= 18 ~ "Adults",
    Age < 18 & Age >=13 ~ "Adolescents",
    Age < 13 ~ "Children")
  )

# To facilitate the t-tests (ceiling/chance levels):
Kids <-subset(d, Age_band == "Children")
Ado <- subset(d, Age_band == "Adolescents")
Adults <- subset(d, Age_band == "Adults")


# Is the data normally distributed?
ks.test(d$baseline, "pnorm", mean = mean(d$baseline), sd = sd(d$baseline))
ks.test(d$TwoVoices, "pnorm", mean = mean(d$TwoVoices), sd = sd(d$TwoVoices))
ks.test(d$SiQ, "pnorm", mean = mean(d$SiQ, na.rm = TRUE), sd = sd(d$SiQ, na.rm = TRUE))
ks.test(d$SSN, "pnorm", mean = mean(d$SSN, na.rm = TRUE), sd = sd(d$SSN, na.rm = TRUE))
ks.test(d$Babble, "pnorm", mean = mean(d$Babble, na.rm = TRUE), sd = sd(d$Babble, na.rm = TRUE))

##############
### ATTENTIVE TRACKING
##############
# Check chance level
tt_baseline <- t.test(d$baseline, alternative = c("greater"), mu = 50)
tt_TwoVoices <- t.test(d$TwoVoices, alternative = c("greater"), mu = 50)

# Build dataframe for Attentive Tracking
AttenTrack <- data.frame("code" = d$code_sujet, "Gender" = d$Gender, "Age" = d$Age, 
                         "baseline" = d$baseline, "twoVoices" = d$TwoVoices)
AttenTrack <- gather(AttenTrack, key = "Condition", value = "Score", baseline, twoVoices)
temp <- gather(d, key = "Condition", value = "Difficulty", diff_baseline, diff2Voices)
AttenTrack$difficulty <- temp$Difficulty

# LME
m1 <- lmer(Score~Condition*Age + (1|code), data = AttenTrack)
anova(m1)
# Model reduction
sm <- step(m1, reduce.random = F)
fm <- get_model(sm)
anova(fm)

F_to_eta2(f = c(67.20, 36.54), df = c(1, 1), df_error = c(194, 193))

# Broken stick regression  (see: https://www.statology.org/piecewise-regression-in-r/)
df <- data.frame(Age = d$Age, Baseline = d$baseline)
df <- df[with(df, order(Age)), ]
fit <- lm(Baseline ~ Age, data=df)
segmented.fit <- segmented(fit, seg.Z = ~Age, psi=16)
summary(segmented.fit)
anova(fit, segmented.fit)

plot(df$Age, df$Baseline, pch=16, col='steelblue')
plot(segmented.fit, add=T)

df <- data.frame(Age = d$Age, TwoVoices = d$TwoVoices)
df <- df[with(df, order(Age)), ]
fit <- lm(TwoVoices ~ Age, data=df)
segmented.fit <- segmented(fit, seg.Z = ~Age, psi=16)
summary(segmented.fit)
anova(fit, segmented.fit)

plot(df$Age, df$TwoVoices, pch=16, col='steelblue')
plot(segmented.fit, add=T)

# Is there a correlation between performance in the 1-stream and 2-streams ?
cor.test(d$baseline, d$TwoVoices, method = "pearson")

p1cor <- ggplot(data = d, aes(x = TwoVoices, y = baseline)) +
  geom_point(shape = 21, size = 3, alpha = 0.7) +
  geom_smooth(method = "lm", alpha = 0.1) +
  coord_cartesian(y = c(0, 100), x = c(0, 100)) +
  scale_color_manual(values = cols) +
  scale_y_continuous(name = "1 stream (%)") +
  scale_x_continuous(name = "2 streams (%)") +
  geom_hline(yintercept = 50, colour = "grey", linetype = "dashed") +
  geom_vline(xintercept = 50, colour = "grey", linetype = "dashed") +
  theme_minimal() + theme_template
p1cor


##############
### SiN
##############
# Check chance level
tt_Quiet <- t.test(d$SiQ, alternative = c("less"), mu = 100)
tt_SSN <- t.test(d$SSN, alternative = c("less"), mu = 100)
tt_babble <- t.test(d$Babble, alternative = c("less"), mu = 100)

# How about the quiet condition?
fit <- aov(SiQ~Age, data = d)
summary(fit)
F_to_eta2(f = c(25.28), df = c(1), df_error = c(186))

# Plot me now
p1 <- ggplot(d, aes(x = Age, y = SiQ, colour = Age_band)) +
  geom_point(shape = 21, size = 3, alpha = 0.7) +
  geom_smooth(method = "lm", alpha = 0.2) +
  scale_color_manual(values = cols) +
  coord_cartesian(y = c(0, 100), x = c(7, 23)) +
  scale_y_continuous(name = "Quiet (%)") +
  scale_x_continuous(name = "Age") +
  theme_minimal() + theme_template
p1

### 1. Overall model to check whether SSN // Babble
Noise <- gather(d, key = "Condition", value = "Score", SSN, Babble)
m2 <- lmer(Score~Condition*Age*TwoVoices*SiQ + (1|code_sujet), data = Noise)
anova(m2)
# model reduction 
sm <- step(m2, reduce.random = F)
fm <- get_model(sm)
anova(fm)
F_to_eta2(f = c(42.13, 7.02, 8.38), df = c(1, 1, 1), df_error = c(183, 184, 184))

# Check for multicollinearity
vif_values <- vif(m2)
barplot(vif_values, main = "VIF Values", horiz = TRUE, col = "steelblue")
abline(v = 5, lwd = 3, lty = 2)

### 2. Simplified model - SSN - repeated measures
m3 <- lm(SSN~ SiQ*TwoVoices, data = d)
anova(m3)
F_to_eta2(f = c(40.99, 4.04), df = c(1, 1), df_error = c(184, 184))

# Look at the SiQ x TwoVoices interaction
SiQ_plusSD <- mean(d$SiQ, na.rm = TRUE) + sd(d$SiQ, na.rm = TRUE)
SiQ_m <- mean(d$SiQ, na.rm = TRUE)
SiQ_minusSD <- mean(d$SiQ, na.rm = TRUE) - sd(d$SiQ, na.rm = TRUE)

# Because it's speech, with a lot of 100%, custom-make labels:
SiQ_plusSD <- 100
SiQ_m <- 95
SiQ_minusSD <- 90

mylist <- list(SiQ = c(SiQ_minusSD, SiQ_m, SiQ_plusSD))
SiQ <- emtrends(m3, ~SiQ, var = "TwoVoices", at = mylist)
pairs(SiQ)
summary(SiQ)  # estimated trends at grid nodes

(mylist <- list(TwoVoices = seq(30,100,by = 10), SiQ = c(SiQ_minusSD, SiQ_m, SiQ_plusSD)))

ModelToPlot <- emmip(m3, SiQ~TwoVoices, at = mylist, CIs = TRUE, plotit = FALSE)
ModelToPlot$fSiQ <- factor(ModelToPlot$SiQ)
levels(ModelToPlot$fSiQ) <- c("90", "95", "100")

p1 <- ggplot(data = ModelToPlot, aes(x = TwoVoices, y = yvar, color = fSiQ)) + 
  geom_line() + 
  geom_ribbon(aes(ymax = UCL, ymin = LCL, fill = fSiQ), alpha = 0.3) +
  coord_cartesian(y = c(25, 100), x = c(25, 100)) + 
  scale_y_continuous(name = "SSN") + 
  scale_x_continuous(name = "TwoVoices") + 
  theme_minimal() + theme_template + theme(legend.position = "bottom")
p1


### 3. Simplified model - Babble
m4 <- lm(Babble~ SiQ*TwoVoices, data = d)
anova(m4)
F_to_eta2(f = c(74.60, 2.28, 9.93), df = c(1, 1, 1), df_error = c(184, 184, 184))

# Look at the Baseline x TwoVoices interaction
SiQ <- emtrends(m4, ~SiQ, var = "TwoVoices", at = mylist)
pairs(SiQ)
summary(SiQ)  # estimated trends at grid nodes

(mylist <- list(TwoVoices = seq(30,100,by = 10), SiQ = c(SiQ_minusSD, SiQ_m, SiQ_plusSD)))

ModelToPlot <- emmip(m4, SiQ~TwoVoices, at = mylist, CIs = TRUE, plotit = FALSE)
ModelToPlot$fSiQ <- factor(ModelToPlot$SiQ)
levels(ModelToPlot$fSiQ) <- c("90", "95", "100")

p2<- ggplot(data = ModelToPlot, aes(x = TwoVoices, y = yvar, color = fSiQ)) + 
  geom_line() + 
  geom_ribbon(aes(ymax = UCL, ymin = LCL, fill = fSiQ), alpha = 0.3) +
  coord_cartesian(y = c(25, 100), x = c(25, 100)) + 
  scale_y_continuous(name = "1-talker") + 
  scale_x_continuous(name = "TwoVoices") + 
  theme_minimal() + theme_template + theme(legend.position = "bottom")
p2


# Look into the age effect on SiN (cf model m2)
# Broken stick regression  (see: https://www.statology.org/piecewise-regression-in-r/)
df <- data.frame(Age = Noise$Age, Score = Noise$Score)
df <- df[with(df, order(Age)), ]
fit <- lm(Score ~ Age, data=df)
segmented.fit <- segmented(fit, seg.Z = ~Age, psi=16)
summary(segmented.fit)
anova(fit, segmented.fit)

plot(df$Age, df$Score, pch=16, col='steelblue')
plot(segmented.fit, add=T)

