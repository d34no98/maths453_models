load("batData.RData")
load("mothData.RData")

## Moths EDA
summary(moths)
str(moths)
library(nlme)
library(dplyr)
moths %>% 
  distinct(WoodID,
           TreeID,
           MothID)

moths %>% 
  group_by(WoodID, TreeID) %>%
  summarize(N=n()) %>% 
  print(n=100)

moths %>% 
  group_by(WoodID) %>% 
  summarise(N=n()) %>% 
  print(n=25)
hist(moths$Distance)
range(moths$Distance)

moths %>% 
  group_by(WoodID, Morph, Gender) %>% 
  summarise(N=n()) %>% 
  print(n=100)

moths %>% 
  group_by(Morph) %>% 
  summarize(survival_rate = sum(Survival, na.rm = TRUE))

moths %>% 
  group_by(WoodID, Distance, Morph) %>% 
  summarize(surv_rate = sum(Survival, na.rm = TRUE)) %>% 
  arrange(Distance) %>% 
  print(n=50)
levels(moths$Age) <- c("Infant", "Juvenile", "Adult")

moths_df <- moths %>% 
  arrange(Distance)

woodID_vec <- unique(moths_df$WoodID)
levels(moths$WoodID) <- c(woodID_vec)
moths$surv_chr <- if_else(moths$Survival == 1, "Survived", "Died")

library(ggplot2)
ggplot(data = moths, aes(x=Distance, y=Survival)) +
  geom_point() +
  facet_wrap(. ~ Morph)

ggplot(data=moths, aes(x=Morph, y=Distance, fill = surv_chr)) +
  geom_boxplot() +
  labs(fill = "Survival Status") +
  theme_bw()
ggplot(data=moths, aes(x=Morph, y=Distance)) +
  geom_boxplot() +
  facet_wrap(. ~ Survival)

## EDA
ggplot(data = moths, aes(Distance, y=Survival)) +
  geom_jitter(height=.05, alpha=0.4) +
  geom_smooth(method = "glm", 
              methods.args = list(family = "binomial")) +
  theme_bw()

ggplot(data=moths, aes(x=Distance,y=Survival, colour=Morph)) +
  geom_jitter(height = .05, alpha=.4) +
  geom_smooth(method="glm", 
              method.args = list(family = "binomial")) +
  theme_bw()

ggplot(data = moths, aes(x=Morph, fill = factor(Survival))) +
  geom_bar(position = "fill") +
  ylab("Proportion Survived") +
  theme_bw()

ggplot(data=moths, aes(x=Age)) +
  geom_bar() +
  theme_bw()

ggplot(data=moths, aes(x=Gender)) +
  geom_bar() +
  theme_bw()

ggplot(data=moths, aes(x=Age, fill = factor(Survival))) +
  geom_bar(position = "fill") +
  theme_bw()

ggplot(data=moths, aes(x=Gender, fill = factor(Survival))) +
  geom_bar(position = "fill") +
  theme_bw()

ggplot(data=moths, aes(x=Morph, y=Distance)) +
  geom_boxplot() +
  theme_bw()

# Proportion of Survival between Woodland Areas
prop_surv <- moths %>% 
  group_by(WoodID) %>% 
  summarize(prop_surv = mean(Survival))

moths %>% 
  left_join(prop_surv, by = c("WoodID")) %>% 
  ggplot(aes(x=Distance, y=prop_surv, colour=WoodID)) +
  geom_point() +
  labs(y="Proportion Survived in Woodland",
       colour = "Woodland Area ID") +
  theme_bw()

# Proportion of Survival between Trees across Woodlands
prop_surv_trees <- moths %>% 
  group_by(TreeID) %>% 
  summarize(prop_surv_tree = mean(Survival))
moths %>% 
  left_join(prop_surv_trees, by = c("TreeID")) %>% 
  ggplot(aes(x=Distance, y=prop_surv_tree, colour=TreeID)) +
  geom_point() +
  facet_wrap(. ~ WoodID) +
  labs(y="Proportion Survived on trees in Woodlands") +
  theme_bw() +
  theme(legend.position = "none")


library(lme4)
random_moths_1 <- glmer(data = moths,
                        formula = Survival ~ Distance + Morph + Age + Gender
                        + (1|WoodID),
                        family = binomial(link = "logit"))
# AIC:     450.8
# logLik: -218.4

random_moths_2 <- glmer(data = moths,
                        formula = Survival ~ Distance + Morph + Age + Gender
                        + (1|WoodID) + (1|TreeID),
                        family = binomial(link = "logit"))


# AIC:     406.6
# logLik: -195.3

random_moths_3 <- glmer(data = moths,
                        formula = Survival ~ Distance + Morph + Age + Gender
                        + (1|TreeID),
                        family = binomial(link = "logit"))
# AIC:     438.4
# logLik: -212.2 

random_moths_4 <- glmer(data = moths,
                        formula = Survival ~ Distance + Morph + Age + Gender
                        + (1 | WoodID/TreeID) + (1 | MothID),
                        family = binomial(link = "logit"))
# AIC:     408.6
# logLik: -195.3

random_moths_5 <- glmer(data = moths,
                        formula = Survival ~ Distance + Morph + Age + Gender
                        + (1 | WoodID/TreeID),
                        family = binomial(link = "logit"))
# AIC:     406.6
# logLik: -195.3

random_moths_6 <- glmer(data = moths,
                        formula = Survival ~ Distance + Morph + Age + Gender
                        + (1 + Distance | WoodID) + (1|TreeID),
                        family = binomial(link = "logit"))
# AIC:     410.5
# logLik: -195.2  

random_moths_7 <- glmer(data = moths,
                        formula = Survival ~ Distance + Morph + Age + Gender
                        + (1|WoodID) + (1 + Distance | TreeID),
                        family = binomial(link = "logit"))

# AIC:     409.2
# logLik: -194.6 

random_moths_8 <- glmer(data = moths,
                        formula = Survival ~ Distance + Morph + Age + Gender
                        + (1|WoodID) + (1 | WoodID:TreeID),
                        family = binomial(link = "logit"))
# AIC:     406.6
# logLik: -195.3

random_moths_9 <- glmer(data = moths,
                        formula = Survival ~  Distance + Morph + Age + Gender
                        + Distance:Morph + (1|TreeID) + (1|WoodID),
                        family = binomial(link = "logit"))

best_model <- random_moths_2
AIC(random_moths_1, random_moths_2, random_moths_3, random_moths_4,
    random_moths_5, random_moths_6, random_moths_7, random_moths_8)
# Quality Check through removing Fixed Effects from the model at a time:

m_no_morph  <- update(best_model, . ~ . - Morph)
m_no_age    <- update(best_model, . ~ . - Age)
m_no_gender <- update(best_model, . ~ . - Gender)
m_no_dist   <- update(best_model, . ~ . - Distance)

AIC(best_model, m_no_morph, m_no_age, m_no_gender, m_no_dist)
anova(m_no_gender, best_model)

# Quality Check through modifying interactions in to the model:

m_int2 <- update(best_model, . ~ . + Distance:Age)
m_int3 <- update(best_model, . ~ . + Morph:Age)
#Check interaction term Convergence
#summary(m_int1)$optinfo$conv$lme4$messages
#Check Singularity
#isSingular(m_int1)
#Check Gradient --> 0
#summary(m_int1)$optinfo$derivs$grad

# Check AIC
AIC(best_model, m_int2, m_int3)

## NOTE: Lack of Convergence, keep current `best_model`
best_model <- random_moths_2

## retrieve 95% CI
fixed_2 <- summary(best_model)$coefficients[, "Estimate"]
se_2 <- summary(best_model)$coefficients[, "Std. Error"]
lower_2 <- fixed_2 - 1.96*se_2
upper_2 <- fixed_2 + 1.96*se_2

## Bootstrap Method
library(lme4)

m_0 <- glm(
  Survival ~ Distance + Morph + Age + Gender ,
  data = moths,
  family = binomial
)

m_1 <- glmer(
  Survival ~ Distance + Morph + Age + Gender + 
    (1|WoodID),
  data = moths,
  family = binomial(link = "logit")
)

Dist <- as.numeric(2*(logLik(m_1) - logLik(m_0)))

nb <- 100
lrstat <- numeric(nb)
for (j in 1:nb) {
  y <- unlist(simulate(m_0))
  hyp_null <- glm(y ~ Distance + Morph + Age + Gender + Distance:Morph,
                  data = moths,
                  family = binomial(link = "logit"))
  hyp_alt <- glmer(y ~ Distance + Morph + Age + Gender + Distance:Morph +
                     (1|WoodID),
                   data = moths,
                   family = binomial(link = "logit"))
  lrstat[j] <- as.numeric(2*(logLik(hyp_alt) - logLik(hyp_null)))
}
hist(lrstat, main = "", xlab="Distance under H0")
p_val_fixed_boot <- mean(lrstat >= Dist)
p_val_fixed_boot


############ BootStrap Method: Add Additional ############
############           Random Effect          ############

m0 <- glmer(Survival ~ Distance + Morph + Age + Gender + Distance:Morph +
              (1|WoodID),
            data = moths,
            family = binomial(link = "logit"))
m1 <- glmer(Survival ~ Distance + Morph + Age + Gender + Distance:Morph +
              (1|WoodID) + (1|TreeID),
            data = moths,
            family = binomial(link = "logit"))
D <- as.numeric(2*(logLik(m1) - logLik(m0)))

nb <- 100
lrstat <- numeric(nb)
for (j in 1:nb) {
  y <- unlist(simulate(m0))
  hyp_null <- glmer(y ~ Distance + Morph + Age + Gender + Distance:Morph +
                      (1|WoodID),
                    data = moths,
                    family = binomial(link = "logit"))
  hyp_alt <- glmer(y ~ Distance + Morph + Age + Gender + Distance:Morph +
                     (1|WoodID) + (1|TreeID),
                   data = moths,
                   family = binomial(link = "logit"))
  lrstat[j] <- as.numeric(2*(logLik(hyp_alt) - logLik(hyp_null)))
}
hist(lrstat, main = "", xlab = "Distance under H0")
p_val_boot <- mean(lrstat >= D)
p_val_boot

## MOTHS: Diagnostics ##

# Residual Diagnostics

par(mfrow=c(2,1))
# See is there is a lack of fit
plot(best_model, resid(.) ~ fitted(.),
     ylab="Residuals",
     xlab="Fitted values")

# Residuals vs. Covariates
plot(best_model, resid(.) ~ Distance,
     ylab="Residuals")

# Residuals by Grouping Factor
plot(best_model, WoodID ~ resid(.))
plot(best_model, TreeID ~ resid(.))

plot(best_model, Survival ~ fitted(.) | WoodID, abline = c(0,1))

# Random-effects Diagnostics

rs_wood_residuals <- ranef(best_model)$WoodID
rs_tree_residuals <- ranef(best_model)$TreeID

par(mfrow=c(2,2))
# RE scatterplot
plot(ranef(best_model)$WoodID$`(Intercept)`,
     ylab="Random Intercept (Woodland)")
plot(ranef(best_model)$TreeID$`(Intercept)`,
     ylab="Random Intercept (Tree)")

# RE `WoodID` #
qqnorm(rs_wood_residuals$`(Intercept)`,
       main = "")
qqline(rs_wood_residuals$`(Intercept)`)

# RE `TreeID` #
qqnorm(rs_tree_residuals$`(Intercept)`,
       main = "")
qqline(rs_tree_residuals$`(Intercept)`)

rdf <- df.residual(best_model)
rp <- residuals(best_model, type = "pearson")
Pearson.chisq <- sum(rp^2)
ratio <- Pearson.chisq / rdf
pval <- pchisq(Pearson.chisq, df = rdf, lower.tail = FALSE)
c(chisq = Pearson.chisq, ratio = ratio, rdf = rdf, p = pval)
##########################################################
############ END OF `MOTHS` ANALYSIS #####################
##########################################################

################### `BATS` ANALYSIS ######################

## Bats EDA
bats %>% 
  group_by(BatID) %>% 
  summarize(N=n()) 

bats %>% 
  group_by(Time.week) %>% 
  summarize(N=n()) 

library(ggplot2)
ggplot(data = bats, aes(x=Time.week, y=Wingspan.mm, colour=BatID)) +
  geom_line(, linewidth = 0.1) +
  labs(y="Wingspan (mm)",
       x="Time (Weeks)",
       colour = "Bat ID #") +
  theme_bw()

ggplot(data = bats, aes(x=Time.week, y=Wingspan.mm, colour=BatID)) +
  geom_line(aes(linetype = "solid")) +
  facet_wrap(. ~ BatID)

## Bootstrap Method: justify a random intercept
library(lme4)

m0_bats <- lm(
  Wingspan.mm ~ Time.week + Distance.km ,
  data = bats
)

m1_bats <- lme(
  Wingspan.mm ~  Time.week + Distance.km,
  random = ~ 1 + Time.week | BatID,
  data = bats
)

bat_dist <- as.numeric(2*(logLik(m1_bats) - logLik(m0_bats)))

nb <- 1000
lrstat <- numeric(nb)
for (j in 1:nb) {
  y <- unlist(simulate(m0_bats))
  hyp_null <- lm(y ~ Time.week + Distance.km,
                  data = bats)
  hyp_alt <- lme(y ~ Time.week + Distance.km,
                  random = ~ 1 | BatID,
                   data = bats)
  lrstat[j] <- as.numeric(2*(logLik(hyp_alt) - logLik(hyp_null)))
}
par(mfrow=c(1,1))
hist(lrstat, main = "", xlab="Distance under H0")
p_val_bat_boot <- mean(lrstat >= bat_dist)
p_val_bat_boot

random_bats_base <- nlme::lme(Wingspan.mm ~ Time.week + Distance.km,
                         data = bats,
                         random = ~ 1 | BatID,
                         method = "ML")

random_bats <- nlme::lme(Wingspan.mm ~ Time.week + Distance.km,
                         data = bats,
                         random = ~ 1 + Time.week | BatID,
                         method = "ML")

# AIC:    1589.451.
#logLik: -787.7257


random_bats_1 <- nlme::lme(Wingspan.mm ~ Time.week + Distance.km,
                           data = bats,
                           #random = ~ 1 | BatID,
                           random = ~ 1 + Time.week | BatID,
                           #correlation = corAR1(form = ~ 1 | BatID),
                           correlation = corAR1(form = ~ 1 + Time.week | BatID),
                           method = "ML")
# AIC:     1470.967
# logLik: -727.4835

random_bats_2 <- nlme::lme(Wingspan.mm ~  Time.week + Distance.km,
                           data = bats,
                           random = ~ 1 + Time.week | BatID,
                           correlation = corARMA(form = ~ 1 + Time.week | BatID ,
                                                 p = 4, q = 0),
                           method = "ML")
# AIC:     1471.025
# logLik:  -724.5126
anova(random)

random_bats_3 <- nlme::lme(Wingspan.mm ~ Distance.km,
                         data = bats,
                         random = ~ 1 | BatID,
                         method = "ML")
# AIC:     2821.378
# logLik: -1406.689

random_bats_4 <- nlme::lme(Wingspan.mm ~ Distance.km,
                           data = bats,
                           random = ~ 1 + Time.week | BatID,
                           correlation = corAR1(form = ~ 1 + Time.week | BatID),
                           method = "ML")
# AIC:     1757.422
# logLik: -873.7109

random_bats_5 <- nlme::lme(Wingspan.mm ~ Distance.km,
                           data = bats,
                           random = ~ 1 | BatID,
                           correlation = corARMA(form = ~ Time.week | BatID,
                                                 p = 4, q = 0),
                           method = "ML")
# AIC:     1625.955
# logLik: -804.9775

random_bats_6 <- nlme::lme(Wingspan.mm ~ Time.week + Distance.km,
                           data = bats,
                           random = ~ 1 | BatID,
                           correlation = corARMA(form = ~ Time.week | BatID,
                                                 p = 5, q = 0),
                           method = "ML")
# AIC:     1530.336
# logLik: -755.1681
anova(random_bats_2, random_bats_6)

random_bats_7 <- nlme::lme(Wingspan.mm ~ Time.week + Distance.km,
                           data = bats,
                           random = ~ 1 | BatID,
                           correlation = corARMA(form = ~ Time.week | BatID,
                                                 p = 7, q = 0),
                           method = "ML")
# AIC:     1517.757
# logLik: -746.8786
anova(random_bats_6, random_bats_7)

random_bats_8 <- nlme::lme(Wingspan.mm ~ Time.week + Distance.km,
                           data = bats,
                           random = ~ 1 | BatID,
                           correlation = corARMA(form = ~ Time.week | BatID,
                                                 p = 8, q = 0),
                           method = "ML")
# AIC:     1519.059
# logLik: -746.5296
anova(random_bats_7, random_bats_8)

maxLag = 14
plot(ACF(random_bats_base, resType = "normalized", maxLag = 14), alpha = .05/maxLag)
plot(ACF(random_bats, resType = "normalized", maxLag = 14), alpha = .05/maxLag)
plot(ACF(random_bats_1, resType = "normalized", maxLag = 14), alpha = .05/maxLag)
plot(ACF(random_bats_2, resType = "normalized", maxLag = 14), alpha = .05/maxLag)
plot(ACF(random_bats_3, resType = "normalized", maxLag = 14), alpha = .05/maxLag)
plot(ACF(random_bats_4, resType = "normalized", maxLag = 14), alpha = .05/maxLag)
plot(ACF(random_bats_5, resType = "normalized", maxLag = 14), alpha = .05/maxLag)
plot(ACF(random_bats_6, resType = "normalized", maxLag = 14), alpha = .05/maxLag)
plot(ACF(random_bats_7, resType = "normalized", maxLag = 14), alpha = .05/maxLag)
plot(ACF(random_bats_8, resType = "normalized", maxLag = 14), alpha = .05/maxLag)

best_bat_model <- random_bats_1
summary(random_bats_1)

bats_fixed <- fixed.effects(best_bat_model)
bat_se <- summary(best_bat_model)$tTable[, "Std.Error"]
# 95% CI
bats_lower <- bats_fixed - 1.96*bat_se
bats_higher <- bats_fixed + 1.96*bat_se
bats_rand <- random.effects(best_bat_model)

## 1. The probability that in week 10, bat 6 has a wingspan of under 68mm.
# retrieve bat distance
bat_6_dist <- bats$Distance.km[bats$BatID == 6 &
                                 bats$Time.week == 10]
bat_under_68 <- unname(bats_fixed["(Intercept)"]) + unname(bats_fixed["Time.week"])*10 +
  unname(bats_fixed["Distance.km"])*bat_6_dist + bats_rand[6,]$`(Intercept)` +
  bats_rand[6,]$Time.week*10
# retrieve the standard deviation
# Retrieve for the random effects
vc <- VarCorr(best_bat_model)

# Random Intercept SD
sd_beta0 <- as.numeric(vc["(Intercept)", "StdDev"])
# Random Slope SD
sd_beta1 <- as.numeric(vc["Time.week", "StdDev"])
# Residual SD
sd_res <- as.numeric(vc["Residual", "StdDev"])
# Corr. between intercept and slope
corr_beta_0_1 <- as.numeric(vc["Time.week", "Corr"])

# Retrieve Covariance
cov_beta_0_1 <- corr_beta_0_1 * sd_beta0 * sd_beta1

# Compute SD Total
var_total <- sd_res^2 + sd_beta0^2 + (10^2) * sd_beta1^2 +
  2*(10)*cov_beta_0_1
sd_total <- sqrt(var_total)
sd_total
sd_prob <- sigma(best_bat_model)
prob_bat_under_68 <- pnorm(68,
                           mean = bat_under_68,
                           sd = sd_total)

## 2. The probability of a bat roosting 9km from the city centre has a wing span between 82 and 86mm at
## the time of the final measurement.

## final measurement will be max Time
end_time <- max(bats$Time.week)
bat_between_82_and_86 <- unname(bats_fixed["(Intercept)"]) + 
  unname(bats_fixed["Time.week"])*end_time +
  unname(bats_fixed["Distance.km"])*9

var_end_total <- sd_res^2 + sd_beta0^2 + (end_time^2) * sd_beta1^2 +
  2*(end_time)*cov_beta_0_1

sd_end_total <- sqrt(var_end_total)

prob_82_86 <- pnorm(86, mean = bat_between_82_and_86, sd = sd_total) - 
              pnorm(82, mean = bat_between_82_and_86, sd = sd_total)

prob_bat_under_68*100
prob_82_86*100

# Model Diagnostics

## BATS: Diagnostics ##

plot(best_bat_model)
# Residual Diagnostics

# See is there is a lack of fit
plot(best_bat_model, resid(.) ~ fitted(.))

# Residuals vs. Covariates
plot(best_bat_model, resid(.) ~ Distance.km)
plot(best_bat_model, resid(.) ~ Time.week)

# Residuals by Grouping Factor
plot(best_bat_model, BatID ~ resid(.))
plot(best_bat_model,
     form = fitted(.) ~ Time.week | BatID,
     xlab = "Week",
     ylab = "Fitted wingspan")

plot(best_bat_model, Wingspan.mm ~ fitted(.) | BatID, abline = c(0,1))

# Random-effects Diagnostics

rs_wingspan_residuals <- ranef(best_bat_model)

# RE scatterplot
plot(ranef(best_bat_model)$`(Intercept`)
par(mfrow=c(1,2))
# RE `BatID` #
qqnorm(rs_wingspan_residuals$`(Intercept)`,
       main = "")
qqline(rs_wingspan_residuals$`(Intercept)`)

qqnorm(rs_wingspan_residuals$Time.week,
       main = "")
qqline(rs_wingspan_residuals$Time.week)

qqnorm(best_bat_model)
