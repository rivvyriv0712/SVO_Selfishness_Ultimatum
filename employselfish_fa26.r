required_packages <- c("car", "rstatix", "ggplot2", "dplyr", "broom", "tidyverse", "emmeans", "effectsize", "performance", "sjPlot", "interactions", "flextable", "mediation", "brglm2")
new_packages <- required_packages[!(required_packages %in% installed.packages() [, "Package"])]
if(length(new_packages)) install.packages(new_packages)
lapply(required_packages, library, character.only = TRUE)

employselfish <- employselfish %>%
  filter(!is.na(Status))
employselfish <- employselfish[-1, ]

library(readr)
employselfish <- read_csv("//home//ribby//Downloads//EmploymentSelfishnessStudy_Fall26_RRobinson(CLEAN DATA).csv", locale=locale(encoding="latin1"))
str(employselfish)
nrow(employselfish)
head(employselfish)

employselfish <- employselfish %>%
  mutate(across(everything(), ~ type.convert(., as.is = TRUE)))

employselfish <- employselfish %>%
  mutate(Resource_Availability = factor(Resource_Availability, levels = c(1, 2), labels = c("abundance", "scarcity")),
         `Social Feedback` = factor(`Social Feedback`, levels = c(1, 2) , labels = c("inclusion", "exclusion")),
         FairnessLevel = factor(FairnessLevel, levels = c(1, 2), labels = c("fair", "unfair")))

employselfish <- employselfish %>%
  mutate(SVO_C = as.numeric(scale(SVO_Angle, center = TRUE, scale = FALSE)))


employselfish <- employselfish %>%
  mutate(EgocentricSelfishness = as.numeric(EgocentricSelfishness),
         AdaptiveSelfishness = as.numeric(AdaptiveSelfishness),
         PathologicalSelfishness = as.numeric(PathologicalSelfishness),
         TotalSelfishness = as.numeric(TotalSelfishness))


contrasts(employselfish$Resource_Availability) <- contr.sum(2)
contrasts(employselfish$`Social Feedback`) <- contr.sum(2)
contrasts(employselfish$FairnessLevel) <- contr.sum(2)

employselfish <- employselfish %>%
  mutate(rejection = case_when(
    FairnessLevel == "fair" ~ FairProposal,
    FairnessLevel == "unfair" ~ UnfairProposal,
    TRUE ~ NA_real_),
    reject_prop = as.integer(rejection == 2))

employselfish <- employselfish %>%
  mutate(scarcity = case_when(
    Resource_Availability == "abundance" ~ RealVignette...28,
    Resource_Availability == "scarcity" ~ RealVignette...30,
    TRUE ~ NA_real_),
    scarcity_percept = as.integer(scarcity))

employselfish <- employselfish %>%
  mutate(fairness = case_when(
    FairnessLevel == "fair" ~ UltimatumMani...63,
    FairnessLevel == "unfair" ~ UltimatumMani...65,
    TRUE ~ NA_real_),
    fairness_percept = as.integer(fairness))

# Factors — check labels and counts
table(employselfish$Resource_Availability, useNA = "always")
table(employselfish$`Social Feedback`, useNA = "always")
table(employselfish$FairnessLevel, useNA = "always")

# Outcome coalescing workedemployselfish
sum(is.na(employselfish$reject_prop))
table(employselfish$rejection, employselfish$reject_prop, useNA = "always")
# Expect diagonal: 1 -> 0, 2 -> 1

# Source column ties to condition
table(employselfish$FairnessLevel, employselfish$reject_prop, useNA = "always")

# No participant with both columns filled
sum(!is.na(employselfish$FairProposal) &
      !is.na(employselfish$UnfairProposal))

# SVO centered
mean(employselfish$SVO_C)   # should be ~0
sd(employselfish$SVO_Angle) # for later use

# Selfishness scores present
summary(employselfish$EgocentricSelfishness)
summary(employselfish$PathologicalSelfishness)
summary(employselfish$AdaptiveSelfishness)
summary(employselfish$TotalSelfishness)

#------------------------------------------- BEGINNING ANALYSIS---------------------------------------------------------------------------------
employselfish %>% count(Resource_Availability, `Social Feedback`, FairnessLevel)

employselfish %>%
  group_by(Resource_Availability, `Social Feedback`, FairnessLevel) %>%
  summarise(
    n = n(),
    reject_rate = mean(reject_prop),
    .groups = "drop"
  )

employselfish %>%
  summarise(
    mean = mean(SVO_Angle, na.rm = TRUE),
    sd = sd(SVO_Angle, na.rm = TRUE),
    min = min(SVO_Angle, na.rm = TRUE),
    max = max(SVO_Angle, na.rm = TRUE)
  )

#DESCRIPTIVE ONLY
employselfish <- employselfish %>%
  mutate(SVO_cat = case_when(
    SVO_Angle > 57.15 ~ "altruistic",
    SVO_Angle > 22.45 ~ "prosocial",
    SVO_Angle > -12.04 ~"individualistic",
    TRUE ~ "competitive"
  ))
table(employselfish$SVO_cat)

#--------------------------------------------------- MANIPULATION CHECKS NEED TO BE COMPLETED---------------------------------------------------
mc1 <- lm(scarcity_percept ~ `Social Feedback` + Resource_Availability + FairnessLevel,
          data = employselfish)
summary(mc1)
eta_squared(mc1)

mc2 <- lm(ReflexiveAverage ~ `Social Feedback` + Resource_Availability + FairnessLevel,
          data = employselfish)
summary(mc2)
eta_squared(mc2)

mc3 <- lm(fairness_percept ~ `Social Feedback` + Resource_Availability + FairnessLevel,
          data = employselfish)
summary(mc3)
eta_squared(mc3)

#Plots
employselfish %>%
  ggplot(aes(x = Resource_Availability, y = scarcity_percept)) +
  geom_boxplot() + theme_minimal()

employselfish %>%
  ggplot(aes(x = `Social Feedback`, y = ReflexiveAverage)) +
  geom_boxplot() + theme_minimal()

employselfish %>%
  ggplot(aes(x = FairnessLevel, y = fairness_percept)) +
  geom_boxplot() + theme_minimal()
#----------------------------------------------------- HYPOTHESIS I---------------------------------------------------------------------------
h1 <- glm(reject_prop ~ FairnessLevel, data = employselfish, family = binomial)
summary(h1)
exp(coef(h1))

emmeans(h1, ~ FairnessLevel, type = "response")

h1_firth <- glm(reject ~ FairnessLevel, data = employselfish,
                family = binomial, method = "brglmFit")
#------------------------------------------------------ HYPOTHESIS II----------------------------------------------------------------------------

unfair_prop <- employselfish %>% filter(FairnessLevel == "unfair")
fair_prop <- employselfish %>% filter(FairnessLevel == "fair")

binom.test(sum(fair_prop$reject), nrow(fair_prop))

h2a <- glm(reject ~ SVO_C, data = fair_prop, family = binomial)
summary(h2a)
exp(coef(h2a))

emtrends(h2a ~ 1, var = "SVO_C")
emmeans(h2a, ~ SVO_C,
        at = list(SVO_C = c(-1, 0, 1) * sd(fair_prop$SVO_C)),
        type = "response")

h2b <- glm(reject ~ SVO_C + Resource_Availability + `Social Feedback`,
           data = unfair_prop, family = binomial)
summary(h2b)
exp(coef(h2b))

emtrends(h2b ~ 1, var = "SVO_C")
emmeans(h2b, ~ SVO_C,
        at = list(SVO_C = c(-1, 0, 1) * sd(unfair_prop$SVO_C)),
        type = "response")

h2_full <- glm(reject ~ FairnessLevel * SVO_C + Resource_Availability + `Social Feedback`,
               data = employselfish, family = binomial)
Anova(h2_full, type = "III")
summary(h2_full)
emtrends(h2_full, ~ FairnessLevel, var = "SVO_C")
#------------------------------------------------------ HYPOTHESIS III--------------------------------------------------------------------------
h3 <- glm(reject ~ Resource_Availability * SVO_C + `Social Feedback`,
          data = unfair_prop, family = binomial)
summary(h3)
Anova(h3, type = "III")

emtrends(h3, ~Resource_Availability, var = "SVO_C")

emmeans(h3, ~ Resource_Availability | SVO_C,
        at = list(SVO_C = c(-1, 0, 1) * sd(unfair_prop$SVO_C)),
        type = "response")

emmeans(h3, ~ Resource_Availability | SVO_C,
        at = list(SVO_C = c(-1, 0, 1) * sd(unfair_prop$SVO_C)),
        type = "response") %>%
  pairs()

plot_model(h3, type = "pred",
           terms = c("SVO_C", "Resource_Availability"))
#------------------------------------------------------ HYPOTHESIS IV--------------------------------------------------------------------------
h4 <- glm(reject ~ `Social Feedback` * SVO_C + Resource_Availability,
          data = unfair_prop, family = binomial)
summary(h4)
Anova(h4, type = "III")

emtrends(h4, ~ `Social Feedback`, var = "SVO_C")

emmeans(h4, ~ Resource_Availability | SVO_C,
        at = list(SVO_C = c(-1, 0, 1) * sd(unfair_prop$SVO_C)),
        type = "response")

emmeans(h4, ~ `Social Feedback` | SVO_C,
        at = list(SVO_C = c(-1, 0, 1) * sd(unfair_prop$SVO_C)),
        type = "response") %>%
  pairs()

plot_model(h4, type = "pred",
           terms = c("SVO_C", "`Social Feedback`"))
#---------------------------------------------------- HYPOTHESIS V------------------------------------------------------------------------------
h5 <- glm(reject ~ Resource_Availability * `Social Feedback` + SVO_C + FairnessLevel,
          data = employselfish, family = binomial)
summary(h5)
Anova(h5, type = "III")

emmeans(h5, ~ Resource_Availability * `Social Feedback`, type = "response")
contrast(emmeans(h5, ~ Resource_Availability * `Social Feedback`, type = "response"), interaction = TRUE)

library(BayesFactor)
bf_h5 <- generalTestBF(reject ~ Resource_Availability * `Social Feedback` + SVO_C + FairnessLevel,
                       data = employselfish, neverExclude = c("SVO_C", "FairnessLevel"))
bf_h5
#---------------------------------------------------- HYPOTHESIS VI----------------------------------------------------------------------------
h6a <- glm(reject ~ EgocentricSelfishness + PathologicalSelfishness + AdaptiveSelfishness +
             Resource_Availability + `Social Feedback`,
           data = unfair_prop, family = binomial)
summary(h6a)
Anova(h6a, type = "III")

#need to do one for each type of selfishness--has not been ran
med_fit <- lm(ego_self ~ SVO_C + Resource_Availability + Feedback,
              data = unfair_prop, family = binomial)
out_fit <- glm(reject ~ SVO_C + ego_self + Resource_Availability + `Social Feedback`,
               data = unfair_prop, family = binomial)
med <- mediate(med_fit, out_fit, treat = "SVO_C", mediator = "ego_self",
               boot = TRUE, sims = 1000)
summary(med)
#----------------------------------------------- HYPOTHESIS VII----------------------------------------------------------------------------------
h7 <- lm(cbind(EgocentricSelfishness, PathologicalSelfishness, AdaptiveSelfishness) ~
           SVO_C + Resource_Availability + `Social Feedback` + FairnessLevel,
         data = employselfish)
Anova(h7, type = "III")
summary(h7)