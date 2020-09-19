# Library Installations and Working Directory
library(psy)
source("mtmm.R")
library(rstudioapi)
library(ggpubr)
library(car)
library(tidyverse)
library(psych)
library(GPArotation)
setwd(dirname(getActiveDocumentContext()$path))

# Data Import 
raw_data <- read_delim("../Data/data.csv", delim = "\t")

# Data Cleaning and Manipulation
# Editing scales that need to be reverse scored
raw_data[, paste0("E", c(2, 4, 6, 8, 10))] <- 6 - raw_data[, paste0("E", c(2, 4, 6, 8, 10))]
raw_data[, paste0("N", c(2, 4))] <- 6 - raw_data[, paste0("N", c(2, 4))]
raw_data[, paste0("A", c(1, 3, 5, 7))] <- 6 - raw_data[, paste0("A", c(1, 3, 5, 7))]
raw_data[, paste0("C", c(2, 4, 6, 8))] <- 6 - raw_data[, paste0("C", c(2, 4, 6, 8))]
raw_data[, paste0("O", c(2, 4, 6))] <- 6 - raw_data[, paste0("O", c(2, 4, 6))]
raw_data <- raw_data %>%
  rename("Race" = "race", "Age" = "age", "NatEng" = "engnat", "Gender" = "gender", "Hand" = "hand", "Source" = "source", "Country" = "country") %>%
  mutate(NatEng = factor(NatEng, levels = paste0(0:2), labels = c("Missed", "Yes", "No")), 
         Race = factor(Race, levels = paste0(0:13), labels = c("Missed", "Mixed", "Arctic", "Caucasion (European)", "Caucasion (Indian)", "Caucasion (Middle East)", "Caucasion (North African/Other)", "Indigenous Australian", "Native American", "Northeast Asian", "Pacific Islander", "Southeast Asian", "West African", "Other")),
         Gender = factor(Gender, levels = paste0(0:3), labels = c("Missed", "Male", "Female", "Other")),
         Hand = factor(Hand, levels = paste0(0:3), labels = c("Missed", "Right", "Left", "Both")),
         Source = factor(Source, levels = paste0(1:6)),
         Age = if_else(Age > 1970 & Age < 2005, 2012 - Age, Age), # Assuming these respondents accidently put their birth year instead of Age (taken in 2012)
         Country = str_replace_all(Country, "\\(nu", "NA"),
         Extroversion = (E1 + E2 + E3 + E4 + E5 + E6 + E7 + E8 + E9 + E10)/50,
         Neuroticism = (N1 + N2 + N3 + N4 + N5 + N6 + N7 + N8 + N9 + N10)/50,
         Agreeableness = (A1 + A2 + A3 + A4 + A5 + A6 + A7 + A8 + A9 + A10)/50,
         Conscientiousness = (C1 + C2 + C3 + C4 + C5 + C6 + C7 + C8 + C9 + C10)/50,
         Openness = (O1 + O2 + O3 + O4 + O5 + O6 + O7 + O8 + O9 + O10)/50) %>%
  dplyr::filter(Age > 0 & Age < 110) # Removing Age values that don't make sense
raw_data <- raw_data[ , c(1:7, 58:62, 8:57)] # Composite scores after demographics and before scale scores

# Statistical Analysis
par(mfrow = c(1, 1))
# Validity
# Factor Analysis
scales_only <- raw_data[, 13:62]
fa.parallel(scales_only, fm = "minres", fa = "fa") # Determine number of factors through parallel analysis
factor_analysis <- fa(scales_only, nfactors = 5, rotate = "oblimin", fm = "minres")
print(factor_analysis$loadings, cutoff = .3)
fa.diagram(print(factor_analysis$loadings, cutoff = .3))
# Convergent and Discriminant Validity
mtmm(raw_data, list(paste0("E", 1:10), paste0("N", 1:10), paste0("A", 1:10), paste0("C", 1:10), paste0("O", 1:10)))
# Reliability
psych::alpha(scales_only[, 1:10])
psych::alpha(scales_only[, 1:10])$total$std.alpha
psych::alpha(scales_only[, 11:20])
psych::alpha(scales_only[, 11:20])$total$std.alpha
psych::alpha(scales_only[, 21:30])
psych::alpha(scales_only[, 21:30])$total$std.alpha
psych::alpha(scales_only[, 31:40])
psych::alpha(scales_only[, 31:40])$total$std.alpha
psych::alpha(scales_only[, 41:50])
psych::alpha(scales_only[, 41:50])$total$std.alpha

# Mean differences in scales
# Mean Difference in Males/Females - Extroversion
gender_md_e <- raw_data[, c(4,8)] %>%
  dplyr::filter(Gender == "Male" | Gender == "Female")
# Normality
male_normality_e <- gender_md_e %>%
  filter(Gender == "Male") 
ggplot(male_normality_e, aes(Extroversion)) +
  geom_histogram(aes(y = ..density..), bins = 40) +
  ggtitle("Male Normality by Extroversion") +
  stat_function(fun = dnorm, args = list(mean(male_normality_e$Extroversion), sd = sd(male_normality_e$Extroversion)))
female_normality_e <- gender_md_e %>%
  filter(Gender == "Female") 
ggplot(female_normality_e, aes(Extroversion)) +
  geom_histogram(aes(y = ..density..), bins = 40) +
  ggtitle("Female Normality by Extroversion") +
  stat_function(fun = dnorm, args = list(mean(female_normality_e$Extroversion), sd = sd(female_normality_e$Extroversion)))
var.test(Extroversion ~ Gender, data = gender_md_e, conf.level = .95) # Test for homogeneity in variances
t.test(Extroversion ~ Gender, data = gender_md_e, var.equal = FALSE, conf.level = .95)
ggplot(gender_md_e, aes(Gender, Extroversion, color = Gender)) + geom_boxplot() + ggtitle("Extroversion Scores per Gender") + theme(plot.title = element_text(hjust = .5))
# Mean Difference in Males/Females - Neuroticism
gender_md_n <- raw_data[, c(4,9)] %>%
  dplyr::filter(Gender == "Male" | Gender == "Female")
male_normality_n <- gender_md_n %>%
  filter(Gender == "Male") 
ggplot(male_normality_n, aes(Neuroticism)) +
  geom_histogram(aes(y = ..density..), bins = 40) +
  ggtitle("Male Normality by Neuroticism") +
  stat_function(fun = dnorm, args = list(mean(male_normality_n$Neuroticism), sd = sd(male_normality_n$Neuroticism)))
female_normality_n <- gender_md_n %>%
  filter(Gender == "Female") 
ggplot(female_normality_n, aes(Neuroticism)) +
  geom_histogram(aes(y = ..density..), bins = 40) +
  ggtitle("Female Normality by Neuroticism") +
  stat_function(fun = dnorm, args = list(mean(female_normality_n$Neuroticism), sd = sd(female_normality_n$Neuroticism)))
var.test(Neuroticism ~ Gender, data = gender_md_n, conf.level = .95) # Test for homogeneity in variances
t.test(Neuroticism ~ Gender, data = gender_md_n, var.equal = FALSE, conf.level = .95)
ggplot(gender_md_n, aes(Gender, Neuroticism, color = Gender)) + geom_boxplot() + ggtitle("Neuroticism Scores per Gender") + theme(plot.title = element_text(hjust = .5))
# Mean Difference in Males/Females - Agreeableness
gender_md_a <- raw_data[, c(4,10)] %>%
  dplyr::filter(Gender == "Male" | Gender == "Female")
male_normality_a<- gender_md_a %>%
  filter(Gender == "Male") 
ggplot(male_normality_a, aes(Agreeableness)) +
  geom_histogram(aes(y = ..density..), bins = 40) +
  ggtitle("Male Normality by Agreeableness") +
  stat_function(fun = dnorm, args = list(mean(male_normality_a$Agreeableness), sd = sd(male_normality_a$Agreeableness)))
female_normality_a <- gender_md_a %>%
  filter(Gender == "Female") 
ggplot(female_normality_a, aes(Agreeableness)) +
  geom_histogram(aes(y = ..density..), bins = 40) +
  ggtitle("Female Normality by Agreeableness") +
  stat_function(fun = dnorm, args = list(mean(female_normality_a$Agreeableness), sd = sd(female_normality_a$Agreeableness)))
var.test(Agreeableness ~ Gender, data = gender_md_a, conf.level = .95) # Test for homogeneity in variances
t.test(Agreeableness ~ Gender, data = gender_md_a, conf.level = .95, var.equal = FALSE)
ggplot(gender_md_a, aes(Gender, Agreeableness, color = Gender)) + geom_boxplot() + ggtitle("Agreeableness Scores per Gender") + theme(plot.title = element_text(hjust = .5))
# Mean Difference in Males/Females - Conscientiousness
gender_md_c <- raw_data[, c(4,11)] %>%
  dplyr::filter(Gender == "Male" | Gender == "Female")
male_normality_c <- gender_md_c %>%
  filter(Gender == "Male") 
ggplot(male_normality_c, aes(Conscientiousness)) +
  geom_histogram(aes(y = ..density..), bins = 40) +
  ggtitle("Male Normality by Conscientiousness") +
  stat_function(fun = dnorm, args = list(mean(male_normality_c$Conscientiousness), sd = sd(male_normality_c$Conscientiousness)))
female_normality_c <- gender_md_c %>%
  filter(Gender == "Female") 
ggplot(female_normality_c, aes(Conscientiousness)) +
  geom_histogram(aes(y = ..density..), bins = 40) +
  ggtitle("Female Normality by Conscientiousness") +
  stat_function(fun = dnorm, args = list(mean(female_normality_c$Conscientiousness), sd = sd(female_normality_c$Conscientiousness)))
var.test(Conscientiousness ~ Gender, data = gender_md_c, conf.level = .95) # Test for homogeneity in variances
t.test(Conscientiousness ~ Gender, data = gender_md_c, var.equal = FALSE, conf.level = .95)
ggplot(gender_md_c, aes(Gender, Conscientiousness, color = Gender)) + geom_boxplot() + ggtitle("Conscientiousness Scores per Gender") + theme(plot.title = element_text(hjust = .5))
# Mean Difference in Males/Females - Openness
gender_md_o <- raw_data[, c(4,12)] %>%
  dplyr::filter(Gender == "Male" | Gender == "Female")
male_normality_o <- gender_md_o %>%
  filter(Gender == "Male") 
ggplot(male_normality_o, aes(Openness)) +
  geom_histogram(aes(y = ..density..), bins = 40) +
  ggtitle("Male Normality by Openness") +
  stat_function(fun = dnorm, args = list(mean(male_normality_o$Openness), sd = sd(male_normality_o$Openness)))
female_normality_o <- gender_md_o %>%
  filter(Gender == "Female") 
ggplot(female_normality_o, aes(Openness)) +
  geom_histogram(aes(y = ..density..), bins = 40) +
  ggtitle("Female Normality by Openness") +
  stat_function(fun = dnorm, args = list(mean(female_normality_o$Openness), sd = sd(female_normality_o$Openness)))
var.test(Openness ~ Gender, data = gender_md_o, conf.level = .95) # Test for homogeneity in variances
t.test(Openness ~ Gender, data = gender_md_o, conf.level = .95, var.equal = FALSE)
ggplot(gender_md_o, aes(Gender, Openness, color = Gender)) + geom_boxplot() + ggtitle("Openness Scores per Gender") + theme(plot.title = element_text(hjust = .5))
# Mean Differences by Race - Extroversion
race_md_e <- raw_data[ , c(1,8)] 
race_md_e_summary <- race_md_e %>%
  group_by(Race) %>%
  summarize(count = n(), mean = mean(Extroversion), sd = sd(Extroversion))
leveneTest(Extroversion ~ Race, data = race_md_e) # Test for homogeneity in variances
# ANOVA test with no assumption of equal variances
oneway.test(Extroversion ~ Race, data = race_md_e)
# Pairwise t-test with no assumption of equal variances (to find mean differences between groups)
pairwise.t.test(race_md_e$Extroversion, race_md_e$Race, p.adjust.method = "BH", pool.sd = FALSE)
ggplot(race_md_e, aes(Race, Extroversion, color = Race)) + geom_boxplot() + theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank())
# Mean Differences by Race - Neuroticism
race_md_n <- raw_data[ , c(1,9)] 
race_md_n_summary <- race_md_n %>%
  group_by(Race) %>%
  summarize(count = n(), mean = mean(Neuroticism), sd = sd(Neuroticism))
leveneTest(Neuroticism ~ Race, data = race_md_n) # Test for homogeneity in variances
# ANOVA test with no assumption of equal variances
oneway.test(Neuroticism ~ Race, data = race_md_n)
# Pairwise t-test with no assumption of equal variances (to find mean differences between groups)
pairwise.t.test(race_md_n$Neuroticism, race_md_n$Race, p.adjust.method = "BH", pool.sd = FALSE)
ggplot(race_md_n, aes(Race, Neuroticism, color = Race)) + geom_boxplot() + theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank())
# Mean Differences by Race - Agreeableness
race_md_a <- raw_data[ , c(1,10)] 
race_md_a_summary <- race_md_a %>%
  group_by(Race) %>%
  summarize(count = n(), mean = mean(Agreeableness), sd = sd(Agreeableness))
leveneTest(Agreeableness ~ Race, data = race_md_a) # Test for homogeneity in variances
# ANOVA test with no assumption of equal variances or normality
oneway.test(Agreeableness ~ Race, data = race_md_a)
# Pairwise t-test with no assumption of equal variances (to find mean differences between groups)
pairwise.t.test(race_md_a$Agreeableness, race_md_a$Race, p.adjust.method = "BH", pool.sd = FALSE)
ggplot(race_md_a, aes(Race, Agreeableness, color = Race)) + geom_boxplot() + theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank())
# Mean Differences by Race - Conscientiousness
race_md_c <- raw_data[ , c(1,11)] 
race_md_c_summary <- race_md_c %>%
  group_by(Race) %>%
  summarize(count = n(), mean = mean(Conscientiousness), sd = sd(Conscientiousness))
leveneTest(Conscientiousness ~ Race, data = race_md_c) # Test for homogeneity in variances
# ANOVA test with no assumption of equal variances
oneway.test(Conscientiousness ~ Race, data = race_md_c)
# Pairwise t-test with no assumption of equal variances (to find mean differences between groups)
pairwise.t.test(race_md_c$Conscientiousness, race_md_c$Race, p.adjust.method = "BH", pool.sd = FALSE)
ggplot(race_md_c, aes(Race, Conscientiousness, color = Race)) + geom_boxplot() + theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank())
# Mean Differences by Race - Openness
race_md_o <- raw_data[ , c(1,12)] 
race_md_o_summary <- race_md_o %>%
  group_by(Race) %>%
  summarize(count = n(), mean = mean(Openness), sd = sd(Openness))
leveneTest(Openness ~ Race, data = race_md_o) # Test for homogeneity in variances
# ANOVA test with no assumption of equal variances
oneway.test(Openness ~ Race, data = race_md_o)
# Pairwise t-test with no assumption of equal variances or normality (to find mean differences between groups)
pairwise.t.test(race_md_o$Openness, race_md_o$Race, p.adjust.method = "BH", pool.sd = FALSE)
ggplot(race_md_o, aes(Race, Openness, color = Race)) + geom_boxplot() + theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank())
# Mean Difference in English as a first language - Extroversion
nateng_md_e <- raw_data[, c(3,8)] %>%
  filter(raw_data$NatEng == "Yes" | raw_data$NatEng == "No")
nateng_normality_e_e <- nateng_md_e %>%
  filter(NatEng == "Yes") 
ggplot(nateng_normality_e_e, aes(Extroversion)) +
  geom_histogram(aes(y = ..density..), bins = 40) +
  ggtitle("Extroversion Normality for English as a Natural Language") +
  stat_function(fun = dnorm, args = list(mean(nateng_normality_e_e$Extroversion), sd = sd(nateng_normality_e_e$Extroversion)))
nateng_normality_e_e <- nateng_md_e %>%
  filter(NatEng == "No") 
ggplot(nateng_normality_e_e, aes(Extroversion)) +
  geom_histogram(aes(y = ..density..), bins = 40) +
  ggtitle("Extroversion Normality for English not as a Natural Language") +
  stat_function(fun = dnorm, args = list(mean(nateng_normality_e_e$Extroversion), sd = sd(nateng_normality_e_e$Extroversion)))
var.test(Extroversion ~ NatEng, data = nateng_md_e, conf.level = .95) # Test for homogeneity in variances
t.test(Extroversion ~ NatEng, data = nateng_md_e, var.equal = FALSE, conf.level = .95)
ggplot(nateng_md_e, aes(NatEng, Extroversion, color = NatEng)) + geom_boxplot() + scale_x_discrete(name = "English First Language?") + ggtitle("Extroversion Scores per English as First Langauge") + theme(plot.title = element_text(hjust = .5))
# Mean Difference in English as a first language - Neuroticism
nateng_md_n <- raw_data[, c(3,9)] %>%
  filter(raw_data$NatEng == "Yes" | raw_data$NatEng == "No")
nateng_normality_e_n <- nateng_md_n %>%
  filter(NatEng == "Yes") 
ggplot(nateng_normality_e_n, aes(Neuroticism)) +
  geom_histogram(aes(y = ..density..), bins = 40) +
  ggtitle("Neuroticism Normality for English as a Natural Language") +
  stat_function(fun = dnorm, args = list(mean(nateng_normality_e_n$Neuroticism), sd = sd(nateng_normality_e_n$Neuroticism)))
nateng_normality_n_n <- nateng_md_n %>%
  filter(NatEng == "No") 
ggplot(nateng_normality_n_n, aes(Neuroticism)) +
  geom_histogram(aes(y = ..density..), bins = 40) +
  ggtitle("Neuroticism Normality for English not as a Natural Language") +
  stat_function(fun = dnorm, args = list(mean(nateng_normality_n_n$Neuroticism), sd = sd(nateng_normality_n_n$Neuroticism)))
var.test(Neuroticism ~ NatEng, data = nateng_md_n, conf.level = .95) # Test for homogeneity in variances
t.test(Neuroticism ~ NatEng, data = nateng_md_n, var.equal = FALSE, conf.level = .95)
ggplot(nateng_md_n, aes(NatEng, Neuroticism, color = NatEng)) + geom_boxplot() + scale_x_discrete(name = "English First Language?") + ggtitle("Neuroticism Scores per English as First Langauge") + theme(plot.title = element_text(hjust = .5))
# Mean Difference in English as a first language - Agreeableness
nateng_md_a <- raw_data[, c(3,10)] %>%
  filter(raw_data$NatEng == "Yes" | raw_data$NatEng == "No")
nateng_normality_e_a <- nateng_md_a %>%
  filter(NatEng == "Yes") 
ggplot(nateng_normality_e_a, aes(Agreeableness)) +
  geom_histogram(aes(y = ..density..), bins = 40) +
  ggtitle("Agreeableness Normality for English as a Natural Language") +
  stat_function(fun = dnorm, args = list(mean(nateng_normality_e_a$Agreeableness), sd = sd(nateng_normality_e_a$Agreeableness)))
nateng_normality_n_a <- nateng_md_a %>%
  filter(NatEng == "No") 
ggplot(nateng_normality_n_a, aes(Agreeableness)) +
  geom_histogram(aes(y = ..density..), bins = 40) +
  ggtitle("Agreeableness Normality for English not as a Natural Language") +
  stat_function(fun = dnorm, args = list(mean(nateng_normality_n_a$Agreeableness), sd = sd(nateng_normality_n_a$Agreeableness)))
var.test(Agreeableness ~ NatEng, data = nateng_md_a, conf.level = .99) # Test for homogeneity in variances
t.test(Agreeableness ~ NatEng, data = nateng_md_a, conf.level = .99)
ggplot(nateng_md_a, aes(NatEng, Agreeableness, color = NatEng)) + geom_boxplot()+ scale_x_discrete(name = "English First Language?") + ggtitle("Agreeableness Scores per English as First Langauge") + theme(plot.title = element_text(hjust = .5))
# Mean Difference in English as a first language - Conscientiousness
nateng_md_c <- raw_data[, c(3,11)] %>%
  filter(raw_data$NatEng == "Yes" | raw_data$NatEng == "No")
nateng_normality_e_c <- nateng_md_c %>%
  filter(NatEng == "Yes") 
ggplot(nateng_normality_e_c, aes(Conscientiousness)) +
  geom_histogram(aes(y = ..density..), bins = 40) +
  ggtitle("Conscientiousness Normality for English as a Natural Language") +
  stat_function(fun = dnorm, args = list(mean(nateng_normality_e_c$Conscientiousness), sd = sd(nateng_normality_e_c$Conscientiousness)))
nateng_normality_n_c <- nateng_md_c %>%
  filter(NatEng == "No") 
ggplot(nateng_normality_n_c, aes(Conscientiousness)) +
  geom_histogram(aes(y = ..density..), bins = 40) +
  ggtitle("Conscientiousness Normality for English not as a Natural Language") +
  stat_function(fun = dnorm, args = list(mean(nateng_normality_n_c$Conscientiousness), sd = sd(nateng_normality_n_c$Conscientiousness)))
var.test(Conscientiousness ~ NatEng, data = nateng_md_c, conf.level = .99) # Test for homogeneity in variances
t.test(Conscientiousness ~ NatEng, data = nateng_md_c, var.equal = FALSE, conf.level = .99)
ggplot(nateng_md_c, aes(NatEng, Conscientiousness, color = NatEng)) + geom_boxplot() + scale_x_discrete(name = "English First Language?") + ggtitle("Conscientousness Scores per English as First Langauge") + theme(plot.title = element_text(hjust = .5))
# Mean Difference in English as a first language - Openness
nateng_md_o <- raw_data[, c(3,12)] %>%
  filter(raw_data$NatEng == "Yes" | raw_data$NatEng == "No")
nateng_normality_e_o <- nateng_md_o %>%
  filter(NatEng == "Yes") 
ggplot(nateng_normality_e_o, aes(Openness)) +
  geom_histogram(aes(y = ..density..), bins = 40) +
  ggtitle("Openness Normality for English as a Natural Language") +
  stat_function(fun = dnorm, args = list(mean(nateng_normality_e_o$Openness), sd = sd(nateng_normality_e_o$Openness)))
nateng_normality_n_o <- nateng_md_o %>%
  filter(NatEng == "No") 
ggplot(nateng_normality_n_o, aes(Openness)) +
  geom_histogram(aes(y = ..density..), bins = 40) +
  ggtitle("Openness Normality for English not as a Natural Language") +
  stat_function(fun = dnorm, args = list(mean(nateng_normality_n_o$Openness), sd = sd(nateng_normality_n_o$Openness)))
var.test(Openness ~ NatEng, data = nateng_md_o, conf.level = .99) # Test for homogeneity in variances
t.test(Openness ~ NatEng, data = nateng_md_o, conf.level = .99)
ggplot(nateng_md_o, aes(NatEng, Openness, color = NatEng)) + geom_boxplot()+ scale_x_discrete(name = "English First Language?") + ggtitle("Openness Scores per English as First Langauge") + theme(plot.title = element_text(hjust = .5))

# Regression Analysis
raw_data$Gender <-  relevel(raw_data$Gender, ref = "Male")
# Age ~ Extroversion by Gender
summary(lm(Extroversion ~ Age + Gender, data = raw_data))
par(mfrow = c(2, 2))
plot(lm(Extroversion ~ Age + Gender, data = raw_data))
ggplot(raw_data, aes(Age, Extroversion, shape = Gender, fill = Gender, color = Gender)) +
geom_smooth(method = "lm") +
theme_bw() +
ggtitle("Extroversion Regression Model")
# Extroversion = .55 + .0016(Age) + .009(Missed) + .02(Female) - .102(Other)
# Age ~ Neuroticism by Gender
summary(lm(Neuroticism ~ Age + Gender, data = raw_data))
plot(lm(Neuroticism ~ Age + Gender, data = raw_data))
ggplot(raw_data, aes(Age, Neuroticism, shape = Gender, fill = Gender, color = Gender)) +
  geom_smooth(method = "lm") +
  theme_bw() +
  ggtitle("Neuroticism Regression Model")
# Neuroticism = .65 - .0023(Age) - .035(Missed) + .058(Female) - .092(Other)
# Age ~ Agreeableness by Gender
summary(lm(Agreeableness ~ Age + Gender, data = raw_data))
plot(lm(Agreeableness ~ Age , data = raw_data))
ggplot(raw_data, aes(Age, Agreeableness, shape = Gender, fill = Gender, color = Gender)) +
  geom_smooth(method = "lm") +
  theme_bw() +
  ggtitle("Agreeableness Regression Model")
# Agreeableness = .068 + .002(Age) + .039(Missed) + .064(Female) - .018(Other)
# Age ~ Conscientiousness by Gender
summary(lm(Conscientiousness ~ Age + Gender, data = raw_data))
plot(lm(Conscientiousness ~ Age, data = raw_data))
ggplot(raw_data, aes(Age, Conscientiousness, shape = Gender, color = Gender, fill = Gender)) +
  geom_smooth(method = "lm") +
  theme_bw() +
  ggtitle("Conscientiousness Regression Model")
# Conscientiousness = .59 + .0027(Age) + .004(Missed) + .01(Female) - .028(Other)
# Age ~ Openness by Gender
summary(lm(Openness ~ Age + Gender, data = raw_data))
plot(lm(Openness ~ Age + Gender, data = raw_data))
ggplot(raw_data, aes(Age, Openness, shape = Gender, fill = Gender, color = Gender)) +
  geom_smooth(method = "lm") +
  theme_bw() +
  ggtitle("Openness Regression Model")
# Openness = .77 + .0009(Age) - .027(Missed) - .025(Female) + .035(Other)

# Descriptive Statistics
par(mfrow = c(1, 1))
ggplot(raw_data, aes(Age, color = Gender, fill = Gender, shape = Gender)) + 
  geom_histogram() +
  scale_y_continuous("Count") +
  ggtitle("Respondents by Age and Gender") +
  theme(plot.title = element_text(hjust = .5))

Race_Overview <- raw_data %>%
 group_by(Race) %>%
 tally()
ggplot(Race_Overview, aes(Race, n, fill = Race, color = Race, shape = Race)) +
 geom_bar(stat = "identity") +
 scale_y_continuous("Amount of Respondents") +
 theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
 ggtitle("Overview of Respondent Race") +
 theme(plot.title = element_text(hjust = .5))

