# Load necessary libraries
library(dplyr)
library(ggplot2)

# Read the CSV file
df_1 <- read.csv("/Users/sahabaarabi/Desktop/پروژه های بیمه/Drugs Interaction/Results/Second Results/فایل های نهایی و ارسالی به غمخوار بعد از آنالیز مجدد /Regression_only_with_interaction.csv")
df_2 <- read.csv("/Users/sahabaarabi/Desktop/پروژه های بیمه/Drugs Interaction/Results/Second Results/فایل های نهایی و ارسالی به غمخوار بعد از آنالیز مجدد /Regression_total.csv")

# Create a column to identify any prescription with at least one DDI
df_2 <- df_2 %>%
  mutate(
    has_ddi = ifelse(interaction_count > 0, "Yes", "No"),
    has_contraindicated = ifelse(Contraindicated > 0, "Yes", "No"),
    has_major = ifelse(Major > 0, "Yes", "No"),
    has_moderate = ifelse(Moderate > 0, "Yes", "No")
  )

df_2 %>% group_by(sex) %>% summarise (
  mean(interaction_count),
  sd(interaction_count)
)
# --- Perform the t-test ---
ttest_interaction_count_sex <- t.test(interaction_count ~ sex, data = df_2)

print("T-test results for Interactions per Prescription by Sex:")
print(ttest_interaction_count_sex)


# --- Prescriptions with ANY DDI ---
tbl_ddi_sex <- table(df_2$sex, df_2$has_ddi)
tbl_ddi_sex
print("Chi-squared test for Prescriptions with any DDI by Sex:")
print(chisq.test(tbl_ddi_sex))

# --- Prescriptions with Contraindicated DDIs ---
tbl_contra_sex <- table(df_2$sex, df_2$has_contraindicated)
tbl_contra_sex
print("Chi-squared test for Prescriptions with Contraindicated DDIs by Sex:")
print(chisq.test(tbl_contra_sex))

# --- Prescriptions with Major DDIs ---
tbl_major_sex <- table(df_2$sex, df_2$has_major)
tbl_major_sex
print("Chi-squared test for Prescriptions with Major DDIs by Sex:")
print(chisq.test(tbl_major_sex))

# --- Prescriptions with Moderate DDIs ---
tbl_moderate_sex <- table(df_2$sex, df_2$has_moderate)
tbl_moderate_sex
print("Chi-squared test for Prescriptions with Moderate DDIs by Sex:")
print(chisq.test(tbl_moderate_sex))


df_2 %>% group_by(age_group) %>% summarise (
  mean(interaction_count),
  sd(interaction_count)
)
# --- Perform the ANOVA ---
anova_interaction_count_age <- aov(interaction_count ~ age_group, data = df_2)
print("ANOVA results for Interactions per Prescription by Age Group:")
summary(anova_interaction_count_age)

# --- Post-Hoc Test (to see which specific groups differ) ---
print("Tukey HSD post-hoc test:")
TukeyHSD(anova_interaction_count_age)

# --- Prescriptions with ANY DDI ---
tbl_ddi_age <- table(df_2$age_group, df_2$has_ddi)
tbl_ddi_age
print("Chi-squared test for Prescriptions with any DDI by Age Group:")
print(chisq.test(tbl_ddi_age))

# --- Prescriptions with Contraindicated DDIs ---
tbl_contra_age <- table(df_2$age_group, df_2$has_contraindicated)
tbl_contra_age
print("Chi-squared test for Prescriptions with Contraindicated DDIs by Age Group:")
print(chisq.test(tbl_contra_age))

# --- Prescriptions with Major DDIs ---
tbl_major_age <- table(df_2$age_group, df_2$has_major)
tbl_major_age
print("Chi-squared test for Prescriptions with Major DDIs by Age Group:")
print(chisq.test(tbl_major_age))

# --- Prescriptions with Moderate DDIs ---
tbl_moderate_age <- table(df_2$age_group, df_2$has_moderate)
tbl_moderate_age 
print("Chi-squared test for Prescriptions with Moderate DDIs by Age Group:")
print(chisq.test(tbl_moderate_age))


library(dplyr)

# Summarize total interactions per specialty
top_specialties <- df_2 %>%
  group_by(speciality) %>%
  summarize(total_interactions = sum(interaction_count, na.rm = TRUE)) %>%
  arrange(desc(total_interactions)) # pick top 3
typeof(top_specialties)
write.csv( top_specialties, '/Users/sahabaarabi/Desktop/salam.csv', row.names = FALSE)
print(top_specialties)

df_filtered <- df_2 %>% filter(interaction_count > 0)
df_filtered

library(dplyr)

interaction_stats <- df_filtered %>%
  group_by(speciality) %>%
  summarize(
    mean_interactions = mean(interaction_count, na.rm = TRUE),
    sd_interactions = sd(interaction_count, na.rm = TRUE),
    n_prescriptions = n()
  )

print(interaction_stats)
write.csv(interaction_stats, '/Users/sahabaarabi/Desktop/salam3.csv', row.names = FALSE)

library(dplyr)

# For each specialty: count total prescriptions, then count those with each interaction type
interaction_counts <- df_filtered %>%
  group_by(speciality) %>%
  summarize(
    total_prescriptions = n(),
    prescriptions_with_contraindication = sum(Contraindicated > 0, na.rm = TRUE),
    prescriptions_with_major = sum(Major > 0, na.rm = TRUE),
    prescriptions_with_moderate = sum(Moderate > 0, na.rm = TRUE)
  ) %>%
  mutate(
    pct_contraindication = round(100 * prescriptions_with_contraindication / total_prescriptions, 2),
    pct_major = round(100 * prescriptions_with_major / total_prescriptions, 2),
    pct_moderate = round(100 * prescriptions_with_moderate / total_prescriptions, 2)
  )

print(n = 40, interaction_counts)
write.csv(interaction_counts, '/Users/sahabaarabi/Desktop/salam4.csv', row.names = FALSE)


# View basic structure
str(df)

# Summary statistics for numerical columns
summary(df)


library(dplyr)

avg_drugs_by_speciality <- df_2 %>%
  group_by(speciality) %>%
  summarise(
    sum_drug_interaction = mean(interaction_count, na.rm = TRUE),
    sd_drug_interaction = sd(interaction_count, na.rm = TRUE),
    n_prescriptions = n()
  ) %>%
  arrange(desc(sum_drug_interaction))


avg_drugs_by_speciality
write.csv(avg_drugs_by_speciality, "/Users/sahabaarabi/Desktop/avg_drugs_per_speciality.csv", row.names = FALSE)

library(dplyr)

monthly_summary <- df_2 %>%
  group_by(pre_month) %>%
  summarise(
    total_prescriptions = n(),
    #prescriptions_four_drugs = sum(drug_count > 4, na.rm = TRUE),
    total_interactions_per_prescription = mean(interaction_count, na.rm = TRUE),
    sd_drugs_per_prescription = sd(interaction_count, na.rm = TRUE),
    #prescriptions_with_interaction = sum(interaction_count > 0, na.rm = TRUE),
    #prescriptions_with_contraindication = sum(Contraindicated > 0, na.rm = TRUE),
    #prescriptions_with_major = sum(Major > 0, na.rm = TRUE),
    #prescriptions_with_moderate = sum(Moderate > 0, na.rm = TRUE)
  ) %>%
  arrange(pre_month)

monthly_summary
write.csv(monthly_summary, "/Users/sahabaarabi/Desktop/avg_drugs_per_speciality.csv", row.names = FALSE)



sex_summary <- df_2 %>%
  group_by(sex) %>%
  summarise(
    total_prescriptions = n(),
   # prescriptions_four_drugs = sum(drug_count > 4, na.rm = TRUE),
    avg_drugs_per_prescription = mean(interaction_count, na.rm = TRUE),
    sd_drugs_per_prescription = sd(interaction_count, na.rm = TRUE),
    #prescriptions_with_interaction = sum(interaction_count > 0, na.rm = TRUE),
    #prescriptions_with_contraindication = sum(Contraindicated > 0, na.rm = TRUE),
    #prescriptions_with_major = sum(Major > 0, na.rm = TRUE),
    #prescriptions_with_moderate = sum(Moderate > 0, na.rm = TRUE)
  ) %>%
  arrange(sex)

write.csv(sex_summary, "/Users/sahabaarabi/Desktop/avg_drugs_per_speciality.csv", row.names = FALSE)



age_summary <- df_2 %>%
  group_by(age_group) %>%
  summarise(
    total_prescriptions = n(),
    #prescriptions_four_drugs = sum(drug_count > 4, na.rm = TRUE),
    avg_drugs_per_prescription = mean(interaction_count, na.rm = TRUE),
    sd_drugs_per_prescription = sd(interaction_count, na.rm = TRUE),
    #prescriptions_with_interaction = sum(interaction_count > 0, na.rm = TRUE),
    #prescriptions_with_contraindication = sum(Contraindicated > 0, na.rm = TRUE),
    #prescriptions_with_major = sum(Major > 0, na.rm = TRUE),
    #prescriptions_with_moderate = sum(Moderate > 0, na.rm = TRUE)
  ) %>%
  arrange(age_group)

write.csv(age_summary, "/Users/sahabaarabi/Desktop/avg_drugs_per_speciality.csv", row.names = FALSE)




library(dplyr)

install.packages('rcompanion')
# Load library
library(rcompanion)

# Age group

# Kruskal-Wallis test
kw_age <- kruskal.test(interaction_count ~ age_group, data = df_2)
kw_age
# Manual epsilon squared calculation
H <- kw_age$statistic
k <- length(unique(df_2$age_group))
n <- nrow(df_2)

epsilon2_age <- (H - k + 1) / (n - k)
print(epsilon2_age)

# Speciality
kw_spec <- kruskal.test(interaction_count ~ speciality, data = df_2)
kw_spec
epsilonSquared(kw_spec)

# Pre-month
kw_month <- kruskal.test(interaction_count ~ pre_month, data = df_2)
kw_month
epsilonSquared(kw_month)


library(effectsize)

# Wilcoxon test
wilcox_sex <- wilcox.test(interaction_count ~ sex, data = df_2)
wilcox_sex
# Effect size
rank_biserial(interaction_count ~ sex, data = df_2)


cor.test(df_2$interaction_count, df_2$drug_count, method = "spearman")


library(ggplot2)

ggplot(df_2, aes(x = age_group, y = interaction_count)) +
  geom_boxplot(fill = "skyblue") +
  theme_minimal() +
  labs(title = "Interaction Count by Age Group", x = "Age Group", y = "Interaction Count")


ggplot(df_2, aes(group = sex, y = interaction_count)) +
  geom_boxplot(fill = "orange") +
  theme_minimal() +
  labs(title = "Interaction Count by Sex", x = "Sex", y = "Interaction Count")



top_specialities <- df_2 %>%
  count(speciality) %>%
  top_n(10, n) %>%
  pull(speciality)

ggplot(df_2 %>% filter(speciality %in% top_specialities),
       aes(x = reorder(speciality, interaction_count, median),
           y = interaction_count)) +
  geom_violin(fill = "lightgreen") +
  coord_flip() +
  theme_minimal() +
  labs(title = "Interaction Count by Speciality", x = "Speciality", y = "Interaction Count")



monthly_avg <- df_2 %>%
  group_by(pre_month) %>%
  summarise(avg_interaction = mean(interaction_count, na.rm = TRUE))

ggplot(monthly_avg, aes(x = as.numeric(pre_month), y = avg_interaction)) +
  geom_line(color = "darkred", size = 1.2) +
  geom_point(color = "red", size = 3) +
  theme_minimal() +
  labs(title = "Average Interaction Count by Month", x = "Prescription Month", y = "Average Interaction Count")

anova_age <- aov(interaction_count ~ speciality, data = df_2)
summary(anova_age)

library(dplyr)

prescriptions_per_patient <- df_2 %>%
  group_by(natinal_id) %>%
  summarise(
    total_prescriptions = n(),
    sex = first(sex),
    age_group = first(age_group),
    speciality = first(speciality),
    pre_month = first(pre_month)
  )

prescription_by_sex <- prescriptions_per_patient %>%
  group_by(sex) %>%
  summarise(
    mean_prescriptions = mean(total_prescriptions, na.rm = TRUE),
    sd_prescriptions = sd(total_prescriptions, na.rm = TRUE),
    n_patients = n()
  )

write.csv(prescription_by_month, "/Users/sahabaarabi/Desktop/prescriptions_by_age_group.csv", row.names = FALSE)

prescription_by_speciality <- prescriptions_per_patient %>%
  group_by(speciality) %>%
  summarise(
    mean_prescriptions = mean(total_prescriptions, na.rm = TRUE),
    sd_prescriptions = sd(total_prescriptions, na.rm = TRUE),
    n_patients = n()
  )

prescription_by_month <- prescriptions_per_patient %>%
  group_by(pre_month) %>%
  summarise(
    mean_prescriptions = mean(total_prescriptions, na.rm = TRUE),
    sd_prescriptions = sd(total_prescriptions, na.rm = TRUE),
    n_patients = n()
  )
































# Count unique prescriptions and patients
unique_prescriptions <- length(unique(df$pre_id))
unique_patients <- length(unique(df$natinal_id))
unique_prescriptions
unique_patients

# Distribution of categorical variables
sex_distribution <- table(df$sex)
sex_distribution
speciality_distribution <- table(df$speciality)
speciality_distribution
pre_month_distribution <- table(df$pre_month)
pre_month_distribution

# Summary of interaction counts
interaction_summary <- df %>%
  summarise(
    Total_Prescriptions = n(),
    Avg_Drug_Count = mean(drug_count, na.rm = TRUE),
    Avg_Interactions_All = mean(interaction_count, na.rm = TRUE),
    Avg_Moderate_Interactions = mean(Moderate, na.rm = TRUE),
    Avg_Major_Interactions = mean(Major, na.rm = TRUE),
    Avg_Contraindicated_Interactions = mean(Contraindicated, na.rm = TRUE)
  )
interaction_summary
# Visualizing age distribution
ggplot(df, aes(x = age_years)) +
  geom_histogram(binwidth = 5, fill = "steelblue", color = "black", alpha = 0.7) +
  labs(title = "Age Distribution", x = "Age (years)", y = "Count")

# Visualizing drug count distribution
ggplot(df, aes(x = drug_count)) +
  geom_histogram(binwidth = 1, fill = "darkred", color = "black", alpha = 0.7) +
  labs(title = "Drug Count per Prescription", x = "Number of Drugs", y = "Count")

# Print key summaries
print(paste("Unique Prescriptions:", unique_prescriptions))
print(paste("Unique Patients:", unique_patients))
print("Sex Distribution:")
print(sex_distribution)
print("Speciality Distribution:")
print(speciality_distribution)
print("Prescription Month Distribution:")
print(pre_month_distribution)
print("Interaction Summary:")
print(interaction_summary)

ggplot(df, aes(x = age_years)) +
  geom_histogram(binwidth = 5, fill = "steelblue", color = "black", alpha = 0.7) +
  labs(title = "Age Distribution", x = "Age (years)", y = "Count") +
  theme_minimal()

# Drug count distribution
ggplot(df, aes(x = drug_count)) +
  geom_histogram(binwidth = 1, fill = "darkred", color = "black", alpha = 0.7) +
  labs(title = "Drug Count per Prescription", x = "Number of Drugs", y = "Count") +
  theme_minimal()

# Interaction distribution
ggplot(df, aes(x = interaction_count)) +
  geom_histogram(binwidth = 5, fill = "green", color = "black", alpha = 0.7) +
  labs(title = "Total Interactions per Prescription", x = "Number of Interactions", y = "Count") +
  theme_minimal()

# Moderate interactions distribution
ggplot(df, aes(x = Moderate)) +
  geom_histogram(binwidth = 1, fill = "orange", color = "black", alpha = 0.7) +
  labs(title = "Moderate Interactions Distribution", x = "Number of Moderate Interactions", y = "Count") +
  theme_minimal()

# Major interactions distribution
ggplot(df, aes(x = Major)) +
  geom_histogram(binwidth = 1, fill = "red", color = "black", alpha = 0.7) +
  labs(title = "Major Interactions Distribution", x = "Number of Major Interactions", y = "Count") +
  theme_minimal()

# Contraindicated interactions distribution
ggplot(df, aes(x = Contraindicated)) +
  geom_histogram(binwidth = 1, fill = "purple", color = "black", alpha = 0.7) +
  labs(title = "Contraindicated Interactions Distribution", x = "Number of Contraindicated Interactions", y = "Count") +
  theme_minimal()

write.csv(interaction_summary, "/Users/sahabaarabi/Desktop/interaction_summary.csv", row.names = FALSE)

#correlation analysis
# Load necessary libraries
library(dplyr)
library(ggplot2)
library(corrplot)
library(ggpubr)


# Step 2: Correlation for numerical variables using Pearson or Spearman
# Choose correlation method based on normality
#if (ks_interaction$p.value > 0.05 & ks_age$p.value > 0.05) {
  # Pearson correlation
  cor_interaction_age <- cor.test(df$interaction_count, df$age_years, method = "pearson")
  cor_interaction_age$estimate
  cor_interaction_drug <- cor(df$interaction_count, df$drug_count)
  cor_interaction_drug
  cat("\nPearson Correlation (Interaction vs Age):", cor_interaction_age, "\n")
  cat("Pearson Correlation (Interaction vs Drug Count):", cor_interaction_drug, "\n")
#} else {
  # Spearman correlation (non-parametric)
  cor_interaction_age <- cor(df$interaction_count_all, df$age_years, method = "spearman")
  cor_interaction_drug <- cor(df$interaction_count_all, df$drug_count, method = "spearman")
  cat("\nSpearman Correlation (Interaction vs Age):", cor_interaction_age, "\n")
  cat("Spearman Correlation (Interaction vs Drug Count):", cor_interaction_drug, "\n")
}

# Independent T-test to check if the mean drug interactions differ by sex
t_test_result <- t.test(interaction_count ~ sex, data = df)
print(t_test_result)

# Interpretation
if(t_test_result$p.value < 0.05) {
  print("There is a significant difference in drug interactions between Male and Female.")
} else {
  print("There is no significant difference in drug interactions between Male and Female.")
}

# ANOVA to test if drug interactions differ across different specialties
anova_specialty <- aov(interaction_count ~ speciality, data = df)
sum_anova_specialty <- summary(anova_specialty)

# Interpretation
if(summary(anova_specialty)[[1]]$`Pr(>F)`[1] < 0.05) {
  print("There is a significant difference in drug interactions across different specialties.")
} else {
  print("There is no significant difference in drug interactions across different specialties.")
}

# ANOVA for 'pre_month' (assuming multiple months in the categorical variable)
anova_pre_month <- aov(interaction_count ~ pre_month, data = df)
sum_anova_pre_month <- summary(anova_pre_month)

# Interpretation
if(summary(anova_pre_month)[[1]]$`Pr(>F)`[1] < 0.05) {
  print("There is a significant difference in drug interactions across different months.")
} else {
  print("There is no significant difference in drug interactions across different months.")
}




# Cohen's d for independent T-test (example with 'sex')
# Assuming data is split by 'sex'
male_data <- df[df$sex == 1,]$interaction_count
female_data <- df[df$sex == 0,]$interaction_count

# Calculate Cohen's d
mean_male <- mean(male_data)
mean_male
mean_female <- mean(female_data)
mean_female
sd_male <- sd(male_data)
sd_female <- sd(female_data)

pooled_sd <- sqrt(((length(male_data) - 1) * sd_male^2 + (length(female_data) - 1) * sd_female^2) / (length(male_data) + length(female_data) - 2))
cohen_d <- (mean_male - mean_female) / pooled_sd
print(cohen_d)


#effect size for specialty
# Extract Sum of Squares
SS_between <- sum((sum_anova_specialty[[1]]$`Sum Sq`[1:length(sum_anova_specialty[[1]]$`Sum Sq`)-1]))
SS_total <- sum(sum_anova_specialty[[1]]$`Sum Sq`)

# Eta-squared calculation
eta_squared <- SS_between / SS_total
print(eta_squared)


#effect size for pre_month
# Extract Sum of Squares
SS_between <- sum((sum_anova_pre_month[[1]]$`Sum Sq`[1:length(sum_anova_pre_month[[1]]$`Sum Sq`)-1]))
SS_total <- sum(sum_anova_pre_month[[1]]$`Sum Sq`)

# Eta-squared calculation
eta_squared <- SS_between / SS_total
print(eta_squared)


# ANOVA to test if drug interactions differ across different age_groups
anova_age_group <- aov(interaction_count ~ age_group, data = df)
sum_anova_age_group <- summary(anova_age_group)
sum_anova_age_group
# Interpretation
if(summary(anova_specialty)[[1]]$`Pr(>F)`[1] < 0.05) {
  print("There is a significant difference in drug interactions across different specialties.")
} else {
  print("There is no significant difference in drug interactions across different specialties.")
}

#effect size for pre_month
# Extract Sum of Squares
SS_between <- sum((sum_anova_age_group[[1]]$`Sum Sq`[1:length(sum_anova_age_group[[1]]$`Sum Sq`)-1]))
SS_total <- sum(sum_anova_age_group[[1]]$`Sum Sq`)

# Eta-squared calculation
eta_squared <- SS_between / SS_total
print(eta_squared)
