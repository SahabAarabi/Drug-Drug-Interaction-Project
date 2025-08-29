#assumptions
#linear relationship between y and x
#scatter plot
#havrvey-collier test: harvtest(model) : p > 0.05 show linearity
#3 for residuals
#1. distribution of residuals be normal
#histogram
#qq plot
#shapiro wilk : shapiro.test(residuals(model))
#2. homoscasticity : the variance of residuals be nearly equavalent (variance for above and under estimation from real variable be equavalent)
#fitted value vs residuals
#breusch-pagan test : bptest(model) : p > 0.05 show homoscasticity
#3. residuals are independent
#fitted value vs residuals
#Durbin Watson test : durbinWatsonTest(model) : close to 2 means independency
#for multiple regression we have to check co-linearity too 
#if we have colinearity we should delete one of them or make a new variable from both of them(sometimes we keep both of them if we assume that a confounding variable is associated them)

#library(MASS)


# Convert 'speciality' to numeric
#df_2$speciality_num <- as.numeric(as.factor(df_2$speciality))
# Convert 'age_group' to numeric
#df_2$age_group_num <- as.numeric(as.factor(df_2$age_group))
#df_numeric <- df_2[, c("sex", "pre_month", "drug_count", "speciality_num", "age_group_num", 'interaction_count')]
#cor(df_numeric, use = "complete.obs")



# Convert 'speciality' to numeric
#df_1$speciality_num <- as.numeric(as.factor(df_1$speciality))
# Convert 'age_group' to numeric
#df_1$age_group_num <- as.numeric(as.factor(df_1$age_group))
#df_numeric <- df_1[, c("sex", "pre_month", "drug_count", "speciality_num", "age_group_num", 'interaction_count')]
#cor(df_numeric, use = "complete.obs")


df_1 <- df_1 %>%
  mutate(
    age_group = factor(age_group, levels = c("0-18", "19-39", "40-64", "65+")),
    sex = factor(sex, levels = c(0, 1), labels = c("Male", "Female")),
    speciality = as.factor(speciality),
    pre_month = as.factor(pre_month)
  )
# Load required libraries
library(readr)
library(dplyr)

df_2 <- read.csv('/Users/sahabaarabi/Desktop/پروژه های بیمه/Drugs Interaction/Regression_total.csv')
df_2 <- df_2 %>%
  mutate(
    age_group = factor(age_group, levels = c("0-18", "19-39", "40-64", "65+")),
    sex = factor(sex, levels = c(0, 1), labels = c("Male", "Female")),
    speciality = relevel(as.factor(speciality), ref = "General practice"),
  )

# List of desired specialties
desired_specialties <- c(
  "General practice", "Internal medicine", "Cardiology", 
  "Emergency medicine", "Psychiatry", "Orthopedics", 
  "Neurology", "Obstetrics and gynecology", "Pediatrics", "Urology", "Other", "General surgery", "Neurosurgery",
  "Infectious and tropical diseases", "Otorhinolaryngology", "Radiation oncology", "Ophthalmology", "Dentistry",
  "PT and rehabilitation", "Dermatology", "Anesthesiology")


# Filter your dataframe
df_filtered <- df_2[df_2$speciality %in% desired_specialties, ]

library(dplyr)
library(ggplot2)

# Summarize counts per specialty
#specialty_counts <- df_2 %>%
  #count(speciality, sort = TRUE)

# Set the factor levels in the order of counts
#specialty_counts$speciality <- factor(specialty_counts$speciality, levels = specialty_counts$speciality)

#summary(specialty_counts$n)

# Plot
#ggplot(specialty_counts, aes(x = speciality, y = n)) +
  #geom_bar(stat = "identity", fill = "steelblue") +
  #labs(title = "Prescription counts per specialty",
      # x = "Specialty",
      # y = "Number of prescriptions") +
  #theme_minimal() +
  #theme(axis.text.x = element_text(angle = 90, vjust = 0.5))



model <- lm(interaction_count ~ age_group + sex + drug_count + speciality, data = df_filtered)
summary(model)
model <- lm(interaction_count ~ drug_count + speciality , data = df_2)
summary(model)
confint(model, conf.level = 0.95)

model <- lm(interaction_count ~  drug_count * speciality + drug_count * age_group + sex, data = df_filtered)
summary(model)



# Residual plots
par(mfrow = c(2, 2))
plot(model)



model <- lm(interaction_count ~ pre_month * speciality , data = df_2)
summary(model)
model <- lm(interaction_count ~ pre_month + speciality , data = df_2)
summary(model)

model <- lm(interaction_count ~ 
              drug_count * age_group + 
              drug_count * speciality + 
              sex,
            data = df_filtered)
summary(model)

model <- lm(interaction_count ~ 
              sex, 
            data = df_2)


library(car)
library(lmtest)
harvtest(model) #linearity
shapiro.test(model$residuals) #normality
bptest(model)#homoscadasticity
dwtest(model) #or
durbinWatsonTest(model) #both of them for independency
vif(model, type = 'predictor') #for multicolinearity

plot(model, which = 1)  # Residuals vs Fitted and other plots 
par(mfrow = c(1, 1))
hist(model$residuals)

#model_pois <- glm(interaction_count ~ age_group + sex + drug_count + speciality + pre_month,
                  #family = poisson(link = "log"), data = df_2)
#summary(model_pois)
# Calculate dispersion statistic
#dispersion <- sum(residuals(model_pois, type = "pearson")^2) / model_pois$df.residual
#dispersion

#library(MASS)
#model_nb <- glm.nb(interaction_count ~ age_group + sex + drug_count + speciality + pre_month,
                   #data = df_2)
#summary(model_nb)

#install.packages('pscl')
#library(pscl)
#model_zip <- zeroinfl(interaction_count ~ age_group + sex + drug_count + speciality + pre_month | 1,
                      #data = df, dist = "poisson")
#summary(model_zip)
# Extract summary
model_summary <- summary(model)

# Create table of estimates
linear_results <- as.data.frame(coef(model_summary))

# Add term names
linear_results$term <- rownames(linear_results)
rownames(linear_results) <- NULL

# Export to CSV
write.csv(linear_results, "/Users/sahabaarabi/Desktop/linear_regression_results_contraindicated.csv", row.names = FALSE)

# Get coefficient estimates
estimates <- coef(model)

# Get 95% confidence intervals
conf_int <- confint(model)

# Combine into a data frame
coef_df <- data.frame(
  term = names(estimates),
  estimate = estimates,
  lower_95 = conf_int[, 1],
  upper_95 = conf_int[, 2]
)
write.csv(coef_df, "/Users/sahabaarabi/Desktop/linear_regression_results_contraindicated_with_ci.csv", row.names = FALSE)
library(ggplot2)

# Order terms by effect size for better layout
#coef_df$term <- factor(coef_df$term, levels = coef_df$term[order(coef_df$estimate)])
# 1. Clean up the term names by adding spaces after prefixes
coef_df_clean <- coef_df %>%
  mutate(
    # Create a new column 'clean_term' for the plot labels
    clean_term = gsub("speciality", "speciality ", term),
    clean_term = gsub("age_group", "age_group ", clean_term),
    clean_term = gsub("sex", "sex ", clean_term)
  )

# Display the cleaned names to check the result
head(coef_df_clean)
plot_data <- coef_df_clean %>%
  filter(term != "(Intercept)")
# Create the plot
my_plot <- ggplot(plot_data, aes(y = estimate, x = reorder(clean_term, estimate))) +
  geom_point(size = 3, color = "darkgreen") +
  geom_errorbar(aes(ymin = lower_95, ymax = upper_95), width = 0.2, color = "forestgreen") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  coord_flip() +
  labs(
    title = "Beta Coefficients with 95% Confidence Intervals",
    x = "Predictor",
    y = "Estimate"
   ) +
    theme_minimal(base_size = 13)+
    theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
)


ggsave("coef_plot_600dpi.png", my_plot, width = 10, height = 6, dpi = 600)

library(broom)
library(dplyr)
library(ggplot2)

# Extract tidy summary with confidence intervals
coef_df <- broom::tidy(model, conf.int = TRUE)

# Filter for relevant coefficients
filtered_coef <- coef_df %>%
  filter(
    term == "sexFemale" |
      #grepl("^pre_month", term) |
      grepl("^drug_count$", term) |
      grepl("^drug_count:speciality", term) |
      grepl("^drug_count:age_group", term)
  )

# Clean term labels for better display
filtered_coef$term <- gsub("drug_count:", "", filtered_coef$term)
filtered_coef$term <- gsub("sex", "", filtered_coef$term)
# 1. Clean up the term names by adding spaces after prefixes
coef_df_clean <- filtered_coef %>%
  mutate(
    # Create a new column 'clean_term' for the plot labels
    clean_term = gsub("speciality", "speciality ", term),
    clean_term = gsub("age_group", "age_group ", clean_term),
    clean_term = gsub("sex", "sex ", clean_term)
  )

# Display the cleaned names to check the result
head(coef_df_clean)
plot_data <- coef_df_clean %>%
  filter(term != "(Intercept)")
# Plot
my_plot <- ggplot(plot_data, aes(x = reorder(clean_term, estimate), y = estimate)) +
  geom_point(size = 3, color = "darkgreen") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2, color = "forestgreen") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  coord_flip() +
  labs(
    title = "Beta Coefficients with 95% Confidence Intervals",
    x = "Predictor",
    y = "Estimate"
  ) +
  theme_minimal(base_size = 13)+
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )

ggsave("coef_plot_600dpi.png", my_plot, width = 10, height = 6, dpi = 600)


summary(model)

#install.packages("boot")  # if not already installed
library(boot)
library(dplyr)
library(purrr)
library(ggplot2)
library(tidyr)

# Define the bootstrapping function
boot_fn <- function(data, indices) {
  d <- data[indices, ]
  model <- lm(interaction_count ~ age_group + sex + drug_count + speciality , data = d)
  return(coef(model))
}

# Run the bootstrap
set.seed(123)
boot_results <- boot(data = df_filtered, statistic = boot_fn, R = 100)

# Extract coefficient names
coef_names <- names(coef(lm(interaction_count ~ age_group + sex + drug_count + speciality , data = df_filtered)))

# Compute percentile CI and bootstrapped p-values
boot_ci_df <- purrr::map_dfr(
  1:length(coef_names),
  function(i) {
    # Confidence Interval
    ci <- boot.ci(boot_results, type = "perc", index = i)
    
    # Two-tailed bootstrapped p-value: fraction of boot samples where coef is on the other side of 0 from the observed coef
    boot_vals <- boot_results$t[, i]
    obs_coef <- boot_results$t0[i]
    p_value <- mean((boot_vals < 0 & obs_coef > 0) | (boot_vals > 0 & obs_coef < 0)) * 2
    
    data.frame(
      term = coef_names[i],
      estimate = obs_coef,
      lower_95 = if (!is.null(ci$percent)) ci$percent[4] else NA,
      upper_95 = if (!is.null(ci$percent)) ci$percent[5] else NA,
      p_value = p_value
    )
  }
)

# Show bootstrapped results with CIs and p-values
print(boot_ci_df)

# --- Plot distributions of bootstrapped coefficients ---

# Convert bootstrapped samples to tidy format
boot_df <- as.data.frame(boot_results$t)
colnames(boot_df) <- coef_names

boot_df_long <- boot_df %>%
  pivot_longer(cols = everything(), names_to = "term", values_to = "value")

# Plot using ggplot
ggplot(boot_df_long, aes(x = value)) +
  geom_histogram(bins = 50, fill = "#69b3a2", color = "white", alpha = 0.8) +
  facet_wrap(~ term, scales = "free", ncol = 3) +
  geom_vline(xintercept = 0, color = "red", linetype = "dashed") +
  theme_minimal() +
  labs(title = "Bootstrapped Coefficient Distributions",
       x = "Bootstrapped Coefficient Value",
       y = "Frequency")

write.csv(boot_ci_df, "/Users/sahabaarabi/Desktop/bootstrapped_results.csv", row.names = FALSE)
ggplot(boot_ci_df, aes(x = reorder(term, estimate), y = estimate)) +
  geom_point(size = 3, color = "darkblue") +
  geom_errorbar(aes(ymin = lower_95, ymax = upper_95), width = 0.2, color = "blue") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "Bootstrapped Coefficient Estimates with 95% CI",
    x = "Term",
    y = "Estimate"
  )



library(ggplot2)

# Example: Visualize interaction between drug_count and age_group
ggplot(df_filtered, aes(x = drug_count, y = interaction_count, color = age_group)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Interaction: drug_count and age_group",
       x = "Drug Count",
       y = "Interaction Count") +
  theme_minimal()

# Example: Visualize interaction between drug_count and speciality
ggplot(df_filtered, aes(x = drug_count, y = interaction_count, color = speciality)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Interaction: drug_count and speciality",
       x = "Drug Count",
       y = "Interaction Count") +
  theme_minimal()

# Your model: linear regression formula
#model_formula <- interaction_count ~ 
 # age_group + 
  #speciality + 
  #pre_month + drug_count
# Custom function for boot()
#boot_fn <- function(data, indices) {
 # boot_data <- data[indices, ]  # resample data
  #fit <- lm(model_formula, data = boot_data)
  #return(coef(fit))  # extract model coefficients
#}
#df_subset <- df_2[, c("interaction_count", "drug_count", "sex", "age_group", "speciality", "pre_month")]
#set.seed(123)  # for reproducibility
#results <- boot(data = df_sample, statistic = boot_fn, R = 1000)  # 1000 resamples
#print(results)  # view standard errors and bias
# 95% confidence interval for each coefficient
#for (i in 1:length(results$t0)) {
#  print(boot.ci(results, type = "perc", index = i))
#}
#set.seed(123)
#df_sample <- df_2[sample(nrow(df_2), 10000), ]  # Take a sample of 10,000 rows


#library(boot)

# Define the statistic function for bootstrapping
#boot_fn <- function(data, indices) {
 # d <- data[indices, ]
  #model <- tryCatch(
   # lm(interaction_count ~ age_group + sex + drug_count + speciality + pre_month, data = d),

 # )
  
  #if (inherits(model, "lm")) {
   # return(coef(model))
  #} else {
   # return(rep(NA, length(coef(model_simple))))
#  }
#}

#boot_fn <- function(data, indices) {
  # Resample the data
#  d <- data[indices, ]
  
  # Ensure factors keep original levels
#  d$age_group <- factor(d$age_group, levels = levels(df_sample$age_group))
 # d$sex <- factor(d$sex, levels = levels(df_sample$sex))
  #d$speciality <- factor(d$speciality, levels = levels(df_sample$speciality))
  #d$pre_month <- factor(d$pre_month, levels = levels(df_sample$pre_month))
  
  # Fit model with try-catch to handle failed fits
  #model <- tryCatch(lm(interaction_count ~ age_group + sex + drug_count + speciality + pre_month, data = d), 
  #error = function(e) return(rep(NA, length(coef(model_simple)))))
  # handle error
  #if (inherits(model, "lm")) {
   # return(coef(model))
  #} else {
   # return(rep(NA, length(coef(model_simple))))
  #}
#}

# Sample 10,000 rows for faster bootstrapping
#set.seed(123)
#df_sample <- df_2[sample(nrow(df_2), 10000), ]


# 1. Fit reference model once to get the expected coefficient length
#model_simple <- lm(interaction_count ~ age_group + sex + drug_count + speciality + pre_month, data = df_sample)


# Run bootstrap with 1000 resamples
#set.seed(123)
#results <- boot(data = df_sample, statistic = boot_fn, R = 1000)






#logistic regression
df_2$interaction_binary <- ifelse(df_2$interaction_count > 0, 1, 0)
df_filtered$contraindicated_binary <- ifelse(df_filtered$Contraindicated > 0, 1, 0)
model_l <- glm(interaction_count ~ drug_count , data = df_2, family = binomial)
summary(model_l)



model_logit_contra <- glm(contraindicated_binary ~ drug_count  +
                            age_group + 
                            speciality + 
                            sex,
                          data = df_filtered, family = binomial)

model_logit_contra <- glm(contraindicated_binary ~ drug_count * speciality + 
                            drug_count * age_group,
                          data = df_2, family = binomial)

model_logit <- glm(interaction_binary ~ 
                     drug_count  +
                     age_group + 
                     speciality + 
                     pre_month + 
                     sex,
                   data = df_2, family = binomial)

model_logit_interaction <- glm(contraindicated_binary ~ 
                     drug_count * sex +
                     drug_count * age_group + 
                     drug_count * speciality + 
                     drug_count * pre_month,
                   data = df_2, family = binomial)
summary(model_logit_contra)

# Extract summary
logit_summary <- summary(model_logit_contra)

# Create table
logit_results <- as.data.frame(coef(logit_summary))

# Add term names
logit_results$term <- rownames(logit_results)
rownames(logit_results) <- NULL

# Export to CSV
write.csv(logit_results, "/Users/sahabaarabi/Desktop/logistic_regression_results.csv", row.names = FALSE)

vif(model_logit_contra, type = 'predictor') #for multicolinearity

par(mfrow = c(2, 2))
plot(model_logit_contra)  # Influential observations (Cook’s distance)

install.packages("car")  # if not already installed
library(car)
df_filtered$drug_count_log <- df_filtered$drug_count * log(df_filtered$drug_count + 1)  # Add 1 to avoid log(0)
box_tidwell_model <- glm(contraindicated_binary ~ drug_count + drug_count_log + age_group + sex + speciality,
                         data = df_filtered, family = binomial)
summary(box_tidwell_model)

install.packages('ResourceSelection')
library(ResourceSelection)
# Linearity assumption for continuous predictor (drug_count)
df_filtered$log_drug_count <- log(df_filtered$drug_count + 1)
model_test <- glm(contraindicated_binary ~ log_drug_count, data = df_filtered, family = binomial)
plot(df_filtered$log_drug_count, model_test$fitted.values)
library(ggplot2)
ggplot(df_filtered, aes(x = log_drug_count, y = model_test$fitted.values)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "loess") +
  labs(x = "Log Drug Count", y = "Predicted Probability of Interaction",
       title = "Effect of Drug Count on Predicted Probability of Interaction")

library(ResourceSelection)#goodness of fit
hoslem.test(df_filtered$contraindicated_binary, fitted(model_logit_contra))

install.packages('pROC')
library(pROC)
roc_obj <- roc(df_2$contraindicated_binary, fitted(model_logit_contra))
plot(roc_obj, print.auc = TRUE)


library(pROC)
library(ggplot2)

# Compute ROC object
roc_obj <- roc(df_2$contraindicated_binary, fitted(model_logit_contra))

# Create a data frame from the ROC object
roc_df <- data.frame(
  FPR = 1 - roc_obj$specificities,
  TPR = roc_obj$sensitivities
)

# Extract AUC
auc_value <- auc(roc_obj)

# Plot with ggplot2
ggplot(roc_df, aes(x = FPR, y = TPR)) +
  geom_line(color = "#2C3E50", size = 1.5) +
  geom_abline(linetype = "dashed", color = "gray") +
  annotate("text", x = 0.65, y = 0.1,
           label = paste("AUC =", round(auc_value, 3)),
           size = 5, color = "#34495E") +
  labs(
    title = "ROC Curve",
    x = "1 - Specificity (False Positive Rate)",
    y = "Sensitivity (True Positive Rate)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title = element_text(face = "bold"),
    panel.grid = element_line(color = "gray90")
  )


plot(roc_obj,
     col = "#2E86C1",
     lwd = 3,
     print.auc = TRUE,
     print.auc.col = "black",
     print.auc.cex = 1.2,
     main = "ROC Curve with AUC")

abline(a = 0, b = 1, lty = 2, col = "gray")  # Diagonal line


# Odds Ratios
odds_ratios <- exp(coef(model_logit_contra))
#confint(model_logit, conf.level = 0.95)
x <- confint.default(model_logit_contra)
summary(model_logit_contra)
# 95% Confidence Intervals
conf_int <- exp(x)
# Combine into one table
odds_table <- cbind(Odds_Ratio = odds_ratios, `2.5%` = conf_int[,1], `97.5%` = conf_int[,2])
print(odds_table)

# Convert to data frame and add term names
odds_df <- as.data.frame(odds_table)
odds_df$term <- rownames(odds_df)
rownames(odds_df) <- NULL
write.csv(odds_df, "/Users/sahabaarabi/Desktop/logistic_odds_ratios_contraindicated_interaction.csv", row.names = FALSE)

library(ggplot2)

# Ensure 'term' is a factor in desired order
odds_df$term <- factor(odds_df$term, levels = odds_df$term[order(odds_df$Odds_Ratio)])
odds_df
# Plot
ggplot(odds_df, aes(x = term, y = Odds_Ratio)) +
  geom_point(size = 3, color = "darkblue") +
  geom_errorbar(aes(ymin = `2.5%`, ymax = `97.5%`), width = 0.2, color = "steelblue") +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") +  # Correct for odds ratios
  coord_flip() +
  labs(
    title = "Odds Ratios with 95% Confidence Intervals",
    x = "Predictor",
    y = "Odds Ratio"
  ) +
  theme_minimal(base_size = 13)

# 1. Clean up the term names by adding spaces after prefixes
coef_df_clean <- odds_df %>%
  mutate(
    # Create a new column 'clean_term' for the plot labels
    clean_term = gsub("speciality", "speciality ", term),
    clean_term = gsub("age_group", "age_group ", clean_term),
    clean_term = gsub("sex", "sex ", clean_term)
  )
coef_df_clean
# Display the cleaned names to check the result
head(coef_df_clean)
plot_data <- coef_df_clean %>%
  filter(term != "(Intercept)")
my_plot <- ggplot(plot_data, aes(x = reorder(clean_term, Odds_Ratio), y = Odds_Ratio)) +
  geom_point(size = 3, color = "darkgreen") +
  geom_errorbar(aes(ymin = `2.5%`, ymax = `97.5%`), width = 0.2, color = "forestgreen") +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
  coord_flip(ylim = c(0, 5)) +  # You can adjust this to zoom in
  labs(
    title = "Odds Ratios with 95% Confidence Intervals",
    x = "Predictor",
    y = "Odds Ratio"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )

ggsave("coef_plot_600dpi.png", my_plot, width = 10, height = 6, dpi = 600)





library(boot)

# Define bootstrap function for logistic regression
boot_fn_logit <- function(data, indices) {
  d <- data[indices, ]
  model <- glm(contraindicated_binary ~ 
                 drug_count +
                 age_group + 
                 speciality + 
                 sex,
               data = d, family = binomial)
  
  # Return named coefficients (will pad later if needed)
  coefs <- coef(model)
  
  # Create full list of terms to ensure equal length
  #all_terms <- names(coef(glm(interaction_binary ~ 
                                #drug_count +
                               # age_group + 
                               # speciality + 
                               # pre_month + 
                               # sex,
                              #data = data, family = binomial)))
  
  # Fill missing terms with NA (if dropped due to collinearity)
  #full_coefs <- setNames(rep(NA, length(all_terms)), all_terms)
  #full_coefs[names(coefs)] <- coefs
  
  return(coefs)
}
library(pbapply)

# Define number of bootstrap resamples
R <- 100

# Function for one bootstrap iteration
one_boot <- function(i) {
  indices <- sample(1:nrow(df_filtered), replace = TRUE)
  boot_fn_logit(df_filtered, indices)
}

# Apply with progress bar
boot_coefs <- pblapply(1:R, one_boot)

# Convert results to a data frame
boot_results_logit <- do.call(rbind, boot_coefs)

set.seed(123)
#boot_results_logit <- boot(data = df_2, statistic = boot_fn_logit, R = 100, simple = TRUE)
str(boot_results_logit)
boot.ci(boot_results_logit, type = "perc", index = 1)

# Extract coefficient names
coef_names <- names(coef(model_logit_contra))
coef_names
# Compute percentile CI and bootstrapped p-values
library(purrr)

boot_ci_logit <- map_dfr(
  1:length(coef_names),
  function(i) {
    # Compute confidence interval
    ci <- boot.ci(boot_results_logit, type = "perc", index = i)
    
    # Bootstrapped coefficient estimates
    boot_vals <- boot_results_logit$t[, i]
    
    # Observed coefficient
    obs_coef <- boot_results_logit$t0[i]
    
    # Two-tailed bootstrapped p-value
    p_value <- mean((boot_vals < 0 & obs_coef > 0) | (boot_vals > 0 & obs_coef < 0)) * 2
    
    data.frame(
      term = coef_names[i],
      estimate = obs_coef,
      lower_95 = if (!is.null(ci$percent)) ci$percent[4] else NA,
      upper_95 = if (!is.null(ci$percent)) ci$percent[5] else NA,
      p_value = p_value
    )
  }
)


# Show bootstrapped results with CIs and p-values
print(boot_ci_logit)

# --- Plot distributions of bootstrapped coefficients ---

# Convert bootstrapped samples to tidy format
boot_df <- as.data.frame(boot_results_logit$t)
colnames(boot_df) <- coef_names

boot_df_long <- boot_df %>%
  pivot_longer(cols = everything(), names_to = "term", values_to = "value")

# Plot using ggplot
ggplot(boot_df_long, aes(x = value)) +
  geom_histogram(bins = 50, fill = "#69b3a2", color = "white", alpha = 0.8) +
  facet_wrap(~ term, scales = "free", ncol = 3) +
  geom_vline(xintercept = 0, color = "red", linetype = "dashed") +
  theme_minimal() +
  labs(title = "Bootstrapped Coefficient Distributions",
       x = "Bootstrapped Coefficient Value",
       y = "Frequency")

write.csv(boot_ci_logit, "/Users/sahabaarabi/Desktop/bootstrapped_results_logistic_regression.csv", row.names = FALSE)
ggplot(boot_ci_logit, aes(x = reorder(term, estimate), y = estimate)) +
  geom_point(size = 3, color = "darkblue") +
  geom_errorbar(aes(ymin = lower_95, ymax = upper_95), width = 0.2, color = "blue") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "Bootstrapped Coefficient Estimates with 95% CI",
    x = "Term",
    y = "Estimate"
  )



# Load libraries
library(ggplot2)
library(dplyr)
# Convert log-odds to odds ratios
logit_df <- read.csv('/Users/sahabaarabi/Desktop/All results/Final results - Logistic model/bootstrapped_results_logistic_regression.csv')
logit_df <- logit_df %>%
  mutate(
    odds_ratio = exp(estimate),
    lower_ci = exp(lower_95),
    upper_ci = exp(upper_95)
  )  %>%
  # Reorder terms by odds ratio (descending)
  arrange(odds_ratio) %>%
  mutate(term = factor(term, levels = term))

# Plot
ggplot(logit_df, aes(x = term, y = odds_ratio)) +
  geom_point(color = "darkblue", size = 4) +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), color = "blue", width = 0.2, size = 1) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
  coord_flip() +
  labs(
    title = "Odds Ratios with 95% Confidence Intervals",
    x = "Predictor",
    y = "Odds Ratio"
  ) +
  theme_minimal(base_size = 14)








