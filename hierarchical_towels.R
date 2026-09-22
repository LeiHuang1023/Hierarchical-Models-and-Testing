# Exercise: Hierarchical models and testing
# Run the numbered sections in order in RStudio.
# Input: the teacher's towelData.csv (semicolon-separated).
# The actual CSV has not been inspected: check settings in Section 2.
# This script has not been executed against your data.
#
# Model:
# Y_jg ~ Binomial(n_jg, p_jg)
# logit(p_jg) = alpha + u_j + beta * treatment_g
# u_j ~ Normal(0, tau^2)
# Priors: alpha ~ Normal(0, 1.5^2), beta ~ Normal(0, 1^2),
#         tau ~ Exponential(rate = 1).
# H0: beta <= 0; H1: beta > 0.
# The model allows baseline differences between studies and assumes
# a common intervention log odds ratio. It does not include random slopes.
# Counts are assumed independent conditional on the study effects.
# Repeated observations on the same guest/room could violate this assumption.
# Background: Scheibehenne, Jamil & Wagenmakers (2016),
# doi:10.1177/0956797616644081. This is not an exact reproduction of their model.

# 1. Packages -------------------------------------------------------------
# Run this installation line ONCE, then leave it commented out.
# install.packages(c("brms", "rstan", "posterior", "bayesplot",
#                    "dplyr", "tidyr", "ggplot2", "pkgbuild"))
# On Windows, install Rtools matching your R version if compilation fails.
# Check the toolchain with: pkgbuild::has_build_tools(debug = TRUE)
install.packages(c(
  "brms", "rstan", "posterior", "bayesplot",
  "dplyr", "tidyr", "ggplot2", "pkgbuild"
))

install.packages("brms")
install.packages("dplyr")
install.packages("tidyr")
install.packages("ggplot2")
install.packages("posterior")

library(brms)
library(dplyr)
library(tidyr)
library(ggplot2)
library(posterior)

set.seed(1234)
dir.create("results", showWarnings = FALSE)

# 2. Read data and check names ---------------------------------------------
# Put towelData.csv in the working directory (check it with getwd()).
raw <- read.csv("towelData.csv")
print(names(raw))
print(head(raw, 8))

# EDIT these settings to match the printed column names and group labels.
# Study must identify each experiment, not just the publication.
study_columns <- c("Source")
group_column <- "Group"
reuse_column <- "Towel.Reuse"
count_column <- "Count"

control_label <- "Control"
social_label <- "Social Norm"

required <- c(study_columns, group_column, reuse_column, count_column)
if (!all(required %in% names(raw))) {
  stop("Check the column settings above. Available columns: ",
       paste(names(raw), collapse = ", "))
}
print(unique(raw[[group_column]]))
print(unique(raw[[reuse_column]]))

# Ignore case, spaces and punctuation when matching group names.
clean_label <- function(x) gsub("[^a-z0-9]", "", tolower(trimws(x)))
group_names <- clean_label(raw[[group_column]])
control_name <- clean_label(control_label)
social_name <- clean_label(social_label)
if (control_name == social_name || anyNA(group_names) ||
    !setequal(unique(group_names), c(control_name, social_name))) {
  stop("Set control_label and social_label to the exact groups in your CSV.")
}

if (anyNA(raw[study_columns]) ||
    any(vapply(raw[study_columns], function(x) any(trimws(x) == ""), logical(1)))) {
  stop("Study identifiers contain missing or blank values.")
}
study_id <- do.call(paste, c(raw[study_columns], sep = " / "))
data_long <- data.frame(
  study = study_id,
  treatment = as.integer(group_names == social_name),
  reuse = clean_label(raw[[reuse_column]]),
  count = as.numeric(as.character(raw[[count_column]]))
)

if (anyNA(data_long) || !setequal(data_long$reuse, c("yes", "no")) ||
    any(!is.finite(data_long$count)) || any(data_long$count < 0) ||
    any(data_long$count != floor(data_long$count))) {
  stop("Check counts and Yes/No labels: counts must be non-negative integers.")
}
if (anyDuplicated(data_long[c("study", "treatment", "reuse")])) {
  stop("Duplicate study/group/outcome rows. Check experiment identifiers;
       do not combine different experiments under one study ID.")
}

# Put the Yes and No counts for each study arm on one row.
towel_data <- data_long %>%
  pivot_wider(names_from = reuse, values_from = count) %>%
  rename(Yes = yes, No = no) %>%
  mutate(Total = Yes + No, study = factor(study),
         observed_rate = Yes / Total) %>%
  arrange(study, treatment)

stopifnot(!anyNA(towel_data), all(towel_data$Total > 0),
          nlevels(towel_data$study) >= 2,
          all(table(towel_data$study, towel_data$treatment) == 1))
print(towel_data)
cat("Number of studies:", nlevels(towel_data$study), "\n")
write.csv(towel_data, "results/prepared_data.csv", row.names = FALSE)

# 3. Describe the observed proportions -------------------------------------
p_observed <- ggplot(towel_data,
                     aes(x = study, y = observed_rate,
                         colour = factor(treatment), group = treatment)) +
  geom_point(size = 3, position = position_dodge(width = 0.4)) +
  scale_colour_manual(values = c("0" = "#2878B5", "1" = "#DB7033"),
                      breaks = c("0", "1"), labels = c("Control", "Social norm")) +
  coord_flip() + ylim(0, 1) + theme_minimal() +
  labs(x = "Study", y = "Observed reuse proportion", colour = "Group")
print(p_observed)
ggsave("results/observed_rates.png", p_observed, width = 8, height = 5, dpi = 200)

# 4. Specify and fit the Bayesian model ------------------------------------
# trials(Total) gives the number of binomial trials.
# (1 | study) adds a random intercept for each study.
# 0 + Intercept gives an explicit intercept so that its prior refers
# to the control baseline, without brms's default intercept centering.
model_formula <- bf(Yes | trials(Total) ~ 0 + Intercept + treatment + (1 | study))

# Normal priors allow both positive and negative effects.
# The beta prior gives about 95% prior probability to odds ratios 0.14-7.1.
# Exponential(1) is a positive prior for the between-study SD.
priors <- c(
  set_prior("normal(0, 1.5)", class = "b", coef = "Intercept"),
  set_prior("normal(0, 1)", class = "b", coef = "treatment"),
  set_prior("exponential(1)", class = "sd", group = "study")
)

fit <- brm(
  formula = model_formula,
  data = towel_data,
  family = binomial(link = "logit"),
  prior = priors,
  chains = 4, iter = 4000, warmup = 2000, cores = 2,
  seed = 1234, backend = "rstan",
  control = list(adapt_delta = 0.99, max_treedepth = 12)
)
saveRDS(fit, "results/towel_model.rds")
# In a later session, load the fit instead of running brm again:
# fit <- readRDS("results/towel_model.rds")

# 5. Check MCMC reliability ------------------------------------------------
print(summary(fit, prob = 0.90))
draws_array <- as_draws_array(fit)
diagnostics <- summarise_draws(draws_array)
print(diagnostics)
write.csv(as.data.frame(diagnostics), "results/mcmc_diagnostics.csv", row.names = FALSE)

pars <- c("b_Intercept", "b_treatment", "sd_study__Intercept")
p_trace <- bayesplot::mcmc_trace(as.array(fit), pars = pars)
print(p_trace)
ggsave("results/trace_plots.png", p_trace, width = 9, height = 6, dpi = 200)

sampler <- rstan::get_sampler_params(fit$fit, inc_warmup = FALSE)
n_divergent <- sum(vapply(sampler, function(x) sum(x[, "divergent__"]), numeric(1)))
n_depth <- sum(vapply(sampler, function(x) sum(x[, "treedepth__"] >= 12), numeric(1)))
cat("Divergent transitions:", n_divergent, "\n")
cat("Maximum treedepth hits:", n_depth, "\n")

# Screening guidelines, not proof of convergence:
# Rhat < 1.01, bulk/tail ESS > 400, no divergences, and well-mixed traces.
if (any(!is.finite(diagnostics$rhat)) || any(diagnostics$rhat >= 1.01, na.rm = TRUE) ||
    any(diagnostics$ess_bulk < 400, na.rm = TRUE) ||
    any(diagnostics$ess_tail < 400, na.rm = TRUE) || n_divergent > 0 || n_depth > 0) {
  warning("MCMC diagnostics need attention. Do not finalize conclusions yet.")
}
capture.output(summary(fit, prob = 0.90),
               file = "results/model_summary.txt")
writeLines(c(paste("Divergent transitions:", n_divergent),
             paste("Maximum treedepth hits:", n_depth)),
           "results/sampler_diagnostics.txt")

# 6. Posterior means and 90% equal-tailed credible intervals ----------------
draws <- as_draws_df(fit)
posterior_values <- data.frame(
  alpha = draws$b_Intercept,
  beta = draws$b_treatment,
  tau = draws$sd_study__Intercept,
  odds_ratio = exp(draws$b_treatment)
)
summarise_one <- function(x) {
  c(mean = mean(x), lower_90 = unname(quantile(x, 0.05)),
    upper_90 = unname(quantile(x, 0.95)))
}
results <- as.data.frame(t(vapply(posterior_values, summarise_one, numeric(3))))
print(round(results, 3))
write.csv(results, "results/posterior_summary.csv")

# Also report the study-specific random intercept deviations.
random_names <- grep("^r_study\\[", names(draws), value = TRUE)
study_results <- as.data.frame(t(vapply(as.data.frame(draws)[random_names],
                                       summarise_one, numeric(3))))
print(round(study_results, 3))
write.csv(study_results, "results/study_effects.csv")

# 7. Bayesian directional hypothesis assessment ----------------------------
# These are posterior probabilities, not frequentist p-values or Bayes factors.
test_results <- data.frame(
  hypothesis = c("H0: beta <= 0", "H1: beta > 0"),
  posterior_probability = c(mean(draws$b_treatment <= 0),
                            mean(draws$b_treatment > 0))
)
print(test_results, digits = 4)
write.csv(test_results, "results/hypothesis_results.csv", row.names = FALSE)
# A proportion of 0 or 1 in a finite MCMC sample is not absolute certainty.

p_beta <- ggplot(data.frame(beta = draws$b_treatment), aes(x = beta)) +
  geom_density(fill = "#2878B5", alpha = 0.35) +
  geom_vline(xintercept = 0, linetype = "dashed") + theme_minimal() +
  labs(x = "Intervention effect (log odds ratio)", y = "Posterior density")
print(p_beta)
ggsave("results/intervention_posterior.png", p_beta, width = 7, height = 4, dpi = 200)

# 8. Posterior predictive check for each observed study arm -----------------
# Simulate new counts with the same group sizes and fitted study effects.
# These intervals describe replicated observations, not parameter intervals.
set.seed(1234)
y_rep <- posterior_predict(fit, ndraws = 1000)
rate_rep <- sweep(y_rep, 2, towel_data$Total, "/")
ppc_data <- towel_data
ppc_data$lower <- apply(rate_rep, 2, quantile, probs = 0.05)
ppc_data$upper <- apply(rate_rep, 2, quantile, probs = 0.95)
ppc_data$replicated_mean <- colMeans(rate_rep)

p_ppc <- ggplot(ppc_data, aes(x = study)) +
  geom_linerange(aes(ymin = lower, ymax = upper), colour = "#2878B5") +
  geom_point(aes(y = replicated_mean), colour = "#2878B5", shape = 1, size = 2.5) +
  geom_point(aes(y = observed_rate), colour = "black", size = 2) +
  facet_wrap(~ treatment, labeller = as_labeller(c("0" = "Control", "1" = "Social norm"))) +
  coord_flip() + ylim(0, 1) + theme_minimal() +
  labs(x = "Study", y = "Reuse proportion",
       caption = "Black: observed; blue: replicated mean and 90% predictive interval")
print(p_ppc)
ggsave("results/posterior_predictive_check.png", p_ppc, width = 10, height = 5, dpi = 200)
write.csv(ppc_data, "results/posterior_predictive_check.csv", row.names = FALSE)

# 9. Reproducibility and report notes ---------------------------------------
capture.output(sessionInfo(), file = "results/session_info.txt")
# Report: background; model and assumptions; priors; MCMC settings and
# diagnostics; posterior summaries; hypotheses and posterior probabilities;
# predictive check; limitations (common effect, prior choices, and the
# conditional independence assumption). Include name, date and collaborator.
# A random-slope extension is needed if intervention effects differ by study.
# A prior-sensitivity analysis is useful if conclusions depend on prior choices.
# Results must be interpreted after running the model; no conclusion is preset.
cat("Outputs saved in:", normalizePath("results"), "\n")

library(rstan)

# Show package versions
sapply(
  c("rstan", "StanHeaders", "Rcpp", "RcppEigen", "BH"),
  function(x) as.character(packageVersion(x))
)

# Check custom compiler settings
Sys.getenv(c(
  "PKG_CPPFLAGS", "PKG_CXXFLAGS",
  "CXXFLAGS", "CXX17FLAGS", "R_MAKEVARS_USER"
))

# Show longer error messages
options(warning.length = 8170)

# Compile a minimal Stan model; no sampling is performed
test_model <- rstan::stan_model(
  model_code = "
    parameters {
      real mu;
    }
    model {
      mu ~ normal(0, 1);
    }
  ",
  verbose = TRUE
)

