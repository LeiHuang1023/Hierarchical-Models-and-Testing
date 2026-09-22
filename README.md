# Exercise: Hierarchical Models and Testing

**Author:** [Your name]  
**Date:** [Submission date]  
**Collaborator:** [Name, or None]

> Working draft. The Stan model has not yet compiled successfully in the current
> project. Posterior results, diagnostics and conclusions are pending. This
> document is not ready for submission.

## 1. Objective

This exercise examines whether descriptive social norm messages increase towel
reuse compared with control messages. Data from several experiments are analysed
together using a Bayesian hierarchical model. The model accounts for differences
in baseline towel reuse between experiments.

## 2. Data

The input file is `towelData.csv`. The supplied data screenshot contains seven
experiments and 28 rows. Each row gives a count for one experiment, one message
group and one towel reuse outcome. The file uses semicolons as separators.

| Variable | Description |
| --- | --- |
| `Source` | Unique experiment identifier |
| `AuthorName` | Authors of the original study |
| `Experiment` | Experiment number within a publication |
| `Year` | Publication year |
| `Group` | Control or Social Norm |
| `Towel.Reuse` | Yes or No |
| `Count` | Count for the corresponding group and outcome |

`Source` is used as the grouping variable because one publication may contain
more than one experiment. Using only the author name would combine distinct
experiments.

The following code reads the original file and displays its structure:

```r
raw <- read.csv2(
  "towelData.csv",
  fileEncoding = "latin1",
  stringsAsFactors = FALSE
)

names(raw)
head(raw, 8)
```

The script's column settings must use `study_columns <- c("Source")` and
`group_column <- "Group"`.

## 3. Data preparation

To be completed after reviewing the preparation code and checking its output.

## 4. Model and prior distributions

To be completed after reviewing the binomial model, study random intercepts and
prior choices. The planned model assumes a common intervention log odds ratio.

## 5. Estimation and reliability checks

To be completed after successful compilation and sampling. Report MCMC settings,
Rhat, effective sample sizes, divergences, trace plots and a posterior predictive
check. Installation of packages alone does not confirm that the model runs.

## 6. Posterior results and hypothesis assessment

Pending. Include posterior means, 90% credible intervals and the posterior
probabilities of the stated directional hypotheses. Include rendered tables and
figures so readers can inspect the results without running the code.

## 7. Discussion and limitations

Pending. Interpret effect size and uncertainty, and discuss the common-effect
assumption, prior choices and conditional independence of observations.

## 8. Reproducibility

The analysis script is `hierarchical_towels.R`. The current project uses R 4.3.3
on Windows with an `renv` project library. The script uses `brms`, `dplyr`,
`tidyr`, `ggplot2`, and `posterior`; it also calls `rstan` and `bayesplot`.

The data and script should be placed in the project directory. Outputs are
written to `results/`. Final execution instructions, tested package versions and
the `renv.lock` file will be added after the analysis runs successfully.

## References

Scheibehenne, B., Jamil, T., & Wagenmakers, E.-J. (2016). Bayesian Evidence
Synthesis Can Reconcile Seemingly Inconsistent Results: The Case of Hotel Towel
Reuse. *Psychological Science, 27*(7), 1043-1046.
https://doi.org/10.1177/0956797616644081

Sahlin, U. *Lecture Hierarchical Models and Testing*. BERN02 course material.
