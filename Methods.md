## Methodology
This project applies five causal inference strategies to the same policy shock. Using multiple identification strategies on the same data is a robustness check: if estimates converge across methods with different assumptions, the finding is more credible.
The policy shock is the 2023 CAP reform, which introduced an "active farmer" eligibility criterion. Farms that lost CAP eligibility form the treatment group, farms that retained it form the control group.

## Summary of methods 
1. **Logistic Model** — Baseline prediction of exit probability using average marginal effects under the Conditional Independence Assumption.
2. **Difference-in-Differences (DiD)** — Two-period panel comparing treated vs. control farms on standardised gross output.
3. **Fuzzy Regression Discontinuity Design (RDD)** — Uses the age-67 cutoff as an instrument in a 2SLS framework.
4. **Difference-in-Discontinuities (Fuzzy)** — Combines DiD and RDD to difference out pre-existing discontinuities at the age cutoff, addressing covariate imbalance
5. **Double/Debiased Machine Learning (DML)** — IRM (ATE) and PLIV (LATE) using Random Forest nuisance learners with 5-fold cross-fitting. Anderson–Rubin weak-IV test confirms instrument strength.

---

## 1. Logistic regression

**Script:** `Logit and RDD/PiA_Logit_and_RDD.R`

A logistic regression model predicts the probability of farm exit as a function of CAP eligibility loss and a set of controls (farm size, output, age, legal status, farming type, region).

Average marginal effects (AMEs) are computed across multiple model specifications to quantify the change in exit probability associated with losing eligibility. This is the most straightforward approach but relies on the **Conditional Independence Assumption** that is, conditional on observables, treatment assignment is as good as random. This assumption is unlikely to hold perfectly here, which motivates the more credible designs below.

---

## 2. Difference-in-Differences (DiD)

**Scripts:** `Difference-in-Differences/DiD.R`, `Difference-in-Differences/Regional_DiD.R`

The DiD design exploits the two-period panel structure (2020 and 2023) to compare the evolution of gross standard output for treated farms (those that lost eligibility) against control farms (those that retained it).

The key identifying assumption is **parallel trends**: in the absence of the reform, treated and control farms would have followed the same trajectory in output. Farm fixed effects absorb time-invariant unobservables; year fixed effects absorb common shocks. Multiple specifications are estimated, and confidence interval plots are produced for each. The regional extension (`Regional_DiD.R`) replicates the analysis separately by region to assess heterogeneity.

---

## 3. Fuzzy Regression Discontinuity Design (RDD)

**Script:** `Logit and RDD/PiA_Logit_and_RDD.R`  
**Validation:** `Logit and RDD/Validation and Balance Checks.R`

The RDD exploits the **age-67 cutoff** in the CAP eligibility rules. Farmers above the cutoff face a discontinuously higher probability of losing eligibility, which creates a natural instrument.

Because the cutoff does not deterministically assign treatment (compliance is imperfect), this is a **fuzzy RDD**, estimated using two-stage least squares (2SLS): the age-67 threshold instruments for actual eligibility loss. Bandwidth selection follows the MSE-optimal procedure. The design estimates a **Local Average Treatment Effect (LATE)** — the effect for farmers whose eligibility status was changed by being above the cutoff.

Before relying on the RDD, `Validation and Balance Checks.R` runs:
- A **McCrary density test** on the running variable (age) to assess the no-manipulation assumption
- **Covariate balance tests** at the cutoff
- **Bandwidth sensitivity analysis** to verify that estimates are stable across bandwidth choices

---

## 4. Difference-in-Discontinuities

**Script:** `Difference-in-Discontinuities/PiA_Diff_in_Disc.R`

The Difference-in-Discontinuities (Diff-in-Disc) design combines the RDD and DiD strategies. It differences out any **pre-existing discontinuity** at the age-67 cutoff that existed before the 2023 reform — for instance, if farmers near 67 were already systematically different in output regardless of the reform.

By comparing the discontinuity at the cutoff in 2023 against the discontinuity at the same cutoff in 2020 (pre-reform), this design isolates the causal effect of the reform itself rather than any age-related discontinuity that predates it. This addresses a key threat to the RDD's validity: covariate imbalance at the cutoff. Gross standard output is the dependent variable. Results are presented across model specifications and by region.

---

## 5. Double/Debiased Machine Learning (DML)

**Script:** `Double Machine Learning/PiA_Double_ML.R`

DML uses machine learning to flexibly partial out the influence of high-dimensional controls, reducing omitted variable bias without imposing parametric assumptions on the nuisance functions.

Two estimators are implemented:

**Interactive Regression Model (IRM) — Average Treatment Effect (ATE)**  
Estimates the average effect of eligibility loss across the full population of farms, using Random Forest learners for the outcome and treatment propensity nuisance functions. 5-fold cross-fitting is used to avoid overfitting bias.

**Partially Linear IV (PLIV) — Local Average Treatment Effect (LATE)**  
Uses the age-67 indicator (`OVER_67`) as an instrument within the DML framework to estimate the LATE. This combines the instrument's identifying variation with the flexibility of machine learning nuisance estimation.

**Instrument validity** is confirmed with an **Anderson–Rubin weak-IV test**, which produces confidence sets robust to weak instruments. The C-statistic is plotted across a grid of parameter values to visualise instrument strength.

---

## Comparison across strategies

| Method | Identifying variation | Estimand | Key assumption |
|---|---|---|---|
| Logit | Observational | AME | Conditional independence |
| DiD | Panel, over time | ATT | Parallel trends |
| Fuzzy RDD | Age-67 cutoff | LATE | Local continuity, no manipulation |
| Diff-in-Disc | Cutoff × time | LATE | No pre-existing trend change at cutoff |
| DML (IRM) | Observational + ML | ATE | Conditional independence (flexible) |
| DML (PLIV) | Age-67 instrument + ML | LATE | Instrument validity + exclusion restriction |



