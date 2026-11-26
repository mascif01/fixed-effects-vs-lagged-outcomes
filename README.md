# fixed-effects-vs-lagged-outcomes
A comparison based on the analysis of the effects of debit cards on consumption

Bachelor's thesis in Statistics, Economics and Society, Sapienza University of Rome.

The analysis uses the Survey on Household Income and Wealth (SHIW) data from Bank of Italy.

## Where to start
The full text is in [`Tesi_finale_pdf.pdf`](Tesi_finale_pdf.pdf). To understand the modelling choices,
the assumptions and the results, please read the thesis: this repository only holds
the supporting code.

## Summary
The thesis studies whether debit card use causally affects consumption and compares
two common strategies for panel data:
- **Fixed-effects models**, which control for unobserved characteristics that do not change over time
- **Lagged-dependent-variable models**, which control for past consumption

It discusses how the choice of model changes the estimated effect.

## Contents
- `Tesi_finale_pdf.pdf`: the thesis
- `r code final.R`: the code used for the analysis
