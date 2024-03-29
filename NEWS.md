# *News and Updates to 'qwickr'*


# qwickr 1.0.0 (TBD)
## Initial CRAN Submission
* **CRAN** Initial Submission

# Changes in version 0.9.0 (2022-03-15)
* Updated `rmdata` dataset to include a binary outcome (KNEE_PAIN) and a non-normally distributed outcome "WALK_GAIT"


# Changes in version 0.8.0 (2022-03-11)
* Updated 48h time point for `pkdata`
* Added `pkssdata` steady-state PK dummy data

# Changes in version 0.7.0 (2022-03-09)
* Added a dummy dataset to help with running package examples
  * `catdata` is a dataset with categorical outcomes

# Changes in version 0.6.0 (2021-11-08)
* Made various improvements to combine all outputs from `qwickr.cont()` as a single level list instead of a multi-level list


# Changes in version 0.5.0 (2021-11-02)
* Added idvar and timevar arguments to `qwickr.cont()` for specifying unique subject ID and time variables. This means users don't have to rename their variables to match the test data in order to use the function.

# Changes in version 0.4.0 (2021-10-30)
* `qwickr.cont()` is now able to export analysis output to Word and Excel
* `qwickr.cont()` bugs with running pairwise comparisons fixed.
* `qwickr.cat()` bug with file export fixed

# Changes in version 0.3.0 (2021-02-18)
* Added `qwickr.pkfe()` function to be used for comparing PK parameters between groups and generate fixed effects tables as per [Health Canada](https://www.canada.ca/content/dam/hc-sc/documents/services/drugs-health-products/drug-products/applications-submissions/guidance-documents/bioavailability-bioequivalence/conduct-analysis-comparative.pdf) format
* Added `qwickr.pkbe()` function to calculate bioequivalence, along with 90% CI and % CV
* Updated documentation for `qwickr.pkparams()` and `qwickr.cont()`
* Removed dependency on `magrittr` ... one less package to depend on.


# Changes in version 0.2.0 (2021-02-12)
* First move towards supporting non-compartmental pharmacokinetics analysis
  * Added `qwickr.pkconcs()` function to generate PK concentration listings for the study report as per [Health Canada](https://www.canada.ca/content/dam/hc-sc/documents/services/drugs-health-products/drug-products/applications-submissions/guidance-documents/bioavailability-bioequivalence/conduct-analysis-comparative.pdf) format
  * Added `qwickr.pkparams()` function to generate PK parameters per study participant for inclusion in the study report and for further analysis.
  * Added `q.netconcs()` function as a helper for the `qwickr.pkparams()` function. It calculates net analyte concentrations from pre-dose. This is used in calculating the incremental AUC (iAUC) in the `qwickr.pkparams()` function.
* Added 2 datasets to help with running package examples
  * `pkdata` is a pharmacokinetics dataset
  * `rmdata` is a repeated measures dataset that mimics efficacy outcomes

# qwickr 0.1.4 (2020-12-14)
## Initial Release to GitHub
* Initial Release to GitHub
* Prior to this it was private package

