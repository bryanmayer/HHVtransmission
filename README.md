# HHVtransmission

A [workflowr][] project.

[workflowr]: https://github.com/jdblischak/workflowr


## Order of analysis files:

1. exposure_data_setup.Rmd -> set up the data for analysis

2. basic_statistics.Rmd -> basic statistics of the data, for background use in methods at this point

3. survival_analysis.Rmd -> establishes outcome data, makes "clean final" data for the exposure analysis

4. exposure_overview.Rmd -> basic statistics for cleaned exposure data, for use in Results

5. regression_analysis.Rmd -> model of time/exposure and how it predicts infection

6. prediction_analysis.Rmd -> use best fitting regression models to estimate dose-response curves. 

