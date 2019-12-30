# builds full pipeline
wflow_build(c("analysis/about.Rmd", "analysis/index.Rmd",
              "analysis/project-functions.Rmd", "analysis/setup-exposure-data.Rmd",
              "analysis/general-statistics.Rmd",  "analysis/setup-model-data.Rmd",
              "analysis/transmission-risk-sensitivity.Rmd", "analysis/transmission-risk.Rmd"))
