# publishes full pipeline
wflow_publish(
  c(
    "analysis/_site.yml",
    "analysis/about.Rmd",
    "analysis/index.Rmd",
    "analysis/project-functions.Rmd",
    "analysis/setup-exposure-data.Rmd",
    "analysis/general-statistics.Rmd",
    "analysis/setup-model-data.Rmd",
    "analysis/transmission-risk-sensitivity.Rmd",
    "analysis/transmission-risk.Rmd"
  )
)
