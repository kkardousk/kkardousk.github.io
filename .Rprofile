# activate renv if present
if (file.exists("renv/activate.R")) source("renv/activate.R")

# increase history size
Sys.setenv(R_HISTFILE = ".Rhistory")
Sys.setenv(R_HISTSIZE = 1000)
Sys.setenv(R_HISTFILE = file.path(Sys.getenv("HOME"), ".Rhistory"))

# set default CRAN mirror
local({
  r <- getOption("repos")
  r["CRAN"] <- "https://cran.rstudio.com"
  options(repos = r)
})

# set some global options
options(
  stringsAsFactors = FALSE,
  digits = 4,
  scipen = 999,
  encoding = "UTF-8",
  tibble.print_min = 10,
  tibble.print_max = 20,
  tibble.width = Inf
)

# enable color if interactive
if (interactive()) options(crayon.enabled = TRUE)

# load tibble or tidyverse early to apply options
suppressPackageStartupMessages({
  if ("tibble" %in% rownames(installed.packages())) {
    library(tibble)
  }
})
