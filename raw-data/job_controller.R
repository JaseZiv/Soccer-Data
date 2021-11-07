# SCHEDULED SCRAPERS

# load libraries
library(here)

# Scrape All Competition Season’s Data ------------------------------------
source(here::here("raw-data", "big-5-stats-and-values", "update_fbref_data.R"))

# Scrape Big Five League Match Results ------------------------------------
source(here::here("raw-data", "big-five-match-results", "get_match_results.R"))
