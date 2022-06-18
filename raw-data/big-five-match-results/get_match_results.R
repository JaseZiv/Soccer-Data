library(worldfootballR)
library(dplyr)
library(here)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# As of June 2022, this now makes use of the new load_match_results() function from the worldfootballR library

# reda in the old data
results <- worldfootballR::load_match_results(country = c("ENG", "FRA", "ITA", "GER", "ESP"), gender = "M", season_end_year = c(2018:2022), tier = "1st")

# add date column to display in app:
results$data_updated <- attr(results, 'scrape_timestamp')


# save up-to-date data
saveRDS(results, here("raw-data", "big-five-match-results", "top5_league_results.rds"))

