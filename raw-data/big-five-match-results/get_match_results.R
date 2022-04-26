library(worldfootballR)
library(dplyr)
library(here)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Past Seasons ------------------------------------------------------------

# ***** NOTE: *********************************************
# This section doesn't need to be run again - the results 
# are already stored (top5_league_results_to_2021.rds)
# *********************************************************

# # first run to get past full season's data
# results <- get_match_results(country = c("ENG", "ESP", "FRA", "GER", "ITA"), gender = "M", season_end_year = c(2018:2021))
# 
# # filter out any playoffs, abandoned games, etc
# results <- results %>% filter(!is.na(Wk),
#                               !is.na(HomeGoals))
# 
# # save original data
# saveRDS(results, here("raw-data", "big-five-match-results", "top5_league_results_to_2021.rds"))


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Updating Data -----------------------------------------------------------

# reda in the old data
results <- readRDS(here("raw-data", "big-five-match-results", "top5_league_results_to_2021.rds"))

# get new data
newdata <- get_match_results(country = c("ENG", "ESP", "FRA", "GER", "ITA"), gender = "M", season_end_year = 2022)

# filter out any playoffs, abandoned games, yet-to-be-played games, etc
newdata <- newdata %>%
  mutate(Wk = as.integer(Wk)) %>% 
  filter(!is.na(Wk),
         !is.na(HomeGoals))

# join on to new data
results <- results %>%
  bind_rows(newdata)

# add date column to display in app:
results$data_updated <- Sys.time()


# save up-to-date data
saveRDS(results, here("raw-data", "big-five-match-results", "top5_league_results.rds"))

