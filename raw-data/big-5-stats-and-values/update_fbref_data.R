library(worldfootballR)
library(dplyr)
library(here)

###########################################################################
# Get Data Using worldfootballR -------------------------------------------

# scrape data
playing_time <- load_fb_big5_advanced_season_stats(season_end_year= c(2020:2022), stat_type= "playing_time", team_or_player= "player")

# select and rename required columns
playing_time <- playing_time %>% 
  select(Season_End_Year, Squad, Comp, Player, Nation, Pos, Age, Born, Url, MP=MP_Playing.Time, Minutes=Min_Playing.Time, MinsPerMatch=Mn_per_MP_Playing.Time,
         NinetiesPlayed=Mins_Per_90_Playing.Time, Starts=Starts_Starts, MinsPerStart=Mn_per_Start_Starts, CompleteMatchesPlayed=Compl_Starts,
         MatchesAsSub=Subs_Subs, MinsPerSub=Mn_per_Sub_Subs, UnusedSub=unSub_Subs, PointsPerMatch=PPM_Team.Success, TeamGFonPitch=onG_Team.Success,
         TeamGAonPitch=onGA_Team.Success, TeamPlusMinusOnPitch=plus_per__minus__Team.Success, TeamPlusMinusOnPitch90=plus_per__minus_90_Team.Success,
         TeamPlusMinusNetOnPitch90=On_minus_Off_Team.Success, TeamxGonPitch=onxG_Team.Success..xG., TeamxGAgainstOnPitch=onxGA_Team.Success..xG)

#-----#
standard <- load_fb_big5_advanced_season_stats(season_end_year= c(2020:2022), stat_type= "standard", team_or_player= "player")

standard <- standard %>% 
  select(Season_End_Year, Squad, Comp, Player, Nation, Pos, Age, Born, Url, Gls, Ast, NonPenGls=G_minus_PK, PensMade=PK, PKatt, CrdY, CrdR, Gls90=Gls_Per,
         Ast90=Ast_Per, GlsPlusAst90=`G+A_Per`, NonPenGls90=G_minus_PK_Per, NonPenGlsPlusAst90=`G+A_minus_PK_Per`, xG=xG_Expected, NonPenxG=npxG_Expected,
         xA=xA_Expected, xGPer90=xG_Per, xAPer90=xA_Per)

#-----#
shooting <- load_fb_big5_advanced_season_stats(season_end_year= c(2020:2022), stat_type= "shooting", team_or_player= "player")

shooting <- shooting %>% 
  select(Season_End_Year, Squad, Comp, Player, Nation, Pos, Age, Born, Url, Shots=Sh_Standard, SoT=SoT_Standard, `SoT%`=SoT_percent_Standard, Shots90=Sh_per_90_Standard,
         SoT90=SoT_per_90_Standard, GlsPerShot=G_per_Sh_Standard, GlsPerSoT90=G_per_SoT_Standard, AvgDistanceYds=Dist_Standard, ShotFK=FK_Standard)

#-----#
passing <- load_fb_big5_advanced_season_stats(season_end_year= c(2020:2022), stat_type= "passing", team_or_player= "player")

passing <- passing %>% 
  select(Season_End_Year, Squad, Comp, Player, Nation, Pos, Age, Born, Url, PassCmp=Cmp_Total, PassAtt=Att_Total, `PassCompletion%`=Cmp_percent_Total,
         TotDistYds=TotDist_Total, ProgressiveDistYds=PrgDist_Total, PassCmpShort=Cmp_Short, PassAttShort=Att_Short, `PassCompletion%Short`=Cmp_percent_Short,
         PassCmpMedium=Cmp_Medium, PassAttMedium=Att_Medium, `PassCompletion%Medium`=Cmp_percent_Medium, PassCmpLong=Cmp_Long, PassAttLong=Att_Long,
         `PassCompletion%Long`=Cmp_percent_Long, KeyPasses=KP, PassFinalThird=Final_Third, CmpPassPenArea=PPA, CrossesPenArea=CrsPA, ProgressivePasses=Prog)

#-----#
passing_types <- load_fb_big5_advanced_season_stats(season_end_year= c(2020:2022), stat_type= "passing_types", team_or_player= "player")

passing_types <- passing_types %>% 
  select(Season_End_Year, Squad, Comp, Player, Nation, Pos, Age, Url, Born, LivePasses=Live_Pass, DeadPasses=Dead_Pass, PassFromFK=FK_Pass, PassUnderPressure=Press_Pass,
         SwitchPasses=Sw_Pass, Crosses=Crs_Pass, CornerKickPasses=CK_Pass, InswingCK=In_Corner, OutswingCK=Out_Corner, StraightCK=Str_Corner, GroundPass=Ground_Height,
         LowPass=Low_Height, HighPass=High_Height, LeftFootPass=Left_Body, RightFootPass=Right_Body, HeaderPass=Head_Body, ThrowInPass=TI_Body,
         PassOffside=Off_Outcomes, PassOutOfBounds=Out_Outcomes, PassIntercepted=Int_Outcomes, PassBlock=Blocks_Outcomes)

#-----#
gca <- load_fb_big5_advanced_season_stats(season_end_year= c(2020:2022), stat_type= "gca", team_or_player= "player")

gca <- gca %>% 
  select(Season_End_Year, Squad, Comp, Player, Nation, Pos, Age, Born, Url, ShotCreatingActions=SCA_SCA, ShotCreatingActions90=SCA90_SCA)

#-----#
defense <- load_fb_big5_advanced_season_stats(season_end_year= c(2020:2022), stat_type= "defense", team_or_player= "player")

defense <- defense %>% 
  select(Season_End_Year, Squad, Comp, Player, Nation, Pos, Age, Born, Url, Tackles=Tkl_Tackles, TacklesWon=TklW_Tackles, TacklesDefThird=`Def 3rd_Tackles`,
         TacklesMidThird=`Mid 3rd_Tackles`, TacklesAttThird=`Att 3rd_Tackles`, DribblersTackled=Tkl_Vs, DribblersAgainst=Att_Vs, DribblersPassed=Past_Vs,
         PressuresApplied=Press_Pressures, SuccessfulPressures=Succ_Pressures, PressuresDefThird=`Def 3rd_Pressures`, PressuresMidThird=`Mid 3rd_Pressures`,
         PressuresAttThird=`Att 3rd_Pressures`, Blocks=Blocks_Blocks, ShotsBlocked=Sh_Blocks, SoTBlocked=ShSv_Blocks, PassesBlocked=Pass_Blocks,
         Intercepts = Int, TackledPlusIntercepted=`Tkl+Int`, Clearances=Clr, Errors=Err)

#-----#
possession <- load_fb_big5_advanced_season_stats(season_end_year= c(2020:2022), stat_type= "possession", team_or_player= "player")

possession <- possession %>% 
  select(Season_End_Year, Squad, Comp, Player, Nation, Pos, Age, Born, Url, Touches=Touches_Touches, TouchesDefPen=`Def Pen_Touches`, TouchesDefThird=`Def 3rd_Touches`,
         TouchesMidThird=`Mid 3rd_Touches`, TouchesAttThird=`Att 3rd_Touches`, TouchesAttPen=`Att Pen_Touches`, TouchesLiveBall=Live_Touches,
         DribblesSuccess=Succ_Dribbles, DribblesAttempted=Att_Dribbles, `DribblesSuccess%`=Succ_percent_Dribbles, DribbledPlayers=`#Pl_Dribbles`,
         Nutmegs=Megs_Dribbles, Carries=Carries_Carries, CarriesTotDist=TotDist_Carries, CarriesProgressiveDist=PrgDist_Carries, CarriesProgressive=Prog_Carries,
         CarriesFinalThird=Final_Third_Carries, CarriesPenArea=CPA_Carries, CarriesFail=Mis_Carries, CarriesDispossessed=Dis_Carries, Targeted=Targ_Receiving,
         ReceivedPass=Rec_Receiving, ProgressivePassesReceived=Prog_Receiving)

#-----#
misc <- load_fb_big5_advanced_season_stats(season_end_year= c(2020:2022), stat_type= "misc", team_or_player= "player")

misc <- misc %>% 
  select(Season_End_Year, Squad, Comp, Player, Nation, Pos, Age, Born, Url, Fouls=Fls, Fouled=Fld, Offsides=Off, PKsWon=PKwon, PKsConceeded=PKcon,
         OwnGoals=OG, LooseBallRecovered=Recov, ArielsWon=Won_Aerial, ArialsLost=Lost_Aerial)

#-----#
keepers <- load_fb_big5_advanced_season_stats(season_end_year= c(2020:2022), stat_type= "keepers", team_or_player= "player")

keepers <- keepers %>% 
  select(Season_End_Year, Squad, Comp, Player, Nation, Pos, Age, Born, Url, KeeperGoalsAgainst=GA, KeeperGoalsAgainst90=GA90, KeeperSoTA=SoTA,
         KeeperSaves=Saves, `KeeperSave%`=Save_percent, KeeperWins=W, KeeperDraws=D, KeeperLosses=L, KeeperCleanSheets=CS, `KeeperCleanSheets%`=CS_percent,
         KeeperPKAtt=PKatt_Penalty, KeeperPKAllowed=PKA_Penalty, KeeperPKSaved=PKsv_Penalty, KeeperPKMissed=PKm_Penalty, `KeeperPKSaved%`=Save_percent_Penalty)

#-----#
keepers_adv <- load_fb_big5_advanced_season_stats(season_end_year= c(2020:2022), stat_type= "keepers_adv", team_or_player= "player")

keepers_adv <- keepers_adv %>%
  select(Season_End_Year, Squad, Comp, Player, Nation, Pos, Age, Born, Url, OwnGoalKeeper=OG_Goals, KeeperPostShotxG=PSxG_Expected, KeeperPostShotxGPerSoT=PSxG_per_SoT_Expected,
         KeeperPostShotxGPlusMinus=`PSxG+_per__minus__Expected`, KeeperAvgLengthPasses=AvgLen_Passes, KeeperGoalKicks=Att_Goal, KeeperAvgLengthGoalKick=AvgLen_Goal,
         KeeperActionsOPA=`#OPA_Sweeper`, KeeperActionsOPA90=`#OPA_per_90_Sweeper`)


###########################################################################
# Create final file -------------------------------------------------------

# join all scraped data into one data frame
fbref_data <- playing_time %>% 
  left_join(standard, by = c("Season_End_Year", "Squad", "Comp", "Player", "Nation", "Pos", "Age", "Born", "Url")) %>% 
  left_join(shooting, by = c("Season_End_Year", "Squad", "Comp", "Player", "Nation", "Pos", "Age", "Born", "Url")) %>% 
  left_join(passing, by = c("Season_End_Year", "Squad", "Comp", "Player", "Nation", "Pos", "Age", "Born", "Url")) %>% 
  left_join(passing_types, by = c("Season_End_Year", "Squad", "Comp", "Player", "Nation", "Pos", "Age", "Born", "Url")) %>% 
  left_join(gca, by = c("Season_End_Year", "Squad", "Comp", "Player", "Nation", "Pos", "Age", "Born", "Url")) %>% 
  left_join(defense, by = c("Season_End_Year", "Squad", "Comp", "Player", "Nation", "Pos", "Age", "Born", "Url")) %>% 
  left_join(possession, by = c("Season_End_Year", "Squad", "Comp", "Player", "Nation", "Pos", "Age", "Born", "Url")) %>% 
  left_join(misc, by = c("Season_End_Year", "Squad", "Comp", "Player", "Nation", "Pos", "Age", "Born", "Url")) %>% 
  left_join(keepers, by = c("Season_End_Year", "Squad", "Comp", "Player", "Nation", "Pos", "Age", "Born", "Url")) %>% 
  left_join(keepers_adv, by = c("Season_End_Year", "Squad", "Comp", "Player", "Nation", "Pos", "Age", "Born", "Url"))

# the new season appears to have player ages listed as years-days. Will strip away the days.
# this can be changed though, possibly by converting the "-" to a "." and making age a decimal
fbref_data$Age <- gsub("-.*", "", all_joined$Age)



# Join Transfermarkt data -------------------------------------------------

# get the mapping dictionary
mapping <- player_dictionary_mapping()

# get the most recent season player market values
player_values <- get_player_market_values(country_name = c("England", "Spain", "Germany", "Italy", "France"),
                                          start_year = 2021)

# player_values <- readRDS("data/tm_players.rds")
# player_values <- player_values %>% filter(season_start_year == 2021)

# read in the old transfer data
past_player_values <- readRDS(here("raw-data", "big-5-stats-and-values", "tm_valuations_2020_to_2021.rds"))

# join them together
player_values <- bind_rows(player_values, past_player_values)

# FBref uses the end of season year while TM uses the start year
player_values <- player_values %>% 
  mutate(Season_End_Year = season_start_year + 1)

# read in team names dictionary to make joining easier and join to both fbref and TM data
team_names <- read.csv(here("raw-data", "big-5-stats-and-values", "teams_map.csv"), stringsAsFactors = F)

fbref_data <- fbref_data %>% left_join(team_names, by = c("Squad" = "joining_name"), keep = T)
player_values <- player_values %>% left_join(team_names, by = c("squad" = "joining_name"), keep = T)

fbref_data <- fbref_data %>% 
  left_join(mapping, by = c("Player" = "PlayerFBref", "Url" = "UrlFBref")) %>% 
  left_join(player_values %>% select(Season_End_Year, TM_Pos=player_position, contract_expiry, player_market_value_euro, player_url, primary_name), by = c("Season_End_Year", "UrlTmarkt" = "player_url", "primary_name"))

fbref_data <- fbref_data %>% 
  distinct(Season_End_Year, Squad, Comp, Player, Nation, Pos, Age, Born, Url, .keep_all = T)

# fbref_data <- fbref_data %>% 
#   mutate(player_market_value_euro = ifelse(is.na(player_market_value_euro), 0, player_market_value_euro))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#----- write final file -----#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
saveRDS(fbref_data, here("raw-data", "big-5-stats-and-values", "fbref_data.rds"))


