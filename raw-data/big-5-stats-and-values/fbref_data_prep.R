library(worldfootballR)
library(tidyverse)


big5_player_playing_time <- fb_big5_advanced_season_stats(season_end_year= c(2020:2021), stat_type= "playing_time", team_or_player= "player")
# saveRDS(big5_player_playing_time, "data/big5_player_playing_time.rds")

playing_time <- big5_player_playing_time %>% 
  select(Season_End_Year, Squad, Comp, Player, Nation, Pos, Age, Born, Url, MP=MP_Playing.Time, Minutes=Min_Playing.Time, MinsPerMatch=Mn_per_MP_Playing.Time,
         NinetiesPlayed=Mins_Per_90_Playing.Time, Starts=Starts_Starts, MinsPerStart=Mn_per_Start_Starts, CompleteMatchesPlayed=Compl_Starts,
         MatchesAsSub=Subs_Subs, MinsPerSub=Mn_per_Sub_Subs, UnusedSub=unSub_Subs, PointsPerMatch=PPM_Team.Success, TeamGFonPitch=onG_Team.Success,
         TeamGAonPitch=onGA_Team.Success, TeamPlusMinusOnPitch=plus_per__minus__Team.Success, TeamPlusMinusOnPitch90=plus_per__minus_90_Team.Success,
         TeamPlusMinusNetOnPitch90=On_minus_Off_Team.Success, TeamxGonPitch=onxG_Team.Success..xG., TeamxGAgainstOnPitch=onxGA_Team.Success..xG)

big5_player_standard <- fb_big5_advanced_season_stats(season_end_year= c(2020:2021), stat_type= "standard", team_or_player= "player")
# saveRDS(big5_player_standard, "data/big5_player_standard.rds")

standard <- big5_player_standard %>% 
  select(Season_End_Year, Squad, Comp, Player, Nation, Pos, Age, Born, Url, Gls, Ast, NonPenGls=G_minus_PK, PensMade=PK, PKatt, CrdY, CrdR, Gls90=Gls_Per,
         Ast90=Ast_Per, GlsPlusAst90=`G+A_Per`, NonPenGls90=G_minus_PK_Per, NonPenGlsPlusAst90=`G+A_minus_PK_Per`, xG=xG_Expected, NonPenxG=npxG_Expected,
         xA=xA_Expected, xGPer90=xG_Per, xAPer90=xA_Per)

big5_player_shooting <- fb_big5_advanced_season_stats(season_end_year= c(2020:2021), stat_type= "shooting", team_or_player= "player")
# saveRDS(big5_player_shooting, "data/big5_player_shooting.rds")

shooting <- big5_player_shooting %>% 
  select(Season_End_Year, Squad, Comp, Player, Nation, Pos, Age, Born, Url, Shots=Sh_Standard, SoT=SoT_Standard, `SoT%`=SoT_percent_Standard, Shots90=Sh_per_90_Standard,
         SoT90=SoT_per_90_Standard, GlsPerShot=G_per_Sh_Standard, GlsPerSoT90=G_per_SoT_Standard, AvgDistanceYds=Dist_Standard, ShotFK=FK_Standard)

big5_player_passing <- fb_big5_advanced_season_stats(season_end_year= c(2020:2021), stat_type= "passing", team_or_player= "player")
# saveRDS(big5_player_passing, "data/big5_player_passing.rds")

passing <- big5_player_passing %>% 
  select(Season_End_Year, Squad, Comp, Player, Nation, Pos, Age, Born, Url, PassCmp=Cmp_Total, PassAtt=Att_Total, `PassCompletion%`=Cmp_percent_Total,
         TotDistYds=TotDist_Total, ProgressiveDistYds=PrgDist_Total, PassCmpShort=Cmp_Short, PassAttShort=Att_Short, `PassCompletion%Short`=Cmp_percent_Short,
         PassCmpMedium=Cmp_Medium, PassAttMedium=Att_Medium, `PassCompletion%Medium`=Cmp_percent_Medium, PassCmpLong=Cmp_Long, PassAttLong=Att_Long,
         `PassCompletion%Long`=Cmp_percent_Long, KeyPasses=KP, PassFinalThird=Final_Third, CmpPassPenArea=PPA, CrossesPenArea=CrsPA, ProgressivePasses=Prog)

big5_player_passing_types <- fb_big5_advanced_season_stats(season_end_year= c(2020:2021), stat_type= "passing_types", team_or_player= "player")
# saveRDS(big5_player_passing_types, "data/big5_player_passing_types.rds")

passing_types <- big5_player_passing_types %>% 
  select(Season_End_Year, Squad, Comp, Player, Nation, Pos, Age, Born, Url, LivePasses=Live_Pass, DeadPasses=Dead_Pass, PassFromFK=FK_Pass, PassUnderPressure=Press_Pass,
         SwitchPasses=Sw_Pass, Crosses=Crs_Pass, CornerKickPasses=CK_Pass, InswingCK=In_Corner, OutswingCK=Out_Corner, StraightCK=Str_Corner, GroundPass=Ground_Height,
         LowPass=Low_Height, HighPass=High_Height, LeftFootPass=Left_Body, RightFootPass=Right_Body, HeaderPass=Head_Body, ThrowInPass=TI_Body,
         PassOffside=Off_Outcomes, PassOutOfBounds=Out_Outcomes, PassIntercepted=Int_Outcomes, PassBlock=Blocks_Outcomes)

big5_player_gca <- fb_big5_advanced_season_stats(season_end_year= c(2020:2021), stat_type= "gca", team_or_player= "player")
# saveRDS(big5_player_gca, "data/big5_player_gca.rds")

gca <- big5_player_gca %>% 
  select(Season_End_Year, Squad, Comp, Player, Nation, Pos, Age, Born, Url, ShotCreatingActions=SCA_SCA, ShotCreatingActions90=SCA90_SCA)

big5_player_defense <- fb_big5_advanced_season_stats(season_end_year= c(2020:2021), stat_type= "defense", team_or_player= "player")
# saveRDS(big5_player_defense, "data/big5_player_defense.rds")

defense <- big5_player_defense %>% 
  select(Season_End_Year, Squad, Comp, Player, Nation, Pos, Age, Born, Url, Tackles=Tkl_Tackles, TacklesWon=TklW_Tackles, TacklesDefThird=`Def 3rd_Tackles`,
         TacklesMidThird=`Mid 3rd_Tackles`, TacklesAttThird=`Att 3rd_Tackles`, DribblersTackled=Tkl_Vs, DribblersAgainst=Att_Vs, DribblersPassed=Past_Vs,
         PressuresApplied=Press_Pressures, SuccessfulPressures=Succ_Pressures, PressuresDefThird=`Def 3rd_Pressures`, PressuresMidThird=`Mid 3rd_Pressures`,
         PressuresAttThird=`Att 3rd_Pressures`, Blocks=Blocks_Blocks, ShotsBlocked=Sh_Blocks, SoTBlocked=ShSv_Blocks, PassesBlocked=Pass_Blocks,
         Intercepts = Int, TackledPlusIntercepted=`Tkl+Int`, Clearances=Clr, Errors=Err)


big5_player_possession <- fb_big5_advanced_season_stats(season_end_year= c(2020:2021), stat_type= "possession", team_or_player= "player")
# saveRDS(big5_player_possession, "data/big5_player_possession.rds")

possession <- big5_player_possession %>% 
  select(Season_End_Year, Squad, Comp, Player, Nation, Pos, Age, Born, Url, Touches=Touches_Touches, TouchesDefPen=`Def Pen_Touches`, TouchesDefThird=`Def 3rd_Touches`,
         TouchesMidThird=`Mid 3rd_Touches`, TouchesAttThird=`Att 3rd_Touches`, TouchesAttPen=`Att Pen_Touches`, TouchesLiveBall=Live_Touches,
         DribblesSuccess=Succ_Dribbles, DribblesAttempted=Att_Dribbles, `DribblesSuccess%`=Succ_percent_Dribbles, DribbledPlayers=`#Pl_Dribbles`,
         Nutmegs=Megs_Dribbles, Carries=Carries_Carries, CarriesTotDist=TotDist_Carries, CarriesProgressiveDist=PrgDist_Carries, CarriesProgressive=Prog_Carries,
         CarriesFinalThird=Final_Third_Carries, CarriesPenArea=CPA_Carries, CarriesFail=Mis_Carries, CarriesDispossessed=Dis_Carries, Targeted=Targ_Receiving,
         ReceivedPass=Rec_Receiving, ProgressivePassesReceived=Prog_Receiving)

big5_player_misc <- fb_big5_advanced_season_stats(season_end_year= c(2020:2021), stat_type= "misc", team_or_player= "player")
# saveRDS(big5_player_misc, "data/big5_player_misc.rds")

misc <- big5_player_misc %>% 
  select(Season_End_Year, Squad, Comp, Player, Nation, Pos, Age, Born, Url, Fouls=Fls, Fouled=Fld, Offsides=Off, PKsWon=PKwon, PKsConceeded=PKcon,
         OwnGoals=OG, LooseBallRecovered=Recov, ArielsWon=Won_Aerial, ArialsLost=Lost_Aerial)

big5_player_keepers <- fb_big5_advanced_season_stats(season_end_year= c(2020:2021), stat_type= "keepers", team_or_player= "player")
# saveRDS(big5_player_keepers, "data/big5_player_keepers.rds")

keepers <- big5_player_keepers %>% 
  select(Season_End_Year, Squad, Comp, Player, Nation, Pos, Age, Born, Url, KeeperGoalsAgainst=GA, KeeperGoalsAgainst90=GA90, KeeperSoTA=SoTA,
         KeeperSaves=Saves, `KeeperSave%`=Save_percent, KeeperWins=W, KeeperDraws=D, KeeperLosses=L, KeeperCleanSheets=CS, `KeeperCleanSheets%`=CS_percent,
         KeeperPKAtt=PKatt_Penalty, KeeperPKAllowed=PKA_Penalty, KeeperPKSaved=PKsv_Penalty, KeeperPKMissed=PKm_Penalty, `KeeperPKSaved%`=Save_percent_Penalty)

big5_player_keepers_adv <- fb_big5_advanced_season_stats(season_end_year= c(2020:2021), stat_type= "keepers_adv", team_or_player= "player")
# saveRDS(big5_player_keepers_adv, "data/big5_player_keepers_adv.rds")

keepers_adv <- big5_player_keepers_adv %>%
  select(Season_End_Year, Squad, Comp, Player, Nation, Pos, Age, Born, Url, OwnGoalKeeper=OG_Goals, KeeperPostShotxG=PSxG_Expected, KeeperPostShotxGPerSoT=PSxG_per_SoT_Expected,
         KeeperPostShotxGPlusMinus=`PSxG+_per__minus__Expected`, KeeperAvgLengthPasses=AvgLen_Passes, KeeperGoalKicks=Att_Goal, KeeperAvgLengthGoalKick=AvgLen_Goal,
         KeeperActionsOPA=`#OPA_Sweeper`, KeeperActionsOPA90=`#OPA_per_90_Sweeper`)

all_joined <- playing_time %>% 
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


saveRDS(all_joined, "data/fbref_data_to_2020_2021.rds")


