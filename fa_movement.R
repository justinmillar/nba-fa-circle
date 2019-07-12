library(tidyverse)
library(nbastatR)
library(circlize)

# The easy, less elegant way ----
roster_old <- seasons_rosters(2019)
roster_new <- seasons_rosters(2020)
assign_nba_teams()

player_df <- bref_players_stats(2019, tables = c("totals", "per_game", "
                                                 per_minute", "per_poss"))

new <- roster_new %>% 
  select(name = namePlayer, new_team = slugTeam, url = urlPlayerStats)

transfer_dat <- roster_old %>% 
  select(name = namePlayer, old_team = slugTeam) %>% 
  full_join(new) %>% 
  arrange(new_team) %>% 
  left_join(player_df, by = c("name" = "namePlayer"))

create_df <- function(df, var, rm_same_team = F) {
  v <- enquo(var)
  
  out <- df %>% 
    filter(!is.na(old_team), !is.na(new_team)) %>% 
    group_by(old_team, new_team) %>% 
    summarize(out = sum(!!v, na.rm = T)) %>% 
    arrange(new_team)
  
  if(rm_same_team == T){
    out <- filter(out, old_team != new_team)
  }
  return(out)
}

grid_col <- structure(
  gsub(",.*", "", df_dict_nba_teams$colorsTeam), 
  names = df_dict_nba_teams$slugTeam
)

d <- create_df(transfer_dat, ptsPerGame)

chordDiagram(d, 
             # directional = 1, 
             grid.border = NULL, 
             grid.col = grid_col, 
             link.visible = d[[1]] != d[[2]]
)