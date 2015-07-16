## functions for working with the NFL dataset
library('softImpute')
boxscores <- readRDS("Data/boxscores.rds")
team_ids <- unique(boxscores$home_team)
boxscores$h_ID <- match(boxscores$home_team,team_ids)
boxscores$a_ID <- match(boxscores$away_team,team_ids)

map <- list(Score = 'home_score',
            Pass_yds_att = 'hpyatt',
            TDs = 'home_tds',
            FGs = 'home_fgs',
            Defense_Ffum = 'home_defense_ffum',
            Rushing_yds = 'home_rushing_yds',
            Defense_int = 'home_defense_int',
            Sacks = 'home_defense_sk')

ranker <- function(stat, year, rank){
  game_data <- subset(boxscores, season_year %in% c(year) & season_type %in% c('Regular'))
  A <- matrix(NA,length(team_ids),length(team_ids))
  idx <- match(stat, names(map))
  stat_idx <- match(map[idx], names(game_data))
    
  for(k in 1:nrow(game_data)){
    A[game_data$h_ID[k],
      game_data$a_ID[k]] <- game_data[k,stat_idx]
    
    A[game_data$a_ID[k],
      game_data$h_ID[k]] <- game_data[k,stat_idx+1]
  }
  
  R <- softImpute(A, rank.max=rank, maxit=1e3)
  return(R)
}