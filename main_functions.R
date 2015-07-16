## functions for working with the NFL dataset
rm(list=ls(all=TRUE))

library('softImpute')
data_box <- readRDS("Data/data_box.rds")
team_ids <- unique(data_box$Team)
data_box$Team_ID <- match(data_box$Team,team_ids)
data_box$Opp_Team_ID <- match(data_box$Opp_Team,team_ids)

ranker <- function(stat, year, rank){
  game_data <- subset(data_box, season_year %in% c(year) & season_type %in% c('Regular'))
  A <- matrix(NA,length(team_ids),length(team_ids))
  idx <- match(stat, names(data_box))
    
  for(k in 1:nrow(game_data)){
    A[game_data$Team_ID[k],
      game_data$Opp_Team_ID[k]] <- game_data[k,idx]
  }
  
  d_right <- sqrt(colSums(A, na.rm=TRUE))
  d_left <- sqrt(rowSums(A, na.rm=TRUE))
  A <- sweep(sweep(A, 2, d_right, FUN='/'), 1, d_left, FUN='/')
  
  #diag(A) <- 0
  R <- softImpute(A, rank.max=rank, maxit=1e3, lambda=.1)

  return(R)
}

linear_features <- function(stat, year){
  game_data <- subset(data_box, season_year %in% c(year) & season_type %in% c('Regular'))
  idx <- match(stat, names(game_data))
  fit <- lm(game_data[,idx] ~ Team + Opp_Team, data = game_data)
  Off <- sapply(names(fit$coefficients[2:32]), strsplit, split = 'Team', fixed=TRUE)
  
  return(fit)
}