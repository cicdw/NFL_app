library(shiny)
library(markdown)
library(ggplot2)

function(input,output){
  
  df <- reactive({ # data frame for ranking scatter
    selected_data <- ranker(stat = input$stat, year = input$year, rank = 1)
    df <- data.frame(Team = team_ids, Off = sqrt(selected_data$d)*abs(selected_data$u),
                     Def = sqrt(selected_data$d)*abs(selected_data$v))
    
    post_season <- subset(boxscores, season_year %in% c(input$year) & season_type %in% c('Postseason'))
    df$post <- ifelse(df$Team %in% post_season$home_team | df$Team %in% post_season$away_team,'In Postseason',
                      'Not in Postseason')
    
    df
  })
  
  df.2 <- reactive({ # data.frame for regression
    post_season <- subset(boxscores, season_year %in% c(input$year) & season_type %in% c('Postseason'))
    idx <- match(input$stat, names(map))
    stat_idx <- match(map[idx], names(post_season))
    y_out <- c(post_season[,stat_idx], post_season[,stat_idx+1]) # dependent variable
    gamekeys <- c(post_season$gamekey, post_season$gamekey) # want to color by whether they were in the game together
    x_in <- c(df()$Off[post_season$h_ID]*df()$Def[post_season$a_ID],
              df()$Off[post_season$a_ID]*df()$Def[post_season$h_ID])
    data.frame(Game = gamekeys, Observed = y_out, Predicted = x_in)
  })
  
  output$plot1 <- renderPlot({
    
      df <- df()
      p <- ggplot(df, aes(Off, Def)) + geom_point(aes(fill=post),size=10, shape=21, position=position_jitter(),
                                                alpha=0.75) +
      geom_text(aes(label=Team), size=3) + theme_bw() + ggtitle(paste(input$stat,"Rankings",sep=" ")) +
      theme(legend.title=element_blank()) + xlab(paste('\nAbility to Generate',input$stat,sep=" ")) +
      ylab(paste('Ability to Prevent',input$stat,'\n',sep=" "))
    
    print(p)
  })
  
  output$plot2 <- renderPlot({
    
    df.2 <- df.2()
    p <- ggplot(df.2, aes(Predicted, Observed)) + geom_smooth(method='lm') + 
      geom_point(aes(fill=factor(Game)),size=5, shape=21) +
      theme_bw() + ggtitle('Predictive Power on Postseason\n') + theme(legend.position="none") +
      xlab(paste('\nPredicted',input$stat,sep=" ")) + ylab(paste('Observed',input$stat,'\n',sep=" ")) +
      scale_fill_brewer(palette="Paired")
    
    print(p)
  })
  
  lmResults <- reactive({
    lm(Observed~Predicted, data=df.2())  
  })
  
  output$lmStats <- renderTable({
    results <- summary(lmResults())
    data.frame(R2=results$r.squared,
               adj.R2=results$adj.r.squared,
               f.value=results$fstatistic[1],
               f.denom=results$fstatistic[2],
               f.numer=results$fstatistic[3],
               p=1-pf(results$fstatistic[1],
                      results$fstatistic[2],
                      results$fstatistic[3]))
  })
  
  # Show coefficients
  
  output$lmResults <- renderTable(summary(lmResults()))
}