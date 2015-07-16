library(shiny)
library(markdown)
library(ggplot2)

function(input,output){
  
  df <- reactive({
    selected_data <- ranker(stat = input$stat, year = input$year, rank = 1)
    df <- data.frame(Team = team_ids, Off = sqrt(selected_data$d)*abs(selected_data$u),
                     Def = sqrt(selected_data$d)*abs(selected_data$v))
    
    post_season <- subset(boxscores, season_year %in% c(input$year) & season_type %in% c('Postseason'))
    df$post <- ifelse(df$Team %in% post_season$home_team | df$Team %in% post_season$away_team,'In Postseason',
                      'Not in Postseason')
    
    df
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
    
    post_season <- subset(boxscores, season_year %in% c(input$year) & season_type %in% c('Postseason'))
    idx <- match(input$stat, names(map))
    stat_idx <- match(map[idx], names(post_season))
    y_out <- c(post_season[,stat_idx], post_season[,stat_idx+1]) # dependent variable
    gamekeys <- c(post_season$gamekey, post_season$gamekey) # want to color by whether they were in the game together
    x_in <- c(df()$Off[post_season$h_ID]*df()$Def[post_season$a_ID],
              df()$Off[post_season$a_ID]*df()$Def[post_season$h_ID])
    df.2 <- data.frame(Game = gamekeys, Observed = y_out, Predicted = x_in)
    
    p <- ggplot(df.2, aes(Predicted, Observed)) + geom_smooth(method='lm') + 
      geom_point(aes(fill=factor(Game)),size=5, shape=21) +
      theme_bw() + ggtitle('Predictive Power on Postseason\n') + theme(legend.position="none") +
      xlab(paste('\nPredicted',input$stat,sep=" ")) + ylab(paste('Observed',input$stat,'\n',sep=" ")) +
      scale_fill_brewer(palette="Paired")
    
    print(p)
  })
}