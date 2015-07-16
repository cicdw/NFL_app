setwd('~/Dropbox/Code/NFL_app/')

library(shiny)
library(markdown)
library(ggplot2)

source('main_functions.R')

function(input,output){
  
  df <- reactive({ # data frame for ranking scatter
    selected_data <- ranker(stat = input$stat, year = input$year, rank = 1)
    df <- data.frame(Team = team_ids, Off = sqrt(selected_data$d)*abs(selected_data$u),
                     Def = sqrt(selected_data$d)*abs(selected_data$v))
    
    post_season <- subset(data_box, season_year %in% c(input$year) & season_type %in% c('Postseason'))
    df$post <- ifelse(df$Team %in% post_season$Team,'In Postseason','Not in Postseason')
    
    df
  })
  
  df.2 <- reactive({ # data.frame for regression
    post_season <- subset(data_box, season_year %in% c(input$year) & season_type %in% c('Postseason'))
    idx <- match(input$stat, names(data_box))
    
    y_out <- post_season[,idx] # dependent variable
    d2 <- ranker(stat = input$stat, year = input$year, rank = input$rank)
    #R <- as.matrix(d2$u)%*%as.matrix(diag(d2$d))%*%t(as.matrix(d2$v))
    x_in <- df()$Off[post_season$Team_ID]*df()$Def[post_season$Opp_Team_ID]
    data.frame(Game = post_season$gamekey, Observed = y_out, Predicted = x_in)
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