rm(list=ls(all=TRUE))
setwd('~/Dropbox/Code/NFL_app/')

library(shiny)
library(markdown)

source('main_functions.R')

fluidPage(
  sidebarLayout(
    sidebarPanel(
      titlePanel("NFL Ranker"),
      helpText(a('Created by Chris White', href='http://math.utexas.edu/~cwhite', target="_blank")),
      hr(),
      h4('Choose some data:'),
      selectInput("stat", label = "Game Statistic:", 
                  choices = names(map),
                  selected = 'Score'),
      selectInput("year", label = "Season Year:", 
                  choices = unique(boxscores$season_year), 
                  selected = '2013'),
      hr()
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Rankings", plotOutput("plot1")),
        tabPanel("Predictive Power", plotOutput("plot2")),
        tabPanel("About", includeHTML("about.html")))
    )
  )
)
