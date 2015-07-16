library(shiny)
library(markdown)

fluidPage(
  sidebarLayout(
    sidebarPanel(
      titlePanel("NFL Ranker"),
      helpText(a('Created by Chris White', href='http://math.utexas.edu/~cwhite', target="_blank")),
      hr(),
      h4('Choose some data:'),
      selectInput("stat", label = "Game Statistic:", 
                  choices = names(data_box),
                  selected = 'Score'),
      selectInput("year", label = "Season Year:", 
                  choices = unique(data_box$season_year), 
                  selected = '2013'),
      hr(),
      sliderInput("rank", label = "Matrix Rank",
                  min = 1, max = 15, value = 1),
      hr()
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Rankings", plotOutput("plot1")),
        tabPanel("Predictive Power", 
                 plotOutput("plot2"),
                 h5("Statistics of the trained model:", style = "color:black"),
                 tableOutput("lmStats"),
                 h5("Coefficients of the trained model:", style = "color:black"),
                 tableOutput("lmResults")),
        tabPanel("About", includeHTML("about.html")))
    )
  )
)
