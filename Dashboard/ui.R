# A dashboard of things I find interesting

library(shiny)
library(bslib)
library(shinythemes)
library(ggplot2)
library(colourpicker)

# Define UI for application that draws a histogram
page_navbar(
  title = "Some apps I found interesting to make",
  theme = bs_theme(version = 4, bootswatch = "sketchy"),
  navbar_options = navbar_options(theme = "light"),
  navset_tab(
  #Dice ----
  nav_panel(title = "Dice",
    # Application title
    titlePanel("Dice Simulator"),
    
    tags$div("My children Play",
     tags$a(href="https://en.wikipedia.org/wiki/Machi_Koro",
                  "Machi Koro"),
     "and this will help them sense which cards are more valuable;
    I find it interesting to switch axes dynmically, as well as having 
    to program a histogram by hand as ",
      tags$code("xkcd"),
      " does not have a histogram function."),
    
    sidebarLayout(
      sidebarPanel(
        h3("How many dice should we roll?"),
        br(),
        radioButtons("dice.dice_amount",
                     label = NULL,
                     choices = c("One" = "one",
                                 "Two" = "two")),
        br(),
        h3("How many times should we roll the dice?"),
        br(),
        actionButton("dice.go1", "Once"),
        actionButton("dice.go10", "10 times"),
        actionButton("dice.go100", "100 times"),
        actionButton("dice.go1000", "1000 times"),
        uiOutput("dice.ui"),
        br(),
        h3("Should we do Counts or Percents?"),
        radioButtons("dice.hist_type",
                     label = NULL,
                     choices = c("Counts" = "counts",
                                 "Percent" = "percent")
                     ),
        br(),
        h3("I want my own custom number of rolls! (Push button above after updating.)"),
        numericInput("dice.custom_number", label = NULL,
                     value = 0)
      ),
      mainPanel(
        plotOutput("dice.histogram"),
        br(),
        colourInput("dice.color", "Select a color (regular names also work):",
                    "pink", returnName = TRUE)
      )
    )
  ),
    
  #Birthdays ----
  nav_panel(title = "Birthdays",
  
    "My children always want to know how many units until their birthday. Now they will know. I think there is legacy code on git"
  
  ),
  #Mondays
  nav_panel(title = "Mondays",
  
    "One of my coworkers always lists the work days of the week in Mondays (1st monday, 2nd monday, etc). Now we can know in terms of other units as well"
  )
)
)
