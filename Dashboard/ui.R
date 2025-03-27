# A dashboard of things I find interesting
#devtools::install_github("ArthurData/confetti")
library(shiny)
library(bslib)
library(shinythemes)
library(ggplot2)
library(colourpicker)
library(lubridate)
library(glue)
library(shinyjs)
#library(confetti)


page_navbar(
  shinyjs::useShinyjs(),
  extendShinyjs(script = "confetti.js",functions =
                  c("startConfettiInner",
                    "removeConfettiInner",
                    "stopConfettiInner")),
  
  title = "Some apps I found interesting to make",
  window_title = "Dyrland's Shiny Dashboard",
  theme = bs_theme(version = 4, bootswatch = "sketchy"),
  navbar_options = navbar_options(theme = "light"),
  navset_tab(
  #Dice ----
  nav_panel(title = "Dice",
    titlePanel("Dice Simulator"),
    tags$div("My children Play",
     tags$a(href="https://en.wikipedia.org/wiki/Machi_Koro",
                  "Machi Koro"),
     "and this will help them sense which cards are more valuable;
    I find it interesting to switch axes dynmically, as well as having 
    to program a histogram by hand as ",
      tags$code("xkcd"),
      " does not have a histogram function. Oh, and apparently I need to upload
      the ",
     tags$code("xkcd"),
     " font as well as it is not native to the Shiny Servers..."
     ),
    
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
        colourpicker::colourInput("dice.color", "Select a color (regular names also work):",
                    "pink", returnName = TRUE)
      )
    )
  ),
    
  #Birthdays ----
  nav_panel(title = "Birthdays",
  
    titlePanel("How long until my birthday?"),
            
    "My children always want to know how many units until their birthday.
    Now they will know. I'm assuming 630am wakeup, maybe later I'll add code
    to let you select a different time.",
    
    sidebarLayout(
      sidebarPanel(
        h3("What date are we counting to?"),
        dateInput("birthday.date",
                  label = "Date (in Month-Day order)",
                  value = Sys.Date() + 1,
                  format = "mm-dd"),
        br(),
        h3("What units would we like to measure in?"),
        radioButtons("birthday.units", 
                     label = NULL,
                     choices = c("Months" = "months",
                                 "Weeks" = "weeks",
                                 "Days" = "days",
                                 "Hours" = "hours",
                                 "Minutes" = "minutes",
                                 "Seconds" = "seconds"),
                     selected = "days"
        ),
        br(),
        h3("What are we counting to?"),
        textInput("birthday.event",
                  label = NULL,
                  value = "Birthday"
        )
      ),
      
      mainPanel(
        h1(textOutput("time.dif"))
      )
    )
  ),
  #Mondays
  nav_panel(title = "Mondays",
  
    "One of my coworkers always lists the work days of the week in Mondays (1st monday, 2nd monday, etc). Now we can know in terms of other units as well. What is a Monday? It's any working day of the week. So we have to exclude (federal) holidays",
    
    #h1(verbatimTextOutput("test")),
    
    h1(textOutput("monday.week")),
    
    h2(textOutput("monday.month")),
    
    h3(textOutput("monday.year")),
    
    h4(textOutput("monday.greg")),
    
    h5(glue("Today is the MESMERIZINGLY, WILDLY, SPELLING BE-GONE CODE Monday of your life (static text until I figure this out, or decide to cut it)")),
    
    HTML('<input type="text" id="client_year" name="client_year" style="display: none;"> '),
    HTML('<input type="text" id="client_month" name="client_month" style="display: none;"> '),
    HTML('<input type="text" id="client_day" name="client_day" style="display: none;"> '),
    # HTML('<input type="text" id="client_time" name="client_time" style="display: none;"> '),
    
    
    tags$script('
    $(function() {
    var time_now = new Date()
    $("input#client_year").val(time_now.getFullYear())
    $("input#client_month").val(time_now.getMonth())
    $("input#client_day").val(time_now.getDate())
    });    
    ')
    
  ),
  
  nav_panel(title = "Shiny",
            
            "Because this app hasn't always been this glam",
            
            fluidPage(
              fluidRow(
                #useConfetti(),
                tags$style(HTML("#confetti-canvas {
                position:absolute;
                top:0;
                }")),
                column(width = 4, offset = 4,
                       div(
                         style = "display: flex; 
                         justify-content: center; align-items: center; height: 75vh;",
                         tags$button(id = "shine_shiny",
                            class = "btn action-button",
                            tags$img(src = "https://shiny.posit.co/images/shiny-solo.png",
                              height = "100px"
                )
                #textOutput("text"),
                #actionButton("deboop", "To Be Disabled")
            )
            )
              )
              )
            )
  )
)
)
