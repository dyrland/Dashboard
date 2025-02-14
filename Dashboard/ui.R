#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(bslib)
library(shinythemes)

# Define UI for application that draws a histogram
page_navbar(
  title = "Some apps for my people",
  theme = bs_theme(version = 4, bootswatch = "sketchy"),
  navbar_options = navbar_options(theme = "light"),
  navset_tab(
  #Dice ----
  nav_panel(title = "Dice",
    # Application title
    titlePanel("Old Faithful Geyser Data"),
    
    "My children Play Machi Koro (https://en.wikipedia.org/wiki/Machi_Koro) and this will help them sense which cards are more valuable",

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("distPlot")
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
