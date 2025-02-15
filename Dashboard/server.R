library(shiny)
library(xkcd)
library(tidyverse)

function(input, output, session) {
  #this is ugly but effective
  #would accept help!
  d1 <- reactiveValues(size = 1)
  observeEvent(input$dice.go1,    {d1$size <- 1})
  observeEvent(input$dice.go10,   {d1$size <- 10})
  observeEvent(input$dice.go100,  {d1$size <- 100})
  observeEvent(input$dice.go1000, {d1$size <- 1000})
  observeEvent(input$dice.custom_button, {d1$size <- input$dice.custom_number})
  eventReactive(input$dice.custom_number, {d1$size <- input$dice.custom_number})
  
  output$dice.ui <- renderUI({
    actionButton("dice.custom_button",
                 renderText({paste(input$dice.custom_number, "times")
                 })
    )
  })
  
  dice.dat <- reactive({
    dice.dat.rv <-
          tibble(x = switch(input$dice.dice_amount,
                            one = sample(1:6, size = d1$size, replace = TRUE),
                            two = sample(1:6, size = d1$size, replace = TRUE) +
                                  sample(1:6, size = d1$size, replace = TRUE)
          )
          ) |>
            group_by(x) |>
            summarize(y = n()) |>
            ungroup()
  }) 
  
  x_range_max <- reactive({
    case_when(input$dice.dice_amount == "one" ~ 6.5,
              input$dice.dice_amount == "two" ~ 12.5,
              .default = 6.5)
  })
  
  output$dice.histogram <- renderPlot({
    
    if(input$dice.hist_type == "counts"){
      
      ggplot() +
        xkcdrect(aes(xmin = x - 0.5,
                     xmax = x + 0.5,
                     ymin = 0,
                     ymax = y),
                 data = dice.dat(),
                 fill = input$dice.color
        ) +
        xkcdaxis(xrange = c(0.5, x_range_max()),
                 yrange = c(0, max(dice.dat()$y))
        ) +
        scale_x_continuous(breaks = switch(input$dice.dice_amount,
                                           one = 1:6,
                                           two = 1:12),
                           limits = switch(input$dice.dice_amount,
                                           one = c(0, 6.75),
                                           two = c(0, 12.75)
                           )
        ) +
        scale_y_continuous(breaks = ~round(unique(pretty(.)))) +
        xlab("Number Rolled")
    } else if(input$dice.hist_type == "percent"){
      
      ggplot() +
        xkcdrect(aes(xmin = x - 0.5,
                     xmax = x + 0.5,
                     ymin = 0,
                     ymax = y / sum(y)),
                 data = dice.dat(),
                 fill = input$dice.color
        ) +
        xkcdaxis(xrange = c(0.5, x_range_max()),
                 yrange = c(0, max(dice.dat()$y / sum(dice.dat()$y)))
        ) +
        scale_x_continuous(breaks = switch(input$dice.dice_amount,
                                          one = 1:6,
                                          two = 1:12),
                          limits = switch(input$dice.dice_amount,
                                          one = c(0, 6.75),
                                          two = c(0, 12.75)
                          )
        ) +
        scale_y_continuous(labels = scales::percent) +
        xlab("Number Rolled")
    }
  })
}
  