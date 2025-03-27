library(shiny)
library(xkcd)
library(tidyverse)
library(lubridate)
library(english)
library(shinyjs)

dir.create('~/.fonts')
file.copy("www/xkcd.ttf", "~/.fonts")
system('fc-cache -f ~/.fonts')

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
        xlab("Number Rolled") +
        ylab("Count of Number Rolled")
      
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
        xlab("Number Rolled") +
        ylab("Proportion of Number Rolled")
    }
  })
  
  #some days to birthday go here
  
  user.date <- reactive({ymd_hms(
                          glue(
                            if(Sys.time() <= input$birthday.date) {
                              year(Sys.Date()) } else {
                              year(Sys.Date()) + 1 },
                               as.character(month(input$birthday.date, label = TRUE)),
                               day(input$birthday.date),
                               " 06:30:00"
                          )
  )
  })
  
  output$time.dif <- renderText({
    invalidateLater(1000, session)
    # glue(user.date()
    # )
    
    glue("There are {num} {units} until your {event}! Congratulations! Happy Waiting!",
         num = round(
                time_length(int_flip(user.date() %--% Sys.time()),
                            unit = input$birthday.units),
                digits = 0),
         units = input$birthday.units,
         event = input$birthday.event
    )
  })
  #Mondays go here!
    #mainly because we should do calculations serve side even if they are static
  
  #maybe we should just make a calendar since 15 Oct 1582 to today
  #column for date (obvi), then wday, mday, yday()
  #we could then use group by and filters to quickly get to our year
  
  session$userData$time <- reactive({
                                    ymd(
                                      paste(input$client_year,
                                            as.numeric(input$client_month) + 1,
                                            input$client_day
                                      )
                                    )
  })
   

  
  data.gregor <- reactive({
    date.data.rv <- tibble(date = seq.Date(ymd(15821015),
                                           session$userData$time(), by = "day")) |> 
    mutate(year = year(date),
           month = month(date),
           day = day(date),
           wday = wday(date, label = TRUE),
           true_monday = wday %in% "Mon",
           monday = case_when(year >= 1870 & month == 1 & day == 1 ~ FALSE,
                                #Jan 1
                              year >= 1983 & month == 1 &
                                day >= 15 & day <= 21 & wday %in% "Mon" ~ FALSE,
                                #MLK
                              year >= 1879 & month == 2 & day == 22 &
                                year < 1971 ~ FALSE,
                                #wash bday
                              year >= 1971 & month == 2 &
                                day >= 15 & day <= 21 & wday %in% "Mon" ~ FALSE,
                                #prez day
                              year >= 1968 & month == 5 &
                                day >= 25 & day <= 31 & wday %in% "Mon" ~ FALSE,
                                #memorial day
                              year >= 2021 & month == 6 & day == 17 ~ FALSE,
                                #juneteenth
                              year >= 1870 & month == 7 & day == 4 ~ FALSE,
                                #july 4th
                              year >= 1894 & month == 9 &
                                day >= 01 & day <= 07 & wday %in% "Mon" ~ FALSE,
                                #labor day
                              year >= 1971 & month == 10 &
                                day >= 08 & day <= 14 & wday %in% "Mon" ~ FALSE,
                                #columbus day/indifenous peopls day
                              year >= 1938 & month == 11 & day == 11 ~ FALSE,
                                #vets day
                              year >= 1870 & month == 11 &
                                day >= 22 & day <= 28 & wday %in% "Thu" ~ FALSE,
                              #thanksgiving - black friday not a holiday
                              year >= 1870 & month == 12 & day == 25 ~ FALSE,
                                #xmas,
                              wday %in% c("Sat", "Sun") ~ FALSE,
                              .default = TRUE)
    )
  })
  
  data.week <- reactive({
    data.gregor() |> 
      filter(date >= floor_date(session$userData$time(), unit = "week")
      ) |> pull(monday) |> sum()
  })
  
  data.month <- reactive({
    data.gregor() |> 
      filter(date >= floor_date(session$userData$time(), unit = "month")
      ) |> pull(monday) |> sum()
  })
  
  data.year <- reactive({
    data.gregor() |> 
      filter(date >= floor_date(session$userData$time(), unit = "year")
      ) |> pull(monday) |> sum()
  })
  
  
  # output$test <- renderPrint({data.week()
  # })
  
  output$monday.week  <- renderText({
    if(wday(session$userData$time(), label = TRUE) %in% c("Sat", "Sun")){
    return("Today is NOT Monday!")
    } else {
      return(glue("Today is the {num} Monday of the week.",
                num = ordinal(data.week())
      )
      )
    }
  })
  
  output$monday.month <- renderText({
    if(wday(session$userData$time(), label = TRUE) %in% c("Sat", "Sun")){
      return("Today is NOT Monday!")
    } else {
      return(glue("Today is the {num} Monday of the month.",
                  num = ordinal(data.month())
      )
      )
    }
  })
  output$monday.year  <- renderText({
    if(wday(session$userData$time(), label = TRUE) %in% c("Sat", "Sun")){
      return("Today is NOT Monday!")
    } else {
      return(glue("Today is the {num} Monday of the year.",
                  num = ordinal(data.year())
      )
      )
    }
  })
  output$monday.greg  <- renderText({
    if(wday(session$userData$time(), label = TRUE) %in% c("Sat", "Sun")){
      return("Today is NOT Monday!")
    } else {
      return(glue("Today is the {num} Monday of the Gregorian \\
                               Calendar, established 1586 Oct 15, a Friday \\
                               (and the first Monday).",
                    num = data.gregor() |>
                      pull(monday) |>
                      sum() |> 
                      ordinal()
      )
      )
    }
  })
  
  #shiny things
  
  observeEvent(input$shine_shiny, {

    #sendConfetti()
    insertUI(selector = "#shine_shiny",
             where = "afterEnd",
             #id = "song",
             ui = tags$audio(src = "Shiny_From_Moana.mp3",
                             type = "audio/mp3",
                             autoplay = TRUE,
                             controls = NA,
                             style="display:none;")
    )
    
    
    shinyjs::disable("shine_shiny")
    shinyjs::delay(28000, js$startConfettiInner())
    shinyjs::delay(205000, shinyjs::enable("shine_shiny"))
    shinyjs::delay(205000, js$stopConfettiInner())
    #here is some code about confetti
    #https://github.com/CodingTigerTang/shiny_spinner/blob/main/app.R
})
  

  # observeEvent(time > (Sys.time() + 5), {
  #   shinyjs::enable("shine_shiny")
  # })
  # 
  
  # out <- reactiveVal("Nothing")
  # 
  # observeEvent(input$deboop, {
  #   out("hmmm")
  # })
  # 
  # output$text <- renderText({out()})
               
}
  