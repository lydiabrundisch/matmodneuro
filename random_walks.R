.libPaths("C:/Program files/R/win-library/4.1")
library(shiny)
library(ggplot2)
library(gganimate)
# library(transformr)
library(tidyr)

walker = function(p_step_right, step_size, time_interval, reps) {
  walk = data.frame(time = 0:time_interval)
  for (i in 1:reps+1) {
    walk[2:(time_interval+1), i] = ifelse(rbinom(time_interval, 1, p_step_right), step_size, -step_size)
  }
  walk[1, ] = rep(0, reps+1)
  for (i in 1:reps+1) {
    walk[, i] = cumsum(walk[, i])
  }
  walk = walk %>% pivot_longer(1:reps+1)
  return(walk)
}

ui <- fluidPage(
  headerPanel("Random walk"),
  sidebarLayout(
    mainPanel(
      plotOutput("walks")
    ),
    sidebarPanel(fluidRow(
        fluidRow(
          column(
            4,
            numericInput(
              inputId = "p_step_right",
              label = "Probabilty of a right step:",
              value = 0.5,
              min = 0,
              max = 1,
              step = 0.1
            ),
            numericInput(
              inputId = "step_size",
              label = "Step size:",
              value = 1,
              min = 1,
              max = 5,
              step = 1
            ),
            numericInput(
              inputId = "time_interval",
              label = "Observation interval:",
              value = 50,
              min = 50,
              max = 500,
              step = 50
            ),
            numericInput(
              inputId = "reps",
              label = "Repetitions:",
              value = 1,
              min = 1,
              max = 10,
              step = 1
            )
          )
        )
      )
    )
  )
)

server <- function(input, output) {
  output$walks = renderImage({
    w = walker(p_step_right = input$p_step_right, step_size = input$step_size, time_interval = input$time_interval, reps = input$reps)
    p1 = ggplot(w, aes(time, value, group=name, color=name)) + 
      geom_line() +
      geom_point() +
      transition_reveal(time)
    animate(p1, height = 250, width = 9500)
    anim_save("outfile.gif")
    list(src = "outfile.gif",
         contentType = 'image/gif')
    }, deleteFile = TRUE)
}

shinyApp(ui, server, options = list(launch.browser = TRUE))
