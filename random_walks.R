.libPaths("C:/Program files/R/win-library/4.1")
library(shiny)
library(ggplot2)
library(gridExtra)

walker = function(p_step,
                  step_size,
                  time_interval,
                  reps) {
  walk = data.frame(time = 0:time_interval)
  for (i in 1:reps + 1) {
    walk[2:(time_interval + 1), i] = ifelse(rbinom(time_interval, 1, p_step),
                                            step_size,
                                            -step_size)
  }
  walk[1,] = rep(0, reps + 1)
  for (i in 1:reps + 1) {
    walk[, i] = cumsum(walk[, i])
  }
  walk = walk %>% pivot_longer(1:reps + 1)
  colnames(walk) = c("time", "particle", "position")
  return(walk)
}

ui = fluidPage(headerPanel("Random walks"),
               sidebarLayout(
                 sidebarPanel(
                   p(
                     withMathJax(
                       "Imagine a particle moving in 1D-space over time. At each
                      time step, it may either move one step forwards or hit
                      another particle, change directions and move one step
                      backwards,
                      $$\\delta = \\pm1$$
                      with a probabilty to step forwards and backwards that is
                      assumed to be equal and independent of its position for 
                      now,
                      $$p(\\delta = +1) = p(\\delta = -1) = 0.5$$
                      The position of the particle at a point in time will
                      fluctuate back and forth, but it will always be exactly
                      one step away from its previous position,
                      $$position(t) = position(t-1) + \\delta$$
                      Over time, the particle may move further away from its
                      origin. In simulating an ensemble of particles, we can
                      show that the mean absolute distance to of a particle to
                      its origin increases proportionally to the time that has
                      passed,
                      $$MSD \\propto t$$"
                     ),
                     em(
                       "Note: To get the absolute distance, the distance is
                        squared, hence this measure is called mean square
                        displacement. This concept corresponds to the position's
                        sample variance."
                     ),
                     numericInput(
                       inputId = "time_interval",
                       label = "Observation interval:",
                       value = 1,
                       min = 1,
                       max = 1000,
                       step = 1
                     ),
                     p("This relation no longer holds true if the stepping 
                       probabilties are inequal or depend on a particle's 
                       current position. An MSD that is larger than expected 
                       might indicate that particles drift away from their 
                       origin. - example -%%"),
                     numericInput(
                       inputId = "p_step",
                       label = "Probabilty of an upwards step:",
                       value = 0.5,
                       min = 0,
                       max = 1,
                       step = 0.1
                     ),
                     p("The particle might have different step sizes in each 
                       direction - example - - MSD indicator -"),
                     p("The particle might also be pulled towards its origin or 
                       repelled from it - example - - MSD indicator -")
                   )
                 ),
                 mainPanel(plotOutput("walks"),
                           plotOutput("msd")),
               )
            )

server <- function(input, output) {
  output$walks = renderPlot({
    w = walker(
      p_step = input$p_step,
      step_size = 1,
      time_interval = input$time_interval,
      reps = 100
    )
    p1 = ggplot(w, aes(time, position, group = particle)) +
      geom_line(alpha = 0.05) +
      geom_point(alpha = 0.05) +
      theme(legend.position = "none")
    p2 =  ggplot(w[which(w$time == input$time_interval), ], aes(position)) +
      geom_histogram(binwidth = 1) + xlim(-100, 100) + ylim(0, 100) + 
      ylab("frequency")
    v = aggregate(w$position, list(w$position, w$time), length)
    v$msd = v$Group.1 ^ 2 * v$x
    test = aggregate(v$msd, list(v$Group.2), sum)
    p3 = ggplot(test, aes(Group.1, x)) + geom_line() + 
      xlab("position") + ylab("MSD")
    print(grid.arrange(p1, p2, p3, nrow = 3))
  })
}

shinyApp(ui, server, options = list(launch.browser = TRUE))
