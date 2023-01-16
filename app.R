.libPaths("C:/Program files/R/win-library/4.1")
library(shiny)
library(lattice)
library(ggplot2)
library(RColorBrewer)
library(gridExtra)
library(bslib)
library(thematic)

stimulus = function(rotation,
                    height,
                    width,
                    x_offset,
                    y_offset) {
  # x-offset {0:8-width}
  # y-offset {0:8-height}
  s = matrix(data = 0,
             nrow = 8,
             ncol = 8)
  if (rotation == 0) {
    s[(x_offset + 1):(width + x_offset), (y_offset + 1):(height + y_offset)] = 1
  }
  else {
    for (i in 1:width) {
      s[(x_offset + i), (y_offset + i):(y_offset + i + height - 1)] = 1
    }
  }
  return(s)
}
receptive_field = function(center_height,
                           center_width,
                           center_x_offset,
                           center_y_offset,
                           surround_thickness,
                           center_polarization,
                           surround_strength) {
  k = matrix(data = 0,
             nrow = 8,
             ncol = 8)
  k[(center_x_offset + 1):(center_width + center_x_offset), (center_y_offset +
                                                                1):(center_height + center_y_offset)] = center_polarization
  k[(center_x_offset - surround_thickness + 1):(center_x_offset + center_width +
                                                  surround_thickness), (center_height + center_y_offset + 1):(center_height +
                                                                                                               center_y_offset + surround_thickness)] = -center_polarization / surround_strength
  k[(center_x_offset - surround_thickness + 1):center_x_offset, (center_y_offset -
                                                                   surround_thickness + 1):(center_height + center_y_offset + surround_thickness)] = -center_polarization /
    surround_strength
  k[(center_x_offset + center_width + 1):(center_x_offset + center_width +
                                             surround_thickness), (center_y_offset - surround_thickness + 1):(center_height +
                                                                                                                center_y_offset + surround_thickness)] = -center_polarization / surround_strength
  k[(center_x_offset - surround_thickness + 1):(center_x_offset + center_width +
                                                  surround_thickness), (center_y_offset - surround_thickness + 1):center_y_offset] = -center_polarization /
    surround_strength
  return(k)
}

ui <- fluidPage(
  tags$style(
    type = 'text/css',
    ".selectize-input { width: 75px; } .selectize-dropdown { width: 75px; } input[type=\"number\"] { width: 60px; } h2 { font-size: 2em;  }"
  ),
  
  headerPanel("Receptive fields model"),
  sidebarLayout(
    mainPanel(
      plotOutput("activation"),
      plotOutput("pointprocess"),
      div(withMathJax("$$\\rho = \\sum \\delta (t - t_n)$$")),
      div("$$\\mathbb{N} = \\int \\rho$$")
    ),
    sidebarPanel(fluidRow(
        h2("Stimulus parameters"),
        fluidRow(
          column(
            4,
            numericInput(
              inputId = "height",
              label = "Height:",
              value = 8,
              min = 1,
              max = 8,
              step = 1
            ),
            br(),
            numericInput(
              inputId = "x_offset",
              label = "x-Offset:",
              value = 3,
              min = 0,
              max = 8,
              step = 1
            )
          ),
          column(
            4,
            numericInput(
              inputId = "width",
              label = "Width:",
              value = 1,
              min = 1,
              max = 8,
              step = 1
            ),
            br(),
            numericInput(
              inputId = "y_offset",
              label = "y-Offset:",
              value = 0,
              min = 0,
              max = 8,
              step = 1
            )
          ),
          column(
            4,
            numericInput(
              inputId = "rotation",
              label = "Rotation:",
              value = 0,
              min = 0,
              max = 45,
              step = 45
            )
          )
        
      ),
        h2("Receptive field parameters"),
        fluidRow(
          column(
            4,
            numericInput(
              inputId = "center_height",
              label = "Height:",
              value = 2,
              min = 1,
              max = 8,
              step = 1
            ),
            br(),
            numericInput(
              inputId = "center_x_offset",
              label = "x-Offset:",
              value = 3,
              min = 0,
              max = 8,
              step = 1
            ),
            br(),
            numericInput(
              inputId = "center_polarization",
              label = "Center polarization:",
              value = 1,
              min = -1,
              max = 1,
              step = 2
            )
          ),
          column(
            4,
            numericInput(
              inputId = "center_width",
              label = "Width:",
              value = 2,
              min = 1,
              max = 8,
              step = 1
            ),
            br(),
            numericInput(
              inputId = "center_y_offset",
              label = "y-Offset:",
              value = 3,
              min = 0,
              max = 8,
              step = 1
            ),
            br(),
            numericInput(
              inputId = "surround_thickness",
              label = "Surround thickness:",
              value = 1,
              min = 1,
              max = 3,
              step = 1
            )
          ),
          column(
            4,
            numericInput(
              inputId = "surround_strength",
              label = "Surround strength:",
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
  output$activation = renderPlot({
    s = stimulus(
      rotation = input$rotation,
      height = input$height,
      width = input$width,
      x_offset = input$x_offset,
      y_offset = input$y_offset
    )
    p1 = levelplot(
      s,
      col.regions = brewer.pal(n = 11, name = "RdBu"),
      at = seq(-1.1, 1.1, 0.2),
      main = list("Stimulus S(x,y)", side = 1, line = 0.5),
      colorkey = list(space = "bottom"),
      xlab = NULL,
      ylab = NULL
    )
    k = receptive_field(
      center_height = input$center_height,
      center_width = input$center_width,
      center_x_offset = input$center_x_offset,
      center_y_offset = input$center_y_offset,
      surround_thickness = input$surround_thickness,
      center_polarization = input$center_polarization,
      surround_strength = input$surround_strength
    )
    p2 = levelplot(
      k,
      col.regions = brewer.pal(n = 11, name = "RdBu"),
      at = seq(-1.1, 1.1, 0.2),
      main = list("Receptive field K(x,y)", side = 1, line = 0.5),
      colorkey = list(space = "bottom"),
      xlab = NULL,
      ylab = NULL
    )
    activation = s * k
    p3 = levelplot(
      activation,
      col.regions = brewer.pal(n = 11, name = "RdBu"),
      at = seq(-1.1, 1.1, 0.2),
      main = paste0("Activation O(S,K) = ", sum(activation)),
      colorkey = list(space = "bottom"),
      xlab = NULL,
      ylab = NULL
    )
    print(grid.arrange(p1, p2, p3, ncol = 3))
  })
  output$pointprocess = renderPlot({
    s = stimulus(
      rotation = input$rotation,
      height = input$height,
      width = input$width,
      x_offset = input$x_offset,
      y_offset = input$y_offset
    )
    k = receptive_field(
      center_height = input$center_height,
      center_width = input$center_width,
      center_x_offset = input$center_x_offset,
      center_y_offset = input$center_y_offset,
      surround_thickness = input$surround_thickness,
      center_polarization = input$center_polarization,
      surround_strength = input$surround_strength
    )
    o = sum(s * k)
    rho = data.frame(t = rep(1:100, 3),
                     trial = c(rep(1, 100), rep(2, 100), rep(3, 100)),
                     spike = rbinom(300, size = 1, prob = o / 40 + 0.08))
    p3 = ggplot() + geom_point(data=rho[which(rho$spike>0),], pch = "|", cex = 7, aes(t, trial)) + theme_classic() +
      scale_y_continuous(breaks = 1:3)
    p4 = ggplot() + geom_line(data=rho, aes(x = t, y = cumsum(spike), group = trial)) + 
      theme_classic() + theme(axis.text.y = element_blank()) + ylab("N(t, trial)") +
      scale_y_continuous(breaks = c(0, sum(rho$spike[rho$trial == 1]), sum(rho$spike[rho$trial == 1 | rho$trial == 2])))
    print(grid.arrange(p3, p4, nrow = 2))
  }, height = 300)
}

shinyApp(ui, server)
