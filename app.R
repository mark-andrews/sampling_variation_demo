library(shiny)
library(tidyverse)
library(patchwork)
library(latex2exp)

theme_set(theme_classic())

ui <- fluidPage(
  withMathJax(),

  # Application title
  titlePanel("Sampling variation demo"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      helpText(
        "This is demonstration of the variation of values of a statistic in samples from a population.",
        br(),
        br(),
        "Imagine we have a box with a large number of coins, some of which are silver and the remainder are gold.
      Using the slider
      below we can set the proportion of coins that are gold.
      We can then simulate what would happen if we repeatedly draw samples (with replacement) from this box
      and count the number of gold coins in each sample.",
        br(),
        "We can use the second slider to set the number of coins in each sample, and we will denote this number by \\(n\\).
      Each time we draw a sample, we count the number of gold coins, which we will denote by \\(m\\).
      We can use the last slider to set the number of times we repeat
      this sampling procedure, which we will denote by \\(N\\).",
        br(),
        br(),
        "The plots to the right show the following:",
        br(),
        "A) The value of \\(m\\) in each of the \\(N\\) repetitions",
        br(),
        "B) The distribution of the values of \\(m\\) over of the \\(N\\) repetitions.
      This is the known as the ",
        em("sampling distribution,"),
        " albeit one calculated from a simulation.",
        br(),
        "C) The distribution of the values of \\(m\\) as \\(N \\to \\infty \\). We use statistical theory to calculate this distribution.
      This is true, or theoretical, sampling distribution.",
        br(),
        br()
      ),
      sliderInput("theta",
        "Proportion of gold coins in the box:",
        min = 0.10,
        max = 0.90,
        step = 0.05,
        value = 0.50
      ),
      sliderInput("n",
        "Number of coins we sample (with replacement) from box (\\(n\\)):",
        min = 10,
        max = 100,
        step = 10,
        value = 20
      ),
      sliderInput("N",
        "Number of repetitions of the samples (\\(N\\)) :",
        min = 50,
        max = 500,
        step = 50,
        value = 100
      )
    ),

    # Show a plot of the generated distribution
    mainPanel(
      width = 5,
      plotOutput("distPlot", height = "800px")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  dataset <- reactive({
    tibble(
      repetition = seq(input$N),
      m = rbinom(n = input$N, size = input$n, prob = input$theta)
    )
  })

  output$distPlot <- renderPlot({
    p1 <- dataset() %>%
      ggplot(aes(x = repetition, y = m)) +
      geom_line(colour = "grey50", alpha = 0.25, linetype = "dashed") +
      geom_point() +
      scale_y_continuous(breaks = seq(0, input$n)) +
      ggtitle(TeX("Traceplot of value of $m$ on each of the $N$ repetitions"))

    p2 <- dataset() %>%
      ggplot(aes(x = m)) +
      scale_x_continuous(
        breaks = seq(0,
          input$n,
          by = (function(n) ifelse(n > 20, 10, ifelse(n > 10, 5, 1)))(input$n)
        ),
        limits = c(0, input$n)
      ) +
      geom_bar() +
      ggtitle(TeX("Distribution of values of $m$ in the $N$ repetitions."))

    p3 <- tibble(
      x = seq(0, input$n),
      p = dbinom(x, size = input$n, prob = input$theta)
    ) %>% ggplot(aes(x = x, y = p)) +
      geom_col() +
      scale_x_continuous(
        breaks = seq(0,
                     input$n,
                     by = (function(n) ifelse(n > 20, 10, ifelse(n > 10, 5, 1)))(input$n)
        ),
        limits = c(0, input$n)
      ) +
      ggtitle(TeX("Distribution of values of $m$ when $N \\to\\ \\infty$."))

    p1 / (p2 + p3) + plot_annotation(tag_levels = "A")
  })
}

# Run the application
shinyApp(ui = ui, server = server)
