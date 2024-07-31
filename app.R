library(shiny)
library(tidyverse)
library(patchwork)
library(latex2exp)

theme_set(theme_classic())

NBOX <- 100
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
      "Imagine we have a box of 100 coins.
      Some of these 100 coins are gold and the remainder are silver.
      Using the slider
      below we can set the number of these 100 coins that are gold (so the
      remainder are silver).
      We can then simulate what would happen if we repeatedly draw samples (with replacement) from this box 
      and count the number of gold coins in each sample.",
      br(),
      "We can use the second slider to set the number of coins in each sample, and we will denote this number by \\(n\\).
      Each time we draw a sample, we count the number of gold coins, which we will denote by \\(m\\).
      We can use the last slider to set the number of times we repeat
      this process of sampling \\(n\\) coins from the box and counting the number of gold coins (\\(m\\)).
      We will use \\(N\\) to denote the number of repetitions.",
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
        "Number of gold coins in the box of 100 coins:",
        min = 10,
        max = 90,
        step = 5,
        value = 50
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
      m = rbinom(n = input$N, size = input$n, prob = input$theta / NBOX)
    )
  })

  output$distPlot <- renderPlot({
    p1 <- dataset() %>%
      ggplot(aes(x = repetition, y = m)) +
      geom_line(colour = "grey50", alpha = 0.25, linetype = "dashed") +
      geom_point() +
      theme_classic() +
      ggtitle(TeX('Traceplot of value of $m$ on each of the $N$ repetitions'))
    
    p2 <- dataset() %>% 
      ggplot(aes(x = m)) +
      scale_x_continuous(breaks = seq(0, input$n), limits = c(0, input$n)) +
      geom_bar() +
      ggtitle(TeX('Distribution of values of $m$ in the $N$ repetitions.'))
    
    p3 <- tibble(x = seq(0, input$n),
           p = dbinom(x, size = input$n, prob = input$theta / NBOX)
    ) %>% ggplot(aes(x = x, y = p)) + geom_col() +
    ggtitle(TeX('Distribution of values of $m$ when $N \\to\\ \\infty$.'))  
    
    p1 / (p2 + p3) + plot_annotation(tag_levels = 'A')
  })
}

# Run the application
shinyApp(ui = ui, server = server)
