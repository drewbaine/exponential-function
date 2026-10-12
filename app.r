library(shiny)
library(ggplot2)
library(dplyr)
library(tibble)

ui <- fluidPage(

  titlePanel("Exponential Function"),

  sidebarLayout(
    
    sidebarPanel(
      selectInput(
        "choice",
        "Choose:",
        choices = c("A", "B", "C","D")
      ),    
    
    conditionalPanel(
      condition = "input.choice == 'A'",
      numericInput(
        inputId = "rate",
        label = "rate",
        value = 1
      ), 
      "No restrictions",
      numericInput(
        inputId = "max",
        label = "Maximum",
        value = 3
      )
    ),
    
    
    
    conditionalPanel(
      condition = "input.choice == 'B'",
      numericInput(
        inputId = "rate",
        label = "rate",
        value = 1
      ), 
      "No restrictions",
      hr(), 
      numericInput(
        inputId = "max",
        label = "Maximum",
        value = 3
      ),
      selectInput(
        inputId = "lowertail",
        label = "lower.tail",
        choices = c("TRUE", "FALSE"),
        selected = "FALSE"
      )
    ),
    
    
    
    conditionalPanel(
      condition = "input.choice == 'C'",
      numericInput(
        inputId = "probability",
        label = "probability",
        value = 1
      ), 
        numericInput(
        inputId = "rate",
        label = "rate",
        value = 1
      ), 
      "No restrictions",
      hr(), 
      numericInput(
        inputId = "max",
        label = "Maximum",
        value = 3
      ),
      selectInput(
        inputId = "lowertail",
        label = "lower.tail",
        choices = c("TRUE", "FALSE"),
        selected = "FALSE"
      )
    ) ,
    
    
    
    conditionalPanel(
      condition = "input.choice == 'D'",
      numericInput(
        inputId = "p",
        label = "p",
        value = 1
      ), 
      numericInput(
        inputId = "rate",
        label = "rate",
        value = 1
      ), 
      "No restrictions",
      hr(), 
      numericInput(
        inputId = "max",
        label = "Maximum",
        value = 3
      ),
      selectInput(
        inputId = "lowertail",
        label = "lower.tail",
        choices = c("TRUE", "FALSE"),
        selected = "FALSE"
      )
    )     
    ),
      
    mainPanel(
      plotOutput("distPlot")
    )
  )
)


server <- function(input, output) {

  output$distPlot <- renderPlot({
    
    if (input$choice == "A") {
      
      tibble(
        x = seq(from = 0, to = input$max, length = 500),
        Density = dexp(x, rate = input$rate)
      ) |> 
        ggplot(aes(x = x, y = Density)) + 
        geom_line() + 
        theme_bw()
      
    } else if (input$choice == "B") {
      
      tibble(
        x = seq(from = 0, to = input$max, length = 500),
        Probability = pexp(
          x,
          rate = input$rate,
          lower.tail = as.logical(input$lowertail)
        )
      ) |> 
        ggplot(aes(x = x, y = Probability)) + 
        geom_line() + 
        theme_bw()
      
    } else if (input$choice == "C") {
      rate <- input$rate
      p <- input$probability
      lower_tail <- as.logical(input$lowertail)
      max_x <- input$max
      
      # Calculate the quantile cutoff
      cutoff <- qexp(p, rate = rate, lower.tail = lower_tail)
      
      # Create density data
      df <- tibble(
        x = seq(from = 0, to = max_x, length.out = 500),
        y = dexp(x, rate = rate)
      )
      
      # Shade the appropriate tail
      shade_df <- if (lower_tail) {
        filter(df, x <= cutoff)
      } else {
        filter(df, x >= cutoff)
      }
      
      ggplot(df, aes(x = x, y = y)) +
        geom_area(
          data = shade_df,
          fill = "skyblue",
          alpha = 0.6
        ) +
        geom_line(linewidth = 1) +
        geom_vline(
          xintercept = cutoff,
          linetype = "dashed"
        ) +
        labs(
          title = "Exponential Distribution",
          subtitle = paste(
            "Rate =", rate,
            "| Probability =", p,
            "| Lower tail =", lower_tail
          ),
          x = "Value",
          y = "Density"
        ) +
        theme_minimal()
      
    } else if (input$choice == "D") {
      
      tibble(
        x = seq(from = 0, to = input$max, length = 500),
        Probability = pexp(
          x,
          rate = input$rate,
          lower.tail = as.logical(input$lowertail)
        )
      ) |> 
        ggplot(aes(x = x, y = Probability)) + 
        geom_line() + 
        theme_bw()
      
    }
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
