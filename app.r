
library(shiny)
library(ggplot2)
library(tibble)

ui <- fluidPage(
  
  titlePanel("Exponential Distribution Explorer"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      selectInput(
        "choice",
        "Choose a function:",
        choices = c(
          "Probability Density Function" = "PDF",
          "Cumulative Distribution Function" = "CDF",
          "Tail Probability" = "Tail",
          "Quantile Function" = "QF"
        )
      ),
      
      sliderInput(
        "rate",
        "Rate parameter (lambda):",
        min = 0.1,
        max = 8,
        value = 1,
        step = 0.1
      ),
      
      helpText("Parameter space: lambda > 0"),
      
      conditionalPanel(
        condition = "input.choice != 'QF'",
        
        sliderInput(
          "time",
          "Select waiting time (x):",
          min = 0,
          max = 10,
          value = 2,
          step = 0.1
        ),
        
        numericInput(
          "max",
          "Maximum x-value:",
          value = 10,
          min = 0.1
        )
      ),
      
      conditionalPanel(
        condition = "input.choice == 'QF'",
        
        sliderInput(
          "prob",
          "Select cumulative probability (p):",
          min = 0.01,
          max = 0.99,
          value = 0.90,
          step = 0.01
        )
      ),
      
      hr(),
      
      h4("Distribution Statistics"),
      verbatimTextOutput("stats"),
      
      h4("Calculated Probability"),
      verbatimTextOutput("result"),
      
      actionButton("reset", "Reset")
    ),
    
    mainPanel(
      
      plotOutput("distPlot", height = "450px"),
      
      hr(),
      
      h4("Interpretation"),
      textOutput("interpretation")
    )
  )
)

  server <- function(input, output, session) {
    
    observeEvent(input$max, {
      
      req(input$max)
      req(input$max > 0)
      
      updateSliderInput(
        session,
        inputId = "time",
        min = 0,
        max = input$max,
        value = min(input$time, input$max)
      )
      
    })
    
    observeEvent(input$reset, {
      
      updateSelectInput(
        session,
        "choice",
        selected = "PDF"
      )
      
      updateSliderInput(
        session,
        "rate",
        value = 1
      )
      
      updateSliderInput(
        session,
        "time",
        min = 0,
        max = 10,
        value = 2
      )
      
      updateNumericInput(
        session,
        "max",
        value = 10
      )
      
      updateSliderInput(
        session,
        "prob",
        value = 0.90
      )
      
    })
    
    
  output$stats <- renderText({
    
    paste0(
      "Mean = ", round(1 / input$rate, 4),
      "\nVariance = ", round(1 / input$rate^2, 4)
    )
  })
  
  output$distPlot <- renderPlot({
    
    req(input$rate)
    
    if (input$choice != "QF") {
      req(input$max, input$time)
      validate(need(input$max > 0,
                    "Maximum x must be positive."))
    }
    
    if (input$choice == "PDF") {
      
      exp_data <- tibble(
        x = seq(0, input$max, length.out = 500),
        density = dexp(x, rate = input$rate)
      )
      
      ggplot(exp_data, aes(x, density)) +
        geom_line(color = "steelblue", linewidth = 1.2) +
        geom_vline(
          xintercept = input$time,
          linetype = "dashed",
          color = "red"
        ) +
        geom_point(
          data = tibble(
            x = input$time,
            density = dexp(input$time, input$rate)
          ),
          color = "red",
          size = 3
        ) +
        labs(
          title = "Exponential Probability Density Function",
          x = "Waiting Time (x)",
          y = "Density"
        ) +
        theme_minimal()
      
    } else if (input$choice == "CDF") {
      
      exp_data <- tibble(
        x = seq(0, input$max, length.out = 500),
        density = dexp(x, rate = input$rate)
      )
      
      shaded <- exp_data[
        exp_data$x <= input$time, ,
        drop = FALSE
      ]
      
      ggplot(exp_data, aes(x, density)) +
        geom_area(
          data = shaded,
          fill = "seagreen3",
          alpha = 0.5
        ) +
        geom_line(
          color = "darkgreen",
          linewidth = 1.2
        ) +
        geom_vline(
          xintercept = input$time,
          linetype = "dashed"
        ) +
        labs(
          title = "CDF: P(X <= x)",
          subtitle = paste(
            "Probability =",
            round(pexp(input$time, input$rate), 4)
          ),
          x = "Waiting Time (x)",
          y = "Probability Density"
        ) +
        theme_minimal()
      
    } else if (input$choice == "Tail") {
      
      exp_data <- tibble(
        x = seq(0, input$max, length.out = 500),
        density = dexp(x, rate = input$rate)
      )
      
      shaded <- exp_data[
        exp_data$x >= input$time, ,
        drop = FALSE
      ]
      
      ggplot(exp_data, aes(x, density)) +
        geom_area(
          data = shaded,
          fill = "orange",
          alpha = 0.5
        ) +
        geom_line(
          color = "darkorange",
          linewidth = 1.2
        ) +
        geom_vline(
          xintercept = input$time,
          linetype = "dashed"
        ) +
        labs(
          title = "Tail Probability: P(X > x)",
          subtitle = paste(
            "Probability =",
            round(
              pexp(input$time,
                   input$rate,
                   lower.tail = FALSE),
              4
            )
          ),
          x = "Waiting Time (x)",
          y = "Probability Density"
        ) +
        theme_minimal()
      
    } else if (input$choice == "QF") {
      
      exp_data <- tibble(
        p = seq(0, 0.999, length.out = 500),
        quantile = qexp(p, rate = input$rate)
      )
      
      selected_q <- qexp(input$prob, input$rate)
      
      ggplot(exp_data, aes(p, quantile)) +
        geom_line(
          color = "purple",
          linewidth = 1.2
        ) +
        geom_point(
          data = tibble(
            p = input$prob,
            quantile = selected_q
          ),
          color = "red",
          size = 3
        ) +
        geom_vline(
          xintercept = input$prob,
          linetype = "dashed"
        ) +
        geom_hline(
          yintercept = selected_q,
          linetype = "dashed"
        ) +
        labs(
          title = "Quantile Function",
          subtitle = paste(
            "Selected quantile =",
            round(selected_q, 4)
          ),
          x = "Cumulative Probability (p)",
          y = "Waiting Time"
        ) +
        theme_minimal()
    }
  })
  
  output$result <- renderText({
    
    if (input$choice == "PDF") {
      
      paste(
        "Density at x =", input$time, ":",
        round(dexp(input$time, input$rate), 4)
      )
      
    } else if (input$choice == "CDF") {
      
      paste(
        "P(X <=", input$time, ") =",
        round(pexp(input$time, input$rate), 4)
      )
      
    } else if (input$choice == "Tail") {
      
      paste(
        "P(X >", input$time, ") =",
        round(
          pexp(input$time, input$rate,
               lower.tail = FALSE),
          4
        )
      )
      
    } else if (input$choice == "QF") {
      
      paste(
        "Quantile for p =", input$prob, ":",
        round(qexp(input$prob, input$rate), 4)
      )
    }
  })
  
  output$interpretation <- renderText({
    
    if (input$choice == "PDF") {
      
      "The PDF shows how probability density
       changes with waiting time."
      
    } else if (input$choice == "CDF") {
      
      paste(
        "The probability that the waiting time is",
        "less than or equal to", input$time,
        "is",
        round(100 * pexp(input$time, input$rate), 2),
        "percent."
      )
      
    } else if (input$choice == "Tail") {
      
      paste(
        "The probability that the waiting time",
        "exceeds", input$time, "is",
        round(100 * pexp(
          input$time,
          input$rate,
          lower.tail = FALSE
        ), 2),
        "percent."
      )
      
    } else if (input$choice == "QF") {
      
      paste(
        round(100 * input$prob, 1),
        "percent of waiting times are less than",
        "or equal to",
        round(qexp(input$prob, input$rate), 4)
      )
    }
  })
}

shinyApp(ui = ui, server = server)
