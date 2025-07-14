if (!require(pwr)) {
  install.packages("pwr", repos = "https://cloud.r-project.org/")
  library(pwr)
}

# Load required packages
library(shiny)
library(pwr)

# Define UI
ui <- fluidPage(
  titlePanel("Power Analysis for Comparing Two T20 Batting Pairs (Batting Average)"),
  
  sidebarLayout(
    sidebarPanel(
      numericInput("meanA", "Mean Batting Average (Pair A):", value = 45, min = 0, max = 100),
      numericInput("meanB", "Mean Batting Average (Pair B):", value = 40, min = 0, max = 100),
      numericInput("sd", "Pooled SD:", value = 5, min = 1, max = 50),
      numericInput("effsize", "Effect Size (Cohen's d):", value = NA, min = 0, max = 3, step = 0.1),
      sliderInput("nRange", "Sample size per group range:",
                  min = 5, max = 200, value = c(10, 100), step = 5),
      numericInput("alpha", "Significance Level (alpha):", value = 0.05, min = 0.001, max = 0.1, step = 0.001),
      numericInput("nsim", "Number of Simulations:", value = 1000, min = 100, max = 5000, step = 100),
      actionButton("run", "Run Simulation")
    ),
    
    mainPanel(
      plotOutput("powerPlot"),
      textOutput("note")
    )
  )
)

# Define Server
server <- function(input, output) {
  
  observeEvent(input$run, {
    
    # Function to simulate power
    simulate_power <- function(n, meanA, meanB, sd, nsim, alpha) {
      count_sig <- 0
      for (i in 1:nsim) {
        sampleA <- rnorm(n, meanA, sd)
        sampleB <- rnorm(n, meanB, sd)
        test <- t.test(sampleA, sampleB, var.equal = TRUE)
        if (test$p.value < alpha) {
          count_sig <- count_sig + 1
        }
      }
      return(count_sig / nsim)
    }
    
    # Reactive plot generation
    output$powerPlot <- renderPlot({
      sample_sizes <- seq(input$nRange[1], input$nRange[2], by = 5)
      
      # If user defines effect size, use it; otherwise compute
      if (!is.na(input$effsize) && input$effsize > 0) {
        eff_size <- input$effsize
      } else {
        eff_size <- (input$meanA - input$meanB) / input$sd
      }
      
      # Analytical power
      analytical_power <- sapply(sample_sizes, function(n) {
        pwr.t.test(n = n, d = eff_size, sig.level = input$alpha, type = "two.sample", alternative = "two.sided")$power
      })
      
      # Simulation-based power
      simulation_power <- sapply(sample_sizes, function(n) {
        simulate_power(n, input$meanA, input$meanB, input$sd, input$nsim, input$alpha)
      })
      
      # Plot
      plot(sample_sizes, analytical_power, type = "l", lwd = 2, col = "blue",
           ylim = c(0, 1), xlab = "Sample size per group", ylab = "Power",
           main = "Power Curve for T20 Batting Pair Comparison (Batting Average)")
      lines(sample_sizes, simulation_power, type = "l", lwd = 2, col = "red")
      legend("bottomright", legend = c("Analytical Power", "Simulation-Based Power"),
             col = c("blue", "red"), lwd = 2)
      abline(h = 0.8, lty = 2, col = "gray")
    })
    
    output$note <- renderText({
      paste("Dashed line at 0.8 indicates commonly desired power level.",
            "\nIf you specify an effect size (Cohen's d), it will override mean & pooled SD for analytical calculation.")
    })
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
