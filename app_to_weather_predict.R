library(shiny)

# Define UI
ui <- fluidPage(
    titlePanel("Weather Prediction Model For Austin, TX"),
    sidebarLayout(
        sidebarPanel(
            numericInput("tempLow", "Lowest Temp of the Day (°F):", value = 50, min = -50, max = 150),
            numericInput("dewPointHigh", "Highest Dew Point (°F):", value = 50, min = -50, max = 150),
            numericInput("humidityHigh", "Highest Humidity (%):", value = 50, min = 0, max = 100),
            numericInput("pressureLow", "Sea Level Pressure Low (Inches):", value = 29.92, min = 20, max = 40),
            checkboxInput("rained", "Did it rain?", value = FALSE),
            checkboxInput("thunderstorm", "Did a thunderstorm occur?", value = FALSE),
            actionButton("predict", "Predict")
        ),
        mainPanel(
            h3("Estimated Highest Temperature (°F):"),
            textOutput("prediction"),
            plotOutput("coolPlot")
        )
    )
)

# Define Server
server <- function(input, output) {
    # Coefficients
    intercept <- 204.603184
    coef_tempLow <- 0.513662
    coef_dewPointHigh <- 0.702989
    coef_humidityHigh <- -0.092383
    coef_pressureLow <- -5.785053
    coef_eventRain <- -7.912801
    coef_eventThunderstorm <- -1.231316
    coef_interaction1 <- -0.002652
    coef_interaction2 <- 3.589518
    
    # Reactive prediction
    prediction <- eventReactive(input$predict, {
        tempLow <- input$tempLow
        dewPointHigh <- input$dewPointHigh
        humidityHigh <- input$humidityHigh
        pressureLow <- input$pressureLow
        eventRain <- ifelse(input$rained, 1, 0)
        eventThunderstorm <- ifelse(input$thunderstorm, 1, 0)
        
        # Calculate prediction
        estimated_temp <- intercept +
            coef_tempLow * tempLow +
            coef_dewPointHigh * dewPointHigh +
            coef_humidityHigh * humidityHigh +
            coef_pressureLow * pressureLow +
            coef_eventRain * eventRain +
            coef_eventThunderstorm * eventThunderstorm +
            coef_interaction1 * dewPointHigh * humidityHigh +
            coef_interaction2 * eventRain * eventThunderstorm
        
        return(list(estimated_temp = estimated_temp, tempLow = tempLow))
    })
    
    # Output prediction
    output$prediction <- renderText({
        req(input$predict)
        round(prediction()$estimated_temp, 2)
    })
    
    # Advanced plot
    output$coolPlot <- renderPlot({
        req(input$predict)
        pred <- prediction()
        estimated_temp <- pred$estimated_temp
        tempLow <- pred$tempLow
        
        # Create a gauge-like plot
        library(ggplot2)
        data <- data.frame(
            Category = c("Lowest Temp", "Estimated Temp"),
            Value = c(tempLow, estimated_temp)
        )
        
        ggplot(data, aes(x = Category, y = Value, fill = Category)) +
            geom_bar(stat = "identity", width = 0.5) +
            geom_text(aes(label = round(Value, 2)), vjust = -0.5, size = 5) +
            scale_fill_manual(values = c("Lowest Temp" = "lightblue", "Estimated Temp" = "orange")) +
            theme_minimal() +
            labs(
                title = "Temperature Comparison",
                y = "Temperature (°F)",
                x = ""
            ) +
            theme(
                plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
                axis.text.x = element_text(size = 12),
                axis.text.y = element_text(size = 12)
            )
    })
}

# Run the app
shinyApp(ui = ui, server = server)