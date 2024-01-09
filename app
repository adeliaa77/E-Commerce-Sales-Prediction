# Load Package
library(shiny)
library(ggplot2)

# Data
sales_data <- data.frame(
  X1 = c(150000,160000,170000,180000,190000,200000,210000,220000,230000,240000,250000,260000),
  X2 = c(8000,9500,10000,10500,11000,9000,11500,12000,12500,13000,14000,15000),
  X3 = c(5,4.5,4.8,4.6,5.1,4.7,4.9,5,5.2,5.3,5.4,5.5),
  X4 = c(8.5,8.2,8.4,8.5,8.6,8.7,8.8,8.9,8.7,8.8,8.9,9),
  X5 = c(20000,22000,25000,23000,30000,28000,27000,35000,40000,45000,50000,60000),
  Y = c(120,150,160,165,180,170,190,210,230,250,300,350)
)

# UI
ui <- fluidPage(
  titlePanel("Sales Estimation"),
  sidebarLayout(
    sidebarPanel(
      numericInput("visitors", "Website Visitors Per Month", value = 150000),
      numericInput("transactions", "Monthly Transactions", value = 8000),
      numericInput("items", "Average Number of Items Per Transaction", value = 5),
      numericInput("rating", "Customer Satisfaction Rating (1-10)", value = 8.5),
      numericInput("ads", "Online Advertisements Run Per Month", value = 20000),
      actionButton("predictButton", "Predict Sales")
    ),
    mainPanel(
      textOutput("regression_equation"),  # Added regression equation output
      textOutput("prediction"),
      plotOutput("salesPlot")
    )
  )
)

# Server
server <- function(input, output) {
  observeEvent(input$predictButton, {
    # Simpan input ke dalam variable
    new_data <- data.frame(
      X1 = input$visitors,
      X2 = input$transactions,
      X3 = input$items,
      X4 = input$rating,
      X5 = input$ads,
      Y = NA
    )
    
    # Gabungkan input dengan data penjualan yang sudah ada
    sales_data <- rbind(sales_data, new_data)
    
    # Hitung perkiraan penjualan dengan model regresi linear sederhana
    lm_model <- lm(Y ~ X1 + X2 + X3 + X4 + X5, data = sales_data)
    prediction_result <- predict(lm_model, newdata = new_data)
    
    # Tampilkan hasil perkiraan
    output$prediction <- renderText({
      paste("Estimated Sales: $USD", round(prediction_result, 2))
    })
    
    # Tampilkan regression equation
    output$regression_equation <- renderText({
      paste("Regression Equation: Y =", paste(round(coef(lm_model), 2), collapse = " + "))
    })
    
    # Tampilkan grafik atau plot yang relevan
    output$salesPlot <- renderPlot({
      ggplot(sales_data, aes(x = Y, y = prediction_result)) +
        geom_point() +
        geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
        geom_smooth(method = "lm", se = FALSE, color = "blue") +  # Regression line
        labs(x = "Actual Sales", y = "Estimated Sales", title = "Comparison of Actual and Estimated Sales") +
        annotate("text", x = max(sales_data$Y), y = max(prediction_result),
                 label = paste("Regression Equation:\nY =", round(coef(lm_model)[1], 2), "+", 
                               round(coef(lm_model)[2], 2), "* X", sep = ""), hjust = 1, vjust = 1, color = "blue")
    })
  })
}

# Run Shiny App
shinyApp(ui = ui, server = server)
