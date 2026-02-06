library(shiny)
library(randomForest)

ui <- fluidPage(
  titlePanel("Chronic Kidney Disease (CKD) Risk Predictor"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Enter Patient Lab Results"),
      sliderInput("hemo", "Hemoglobin (g/dL):", min = 5, max = 20, value = 12),
      sliderInput("sod", "Sodium (mEq/L):", min = 100, max = 160, value = 140),
      numericInput("sc", "Serum Creatinine (mg/dL):", value = 1.2, min = 0.1, max = 15),
      numericInput("age", "Patient Age:", value = 50, min = 0, max = 120),
      selectInput("bp", "Blood Pressure Category:", 
                  choices = c("Normal" = 0, "High" = 1)),
      
      actionButton("predict_btn", "Run Diagnosis", class = "btn-primary")
    ),
    
    mainPanel(
      h3("Model Diagnosis Result"),
      verbatimTextOutput("prediction_text"),
      hr(),
      p("Note: This is a demonstration tool based on synthetic and public datasets.")
    )
  )
)

server <- function(input, output) {
  model <- readRDS("ckd_model.rds")
  
  prediction <- eventReactive(input$predict_btn, {
    new_data <- data.frame(
      hemo = input$hemo,
      sod = input$sod,
      sc = input$sc,
      age = input$age,
      bp_diastolic = as.numeric(input$bp)
    )
    
    result <- predict(model, new_data)
    return(result)
  })
  
  output$prediction_text <- renderText({
    res <- prediction()
    if (res == "ckd") {
      return("RESULT: High Risk of CKD (Positive)")
    } else {
      return("RESULT: Low Risk (Negative)")
    }
  })
}

shinyApp(ui = ui, server = server)
