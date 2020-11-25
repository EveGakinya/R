#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(readr)
RegData <- read_csv("C:/Users/EVAH/Desktop/IWMI intenship/Afhan_data.csv")
ui <- fluidPage(
    headerPanel("Regression and Time Series Analysis"), 
    sidebarPanel(
        p("Select the inputs for the Dependent Variable"),
        selectInput(inputId = "DepVar", label = "Dependent Variables", multiple = FALSE, choices = list("District_Name","DISTID_Mer","Province_name","Year",
                                                                                                        "Month","SPI.3month","Agri.cover....","VHI","IDSI")),
        p("Select the inputs for the Independent Variable"),
        selectInput(inputId = "IndVar", label = "Independent Variables", multiple = FALSE, choices = list( "District_Name","DISTID_Mer","Province_name","Year",
                                                                                                           "Month","SPI.3month","Agri.cover....","VHI","IDSI"))
    ),
    mainPanel(
        verbatimTextOutput(outputId = "RegSum"),
        verbatimTextOutput(outputId = "IndPrint"),
        verbatimTextOutput(outputId = "DepPrint")
        #plotOutput("hist")
    )
)

server <- function(input, output) {
    
    lm1 <- reactive({lm(reformulate(input$IndVar, input$DepVar), data = RegData)})
    
    
    output$DepPrint <- renderPrint({input$DepVar})
    output$IndPrint <- renderPrint({input$IndVar})
    output$RegSum <- renderPrint({summary(lm1())})
    
}

shinyApp(ui = ui, server = server)