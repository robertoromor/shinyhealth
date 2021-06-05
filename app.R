library(shiny)
library(shinyWidgets)
library(broom)
library(dplyr)

source(paste0(getwd(),"/global.R"))

ui <- fluidPage(
  
  sidebarPanel(
    
    numericInput("edad", label=h3("EDAD"), value=30),
    
    h3("Genero"),
    
    switchInput("genero", onLabel="H", offLabel="F", value=TRUE), 
    
    sliderInput("slider1", label=h3("MaxPresion"), min=0, max=200, value=100), 
    
    actionBttn("boton1", label="Calcular", style="gradient", color="primary")
    
  ),
  
  mainPanel(
    
    tabsetPanel(
      
      tabPanel("tab1",
               
               textOutput("prob")
               
               ),
      tabPanel("tab2"),
      tabPanel("tab3"),
      tabPanel("tab4")
      
    )    
    
  )
  
  
)

server <- function(input, output) {
  
  heart_data<-read.csv(paste0(getwd(),"/heart_data.csv"))
  
  observeEvent(input$boton1,{
    
    if(input$genero) {genero1<-"Male"} else {genero1<-"Female"}
    
    
    probability<-model_cardio(heart_data, age=input$edad, sex=genero1, input$slider1)
    
    
    output$prob<-renderText({
      
      probability
      
    })
    
  })
  
  
}

shinyApp(ui = ui, server = server)