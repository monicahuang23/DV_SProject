#ui.R 

library(shiny)

navbarPage(
  title = "DV_SProject",
  tabPanel(title = "Scatter Plot",
     sidebarPanel(
       textInput(inputId = "title", 
                 label = "Scatter Plot Title",
                 value = "Scatter Plot"),
       actionButton(inputId = "clicks1",  label = "Show the plot")
     ),
     
     mainPanel(plotOutput("distPlot1")
     )
  ),
  tabPanel(title = "CrossTab",
     sidebarPanel(
       actionButton(inputId = "light", label = "Light"),
       actionButton(inputId = "dark", label = "Dark"),
       sliderInput("KPI1", "KPI_great:", 
                   min = 1, max = 1000,  value = 1000),
       sliderInput("KPI2", "KPI_average:", 
                   min = 1000, max = 10000,  value = 10000),
       textInput(inputId = "title", 
                 label = "CrossTab Title",
                 value = "CrossTab"),
       actionButton(inputId = "clicks2",  label = "Show the plot")
     ),
     
     mainPanel(plotOutput("distPlot2")
     )
  ),
  tabPanel(title = "Barchart",
     sidebarPanel(
       checkboxInput(inputId = "avg_read",
                     label = strong("Show average reading growth grades"),
                     value = FALSE),
       
       checkboxInput(inputId = "avg_math",
                     label = strong("Show average math growth grades"),
                     value = FALSE),

       checkboxInput(inputId = "avg_write",
                     label = strong("Show average writing growth grades"),
                     value = FALSE),
       
       actionButton(inputId = "clicks3",  label = "Show the plot")
     ),
     
     mainPanel(plotOutput("distPlot3")
     )        
  )
)
  