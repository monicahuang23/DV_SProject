# server.R
require("jsonlite")
require("RCurl")
require(ggplot2)
require(dplyr)
require(shiny)
require(shinydashboard)
require(leaflet)

shinyServer(function(input, output) {
    
      df1 <- eventReactive(input$clicks1, {data.frame(fromJSON(getURL(URLencode(gsub("\n", " ", 'skipper.cs.utexas.edu:5001/rest/native/?query="select * from final_grade"')), httpheader=c(DB='jdbc:oracle:thin:@sayonara.microlab.cs.utexas.edu:1521:orcl', USER='C##cs329e_mh42375', PASS='orcl_mh42375', MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON'), verbose = TRUE)))
      })

      output$distPlot1 <- renderPlot({             
            plot <- ggplot() + 
                  coord_cartesian() + 
                  scale_x_continuous() +
                  scale_y_continuous() +
                  labs(title=isolate(input$title)) +
                  labs(x=paste("Spf P's Grad Rate"), y=paste("Rank Total")) +
                  layer(data=df1(), 
                        mapping=aes(x=as.numeric(as.character(SPF_PS_IND_GRAD_RATE)), y=as.numeric(as.character(RANK_TOT)), color=FINAL_PLANTYPE),
                        stat="identity", 
                        stat_params=list(), 
                        geom="point",
                        #geom_params=list(colour="black"), 
                        position=position_jitter(width=0.3, height=0)
                  )
            plot
      }) 

      observeEvent(input$clicks, {
            print(as.numeric(input$clicks))
      })
})