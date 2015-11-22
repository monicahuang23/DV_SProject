# server.R
require("jsonlite")
require("RCurl")
require(ggplot2)
require(dplyr)
require(shiny)
require(shinydashboard)
require(leaflet)

shinyServer(function(input, output) {
        
      KPI_great <- reactive({input$KPI1})     
      KPI_average <- reactive({input$KPI2})
      rv <- reactiveValues(alpha = 0.50)
      observeEvent(input$light, { rv$alpha <- 0.50 })
      observeEvent(input$dark, { rv$alpha <- 0.75 })
    
      df1 <- eventReactive(input$clicks1, {data.frame(fromJSON(getURL(URLencode(gsub("\n", " ", 'skipper.cs.utexas.edu:5001/rest/native/?query=
            "select EMH, DISTRICTNAME, kpi,
            case
            when kpi < "p1" then \\\'Great\\\'
            when kpi < "p2" then \\\'Average\\\'
            else \\\'Ok\\\'
            end kpi
            from (select EMH, DISTRICTNAME, sum(RANK_TOT) as kpi
            from FINAL_GRADE 
            where FINAL_PLANTYPE = \\\'Turnaround Plan\\\'
            group by EMH, DISTRICTNAME)
            order by DISTRICTNAME;
            ')), httpheader=c(DB='jdbc:oracle:thin:@sayonara.microlab.cs.utexas.edu:1521:orcl', USER='C##cs329e_mh42375', PASS='orcl_mh42375', 
                 MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON', p1=KPI_great(), p2=KPI_Medium_average()), verbose = TRUE)))
      })

      output$distPlot1 <- renderPlot({             
            plot <- ggplot() + 
                  coord_cartesian() + 
                  scale_x_discrete() +
                  scale_y_discrete() +
                  labs(title=isolate(input$title)) +
                  labs(x=paste("District Name"), y=paste("EMH")) +
                  layer(data=df1(), 
                        mapping=aes(x=DISTRICTNAME, y=EMH, label=KPI), 
                        stat="identity", 
                        stat_params=list(), 
                        geom="text",
                        geom_params=list(colour="black"), 
                        position=position_identity()
                  ) +
                  layer(data=df1(), 
                        mapping=aes(x=DISTRICTNAME, y=EMH, fill=KPI.1), 
                        stat="identity", 
                        stat_params=list(), 
                        geom="tile",
                        geom_params=list(alpha=0.5), 
                        position=position_identity()
                  )
            plot
      }) 

      observeEvent(input$clicks, {
            print(as.numeric(input$clicks))
      })
})
