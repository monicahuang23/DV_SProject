# server.R
require("jsonlite")
require("RCurl")
require(ggplot2)
require(dplyr)
require(shiny)
require(shinydashboard)
require(leaflet)

shinyServer(function(input, output) {

# start of plot 1
      df1 <- eventReactive(input$clicks1, {data.frame(fromJSON(getURL(URLencode(gsub("\n", " ", 'skipper.cs.utexas.edu:5001/rest/native/?query="select * from final_grade"')), httpheader=c(DB='jdbc:oracle:thin:@sayonara.microlab.cs.utexas.edu:1521:orcl', USER='C##cs329e_mh42375', PASS='orcl_mh42375', MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON'), verbose = TRUE)))
      })

      output$distPlot1 <- renderPlot({             
            plot1 <- ggplot() + 
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
            plot1
      }) 

      observeEvent(input$clicks, {
            print(as.numeric(input$clicks))
      })
      
# start of plot 2      
      KPI_great <- reactive({input$KPI1})     
      KPI_average <- reactive({input$KPI2})
      rv <- reactiveValues(alpha = 0.50)
      observeEvent(input$light, { rv$alpha <- 0.50 })
      observeEvent(input$dark, { rv$alpha <- 0.75 })
      
      df2 <- eventReactive(input$clicks2, {data.frame(fromJSON(getURL(URLencode(gsub("\n", " ", 'skipper.cs.utexas.edu:5001/rest/native/?query=
            "select EMH, final_plantype, kpi,
            case                                                                                   when kpi < "p1" then \\\'Great\\\'
            when kpi < "p2" then \\\'Average\\\'
            else \\\'Ok\\\'
            end kpi                                                                                from (select EMH, final_plantype, sum(RANK_TOT) as kpi
            from FINAL_GRADE 
            where DISTRICTNAME = \\\'ADAMS 12 FIVE STAR SCHOOLS\\\'
            group by EMH, final_plantype)
            order by final_plantype;"
            ')), httpheader=c(DB='jdbc:oracle:thin:@sayonara.microlab.cs.utexas.edu:1521:orcl', USER='C##cs329e_mh42375', PASS='orcl_mh42375', 
                 MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON', p1=KPI_great(), p2=KPI_average()), verbose = TRUE)))
      })
      
      output$distPlot2 <- renderPlot({             
            plot2 <- ggplot() + 
                  coord_cartesian() + 
                  scale_x_discrete() +
                  scale_y_discrete() +
                  labs(title=isolate(input$title)) +
                  labs(x=paste("Final Plantype"), y=paste("EMH")) +
                  layer(data=df2(), 
                        mapping=aes(x=FINAL_PLANTYPE, y=EMH, label=KPI), 
                        stat="identity", 
                        stat_params=list(), 
                        geom="text",
                        geom_params=list(colour="black"), 
                        position=position_identity()
                  ) +
                  layer(data=df2(), 
                        mapping=aes(x=FINAL_PLANTYPE, y=EMH, fill=KPI.1), 
                        stat="identity", 
                        stat_params=list(), 
                        geom="tile",
                        geom_params=list(alpha=rv$alpha), 
                        position=position_identity()
                  )
           plot2
      }) 
      
      observeEvent(input$clicks, {
        print(as.numeric(input$clicks))
      })
})