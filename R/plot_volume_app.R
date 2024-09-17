plot_volume<- function(dataset) {
  library(DT)
  shinyApp(
    ui = fluidPage(
      fluidRow(style = "padding-bottom: 20px;",
               column(4, sliderInput(inputId='year', label='Year', min=2013, max=year(Sys.Date()),value=2013, step=1))),
      fluidRow(column(4, selectInput('consecutive', 'Consecutive',1:100, "001")
               )),
     # fluidRow(column(4, checkboxGroupInput('flag', 'Selected a flag',choices=c("0-5m","5-10m","midlayer","last10m", "depthflag"), selected=c("0-5m","5-10m","midlayer","last10m", "depthflag"))
     # )),
      #fluidRow(tableOutput('kable1')),
     fluidRow(DTOutput('kable2')),
      
      fluidRow(
                 plotOutput('Volume', height = "400px")),
      fluidRow(
                plotOutput('Cast', height = "400px")
               )
      )
    ,
    
    server = function(input, output, session){
      
      # Combine the selected variables into a new data frame
      selectedyear <- reactive({
        datasub<- subset(dataset[[2]], year %in% input$year)
        return(datasub)  
        
      })
      
      observeEvent(input$year, {
        uc<-  unique(selectedyear()[,"consecutive"])
        
        updateSelectInput(session, inputId="consecutive",choices=uc, selected = uc[1])
        
      })
      
      selectedData <- reactive({
        datasub<- subset(selectedyear() , consecutive==input$consecutive)
         return(datasub)  
        
      })
      
      
      output$Volume <-  renderPlot({
        c<- ggplot(selectedData(), aes(x=depth, y=`%Volume`,fill=catZ))+geom_bar(stat="identity")  +
          scale_fill_viridis_d() +
          scale_x_reverse(breaks=rev(seq(0,10*ceiling(max(selectedData()$depth)/10),5)), limits=c(10*ceiling(max(selectedData()$depth)/10), 0))+ 
          coord_flip() + ggtitle(paste("Showing",input$year, "consecutive", input$consecutive, sep=" "))
        
        print(c) 
      })
      
      selectedData2 <- reactive({
        datasub<- subset(dataset[[3]],year == input$year)
        subset(datasub, consecutive==input$consecutive)
      })
      
      output$Cast <-  renderPlot({
        if(input$year !=2013){
          gtit=paste("Showing",input$year, "consecutive", input$consecutive, sep=" ")
        } else {
            gtit=paste("Chrono for 2013 not available at the moment")
        }
        
        d<- ggplot(selectedData2(), aes(x=Chrono, y=Profondeur,size=`VitesseD2`,col=as.factor(cast)))+
          geom_point(shape=16) +
          scale_y_reverse(breaks=rev(seq(0,10*ceiling(max(selectedData2()$Profondeur)/10),5)), limits=c(10*ceiling(max(selectedData2()$Profondeur)/10), 0))+ 
          scale_color_viridis_d()+ ggtitle(gtit)
        print(d) 
      })
      
      
     
      output$kable2 <- renderDT({
     
        flagyear<- subset(dataset[[1]], year==input$year)
        DT::datatable(flagyear %>%  dplyr::select(-`NA`), class="cell-border stripe", rownames = F, filter="top",
                      editable=T, extensions="Buttons", options=list(dom="Bftrop", buttons=c("copy", "csv","excel","pdf", "print"),autoWidth=T, scrollX=T)) %>%
          formatRound(columns=c('meandepth', 'maxdepth', "diffegg"), digits=1)
       
      }
     ) 
    },
    
    options = list(height =2000)
  )
}