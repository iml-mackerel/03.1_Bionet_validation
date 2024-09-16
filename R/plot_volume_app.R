plot_volume<- function(dataset) {
  
  shinyApp(
    ui = fluidPage(
      fluidRow(style = "padding-bottom: 20px;",
               column(4, sliderInput(inputId='year', label='Year', min=2013, max=2023,value=2013)),
               column(4, selectInput('consecutive', 'Consecutive',1:100, "001")
               ),
               fluidRow(
                 plotOutput('Volume', height = "400px")
               )
      )
    ),
    
    server = function(input, output, session){
      
      # Combine the selected variables into a new data frame
      selectedyear <- reactive({
        datasub<- subset(dataset, year %in% input$year)
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
        c<- ggplot(selectedData(), aes(x=depth, y=`%Volume`,fill=catZ))+geom_bar(stat="identity") + coord_flip()
        
        print(c) 
      })
    },
    
    options = list(height =500)
  )
}