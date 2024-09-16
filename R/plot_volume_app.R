plot_volume<- function(dataset) {
  
  shinyApp(
    ui = fluidPage(
      fluidRow(style = "padding-bottom: 20px;",
               column(4, sliderInput(inputId='year', label='Year', min=2013, max=2023,value=2013)),
               column(4, textInput('consecutive', 'Consecutive', "001")
               ),
               fluidRow(
                 plotOutput('Volume', height = "400px")
               )
      )
    ),
    
    server = function(input, output){
      
      # Combine the selected variables into a new data frame
      selectedData <- reactive({
        datasub<- subset(dataset , consecutive==input$consecutive)
        datasub<- subset(datasub, year %in% input$year)
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