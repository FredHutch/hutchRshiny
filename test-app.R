library(shiny)
library(datasets)
library(plotly)
library(dplyr)
ui <- shinyUI(fluidPage(
  titlePanel("Visualization of data"),
  tabsetPanel(
    tabPanel("Test Tab",
             titlePanel("Upload a File to Visualize"),
             sidebarLayout(
               sidebarPanel(
                 fileInput('file1', 'Choose a file to upload',
                           accept=c('text/csv', 
                                    'text/comma-separated-values,text/plain', 
                                    '.csv')),
                 
                 # added interface for uploading data from
                 # http://shiny.rstudio.com/gallery/file-upload.html
                 tags$br(),
                 checkboxInput('header', 'Header', TRUE),
                 selectInput('xcol', 'X Axis:', ""),
                 selectInput('ycol', 'Y Axis', ""),
                 selectInput("plot.type","Plot Type:",
                             list(bar = "bar", boxplot = "boxplot")#, histogram = "histogram", density = "density")
                 ),
                 radioButtons('sep', 'Separator',
                              c(Semicolon=';',
                                Comma=',',
                                Tab='\t'),
                              ','),
                 radioButtons('quote', 'Quote',
                              c(None='',
                                'Double Quote'='"',
                                'Single Quote'="'"),
                              '"')
                 
               ),
               mainPanel(
                 plotlyOutput('MyPlot')#,
                 # tableOutput('contents')
               )
             )
    )
  )
)
)

server <- shinyServer(function(input, output, session) {
  # added "session" because updateSelectInput requires it
  options(warn=-1)
  # options(encoding="UTF-8")
  
  data <- reactive({ 
    req(input$file1) ## ?req #  require that the input is available
    
    inFile <- input$file1 
    
    # tested with a following dataset: write.csv(mtcars, "mtcars.csv")
    # and                              write.csv(iris, "iris.csv")
    df <- read.csv(inFile$datapath, header = input$header, sep = input$sep,
                   quote = input$quote)
    for(i in seq_along(df)){
      df[,i] <- as.factor(df[,i])
      
    }
    df[df=="NA"] <- NA
    
    
    # Update inputs (you could create an observer with both updateSel...)
    # You can also constraint your choices. If you wanted select only numeric
    # variables you could set "choices = sapply(df, is.numeric)"
    # It depends on what do you want to do later on.
    
    updateSelectInput(session, inputId = 'xcol', label = 'X Axis',
                      choices = names(df), selected = names(df))
    updateSelectInput(session, inputId = 'ycol', label = 'Y Axis',
                      choices = names(df), selected = names(df)[1])
    
    return(df)
  })
  
  # output$contents <- renderTable({
  #   data()
  # })
  
  
  output$MyPlot <- renderPlotly({
    
    if(input$plot.type == "boxplot"){
      p <- ggplot(na.omit(data()), aes(x=na.omit(data())[,input$ycol],y=na.omit(data())[,input$xcol], fill =na.omit(data())[,input$ycol]))+
        geom_boxplot(na.rm = TRUE)+ ggtitle("Boxplot")+
        xlab(input$ycol)+ylab(input$xcol) +scale_fill_discrete(name=input$ycol)
      ggplotly(p) %>% layout(autosize=TRUE)# %>% style(hoverinfo = "none")
    }else{
      if(input$plot.type == "bar"){
        p <-  ggplot(data(),aes(x=as.factor(data()[,input$xcol]),fill=as.factor(data()[,input$ycol]),group = as.factor(data()[,input$ycol])))+
          geom_bar(aes(y=..prop..*100),position=position_dodge())+
          ggtitle("Percentages per group")+
          xlab(input$xcol)+ylab("Percent") +scale_fill_discrete(name=input$ycol)
        ggplotly(p) %>% layout(autosize=TRUE) %>% style(hoverinfo = "none")
        # } else {
        #   if(input$plot.type == "histogram"){
        #     p <-  ggplot(data(),aes(x=data()[,input$xcol],fill=data()[,input$ycol],group = data()[,input$ycol]))+
        #       geom_histogram()+
        #       ggtitle("Histogramm")+
        #       xlab(input$xcol) +scale_fill_discrete(name=input$ycol)
        #     ggplotly(p) %>% layout(autosize=TRUE) %>% style(hoverinfo = "none")
        #   }
      }}
  })
})

shinyApp(ui, server)
