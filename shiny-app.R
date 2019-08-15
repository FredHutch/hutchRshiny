library(shiny)
library(datasets)
library(tidyverse)
library(reshape2)
library(ggplot2)
library(plotly)
library(dplyr)
library(shinythemes)

# Shiny app for visualizing taxon tables with associated metadata
# Authors @William Frohlich & @Sam Minot
# August 2019
# Fredricks Lab
# Fred Hutch

# UI portion of the shiny app
ui <- shinyUI(fluidPage(
  theme = shinytheme('flatly'),
  #img(src="hutch.png", height = 30, width = 30),
  #column(9, headerPanel(windowTitle = "Hutch")),
  column(1, img(src="hutch.png", inline = TRUE, height = "100%", width = "100%")),
  column(4, headerPanel("Taxon Analysis", windowTitle = "Hutch"), align = "left", offset = 0),
  column(12, 
  sidebarLayout(
    sidebarPanel(
      actionButton("show", "Upload files", width = '100%'),
      tags$br(),
      tags$br(), #Formatting
      textOutput("selected_sample"),
      tags$br(),
      textOutput("selected_tax"),
      width = 2
    ),
    mainPanel(
      #creating tabs for each type of graph
      tabsetPanel(
        tabPanel("Stacked Bar",
          sidebarLayout(
            sidebarPanel(numericInput("num_stacks", "Select # stacks", 10),
                         actionButton("numeric_input_ok", "OK", width = '100%'), 
                         width = 3),
            mainPanel(plotlyOutput('stacked_bar'))
          )
      ),
      tabPanel("Reads per Sample",
          sidebarLayout(
            sidebarPanel(selectInput("sort.by","Sort By:",list(number_of_reads = "number_of_reads", sample_name = "sample_name")),
                         width = 3), 
            mainPanel(plotlyOutput('reads_per_sample'))
          )   
        )
      )
    )
  )
  )
)
)

#SERVER
server <- shinyServer(function(input, output, session) {
  # added "session" because updateSelectInput requires it
  options(warn=-1)
  # options(encoding="UTF-8")
  
  #Modal popup when button 'show' is clicked
  observeEvent(input$show, {
    showModal(dataModal())
  })
  
  vals <- eventReactive(input$numeric_input_ok, {
    return(input$num_stacks)
  })
  
  
  #Make sure user has uploaded the files before they can remove the modal
  observeEvent(input$modal_ok, {
    # Check if a file has been uploaded
    if (!is.null(input$sample_data) & !is.null(input$taxon_table)){
      print("files uploaded")
      removeModal()
      
    } else {
      showModal(dataModal(failed = TRUE))
    }
  })
  
  #When upload button clicked show a popup modal that will not close until 2 csv files are uploaded
  dataModal <- function(failed = FALSE) {
    #Modal interface
    modalDialog(
      title = "Upload your taxon table and metadata",
      fileInput('sample_data', 'Choose metadata csv', accept=c('text/csv','text/comma-separated-values,text/plain', '.csv')),
      fileInput('taxon_table', 'Choose taxon table csv', accept=c('text/csv','text/comma-separated-values,text/plain', '.csv')),
      if (failed)
        div(tags$b("Please upload both files", style = "color: red;")),
      
      footer = tagList(
        modalButton("Cancel"),
        actionButton("modal_ok", "OK")
      )
    )}
  
  #helper function for reading in the metadata
  read_metadata <- function(sample_data_csv){
    # Read in metadata
    return(read_csv(sample_data_csv))
  }
    
  #helper function for reading in the taxon table
  read_in_taxon_table <- function(taxon_table_csv, metadata_df){
    # Read in read counts
    read_df <- read.csv(taxon_table_csv, row.names=1, stringsAsFactors = FALSE)
    
    # Remove the rows in the read counts that match columns in the metadata
    for(metadata_field in colnames(metadata_df)){read_df <- read_df[rownames(read_df) != metadata_field,]}
    read_df <- data.frame(read_df, stringsAsFactors = FALSE)
      
    # Convert to integers
    for(col_name in colnames(read_df)){read_df[[col_name]] <- as.numeric(read_df[[col_name]])}
      
    return (read_df)
  } 
  
  #reactive helper function for get_read_df
  get_metadata_df <- reactive({
    req(input$sample_data)
    metadata_df <- read_metadata(input$sample_data$name)
    return(metadata_df)
    })

  #get the appropriate data frame
  get_read_df <- reactive({
    req(input$taxon_table)
    #print(input$sample_data$name)
    #note that input$taxon_table actually returns a df so must use $name instead!!
    read_df <- read_in_taxon_table((input$taxon_table)$name, get_metadata_df())
    return(read_df)
    })

  #method to plot a reads per sample graph
  plot_reads_per_sample <- function(read_df, sort_by){
    # `sort_by` is either `sample_name` or `number_of_reads`
    if(sort_by %in% c("number_of_reads", "sample_name")){}else{
      print("Please specify sample_name or number_of_reads")
    }
    
    # Plot the number of reads per sample
    # Add a TOTAL column for the number of reads
    read_df <- rbind(read_df, colSums(read_df))
    rownames(read_df)[nrow(read_df)] <- "TOTAL"
    
    # We have the make the organism names into a column
    read_df$org <- rownames(read_df)
    read_long = as_tibble(melt(read_df, id="org"))
    tot_reads <- read_long[read_long$org == "TOTAL",]
    
    p <- ggplot(
      data=tot_reads
    )
    if(sort_by == "sample_name"){
      p <- p + 
        geom_bar(
          mapping = aes(
            x = reorder(variable, variable), 
            y = value
          ),
          stat = "identity"
        )
    }
    if(sort_by == "number_of_reads"){
      p <- p + 
        geom_bar(
          mapping = aes(
            x = reorder(value, variable), 
            y = value
          ),
          stat = "identity"
        )
    }
    p <- p +
      xlab("Sample Name") +
      ylab("Number of Reads") +
      ggtitle("Sequencing Depth") + 
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
    print(p)  
  }
  
  #method to plot a stacked bar graph
  plot_stacked_bar_graph <- function(read_df, number_of_organisms){
    
    if(number_of_organisms < 1){
      print("Please specify a number of organisms >= 1")
    }
    if(number_of_organisms > 50){
      print("Please specify a number of organisms <= 50")
    }
    
    # Calculate the proportion of reads per sample
    prop_df <- t(t(read_df) / colSums(read_df))
    
    # Pick the top organisms
    # Sum by organism
    org_sum <- rowSums(prop_df)
    # Sort descending
    org_sum <- org_sum[order(-org_sum)]
    # Filter to just those top organisms
    prop_df <- prop_df[names(org_sum[c(1:number_of_organisms)]),]
    # Add in the 'Other'
    prop_df <- rbind(prop_df, 1 - colSums(prop_df)) 
    rownames(prop_df)[nrow(prop_df)] <- "Other"
    
    # Make into long format
    long_df = as_tibble(melt(prop_df))
    
    # Make a bar plot
    p <- ggplot(
      data=long_df
    ) + geom_bar(
      aes(
        y = value, 
        x = Var2, 
        fill = Var1
      ),
      stat="identity"
    ) + ylab(
      "Relative Abundance"
    ) + xlab(
      "Sample Name"
    ) + theme(
      axis.text.x = element_text(angle = 90, hjust = 1)
    ) + guides(
      fill=guide_legend(title="Organism")
    )
    print(p)
  }
  
  #make the stacked bar graph
  output$stacked_bar <- renderPlotly({
    req(input$sample_data)
    req(input$taxon_table)
    #remember with reflexive functions to use parentheses after the function call
    plot_stacked_bar_graph(get_read_df(), vals())
  })
  
  #make the reads per sample graph
  output$reads_per_sample <- renderPlotly({
    req(input$sample_data)
    req(input$taxon_table)
    plot_reads_per_sample(get_read_df(), input$sort.by)
  })
  
  #let the user know what files they've imported
  output$selected_sample <- renderText({ 
    paste("Metadata sheet: ", input$sample_data$name)
  })
  
  output$selected_tax <- renderText({ 
    paste("Taxon sheet: ", input$taxon_table$name)
  })
})


#connect the ui & server and run
shinyApp(ui, server)
