library(shiny)
library(ggplot2)
library(plotly)
library(shinythemes)

# Shiny app for visualizing taxon tables with associated metadata
# Authors @William Frohlich & @Sam Minot
# August 2019
# Fredricks Lab
# Fred Hutch

# UI portion of the shiny app
shinyUI(fluidPage(
  theme = shinytheme('flatly'),
  column(1, img(src="hutch.png", inline = TRUE, height = "100%", width = "100%")),
  column(4, headerPanel("Microbiome Analysis", windowTitle = "Hutch"), align = "left", offset = 0),
  column(12, 
         sidebarLayout(
           sidebarPanel(
             actionButton("show", "Upload files", width = '100%'),
             tags$br(),
             tags$br(), #Formatting
             textOutput("selected_sample"),
             tags$br(),
             textOutput("selected_tax"),
             tags$br(),
             tags$a(
               href="https://github.com/FredHutch/shinyMicrobiomeAnalysis#shiny-microbiome-analysis", 
               icon("question-circle")
              ),
             tags$a(
               href="https://github.com/FredHutch/shinyMicrobiomeAnalysis#shiny-microbiome-analysis", 
               "Getting Started"
              ),
             tags$br(),
             tags$a(
               href="https://github.com/FredHutch/shinyMicrobiomeAnalysis/issues/new", 
               icon("exclamation-circle")
             ),
             tags$a(
               href="https://github.com/FredHutch/shinyMicrobiomeAnalysis/issues/new", 
               "Report an Issue"
             ),
             width = 3
           ),
           mainPanel(
             #creating tabs for each type of graph
             tabsetPanel(
               tabPanel(
                 "Stacked Bar",
                 fluidPage(
                   fluidRow(
                     sliderInput(
                       "num_stacks",
                       "Number of taxa:",
                       min = 2,
                       max = 40,
                       value = 10
                     ),
                     width = 5
                   ),
                   fluidRow(
                     mainPanel(plotlyOutput('stacked_bar')),
                     width = 10
                   )
                 )
                 ),
               tabPanel(
                 "Reads per Sample",
                 fluidPage(
                   fluidRow(
                     selectInput(
                       "sort.by",
                       "Sort By:",
                       list(
                         number_of_reads = "number_of_reads",
                         sample_name = "sample_name"
                       )
                     ),
                     width = 5
                   ),
                   fluidRow(
                     mainPanel(plotlyOutput('reads_per_sample')),
                     width = 10
                   )
                 )
               ),
               tabPanel(
                 "Alpha Div. (breakaway)",
                 fluidPage(
                   fluidRow(
                     selectInput(
                       "sort.by.breakaway",
                       "Sort By:",
                       list(
                         number_of_reads = "number_of_reads",
                         sample_name = "sample_name"
                       )
                     ),
                     width = 5
                   ),
                   fluidRow(
                     mainPanel(plotlyOutput('breakaway_per_sample')),
                     width = 10
                   ),
                   fluidRow(
                     downloadButton("downloadBreakaway", "Download Table")
                   )
                 )
               ),
               tabPanel(
                 "Diff. Abund. (corncob)",
                 fluidPage(
                   fluidRow(
                     column(
                       width = 5,
                       selectInput(
                         "select.corncob",
                         "Select Metadata:",
                         list(
                           all = "all"
                         )
                       )
                     ),
                     column(
                       width = 5,
                       sliderInput(
                         "num.corncob.plots",
                         "Number of plots:",
                         min = 1,
                         max = 10,
                         value = 5
                       )
                     )
                   ),
                   fluidRow(
                     mainPanel(plotlyOutput('top_taxa_boxplot')),
                     width=10
                   ),
                   fluidRow(
                     dataTableOutput("corncob")
                   ),
                   fluidRow(
                     downloadButton("downloadCorncob", "Download Table")
                   )
                 )
               )
               )
             )
           )
         )
  )
  )
