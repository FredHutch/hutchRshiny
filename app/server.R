library(shiny)
library(datasets)
library(tidyverse)
library(reshape2)
library(ggplot2)
library(plotly)
library(dplyr)
library(breakaway)
library(corncob)
library(shinythemes)

# Shiny app for visualizing taxon tables with associated metadata
# Authors @William Frohlich & @Sam Minot
# August 2019
# Fredricks Lab
# Fred Hutch

#SERVER
shinyServer(function(input, output, session) {
  # added "session" because updateSelectInput requires it
  options(warn=-1)
  # options(encoding="UTF-8")
  
  #Modal popup when button 'show' is clicked
  observeEvent(input$show, {
    showModal(dataModal())
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
    id <- showNotification("Reading in metadata sheet...")
    # Read in metadata
    metadata_df <- read_csv(sample_data_csv)
    
    # Set the first column as the specimen name
    metadata_df <- column_to_rownames(metadata_df, var = colnames(metadata_df)[1])
    
    # Update the list to include all metadata columns
    list_of_options <- list(
      number_of_reads = "number_of_reads", 
      sample_name = "sample_name"
    )
    for(metadata_field in colnames(metadata_df)){
      list_of_options[metadata_field] <- metadata_field
    }
    updateSelectInput(
      session,
      "sort.by",
      "Sort By:",
      list_of_options
    )
    
    list_of_options <- list(
      number_of_taxa = "number_of_taxa", 
      sample_name = "sample_name"
    )
    for(metadata_field in colnames(metadata_df)){
      list_of_options[metadata_field] <- metadata_field
    }
    updateSelectInput(
      session,
      "sort.by.breakaway",
      "Sort By:",
      list_of_options
    )
    removeNotification(id)
    return(metadata_df)
  }
    
  #helper function for reading in the taxon table
  read_in_taxon_table <- function(fp, metadata_df){
    id <- showNotification("Reading in taxon table...")
    # Remove factors
    read_df <- read_csv(fp)
    
    # Make sure that the first column is the tax_name
    stopifnot(colnames(read_df)[1] == "tax_name")
    
    # Set tax_name as the rownames
    read_df <- column_to_rownames(read_df, var="tax_name")

    # Make sure that tax_id and rank are in the columns, and remove them
    for(col_name in c("tax_id", "rank")){
      read_df <- read_df[ , -which(colnames(read_df) == col_name)]
    }
    
    # Make sure that we have metadata for each column name (which are all specimen names)
    for(specimen_name in colnames(read_df)){
      stopifnot(specimen_name %in% row.names(metadata_df))
    }
    
    # Convert all values to integers
    for(col_name in colnames(read_df)){read_df[[col_name]] <- as.numeric(read_df[[col_name]])}

    removeNotification(id)
    return (read_df)
  } 
  
  #reactive helper function for get_read_df
  get_metadata_df <- reactive({
    req(input$sample_data)
    metadata_df <- read_metadata(input$sample_data$datapath)
    return(metadata_df)
    })

  #get the appropriate data frame
  get_read_df <- reactive({
    req(input$taxon_table)
    read_df <- read_in_taxon_table(input$taxon_table$datapath, get_metadata_df())
    return(read_df)
    })
  
  # Calculate the proportional abundance of each organism
  get_prop_df <- reactive({
    req(input$taxon_table)
    read_df <- get_read_df()
    prop_df <- read_df / colSums(read_df)
    rownames(prop_df) <- sapply(
      rownames(prop_df),
      function(s){return(sub(" ", ".", s))}
    )
    return(prop_df)
  })
  
  get_total_reads_df <- reactive({
    # Get the metadata
    metadata_df <- get_metadata_df()
    # Get the number of reads
    read_df <- get_read_df()
    
    # Add a TOTAL column for the number of reads
    read_df <- rbind(read_df, colSums(read_df))
    rownames(read_df)[nrow(read_df)] <- "TOTAL"
    
    # We have the make the organism names into a column
    read_df$org <- rownames(read_df)
    read_long = as_tibble(melt(read_df, id="org"))
    tot_reads <- read_long[read_long$org == "TOTAL",]
    
    # Add all of the metadata columns
    for(col_name in colnames(metadata_df)){
      tot_reads[col_name] = as.vector(sapply(tot_reads["variable"], function(specimen_name){metadata_df[specimen_name, col_name]}))
    }
    
    return(tot_reads)
  })
  
  get_breakaway_df <- reactive({
    id <- showNotification("Running breakaway...")
    # Get the metadata
    metadata_df <- get_metadata_df()
    # Get the number of reads
    read_df <- get_read_df()
    
    # Run breakaway for each specimen
    breakaway_df <- data.frame(apply(
      read_df,
      2,
      function(counts){
        r <- breakaway(counts)
        return(c(
          estimate=r$estimate, 
          lower=r$interval[1], 
          upper=r$interval[2],
          error=r$error
        ))
      }
    ), stringsAsFactors = FALSE)
    breakaway_df <- data.frame(t(breakaway_df), stringsAsFactors = FALSE)
    
    breakaway_df$variable <- sapply(
      row.names(breakaway_df),
      function(s){return(sub("\\.", "-", s))}
    )

    # Add all of the metadata columns
    for(col_name in colnames(metadata_df)){
      values_to_replace <- sapply(breakaway_df$variable, function(specimen_name){metadata_df[specimen_name, col_name]})
      print(values_to_replace)
      breakaway_df[[col_name]] <- values_to_replace
    }
    
    breakaway_df$lower <- breakaway_df$estimate - breakaway_df$error
    breakaway_df$upper <- breakaway_df$estimate + breakaway_df$error
    removeNotification(id)
    return(breakaway_df)
  })
  
  get_corncob_df <- reactive({
    corncob_df <- run_corncob(get_read_df(), get_metadata_df())
    
    # Update the list to include all metadata columns
    list_of_options <- list()
    for(metadata_field in unique(corncob_df$metadata)){
      list_of_options[metadata_field] <- metadata_field
    }
    updateSelectInput(
      session,
      "select.corncob",
      "Select Metadata:",
      list_of_options
    )

    return (corncob_df)
  })
  
  output$corncob <- renderDataTable(
    filter(get_corncob_df(), metadata == input$select.corncob),
    options = list(
      pageLength = 10
    )
  )

  #method to plot a reads per sample graph
  plot_reads_per_sample <- function(tot_reads, sort_by){
    if(sort_by == "number_of_reads"){
      sort_by <- "value"
    } else if(sort_by == "sample_name"){
      sort_by <- "variable"
    }
    tot_reads["sort_by"] <- tot_reads[[sort_by]]

    p <- ggplot(
      data=tot_reads
    ) 
    if(sort_by %in% c("value", "variable")){
      p = p + 
        geom_bar(
          mapping = aes(
            x = reorder(variable, sort_by), 
            y = value
          ),
          stat = "identity"
        ) +
        xlab("Sample Name")
    }else{
      p = p + 
        geom_boxplot(
          mapping = aes(
            x = sort_by,
            y = value
          )
        ) +
        xlab(sort_by)
    }
    p = p  +
    ylab("Number of Reads") +
    ggtitle("Sequencing Depth") + theme_bw(
    ) + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
    
    print(p)  
  }
  
  #method to plot a reads per sample graph
  plot_breakaway_per_sample <- function(breakaway_df, sort_by){
    if(sort_by == "number_of_taxa"){
      sort_by <- "estimate"
    } else if(sort_by == "sample_name"){
      sort_by <- "variable"
    }
    breakaway_df$sort_by <- breakaway_df[[sort_by]]
    print(breakaway_df)
    p <- ggplot(
      data=breakaway_df
    )
    if(sort_by %in% c("estimate", "variable")){
      p <- p +
        geom_bar(
          mapping = aes(
            x = reorder(variable, sort_by), 
            y = estimate
          ),
          stat = "identity"
        ) + geom_errorbar(
          mapping = aes(
            x = reorder(variable, sort_by),
            ymin = lower,
            ymax = upper
          )
        ) +
        xlab("Sample Name")
    } else {
      p <- p +
        geom_boxplot(
          mapping = aes(
            x = sort_by, 
            y = estimate
          )
        ) +
        xlab(sort_by)
    }
    p <- p +
      ylab("Estimated number of taxa") +
      ggtitle("Alpha Diversity (breakaway)") + 
      theme_bw() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
    
    print(p)  
  }
  
  
  #method to plot a reads per sample graph
  plot_top_taxa_boxplot <- function(corncob_df, prop_df, metadata_df, selected_metadata, num_taxa){
    orgs_to_plot = head(
      filter(
        corncob_df, 
        metadata == selected_metadata
        ), 
      num_taxa
    )$organism
    
    # Make into long format
    prop_df$org <- rownames(prop_df)
    long_df = as_tibble(melt(
      prop_df[orgs_to_plot,], 
      id="org"
    ))
    
    # Figure out which metadata column to use
    metadata_col = NULL
    for(col_name in colnames(metadata_df)){
      if(startsWith(selected_metadata, col_name)){
        metadata_col <- col_name
      }
    }
    
    long_df$metadata <- sapply(
      long_df$variable,
      function(sample_name){
        return(
          metadata_df[sample_name, metadata_col]
        )
      }
    )
    
    p <- ggplot(
      data=long_df
    ) + geom_boxplot(
      mapping = aes(
        x = metadata,
        y = value
      )
    ) + facet_wrap(
      ~ org,
      scales = "free_y"
    ) + ylab(
      "Relative Abundance"
    ) + xlab(
      ""
    ) + theme_bw(
    ) + theme(
      axis.text.x = element_text(angle = 90, hjust = 1)
    )

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
      data=rename(long_df, percent=value, sample=Var2, organism=Var1)
    ) + geom_bar(
      aes(
        y = percent, 
        x = sample, 
        fill = organism
      ),
      stat="identity"
    ) + ylab(
      "Relative Abundance"
    ) + xlab(
      "Sample Name"
    ) + theme_bw(
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
    plot_stacked_bar_graph(get_read_df(), input$num_stacks)
  })
  
  #make the reads per sample graph
  output$reads_per_sample <- renderPlotly({
    req(input$sample_data)
    req(input$taxon_table)
    plot_reads_per_sample(get_total_reads_df(), input$sort.by)
  })
  
  #make the breakaway per sample graph
  output$breakaway_per_sample <- renderPlotly({
    req(input$sample_data)
    req(input$taxon_table)
    plot_breakaway_per_sample(get_breakaway_df(), input$sort.by.breakaway)
  })
  
  #make the top taxa boxplot
  output$top_taxa_boxplot <- renderPlotly({
    req(input$sample_data)
    req(input$taxon_table)
    plot_top_taxa_boxplot(get_corncob_df(), get_prop_df(), get_metadata_df(), input$select.corncob, input$num.corncob.plots)
  })

  #let the user know what files they've imported
  output$selected_sample <- renderText({ 
    paste("Metadata sheet: ", input$sample_data$name)
  })
  
  output$selected_tax <- renderText({ 
    paste("Taxon sheet: ", input$taxon_table$name)
  })
})

make_metadata_numeric <- function(metadata_df){
  # Convert each metadata category to a numeric
  for(col_name in colnames(metadata_df)){
    if(length(unique(metadata_df[[col_name]])) == 1){
      metadata_df <- metadata_df[,-which(colnames(metadata_df) == col_name)]
      next
    }
    if(is.numeric(metadata_df[[col_name]])){
      next
    }
    # Get the unique set of values
    col_values <- unique(metadata_df[[col_name]])
    for(col_value in col_values[1:(length(col_values) - 1)]){
      metadata_df[[paste(col_name, col_value, sep=".")]] <- as.numeric(metadata_df[[col_name]] == col_value)
    }
    metadata_df <- metadata_df[,-which(colnames(metadata_df) == col_name)]
  }
  return (metadata_df)
}

run_corncob <- function(reads_df, metadata_df){
  id <- showNotification("Running corncob...")
  metadata_df <- make_metadata_numeric(metadata_df)
  
  # Rotate the reads_df
  reads_df <- data.frame(t(reads_df), stringsAsFactors = FALSE)
  
  # Iterate over metadata columns
  all_results <- do.call(rbind, lapply(
    colnames(metadata_df),
    function(metadata_col){
      test_df <- data.frame(
        total=rowSums(reads_df),
        feature=metadata_df[[metadata_col]]
      )
      # Iterate over each organism
      return(do.call(rbind, lapply(
        colnames(reads_df),
        function(org_name){
          test_df$count <- reads_df[[org_name]]
          r <- bbdml(
            formula = cbind(count, total - count) ~ feature,
            phi.formula = ~ feature,
            data = test_df
          )
          s <- data.frame(summary(r)$coefficients, stringsAsFactors = FALSE)
          s$organism <- org_name
          s$metadata <- metadata_col
          return(s["mu.feature",])
        }
      )))
    }
  ))
  
  rownames(all_results) <- c(1:nrow(all_results))
  
  all_results <- rename(
    all_results, 
    p.value="Pr...t..",
    std.error="Std..Error",
    estimate="Estimate"
  )
  
  # Sort by p-value
  all_results <- all_results[order(all_results$p.value),]
  
  removeNotification(id)
  
  return(all_results)
  
}
