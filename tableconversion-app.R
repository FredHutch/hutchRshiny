install.packages("tidyverse")
library(tidyverse)
library(reshape2)
library(ggplot2)

read_metadata <- function(sample_data_csv){
  # Read in metadata
  return(read_csv(sample_data_csv))
}

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

metadata_df <- read_metadata("sample_data.csv")

read_df <- read_in_taxon_table(
  "taxon_table.csv",
  metadata_df
)

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

plot_stacked_bar_graph(read_df, 10)
plot_reads_per_sample(read_df, "sample_name")
