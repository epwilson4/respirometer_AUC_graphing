#Title: Respirometer AUC graphing
#Name: Emma P Wilson
#Date last updated: 4/7/26

#Libraries
library(tibble)
library(tidyverse)
library(tidyr)
library(dplyr)
library(ggplot2)

#Function - to read in csv file
read_in_csv <- function(file_path) {
 data <- read.csv(file_path)
 data <- as_tibble(data)
 return(data)
}

#Calls function to read in csv file
respirometer_data <- read_in_csv("cyr1_mutants_respirometer_data.csv")

colnames(respirometer_data)

plot_media <- function(data, media, parent_strain) {
  #Filters out all other media types, condensing data to only include data points in the media type inputted
  filtered_data <- filter(data, Media == media)
  
  # Get parent strain's AUC for each replicate
  parent_AUC <- filtered_data %>%
    #filters out all other strains, condensing data to only include the parental strain
    filter(Strain == parent_strain) %>%
    #Saves only two columns from the data: replicate and area under the curve 
    select(Replicate, parent_AUC = Area.under.the.curve)
  
  # Join parent AUC by replicate, normalize, then average
  averaged_data <- filtered_data %>%
    left_join(parent_AUC, by = "Replicate") %>%
    #Mutate adds new column to data frame called normalized AUC
    mutate(normalized_AUC = Area.under.the.curve / parent_AUC) %>%
    group_by(Strain, Media) %>%
    summarise(avg_normalized_AUC = mean(normalized_AUC)) %>%
    ungroup()
  
  ggplot(averaged_data, aes(x = Strain, y = avg_normalized_AUC, fill = Strain)) +
    geom_bar(stat = "identity") +
    labs(
      title = paste("AUC", media),
      x = "Strain",
      y = "Relative Growth (normalized to parent)"
    ) +
    theme_minimal()
}

plot_ASGH_DMSO <- plot_media(respirometer_data, "2016 ASGH + 1% DMSO", "Y2084")
plot_ASGH_PROTO <- plot_media(respirometer_data, "2016 ASGH + 18.75 mg/L protodioscin ", "Y2084")
plot_YPX <- plot_media(respirometer_data, "YPX", "Y2084")

plot_YPX
plot_ASGH_PROTO
#plot_ASGH_DMSO

