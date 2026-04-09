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

plot_media <- function(data, media) {
  filtered_data <- filter(data, Media == media)
  averaged_data <- filtered_data %>%
    group_by(Strain, Media) %>%
    summarise(avg_AUC = mean(Area.under.the.curve))
  ggplot(averaged_data, aes(x= Strain, y= avg_AUC, fill = Strain))+
    geom_bar(stat = "identity")+
    labs(
      title = paste("AUC ", media),
      x = "Strain",
      y = "Relative Growth"
    ) +
    theme_minimal()
}

plot_ASGH_DMSO <- plot_media(respirometer_data, "2016 ASGH + 1% DMSO" )
plot_ASGH_PROTO <- plot_media(respirometer_data, "2016 ASGH + 18.75 mg/L protodioscin ")
plot_YPX <- plot_media(respirometer_data, "YPX")

plot_ASGH_DMSO