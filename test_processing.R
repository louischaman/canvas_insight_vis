library(shiny)
library(magrittr)
library(zip)
library(ggplot2)


source('CL_ward_contact_vs_labour_density.R')
source("CL_roadgroup_contact_vs_labour_density.R")
source("CL_Tory_Labour_density_roadgroups.R")
source("turnoutness_vs_labourness.R")
source("generate_plot_zip.R")

input_data = read.csv('../priorityareas.csv', sep = ",",
                      header = T, row.names = NULL, quote = "\"")
names(input_data)

input_data$Contact.rate =  as.numeric(gsub("%", "",input_data$Contact.rate))/100
input_data$Promise.rate =  as.numeric(gsub("%", "",input_data$Promise.rate))/100
input_data$Turnoutness =  as.numeric(gsub("%", "",input_data$Turnoutness))/100
input_data$Labourness =  as.numeric(gsub("%", "",input_data$Labourness))/100

plot_list_ward = generate_plots_ward_contact(input_data)
plot_list_road = generate_plots_road_contact(input_data)
plot_list_tory = generate_plots_tory_labour_density(input_data)
plot_list_turnout = generate_plots_turnoutness(input_data)
plot_list = c(plot_list_ward, plot_list_road, plot_list_tory, plot_list_turnout)
generate_plot_zip_simple('../test.zip', plot_list)

char_data = input_data
for (col in names(input_data)){
  char_data[col] = char_data[[col]] %>% as.character() %>% substring(1,40)
}
