library(shiny)
library(magrittr)
library(zip)
library(ggplot2)

source('CL_ward_contact_vs_labour_density.R')
source("CL_roadgroup_contact_vs_labour_density.R")
source("CL_Tory_Labour_density_roadgroups.R")
source("turnoutness_vs_labourness.R")
source("generate_plot_zip.R")

ui <- fluidPage(
  fluidPage(
    titlePanel("Uploading Files"),
    sidebarLayout(
      sidebarPanel(
        fileInput('file1', 'Choose CSV File',
                  accept=c('text/csv', 
                           'text/comma-separated-values,text/plain', 
                           '.csv')),
        tags$hr(),
        downloadButton('downloadData', 'Download')
      ),
      mainPanel(
        tableOutput('contents')
      )
    )
  )
)

server <- function(input, output) {
  
  getData <- reactive( {
    
    inFile <- input$file1
    
    if (is.null(input$file1))
      return(NULL)
    
    read.csv(inFile$datapath, sep = ",",
             header = T, row.names = NULL, quote = "\"")
    
  } )
  
  
  output$contents <- renderTable(
    
    getData()
    
  )
  
  output$downloadData <- downloadHandler(
    
    filename = function() { 
      paste("insight-plots-", Sys.Date(), ".zip", sep="")
    },
    
    content = function(file) {
      shiny::withProgress(
        message = paste0("Creating your plots"),
        value = 0,
        {
          input_data = getData()
          plot_list_ward = generate_plots_ward_contact(input_data)
          shiny::incProgress(1/10)
          plot_list_road = generate_plots_road_contact(input_data)
          shiny::incProgress(2/10)
          plot_list_tory = generate_plots_tory_labour_density(input_data)
          shiny::incProgress(3/10)
          plot_list_turnout = generate_plots_turnoutness(input_data)
          shiny::incProgress(4/10)
          plot_list = c(plot_list_ward, plot_list_road, plot_list_tory, plot_list_turnout)
          generate_plot_zip(file, plot_list)
        }
      
    )}
  )
}

shinyApp(ui, server)
