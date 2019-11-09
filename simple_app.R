library(shiny)
library(magrittr)
library(zip)
library(ggplot2)

source('CL_ward_contact_vs_labour_density.R')
source("CL_roadgroup_contact_vs_labour_density.R")

# generate list of plot
generate_plot_list <- function(){
  plot_list = list()
  for( i in 1:10 ){
    scatter <- ggplot(data=iris, aes(x = Sepal.Length, y = Sepal.Width)) +
      geom_point(aes(color=Species, shape=Species)) +
      xlab("Sepal Length") +  ylab("Sepal Width") +
      ggtitle("Sepal Length-Width")
    
    plot_file_name = paste0(i,'.png') 
    plot_list[[plot_file_name]] = scatter
  }
  return(plot_list)
}

# generate temp files from list
generate_plot_zip <- function(zip_file, plot_list){
  tmp <- tempfile()
  on.exit(unlink(tmp))
  dir.create(tmp)
  file_name_array = c()
  for(i in 1:length(plot_list)){
    
    plot_file_name = file.path(tmp, names(plot_list)[i]  )
    on.exit(unlink(plot_file_name))
    ggsave(plot_list[[i]], file = plot_file_name)
    print(plot_file_name)
    file_name_array = c(file_name_array, plot_file_name)
  }
  zipr(zip_file, file_name_array)
  return(file_name_array)
}

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
        message = paste0("Creating your plots this may take a few seconds"),
        value = 0,
        {
          input_data = getData()
          plot_list_ward = generate_plots_ward_contact(input_data)
          shiny::incProgress(1/10)
          plot_list_road = generate_plots_road_contact(input_data)
          shiny::incProgress(3/10)
          plot_list = c(plot_list_ward, plot_list_road)
          generate_plot_zip(file, plot_list)
        }
      
    )}
  )
}

shinyApp(ui, server)