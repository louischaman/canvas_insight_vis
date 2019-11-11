### Turnoutness vs Labourness pipeline

#L_promise = Labour promise
#L_pv = Labour postal voter
#L_pv_pc = percentage of Labour postal voters
#pv = postal voters
#L_pv_density = density of Labour postal voters in the electors

library(ggplot2)
library(dplyr)
library(stringr)
library(gridExtra)
library(reshape2)
# 
# setwd("C:/Users/rpolya/Desktop/HackDay") # make new folder for different constituencies
# 
# roads <- read.csv("../sample_dat.csv")
# pt = generate_plots_turnoutness(roads)
generate_plots_turnoutness = function(roads){
  roads$Roadgroup <- gsub(":.*", "", roads$Roadgroup) #remove punctuation, space, etc
  roads$Roadgroup <- gsub("-.*", "", roads$Roadgroup)
  roads$Roadgroup <- gsub("/.*", "", roads$Roadgroup)
  roads$Roadgroup <- gsub(" ", "", roads$Roadgroup)
  roads$Roadgroup <- gsub("[A-Z][a-z]*$", "", roads$Roadgroup) #remove letters from end of string
  roads$Roadgroup <- gsub(" ", "", roads$Roadgroup)
  
  roads$Roadgroup <- as.character(roads$Roadgroup)
  roads$Roadgroup <-str_trunc(roads$Roadgroup, 7, "right", ellipsis = "")
  table(roads$Roadgroup)
  
  ### Create data
  ## Roadgroups data
  
  roads$L_density <- (roads$Labour/roads$Contacted.ever) #make density
  
  roads$Ward <- tolower(roads$Ward) #make lowercase
  
  
  ### Loop to extract all roadgroups for each ward into a separate object
  for (i in roads$Ward){
    x <- subset(roads, grepl(i, roads$Ward))
    assign(paste0(i, "", ""), x)
  }
  
  # LOOPS NEEDED FOR GGPLOT
  
  uniq_wards <- unique(roads$Ward)  #data
  
  plot_list <- list()
  
  for (i in uniq_wards) {
    
    plot_title <- paste0(toupper(i), " WARD ROAD GROUPS")
    
    temp_plot <- ggplot(data = subset(roads, Ward == i),
                        aes(x = reorder(Roadgroup, Turnoutness), y = Turnoutness, fill = Labourness)) +
                        geom_col() +
                        labs(fill = "Labourness") +
                        xlab("Road Group") +
                        ylab("Turnoutness") +
                        coord_flip() +
                        theme(axis.text.x = element_text(angle = 90),
                              text = element_text(size = 8)) +
                        scale_fill_gradient(low = "pink", high = "darkred") +
                        # coord_cartesian(xlim = NULL, ylim = c(0, 1)) +
                        ggtitle(plot_title) + theme_bw()
    
    plot_name <- paste0("Turnout_vs_Labour_", i,".png")
    
    plot_list[[plot_name]] <- temp_plot
    
  }
  
  return(plot_list)
}