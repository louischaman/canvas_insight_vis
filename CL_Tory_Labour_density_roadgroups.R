library(ggplot2)
# 
# setwd("../Desktop/campaignlab")
# 
# ### Create data
# ## Roadgroups data
# roads <- read.table("../sample_dat.csv", sep = ",", header = T, row.names = NULL,
#                     quote = "\"")

generate_plots_tory_labour_density <- function(roads) {
  tory_roadgroups <- roads
  tory_roadgroups$tory.against = roads$Conservative + roads$Against
  tory_roadgroups$T.density <- tory_roadgroups$tory.against / tory_roadgroups$Contacted.ever
  tory_roadgroups$L.density <- tory_roadgroups$Labour / tory_roadgroups$Contacted.ever
  
  uniq_wards <- unique(tory_roadgroups$Ward)
  
  plot_list <- list()
  
  for (i in uniq_wards) {
    temp_plot <- ggplot(data = subset(tory_roadgroups, Ward == i),
                        aes(x = reorder(Roadgroup, -T.density),
                            y = T.density,
                            fill = L.density)) +
      geom_col(position = "dodge2", width = 0.7) +
      labs(fill = "Labour density", y = "Tory/Against density", x = "Road Group") +
      theme(axis.text.x = element_text(angle = 90),
            text = element_text(size = 8)) +
      scale_fill_gradient(low = "pink", high = "darkred") +
      ggtitle(i, subtitle = "Tory and Labour density per road group")
    
    plot_name <- paste0("Tory_Labour_density_", i,".png")
    
    plot_list[[plot_name]] <- temp_plot
    
  }
  
  return(plot_list)

}

# generate_plots_tory_labour_density(roads)
