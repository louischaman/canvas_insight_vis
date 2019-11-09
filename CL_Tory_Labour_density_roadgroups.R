library(ggplot2)
# 
# setwd("../Desktop/campaignlab")
# 
# ### Create data
# ## Roadgroups data
# roads <- read.table("data/example_pv_ward_road_mosaic.csv", sep = ",", header = T, row.names = NULL,
#                     quote = "\"")

generate_plots_tory_labour_density <- function(roads) {
  tory_roadgroups <- data.frame(roadgroup = roads$Roadgroup, ward = roads$Ward,
                                tory.against = roads$Conservative + roads$Against,
                                Labour = roads$Labour, Electors = roads$Electors)
  tory_roadgroups$T.density <- tory_roadgroups$tory.against / tory_roadgroups$Electors
  tory_roadgroups$L.density <- tory_roadgroups$Labour / tory_roadgroups$Electors
  
  uniq_wards <- unique(tory_roadgroups$ward)
  
  plot_list <- list()
  
  for (i in uniq_wards) {
    temp_plot <- ggplot(data = subset(tory_roadgroups, ward == i),
                        aes(x = reorder(roadgroup, -T.density),
                            y = T.density,
                            fill = L.density)) +
      geom_col(position = "dodge2", width = 0.7) +
      labs(fill = "Labour density", y = "Tory/Against density", x = "Road Group") +
      theme(axis.text.x = element_text(angle = 90),
            text = element_text(size = 8)) +
      scale_fill_gradient(low = "pink", high = "darkred") +
      ggtitle(i, subtitle = "Tory and Labour density per road group")
    
    plot_name <- paste0("tory_labour_density_roadgroups_", i,".png")
    
    plot_list[[plot_name]] <- temp_plot
    
  }
  
  return(plot_list)

}

generate_plots_tory_labour_density(roads)
