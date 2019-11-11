### Contact Rate vs Labourness pipeline

library(ggplot2)
library(dplyr)

# setwd("../Desktop/campaignlab/") # make new folder for different constituencies
# 
# ### Create data
# ## Roadgroups data
# roads <- read.table("../data//example_pv_ward_road_mosaic.csv", sep = ",",
#                            header = T, row.names = NULL, quote = "\"")

generate_plots_road_contact = function(roads){
  names(roads) <- tolower(names(roads))
  roads$l_density <- (roads$labour/roads$contacted.ever) #make density
  roads$ward <- tolower(roads$ward) #make lowercase
  
  
  # ### Loop to extract all roadgroups for each ward into a separate object
  # for (i in roads$Ward){
  #   x <- subset(roads, grepl(i, roads$Ward))
  #   assign(paste0(i), x)
  # }
  
  ###Road group level contact rate and Labour density to show where we have least contact
  uniq_wards <- unique(roads$ward)
  
  plot_list <- list()
  
  for (i in uniq_wards) {
    temp_plot <- ggplot(data = subset(roads, ward == i),
                        aes(x = reorder(roadgroup, contact.rate),
                            y = contact.rate,
                            fill = l_density)) +
      geom_col() +
      coord_flip() +
      labs(fill = "Labour\nDensity", y = "Contact Rate", x = "Road Group") +
      theme(axis.text.x = element_text(angle = 90),
            text = element_text(size = 8)) +
      scale_fill_gradient(low = "pink", high = "darkred") +
      ggtitle(i, subtitle = "Contact Rate and Labour Density")
    
    plot_name = paste0("contact.rate_vs_L.density_", i,".png")
    
    plot_list[[plot_name]] = temp_plot
     
    # ggsave(temp_plot, file=paste0("contact.rate", i,".png"), width = 14, height = 10, units = "cm")
  }
  return(plot_list)
}
# plot_list = generate_plots_road_contact(roads)
