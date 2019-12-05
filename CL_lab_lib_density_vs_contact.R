### LIBDEM AND LABOUR DENSITY VS CONTACT RATE

library(ggplot2)
library(reshape2)


# source("scripts/clean_roadgroups.R")

generate_plot_libdem_labour <- function(roads) {

  roads$l_density <- (roads$Labour/roads$Contacted.ever)
  roads$lib_density <- (roads$Lib.Dem/roads$Contacted.ever)
  roads$contact.rate <- as.numeric(sub("%", "",roads$Contact.rate,fixed=TRUE))/100
  
  roads1 <- melt(roads, id.vars = c( "Ward", "Roadgroup", "contact.rate"),
                 measure.vars = c("Lib.Dem", "Labour"),
                 variable.name = "promise",
                 value.name = "density")

### libdem and labour density on fill vs contact rate on Y-axis per roadgroup

  uniq_wards <- unique(roads1$Ward)
  
  
  plot_list <- list()
  
  for (i in uniq_wards) {
    temp_plot <- ggplot(data = subset(roads1, roads1$Ward == i),
                        aes(x = Roadgroup, y = contact.rate, fill = density, color = promise)) +
      geom_bar(stat = "identity", position = position_dodge()) +
      coord_flip() +
      labs(fill = "Density", y = "Contact Rate", x = "Road Group") +
      scale_fill_gradient(low = "pink", high = "darkred") +
      labs(fill = "Density", y = "Contact Rate", x = "Road Group") +
      theme(axis.text.x = element_text(angle = 90), text = element_text(size = 8)) +
      ggtitle(i, subtitle = "Contact Rate vs Labour and Libdem Density")
    
    
    plot_name <- paste0("contact.vs.lib_lab_", i,".png")
    
    plot_list[[plot_name]] <- temp_plot
    
  }
  return(plot_list)

}