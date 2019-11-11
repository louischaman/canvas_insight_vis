### Ward Level Contact Rate vs Labour Density pipeline

library(ggplot2)
library(dplyr)


generate_plots_ward_contact <- function(roads) {
  names(roads) <- tolower(names(roads))
  roads$l_density <- (roads$labour/roads$contacted.ever) #make density
  roads$ward <- tolower(roads$ward) #make lowercase
  
  ###Plot contact rate for each WARD
  contact.labourness <- data.frame(ward = roads$ward, contact.rate = roads$contact.rate,
                                   contact.2019 = roads$contacted.in.2019, l_density = roads$l_density,
                                   electors = roads$electors)
  
  contact.labourness <- cbind(aggregate(contact.2019 ~ ward, sum, data = contact.labourness),
                              aggregate(electors ~ ward, sum, data = contact.labourness),
                              aggregate(l_density ~ ward, mean, data = contact.labourness),
                              aggregate(contact.rate ~ ward, mean, data = contact.labourness))
  contact.labourness <- contact.labourness[,-c(3,5,7) ]
  contact.labourness$rate.2019 <- contact.labourness$contact.2019 / contact.labourness$electors
  
  ### Ward-level contact rate and Labour density
  ward_plot <- ggplot(contact.labourness, aes(reorder(x = ward, l_density), y = rate.2019, fill = l_density)) +
    geom_col() +
    coord_flip() +
    labs(fill = "Labour\nPromise\nDensity") +
    xlab("Ward") +
    ylab("Contact Rate 2019 (2019 Contacts / Electors)") +
    theme(axis.text.x = element_text(angle = 90),
          text = element_text(size = 8)) +
    scale_fill_gradient(low = "pink", high = "darkred") +
    ggtitle("Ward Contact Rate and Labour Density")
  plot_list = list('ward_plot.png' = ward_plot)
  return(plot_list)
}
