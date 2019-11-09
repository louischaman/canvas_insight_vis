### Ward Level Contact Rate vs Labour Density pipeline

library(ggplot2)
library(dplyr)


generate_plots_ward_contact <- function(roads) {
  roads$L_density <- (roads$Labour/roads$Contacted.ever) #make density
  roads$Ward <- tolower(roads$Ward) #make lowercase
  
  ###Plot contact rate for each WARD
  contact.labourness <- data.frame(ward = tolower(roads$Ward), contact.rate = roads$Contact.rate,
                                   contact.2019 = roads$Contacted.in.2019, L_density = roads$L_density,
                                   electors = roads$Electors)
  
  contact.labourness <- cbind(aggregate(contact.2019 ~ ward, sum, data = contact.labourness),
                              aggregate(electors ~ ward, sum, data = contact.labourness),
                              aggregate(L_density ~ ward, mean, data = contact.labourness),
                              aggregate(contact.rate ~ ward, mean, data = contact.labourness))
  contact.labourness <- contact.labourness[,-c(3,5,7) ]
  contact.labourness$rate.2019 <- contact.labourness$contact.2019 / contact.labourness$electors
  
  ### Ward-level contact rate and Labour density
  ward_plot <- ggplot(contact.labourness, aes(reorder(x = ward, L_density), y = rate.2019, fill = L_density)) +
    geom_col() +
    coord_flip() +
    labs(fill = "Labour\nPromise\nDensity") +
    xlab("Ward") +
    ylab("Labour Density") +
    theme(axis.text.x = element_text(angle = 90),
          text = element_text(size = 8)) +
    scale_fill_gradient(low = "pink", high = "darkred") +
    ggtitle("Ward Contact Rate and Labour Density")
  plot_list = list('ward_plot.png' = ward_plot)
  return(plot_list)
}