require(tidytuesdayR)
require(tidyverse)
require(ggthemes)
require(ggbeeswarm)

tuesdata <- tidytuesdayR::tt_load('2021-03-30')

drugstore <- c("CoverGirl","L'Oréal","Maybelline","Revlon",
               "e.l.f. Cosmetics","NYXProfessional Makeup","Milani",
               "Almay")

data <- tuesdata$allCategories %>% 
  filter(brand %in% drugstore)
shades <- data$hex
plot <- ggplot(data=data,aes(x = brand,y=lightness)) +
  geom_quasirandom(color=shades,size=3)+
  coord_flip() + 
  theme(panel.grid.major = element_line(linetype = "blank"), 
    panel.grid.minor = element_line(linetype = "blank"), 
    plot.title = element_text(size = 14, 
        face = "bold", colour = "gray27", 
        hjust = 0, vjust = 1), 
    axis.text = element_text(size = 14, face = "bold", 
                           colour = "gray27"), 
    axis.text.x = element_text(size = 0),
    plot.subtitle = element_text(colour = "gray27"),
    panel.background = element_rect(fill = "#9BBFB9"), 
    plot.background = element_rect(fill = "#9BBFB9")) +
  labs(title = "Available Shade Ranges of Drugstore Foundations",
       x = NULL,
       y = NULL,
       caption = "By @rppfeiffer | #TidyTuesday | Data: The Pudding")
plot
