require(tidyverse)
require(tidytuesdayR)
require(maps)
require(mapproj)
require(usmap)
tuesdata <- tidytuesdayR::tt_load('2021-04-13')
postoff <- tuesdata$post_offices
openpo <- postoff %>% 
  group_by(state) %>% 
  summarise(no_rows = length(state)) 

openpo$state <- factor(openpo$state)

openpo <- openpo[-c(8,24,48),]
  

states <- map_data("state")


plot_usmap(,data = openpo, values = "no_rows", color = "black", labels=FALSE)+
  scale_fill_continuous(low = "#3662DB", high = "#E02F2F",
                        name = "Total Number of Post Offices", 
                        label = scales::comma) +
  theme(legend.position = "right") +
  theme(panel.background = element_rect(colour = "black")) +
  labs(title = "Historical Post Office Distribution in the US") 
  


