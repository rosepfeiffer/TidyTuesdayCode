require(tidyverse)
require(tidytuesdayR)
require(usmap)
tuesdata <- tidytuesdayR::tt_load('2021-04-13')
postoff <- tuesdata$post_offices
openpo <- postoff %>% 
  group_by(state) %>% 
  summarise(no_rows = length(state)) 

openpo$state <- factor(openpo$state)

openpo <- openpo[-c(8,24,48),]

plot <- plot_usmap(,data = openpo, values = "no_rows", color = "black", labels=FALSE)+
  scale_fill_continuous(low = "#3662DB", high = "#E02F2F",
                        name = "Total Number of Post Offices", 
                        label = scales::comma) +
  theme(legend.position = "right") +
  theme(panel.background = element_rect(colour = "black"),
        plot.caption = element_text(hjust = 2.1)) +
  labs(title = "Historical Post Office Distribution in the US",
       caption = 
         "By @rppfeiffer|#TidyTuesday|Data:Cameron Blevins & Richard W. Helbock") 
plot  

ggsave("Week162021.png",plot = plot, height = 5, width = 7, units = 'in')
