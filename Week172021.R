require(tidytuesdayR)
require(tidyverse)
require(maps)
require(extrafont) #Indie FLower font available on fontsquirrel.com
require(patchwork)


# Pull in and clean data
tuesdata <- tt_load("2021-04-20")
netflix <- tuesdata$netflix_titles

LGBT <- netflix %>% 
  filter(str_detect(listed_in,"LGBT")) %>% 
  rename(region = country) 
LGBT$region <-str_replace_all(LGBT$region,'United States','USA')

LGBT$region <- factor(LGBT$region)

# Create map
mappal <- c('#86007D',"#0000F9","#008018","#FFFF41","#FFA52C","#FF0018")
world_map <- map_data("world")
LGBTmap <- LGBT %>% 
  group_by(region) %>% 
  summarise(no_rows = length(region))
mapdata <- left_join(LGBTmap,world_map, by = "region")

Map <- ggplot()+
  geom_polygon(world_map,mapping = aes(long,lat, group = group),fill='gray75')+
  geom_polygon(mapdata,mapping = aes(long,lat,fill=no_rows, group = group))+
  scale_fill_gradientn(colors=mappal) +
  theme(plot.title = element_text(family="Indie Flower",size=18,color="black"),
        legend.title = element_text(family="Indie Flower",
                                    size = 16,color = "black"),
        legend.text = element_text(family="Indie Flower",
                                   size = 14,color = "black"),
        axis.ticks = element_line(linetype = "blank"), 
    axis.text = element_text(colour = NA), 
    panel.background = element_rect(fill = NA)) +
  labs(title = "Country of Origin", x = NULL, 
    y = NULL, fill = "# of Movies")

Map
# Create pie chart of ratings
piepal <- c("#FF0018","#FFA52C","#FFFF41","#008018",
            "#0000F9","#86007D","#EC008C")
LGBT$rating <- factor(LGBT$rating)

LGBTpie <- LGBT %>% 
  group_by(rating) %>% 
  summarise(Count = length(rating))

pie <- ggplot(LGBTpie,aes(x="",y=Count,fill=rating))+
  geom_bar(width=1,stat="identity") +
  coord_polar("y",start = 0) +
  scale_fill_manual(values = piepal) + 
  theme(axis.ticks = element_line(linetype = "blank"), 
    panel.grid.major = element_line(linetype = "blank"), 
    plot.title = element_text(family = "Indie Flower",
                                 size = 18,color = "black"),
    axis.text = element_text(color=NA),
    panel.background = element_rect(fill = NA),
    legend.title = element_text(color = NA),
    legend.text = element_text(family = "Indie Flower",
                               size = 14,color = "black")) +
  labs(title = "Rating", x = NULL, y = NULL)

pie
  
#Create release year barchart 

LGBTyear <- LGBT %>% 
  group_by(release_year) %>% 
  summarise(Count = length(release_year))
yearpal <- c("#FF0018","#FFA52C","#FFFF41","#008018","#0000F9",'#86007D',
                                   "#FF0018","#FFA52C","#FFFF41","#008018","#0000F9",'#86007D',
                                   "#FF0018","#FFA52C","#FFFF41","#008018","#0000F9")

 
year <- ggplot(LGBTyear,aes(x= release_year,y= Count))+
  geom_bar(stat="identity",fill=yearpal,width=1) +
  scale_x_continuous(n.breaks = 6) +
  labs(title= "Release Year", x = NULL, y = "# of Available Movies/TV Shows")+
  theme(plot.title = element_text(family = "Indie Flower",
                                     size = 18,color = "black"),
        axis.text = element_text(family = "Indie Flower",
                                 size = 12,color = "black"),
        axis.title = element_text(family = "Indie Flower",
                                  size = 14,color = "black"),
        axis.ticks = element_line(linetype = "blank"),
        panel.background = element_rect(fill=NA))
year 
   

# create subgenre barchart
genrepal <- c("#FF0018","#FFA52C","#FFFF41","#008018","#0000F9",'#86007D',
              "#FF0018","#FFA52C")
subgenre <- data.frame(genre = c("Comedy","Documentary","Drama","Horror",
                              "Indie","International","Musical","Romantic"),
                    count = c(20,29,40,2,19,27,4,5))
subgenre$genre <- factor(subgenre$genre, 
                         levels = c("Drama","Documentary","International",
                                    "Comedy","Indie","Romantic","Musical","Horror"))
sgenre <- ggplot(subgenre,aes(y=reorder(genre,count), x = count, fill = genre))+
  geom_bar(stat="identity")+
  labs(title = "Genre", x = NULL, y = NULL)+
  scale_fill_manual(values = c("#FF0018","#FFA52C","#FFFF41","#008018",
                               "#0000F9",'#86007D',"#FF0018","#FFA52C")) +
  theme(plot.title = element_text(family = "Indie Flower",size = 18,color = "black"),
        axis.text = element_text(family = "Indie Flower",size = 12,color = "black"),
        axis.title = element_text(family = "Indie Flower",size = 14,color = "black"),
        axis.ticks = element_line(linetype = "blank"),
        panel.background = element_rect(fill=NA),
        legend.position = "none")

sgenre 

#Put together with patchwork
plot <- (Map - year) / (sgenre - pie) +
  plot_annotation(title = "LGBTQ Movies and TV Shows on Netflix", 
                  caption ="By @rppfeiffer | #TidyTuesday | Data from Kaggle",
                  theme = theme(plot.title = 
                                  element_text(family = "Indie Flower",
                                               size = 24,color = "black"),
                                plot.caption = 
                                  element_text(family = "Indie Flower",
                                               size = 14,color = "black")))
plot

#save plot
ggsave("Week172021.png",height = 6, width = 8, units = "in")
