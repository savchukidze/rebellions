library(dplyr)
library(stringr)
library(ggplot2)
library(ggbeeswarm)
library(viridis)
library(season)


rebellions <- read.csv('rebellions.csv', stringsAsFactors = F, encoding = "UTF-8")

rebellions$class <- case_when(
   
   between(rebellions$percent, 60, 100) ~ '5',
   between(rebellions$percent, 50, 60) ~ '4',
   between(rebellions$percent, 30, 50) ~ '3',
   between(rebellions$percent, 15, 30) ~ '2',
   between(rebellions$percent, 0, 15) ~ '1'
)

parties <- rebellions %>% 
   dplyr::select(party) %>% 
   unique.data.frame()

png(filename = 'rebellions.png', width = 2000, height = 2000)

g <- ggplot(rebellions)+
   geom_quasirandom(aes(x = reorder(party, percent), y = percent,
                        color = percent), varwidth = T, alpha = 0.9, size = 8)+
   
   scale_y_continuous(expand = c(0, 0.25),
                      breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90),
                      labels = c('0%', '10%', '20%', '30%', '40%', '50%', '60%', '70%','80%','90%'),
                      limits = c(-30, 95), position = 'top')+
   
   scale_color_viridis(option = "D", direction = -1, alpha = 1)+
   coord_flip()+
   labs(x = NULL, y = NULL,
        title = 'Як часто депутати голосують проти лінії фракції',
        subtitle = 'Графік демонструє наскільки часто нардеп голосує не так, як більшість фракції/групи до якої він входить. \r\nКожна точка — це один депутат.',
        caption = 'Дані: rada4you.org')+
   theme_minimal(base_family = 'Montserrat Medium')+
   theme(
      axis.text = element_text(size = 28, hjust = 0.5, color = "black"),
      axis.text.y = element_blank(),
      axis.ticks.length = unit(0.5, 'lines'),
      panel.grid.major = element_line(linetype = "dotted", size = 0.3, color = '#33385a'),
      panel.spacing.x = unit(5, 'lines'),
      panel.grid.minor = element_blank(),
      legend.position = 'none',
      plot.title = element_text(family ="Montserrat", face = "bold", size = 65, margin = margin(t = 30, b = 30)),
      plot.subtitle = element_text(family ="Montserrat Medium", size = 32, margin = margin(t = 20, b = 20), color = "black"),
      plot.background = element_rect(fill = "#ebf1fb", color = "#ebf1fb", size = 10, linetype='solid'),
      plot.caption = element_text(face = "bold", size = 30, margin = margin(t = 40), color = "#5D646F"),
      plot.margin = unit(c(2, 2, 1.5, 2), "cm"))

g = g + geom_text(data = parties,
                  aes(y = -4, x = party, 
                      label = party, 
                      family = "Ubuntu Condensed"), 
                  hjust = 1, vjust = -1,  
                  color = "black", size = 11)

g

dev.off()