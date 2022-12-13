library(tidyverse)
library(ggplot2)
library(scales)
library(dplyr)
library(magick)
library(rsvg)
library(devtools)
devtools::install_github("clauswilke/ggtextures")
library(ggtextures)
library(cowplot)
library(ggforce)
library(ggpubr)

setwd("/Users/christine/Documents/STAT 3280 Visualization R/DATA")
my_data <- read.csv('Vaccine Prices.csv', stringsAsFactors = F,head = T)

vaccineprices <- my_data[-6,]
colnames(vaccineprices) <- c("Name","USprice","EUprice")

vaccineprice <- vaccineprices %>% pivot_longer(cols=c('EUprice', 'USprice'),
                    names_to='country',
                    values_to='price')

data <- tibble(Name = vaccineprice$Name, country= vaccineprice$country, price=vaccineprice$price, 
               image = list(image_read("https://daubertcromwell.com/wp-content/uploads/euro-europe-flag-stars-round-512.png"),
                            image_read_svg("https://upload.wikimedia.org/wikipedia/commons/8/88/United-states_flag_icon_round.svg"),
                            image_read("https://daubertcromwell.com/wp-content/uploads/euro-europe-flag-stars-round-512.png"),
                            image_read_svg("https://upload.wikimedia.org/wikipedia/commons/8/88/United-states_flag_icon_round.svg"),
                            image_read("https://daubertcromwell.com/wp-content/uploads/euro-europe-flag-stars-round-512.png"),
                            image_read_svg("https://upload.wikimedia.org/wikipedia/commons/8/88/United-states_flag_icon_round.svg"),
                            image_read("https://daubertcromwell.com/wp-content/uploads/euro-europe-flag-stars-round-512.png"),
                            image_read_svg("https://upload.wikimedia.org/wikipedia/commons/8/88/United-states_flag_icon_round.svg"),
                            image_read("https://daubertcromwell.com/wp-content/uploads/euro-europe-flag-stars-round-512.png"),
                            image_read_svg("https://upload.wikimedia.org/wikipedia/commons/8/88/United-states_flag_icon_round.svg")
))
  

plot1 <- ggplot(vaccineprice, aes(factor(Name), price,fill = country), image= ) + 
  geom_bar(stat="identity", position = position_dodge(0.7),width = 0.5) + 
  scale_fill_brewer(palette = "Set1", direction = -1)+
  scale_y_continuous(labels=scales::dollar_format())+
  geom_text(aes(label = paste('$', format(price, digits=3),sep = "")),
            position = position_dodge(0.78), vjust = -0.3, color = "white", size=3.5)+
  labs(x = "", y = "Price per dose",
       title = "COVID-19 Vaccine Prices",
       subtitle = "Comparing Europe and the United States")+
  theme(axis.title.x = element_blank(), axis.text.x=element_blank(),axis.ticks.x=element_blank(),
        panel.grid.major = element_blank(),panel.grid.minor = element_line(color = "darkgray"),
         panel.background = element_rect(fill = 'black', colour = 'black'))


plot1 <- ggplot(vaccineprice, aes(factor(Name), price,fill = country), image= ) + 
  geom_bar(stat="identity", position = position_dodge(0.7),width = 0.5) + 
  scale_fill_brewer(palette = "Set1", direction = -1)+
  scale_y_continuous(labels=scales::dollar_format())+
  geom_text(aes(label = paste('$', format(price, digits=3),sep = "")),
            position = position_dodge(0.78), vjust = -0.3, color = "white", size=3.5)+
  labs(x = "", y = "Price per dose",
       title = "COVID-19 Vaccine Prices",
       subtitle = "Comparing Europe and the United States")+
  theme(axis.title.x = element_blank(), axis.text.x=element_blank(),axis.ticks.x=element_blank(),
        panel.grid.major = element_blank(),panel.grid.minor = element_line(color = "darkgray"),
        panel.background = element_rect(fill = 'black', colour = 'black'))


plot1.1  <- ggplot(data, aes(factor(Name), price,fill = country,image=image) ) + 
  geom_isotype_col(stat="identity", position = position_dodge(0.7), width = 0.5,
    img_height = NULL, img_width = grid::unit(1, "null"),
    ncol = 1, nrow = 1, hjust = 0.5, vjust = 0
  ) +
  #geom_bar(stat="identity", position = position_dodge(0.7),width = 0.5) + 
  scale_fill_brewer(palette = "Set1", direction = -1)+
  scale_y_continuous(labels=scales::dollar_format())+
  geom_text(aes(label = paste('$', format(price, digits=3),sep = "")),
            position = position_dodge(0.78), vjust = -0.3, color = "white", size=3.5)+
  labs(x = "", y = "Price per dose",
       title = "COVID-19 Vaccine Prices",
       subtitle = "Comparing Europe and the United States")+
  theme(axis.title.x = element_blank(), 
        axis.text.x=element_blank(), 
        axis.ticks.x=element_blank(),
        axis.text.y=element_text(color="darkgray"), 
        axis.title.y=element_text(color="darkgray"), 
        legend.position = "none",
        text=element_text(color="white"),
        panel.grid.major = element_blank(),panel.grid.minor = element_line(color = "darkgray"),
        panel.background = element_rect(fill = 'black', colour = 'black'),
        plot.background = element_rect(fill = "black", color = "black"))


pimage <- axis_canvas(plot1.1, axis = 'x') + 
  draw_image("https://www.drugdiscoverytrends.com/wp-content/uploads/2021/07/pfizer-biontech-768x499.gif", x = 0.5, scale = 0.7)+
  draw_image("https://upload.wikimedia.org/wikipedia/commons/thumb/6/6a/Moderna_logo.svg/460px-Moderna_logo.svg.png?20220104162410", x = 1.55, scale = 0.8)+
  draw_image("https://upload.wikimedia.org/wikipedia/commons/thumb/7/79/Sanofi_logo.svg/1515px-Sanofi_logo.svg.png?20191206133144", x = 2.5, scale = 0.6)+ 
  draw_image("https://logodownload.org/wp-content/uploads/2017/08/johnson-johnson-logo-7.png", x = 3.5, scale = 0.6)+
  draw_image("https://thedotdev.com/wp-content/uploads/2018/05/AstraZeneca-Logo.png", x = 4.6, scale = 1) 
plot1.2 <- ggdraw(insert_xaxis_grob(plot1.1, pimage, position="left")) 

plot1.2

pricedifference <- vaccineprices%>% 
  mutate(pricediffpercent = ifelse(USprice>EUprice, 
                                   round(((USprice - EUprice)/EUprice),3),
                                   round(((USprice - EUprice)/USprice),3)))%>% 
  mutate(x = 1:5)


plot2 <- ggplot(pricedifference)+
  geom_arc_bar(aes(x0 = x, y0 = 0, r0 = 0, 
                   r = -0.75 * sign(pricediffpercent) * sqrt(abs(pricediffpercent)),
                   amount = 1),stat = "pie", sep = pi, fill = "darkgray") +
  geom_text(aes(x = x, y = sign(pricediffpercent)/10, 
        label = scales::percent(pricediffpercent)),
        color = "white", size = 4)+
  geom_hline(yintercept = 0, color = "gray") +
  coord_fixed(clip="off") +
  labs(x="", y="", title = "Price difference %", subtitle="% that U.S. is paying more",
       caption = "Data source: Washington Post \n Values are in USD")+
  annotate("text", x = 1,  y = -0.5 , label = "% that U.S. is paying less", color="darkgray", size=3)+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = 'black', colour = 'black'),
        plot.background = element_rect(fill = "black", color = "black"),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(), 
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(), 
        axis.ticks.y=element_blank(),
        text=element_text(color="white"),
        plot.title=element_text(size=9),
        plot.subtitle=element_text(size=8.5, color="darkgray"),
        plot.caption=element_text(color="darkgray"))
  
plot2
  

