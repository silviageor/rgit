
library("tidyverse")
library(reshape2)
library(ggplot2)
library(ggpubr)
library(cowplot)
library(RColorBrewer)
library(data.table)

all <- read_delim("CTD/MSM80_data.csv", ";") %>% filter(depth <=100) # select only the upper 100m

setnames(all, "Lat ", "Lat") # remove space from lat
legend <- "Nitrate \n[??mol/L]"
# set theme 
bw_update <- theme_bw() +
  theme(plot.background = element_blank(),
        panel.background = element_rect(colour = NA, fill = NA),
        panel.border = element_rect(colour = "black",
                                    fill = NA, size = 0.8),
        panel.grid.minor = element_line(colour = "grey",
                                        size = 0.1,
                                        linetype = "dashed"),
        panel.grid.major = element_line(colour = "black",
                                        size = 0.2,
                                        linetype = "solid"),
        axis.title = element_text(size = 10),
        axis.text = element_text(size = 10),
        axis.ticks = element_line(size = 0.5),
        axis.ticks.length = unit(2, "mm"),
        legend.position = "right",
        legend.direction = "vertical",
        legend.title = element_text(size = 10),
        legend.text = element_text( size = 8),
        legend.background = element_blank(),
        plot.title = element_text(size = 10, hjust = 0,face = "italic"),
        strip.background = element_rect(colour = "white", fill = "white"),
        strip.text = element_text(size = 8))



nitrate_plot <- ggplot(all)+
  geom_point(aes(x = Distance_from_shore, y = -depth,colour = all$Nitrate), size = 4)+
  scale_colour_distiller(name =legend, type = "seq", palette = "RdPu",trans = "reverse")+
  facet_wrap(. ~ Transect, strip.position = "top", ncol = 2)+
  scale_y_continuous("Depth [m]")+
  scale_x_reverse(name = 'Distance from shore (km)', limits=c(200,0))+
  bw_update+
  theme(panel.spacing = unit(1, "lines"),plot.title = element_text(size = 10, hjust = 0,face = "italic"),
        axis.title = element_text(size = 10),strip.text = element_text(face = "italic"))

##############################################################################################
T1<- filter(all, station_new %in% c("T1_1", "T1_2", "T1_3", "T1_4", "T1_5", "T1_6"))
T2<- filter(all, station_new %in% c("T2_1", "T2_2", "T2_3", "T2_4", "T2_5"))
T3 <- filter(all, station_new %in% c("T3_1", "T3_2", "T3_3", "T3_4", "T3_5"))
T4<- filter(all, station_new %in% c("T4_1", "T4_2", "T4_3", "T4_4", "T4_5", "T4_6", "T4_7"))
T5<- filter(all, station_new %in% c("T5_1", "T5_2", "T5_3", "T5_4", "T5_5"))
T6 <- filter(all, station_new %in% c("T6_1", "T6_2", "T6_3", "T6_4", "T6_5"))

T1chldepth <- filter(chlmaxdepth, station_new %in% c("T1_1", "T1_2", "T1_3", "T1_4", "T1_5", "T1_6"))
T2chldepth<- filter(chlmaxdepth, station_new %in% c("T2_1", "T2_2", "T2_3", "T2_4", "T2_5"))
T3chldepth <- filter(chlmaxdepth, station_new %in% c("T3_1", "T3_2", "T3_3", "T3_4", "T3_5"))
T4chldepth<- filter(chlmaxdepth, station_new %in% c("T4_1", "T4_2", "T4_3", "T4_4", "T4_5", "T4_6", "T4_7"))
T5chldepth<- filter(chlmaxdepth, station_new %in% c("T5_1", "T5_2", "T5_3", "T5_4", "T5_5"))
T6chldepth <- filter(chlmaxdepth, station_new %in% c("T6_1", "T6_2", "T6_3", "T6_4", "T6_5"))

## Transect_1 
T1_plot<- ggplot() + 
  geom_point(aes(x=Distance_from_shore, y = -Depth, colour = Nitrate), data = T1, size = 4)+
  scale_colour_distiller(name =legend, type = "seq", palette = "RdPu",trans = "reverse")+
  #geom_line(aes(x=Distance_from_shore,y=-Depth), data = T1chldepth)+
  ggtitle("Transect 1")+
  scale_x_reverse(name = 'Distance from shore (km)', limits=c(200,0))+
  scale_y_continuous("Depth [m]",limits=c(-100,0))+
  bw_update +
  theme(axis.title.x = element_blank(),
        legend.position = "none")

# calculate only surface nitrate
# T1_upper <-  T1_nitrate %>% filter(depth<30) %>%group_by(station) %>%   mutate(upper_nitrate = mean(Nitrate))

# Transect_2 
T2_plot <- ggplot() + 
  geom_point(aes(x=Distance_from_shore, y = -Depth, colour = Nitrate), data = T2, size = 4)+
  scale_colour_distiller(name =legend, type = "seq", palette = "RdPu",trans = "reverse")+
  #geom_line(aes(x=Distance_from_shore,y=-Depth), data = T2chldepth)+
  ggtitle("Transect 2")+
  scale_x_reverse(name = 'Distance from shore (km)', limits=c(200,0))+
  scale_y_continuous("Depth [m]",limits=c(-100,0))+
  bw_update+
  theme(axis.title.x = element_blank(),axis.title.y = element_blank(),
        legend.position = "none")


# Transect_3 
T3_plot <- ggplot() + 
  geom_point(aes(x=Distance_from_shore, y = -Depth, colour = Nitrate2), data = T3, size = 4) +
  scale_colour_distiller(name =legend, type = "seq", palette = "RdPu",trans = "reverse")+
#  geom_line(aes(x=Distance_from_shore,y=-Depth), data = T3chldepth)+
  ggtitle("Transect 3")+
  scale_x_reverse(name = 'Distance from shore (km)', limits=c(200,0))+
  scale_y_continuous("Depth [m]",limits=c(-100,0))+
  bw_update+
  theme(axis.title.x = element_blank(),
        legend.position = "none")

# Transect_4
T4_plot <- ggplot() + 
  geom_point(aes(x=Distance_from_shore, y = -Depth, colour = Nitrate2), data = T4, size = 4)+
  scale_colour_distiller(name =legend, type = "seq", palette = "RdPu",trans = "reverse")+
  #geom_line(aes(x=Distance_from_shore,y=-Depth), data = T4chldepth)+
  ggtitle("Transect 4")+
  scale_x_reverse(name = 'Distance from shore (km)', limits=c(200,0))+
  scale_y_continuous("Depth [m]",limits=c(-100,0))+
  bw_update#+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "none")
# Transect_5

T5_plot <- ggplot() + 
  geom_point(aes(x=Distance_from_shore, y = -Depth, colour = Nitrate2), data = T5, size = 4)+
  scale_colour_distiller(name =legend, type = "seq", palette = "RdPu",trans = "reverse")+
 # geom_line(aes(x=Distance_from_shore,y=-Depth), data = T5chldepth)+
  ggtitle("Transect 5")+
  scale_x_reverse(name = 'Distance from shore (km)', limits=c(200,0))+
  scale_y_continuous("Depth [m]", limits = c(-100,0))+
  bw_update+
  theme(legend.position = "none")

# Transect_6 
T6_plot <- ggplot() + 
  geom_point(aes(x=Distance_from_shore, y = -Depth, colour = Nitrate2), data = T6, size = 4)+
  scale_colour_distiller(name =legend, type = "seq", palette = "RdPu", trans = "reverse")+
 # geom_line(aes(x=Distance_from_shore,y=-Depth), data = T6chldepth)+
  ggtitle("Transect 6")+
  scale_x_reverse(name = 'Distance from shore (km)', limits=c(200,0))+
  scale_y_continuous("Depth [m]",limits=c(-100,0))+
  bw_update+
  theme(axis.title.y = element_blank(),
        legend.position = "none")
##Add all of them in one graph
all <- plot_grid(T1_plot, T2_plot, T3_plot,T4_plot,T5_plot,T6_plot,
                 ncol=2, nrow=3,
                 align = "v")

legend_b <- get_legend(T6_plot + theme(legend.position= "right")) #extract legend 

final_nit <- plot_grid(all,legend_b, ncol =2 , rel_widths = c(1, .15)) # give 10% for the legend bar
final_nit
##save the figure 
ggsave(filename = "nitrate.png",width = 10, height = 6, dpi = 150, units = "in", device='png')
