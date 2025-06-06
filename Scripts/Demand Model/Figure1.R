# Fig. 1 Demand Results
# PBH June 2024
# Fig widths note: 1 column=5.7cm, 2 col=12.1cm or 3 col=18.4cm
# Code to create Figure 1 of article


source("Scripts/00-Libraries.R", encoding = "UTF-8")
source("Scripts/01-CommonVariables.R", encoding = "UTF-8")


demand <- read.csv("Parameters/Demand.csv")
demandSector <- read.csv("Parameters/Demand_Detail.csv")
demandRegion <- read.csv("Parameters/Demand_Region.csv")

# rec capacity 2050
demandSector %>% filter(t==2050) %>% filter(Vehicle=="Recycling") %>% 
  left_join(tibble(Scenario=scens_selected,name=scens_names)) %>% 
  dplyr::select(-t,-Vehicle,-Scenario) %>% 
  mutate(Demand=-Demand/1e6) # mtons


theme_set(theme_bw(8)+ theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),axis.title.y=element_text(angle=0,margin=margin(r=0))))

# Demand -----
data_fig1 <- demand %>% 
  filter(t<2051) %>% 
  group_by(Scenario,t) %>%
  reframe(kton=sum(Demand)) %>% ungroup() %>% 
  left_join(tibble(Scenario=scens_selected,name=scens_names)) %>% 
  mutate(name=factor(name,levels=scens_names)) %>% 
  mutate(scen_num=str_extract(name,paste(paste0("\\(",1:11,"\\)"),collapse="|")))

cum_demand <- data_fig1 %>% group_by(name) %>% 
  reframe(Mton=sum(kton)/1e3) %>% ungroup() %>% arrange(desc(Mton))

p0 <- cum_demand %>%
  mutate(name=factor(name,levels=cum_demand$name)) %>%
  ggplot(aes(name,Mton,fill=name))+geom_col()+
  coord_flip(expand = F)+labs(x="",y="Cumulative Demand [million tons Li metal")+
  scale_x_discrete(limits=rev)+
  scale_fill_manual(values = scen_colors)+
  theme(legend.position = "none")
p0

# data_fig1 <- data_fig1 %>% 
#   mutate(name=factor(name,levels=cum_demand$name))

p1 <- ggplot(data_fig1)+
  geom_line(aes(t,kton,group=name,col=name),alpha=.5,linewidth=.5)+
  # highlight ref scenario
  geom_line(data=filter(data_fig1,str_detect(name,"Refere")),
            aes(t,kton,group=name,col=name),alpha=.7,linewidth=.8)+
  geom_text(data=filter(data_fig1,t==2050),show.legend = F,
            aes(label=scen_num,y=kton,col=name),x=2050.5+
              c(-0.3,-0.3,0,0,0,0,0,0.3,0.3,0,0), # nudge x
            size=6*5/14 * 0.8,
            # order: 1,8,9,10,2,3,11,5,4,7,6
            nudge_y = c(0,2,0,-0.8,0,0,0,0.5,2,-0.2,0)*5e1)+
  coord_cartesian(expand=F,xlim=c(2022,2051.1),
                  ylim=c(0,max(data_fig1$kton)*1.02))+
  labs(x="",y="",col="Demand Scenario",tag = "a",
       title="Primary Lithium Demand [ktons]")+
  scale_x_continuous(breaks = c(2022, 2030, 2040, 2050))+
  scale_y_continuous(limits = c(0,NA),labels = scales::comma_format(big.mark = ' '))+
  scale_color_manual(values = scen_colors)+
  theme(panel.spacing.x = unit(0.7, "cm"),
        axis.text.x = element_text(size=9),
        axis.text.y = element_text(size=9),
        # legend.position = "none",
        plot.tag = element_text(face = "bold"),
        legend.position = c(0.18,0.64),
        legend.text = element_text(size=5.3),
        legend.background = element_rect(fill = "transparent", color = NA),
        legend.key.height= unit(0.25, 'cm'),
        legend.key.width= unit(0.25, 'cm'))
p1

write.csv(dplyr::select(data_fig1,name,t,kton),"Figures/Data Sources/Fig1a.csv",row.names = F)

# cumulative demand
unique(demandSector$Vehicle)
data_fig1a <- demandSector %>% 
  mutate(Sector=case_when(
    Vehicle=="Additional LIB" ~ "LIB Replacement\nfor EVs",
    Vehicle %in% c("Heavy truck","Medium truck","Bus") ~ "Heavy-duty",
    Vehicle=="Stationary Power Storage" ~ "SSPS",
    Vehicle =="Two/Three Wheelers" ~ "2-3 Wheelers",
    Vehicle %in% c("Car","Van") ~ 'Car',
    T ~ Vehicle) %>% 
      factor(levels = c("Other Sectors","SSPS","2-3 Wheelers",
                        "Heavy-duty","LIB Replacement\nfor EVs","Car",'Recycling'))) %>% 
  filter(t<2051) %>% 
  group_by(Scenario,Sector) %>% 
  reframe(Demand=sum(Demand)/1e6) %>% 
  left_join(tibble(Scenario=scens_selected,name=scens_names)) %>% 
  mutate(name=paste0("",str_extract(name,paste(paste0(11:1),collapse = "|")))) %>% 
  mutate(name=factor(name,levels=paste0("",1:11)))


p1a <- ggplot(data_fig1a,aes(name,Demand,fill=Sector))+
  geom_col(col="black",linewidth=0.1)+
  scale_fill_viridis_d(option = 7)+
  labs(x="Demand Scenario",y="",tag="b",
       title = "2022-2050 Demand [Mtons]",
       fill="")+
  theme(axis.text.y = element_text(size=9),
        axis.text.x = element_text(size=5.5),
        plot.tag = element_text(face = "bold"),
        legend.text = element_text(size=6))
p1a

write.csv(dplyr::select(data_fig1a,name,Sector,Demand),"Figures/Data Sources/Fig1b.csv",row.names = F)


# save as panel

cowplot::plot_grid(p1,p1a,ncol=2,rel_widths = c(0.61,0.39))

# Save with width size of letter
ggsave("Figures/Figure1.png", ggplot2::last_plot(),
       units="cm",dpi=600,
       width=18.4,height=6.4)

pdf("Figures/PDF/Figure1.pdf",width=18.4/2.54,height=6.4/2.54)
ggplot2::last_plot()
dev.off()

ggsave("Figures/Vector/Figure1.svg", ggplot2::last_plot(),
       units="cm",dpi=600,
       width=18.4,height=6.4)


# Cumulative demand by region
data_fig <- demandRegion %>% 
  mutate(Region=case_when(
    Region %in% c("China") ~ "China",
    Region %in% c("Australia/NZ","ASEAN",
                  "Other Asia Pacific","Japan",
                  "South Korea") ~ "Asia Pacific/Oceania",
    Region %in% c("Middle East","India","Africa") ~"Middle East/Africa",
    Region %in% c("Brazil","Other Latin America and Caribbean") ~ "Latin America",
    Region %in% c("United States","Mexico","Canada") ~ "North America",
    Region %in% c("EFTA","European Union","Other Europe",
                  "United Kingdom") ~ "Europe") %>% 
      factor(levels=c("Middle East/Africa","Asia Pacific/Oceania","China",
                      "Latin America","North America","Europe"))) %>% 
  filter(t<2051) %>% 
  group_by(Scenario,Region) %>%
  reframe(Demand=sum(Demand)/1e6) %>% ungroup() %>% 
  left_join(tibble(Scenario=scens_selected,name=scens_names)) %>% 
  mutate(name=factor(name,levels=scens_names))

# Fig. S1. Cumulative primary lithium demand, 

ggplot(data_fig,aes(name,Demand,fill=Region))+
  geom_col(col="black",linewidth=0.1)+
  coord_flip(ylim=c(0,42),expand = F)+
  guides(fill= guide_legend(reverse = T,byrow=T))+
  scale_x_discrete(limits=rev)+
  scale_fill_manual(values=c("Middle East/Africa"="#D16D6F","Asia Pacific/Oceania"="#cab2d6",
                    "China"="#ff0000","Latin America"="#d95f02",
                    "North America"="#1f78b4","Europe"="#a6cee3"))+
  labs(x="",y="",title = "Cumulative Lithium Demand [2022-2050] [Mtons]",fill="Region")+
  theme_bw(8)+ 
  theme(legend.text = element_text(size=6),
        axis.text.y = element_text(hjust=0),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "bottom")

ggsave("Figures/Demand/FigDemandRegion.png", ggplot2::last_plot(),
       units="cm",dpi=600,
       width=12,height=8.7)

# EoF