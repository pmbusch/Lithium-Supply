# Fig. 2 Supply Dispatch curve
# PBH June 2024
# Fig widths note: 1 column=5.7cm, 2 col=12.1cm or 3 col=18.4cm

source("Scripts/00-Libraries.R", encoding = "UTF-8")
source("Scripts/01-CommonVariables.R", encoding = "UTF-8")

demand <- read.csv("Parameters/Demand.csv")
demandSector <- read.csv("Parameters/Demand_Detail.csv")
deposit <- read.csv("Parameters/Deposit.csv")
demandRegion <- read.csv("Parameters/Demand_Region.csv")

# Cumulative demand
cum_demand <- demand %>% 
  filter(t<2051) %>% 
  group_by(Scenario,t) %>%
  reframe(kton=sum(Demand)) %>% ungroup() %>% 
  left_join(tibble(Scenario=scens_selected,name=scens_names)) %>% 
  mutate(scen_num=substr(name,1,3)) %>% 
  group_by(name) %>% 
  reframe(Mton=sum(kton)/1e3) %>% ungroup() %>% arrange(desc(Mton))


# Supply Curve ---------

# weighted cost avg
data_fig <- deposit %>% 
  dplyr::select(Deposit_Name,Resource_Type,Status,
                reserve,resource_demostrated,resource_inferred,
                cost1,cost2,cost3,cost_source) %>% 
  # average cost by stage size
  mutate(cost=(cost1*reserve+cost2*resource_demostrated+cost3*resource_inferred)/
           (reserve+resource_demostrated+resource_inferred),
         li_size=(reserve+resource_demostrated+resource_inferred)/1e3) %>% 
  mutate(cost=cost/5.323) %>%  # to USD per ton LCE
  filter(li_size>0) %>% 
  filter(cost>0) %>% 
  arrange(cost) %>% 
  mutate(reserve_cum_end=cumsum(li_size),
         reserve_cum_start=lag(reserve_cum_end,default = 0)) %>% 
  mutate(lab_dep=if_else(li_size>1.1,Deposit_Name,"")) %>% 
  mutate(lab_pos=reserve_cum_start+li_size/2) %>% 
  mutate(report_cost=factor(if_else(cost_source=="Report","Report","Quantile"))) %>% 
  mutate(Status=case_when(
    Status %in% c("Producing","Construction","Suspended",
                  "Producing & suspended","Restarting") ~ "Open or under Construction",
    Status %in% c("Permitting","Design","Feasibility","Approved permits",
                  "Pilot plant","Economic assessment") ~ "Feasibility or Permitting",
    T ~ "Exploration") %>% 
      factor(levels=c("Open or under Construction","Feasibility or Permitting","Exploration"))) %>%
  mutate(even_row=ifelse(row_number() %% 2 == 0, 1, -1)) # for labelling

# duplicate last row
nrow(data_fig) #160
last_row <- data_fig[nrow(data_fig),]
last_row$reserve_cum_start <- last_row$reserve_cum_end
last_row$lab_dep <- ""
data_fig <- rbind(data_fig,last_row)

# Limits
lim_x <- ceiling(max(data_fig$reserve_cum_end)/0.5)*0.5 # upper by 500
max(data_fig$cost)
lim_y <- ceiling(max(data_fig$cost)/500)*500

even_row <- data_fig$even_row

p2 <- ggplot(data_fig,aes(reserve_cum_start,cost,group=1))+
  geom_step(linewidth=0.75,direction = "hv",
            alpha=0.7,
            aes(col=Resource_Type))+
  geom_text_repel(aes(x=lab_pos,label=lab_dep),col="black",alpha=.9,
                  min.segment.length = 0.1,
                  segment.color = "darkgrey", segment.size = 0.3,
                  nudge_y = 1000*even_row,size=7*5/14 * 0.8)+
  labs(x="Cumulative Resources [million tons Li]",y="Extraction Cost\n[USD/ton LCE]",
       title="Lithium Cumulative Availability Curve",tag="c",
       col="Resource type")+
  coord_cartesian(xlim = c(0,lim_x),expand = F,ylim=c(NA,lim_y))+
  scale_color_manual(values=resource_colors)+
  # scale_alpha_manual(values = c("Feasibility or Permitting" = 0.6, 
  # "Exploration"=0.3,
  # "Open or under Construction" = 1)) +
  # scale_alpha_manual(values = c("Report"=1,"Quantile" = 0.3, "Regression"=0.6)) +
  scale_y_continuous(labels = scales::comma_format(big.mark = ' ',prefix = "$"))+
  scale_x_continuous(labels = scales::comma_format(big.mark = ' '))+
  theme_bw(7)+
  theme(legend.position = c(0.9,0.23),
        legend.box = "horizontal",
        axis.text.x = element_text(hjust = 1),
        panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
        axis.title.y=element_text(angle=0,margin=margin(r = -75,l=25),vjust = 0.95),
        legend.text = element_text(size=7),
        legend.key.height= unit(0.3, 'cm'),
        legend.key.width= unit(0.3, 'cm'),
        plot.tag = element_text(face = "bold"),
        legend.spacing = unit(0.05,"cm"),
        legend.title = element_text(size=7.5))
p2

write.csv(dplyr::select(data_fig,Deposit_Name,Resource_Type,li_size,cost),
          "Figures/Data Sources/Fig2c.csv",row.names = F)


# Save with width size of letter
# ggsave("Figures/Article/Fig2b.png", ggplot2::last_plot(),
#        units="cm",dpi=600,
#        width=18.4,height=6.2)

# Cost vs Country of deposit ranking ----

data_fig <- deposit %>% 
  mutate(edb=100-edb) %>%
  # avg cost by stage size
  mutate(cost=(cost1*reserve+cost2*resource_demostrated+cost3*resource_inferred)/
           (reserve+resource_demostrated+resource_inferred),
         res=(reserve+resource_demostrated+resource_inferred)/1e3) %>% 
  mutate(cost=cost/5.323)  # to USD per ton LCE
sum(data_fig$res)

# for label
countries <- data_fig %>% filter(res>0.5) %>% 
  group_by(Country) %>% reframe(edb=mean(edb)) %>% ungroup() %>% 
  mutate(Country=case_when(
    Country=="Germany" ~"",
    Country=="Canada" ~"Canada-Germany",
    Country=="Serbia" ~"",
    Country=="Czech Republic" ~"Czechia-Serbia",
    Country=="Mexico" ~"",
    Country=="Chile" ~"Chile-Mexico",
    Country=="Brazil" ~"",
    Country=="Argentina" ~"Argentina-Brazil",
    Country=="Zimbabwe" ~"",
    Country=="Tanzania" ~"Tanzania-Zimbabwe",
    Country=="France"~"",
    T ~ Country))

p2_tradeoff <- ggplot(data_fig,aes(cost,edb,size=res))+
  geom_point(alpha=0.5,aes(col=Resource_Type))+
  geom_text(data=countries,aes(label=Country),x=6000,
            # direction = "x",
            fontface = "italic",
            size=6*5/14 * 0.8,
            hjust=0)+
  # geom_text_repel(aes(label=Country))+
  scale_x_continuous(limits = c(6.2,13.5)*1e3,
                     labels = scales::comma_format(big.mark = ' ',prefix = "$"))+
  scale_color_manual(values=resource_colors)+
  labs(y="",tag="b",title="Extraction Cost and Ease of Doing Business [0-100]",x="Extraction Costs [USD/ton LCE]",
       size="Total Resource \n[M tons Li]",col="Resource \ntype")+
  theme_bw(7)+ 
  guides(col= "none")+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
        legend.background = element_rect(fill = "transparent", color = NA),
        legend.text = element_text(size=6),
        legend.key.height= unit(0.25, 'cm'),
        plot.tag = element_text(face = "bold"),
        legend.key.width= unit(0.25, 'cm'),
        legend.position = c(0.32,0.3))
p2_tradeoff

write.csv(dplyr::select(data_fig,Deposit_Name,Country,Resource_Type,cost,edb,res),
          "Figures/Data Sources/Fig2b.csv",row.names = F)


# ggsave("Figures/Article/Tradeoff.png", ggplot2::last_plot(),
#        units="cm",dpi=600,
#        width=9.2,height=6.2)


# Mosaic Figure --------
# https://stackoverflow.com/questions/19233365/how-to-create-a-marimekko-mosaic-plot-in-ggplot2
dict_region <- tibble(
  Country=c("Argentina","Bolivia","Canada", "Chile","China","Germany",
            "United States","Afghanistan","Australia","Austria","Brazil", "Czech Republic",
            "DR Congo", "Ethiopia", "Finland","France", "Ghana","Mali","Namibia",
            "Portugal", "Russia", "Spain","United Kingdom","Zimbabwe",
            "Mexico", "Peru", "Serbia", "Tanzania")) %>% 
  mutate(Region=case_when(
    Country %in% c("Argentina", "Bolivia", "Chile", "Brazil", "Peru") ~ "South America",
    Country %in% c("Canada", "United States", "Mexico") ~ "North America",
    Country %in% c("China", "Afghanistan", "Russia") ~ "Asia",
    Country %in% c("Germany", "Austria", "Czech Republic", "Finland", "France", 
                   "Portugal", "Spain", "United Kingdom", "Serbia") ~ "Europe",
    Country %in% c("DR Congo", "Ethiopia", "Ghana", "Mali", "Namibia", "Zimbabwe", "Tanzania") ~ "Africa",
    Country == "Australia" ~ "Australia"))

df_country <- deposit %>% 
  # mutate(Resource_Type=Resource_Type_orig) %>% 
  group_by(Resource_Type,Country) %>% 
  reframe(reserve=sum(reserve)/1e3,
          resource_demonstrated=sum(resource_demostrated)/1e3,
          resource_inferred=sum(resource_inferred)/1e3) %>% 
  mutate(resource_all=reserve+resource_demonstrated+resource_inferred) %>% 
  arrange(desc(resource_all)) %>% ungroup()


# create stats for display
data_fig <- df_country %>% 
  # mutate(resource_all=reserve) %>%  # for Reserve analysis
  mutate(Resource_Type=str_replace(Resource_Type,"Volcano-Sedimentary","Volc.-Sed.")) %>% 
  filter(resource_all>0) %>% 
  mutate(Resource_Type=if_else(str_detect(Resource_Type,"Other"),"Other",Resource_Type)) %>% 
  # aggregate countries
  left_join(dict_region) %>% 
  mutate(Country=if_else(resource_all>3 | Resource_Type=="Volc.-Sed.",
                         Country,paste0("",Region))) %>% 
  group_by(Resource_Type,Country) %>% 
  reframe(resource_all=sum(resource_all),
          reserve=sum(reserve),resource_demonstrated=sum(resource_demonstrated),resource_inferred=sum(resource_inferred)) %>% ungroup() %>% 
  group_by(Resource_Type) %>%
  mutate(total_resources=sum(resource_all),
         share_resource=resource_all/sum(resource_all))

# add labels  
data_fig <- data_fig %>% 
  mutate(r_label=if_else(resource_all>2 & share_resource>0.04,
                         paste0(Country,": ",format(round(resource_all,1),big.mark=","),"M"),"")) %>% 
  mutate(r_label_small=if_else(resource_all>0.8 & resource_all<2 & 
                                 Resource_Type=="Volc.-Sed.",
                               paste0(Country,": ",format(round(resource_all,1),big.mark=","),"M"),"")) %>% 
  mutate(r_label=str_replace(r_label,"Africa","Rest of Africa") %>% 
           str_replace("Bolivia: ","Bolivia:   ")) %>% 
  mutate(c_order_lab=paste0(r_label,r_label_small)) %>% 
  ungroup() %>% mutate(share_type=total_resources/sum(resource_all)) %>% 
  mutate(Resource_Type=paste0(Resource_Type,"\n(",round(share_type*100,0),"%)"))


r_order <- data_fig %>% group_by(Resource_Type) %>% summarise(p=sum(resource_all)) %>%
  arrange(desc(p)) %>% pull(Resource_Type)
data_fig <- data_fig %>% mutate(Resource_Type=factor(Resource_Type,levels=r_order))
c_order <- data_fig %>% group_by(c_order_lab) %>% summarise(p=sum(resource_all)) %>%
  mutate(p=if_else(c_order_lab=="",0,p)) %>% # no label at end
  arrange(desc(p)) %>% pull(c_order_lab)
data_fig <- data_fig %>% mutate(c_order_lab=factor(c_order_lab,levels=c_order))
sum(data_fig$resource_all)

# By stage
data_fig <- data_fig %>% 
  mutate(share_reserves=reserve/resource_all,
         share_demResource=resource_demonstrated/resource_all,
         share_infResource=resource_inferred/resource_all) %>% 
  # rectangles X limits
  mutate(x_off=total_resources/2.05,
         x_start_r1=0-x_off,x_end_r1=share_reserves*total_resources-x_off,
         x_start_r2=share_reserves*total_resources-x_off,x_end_r2=(share_reserves+share_demResource)*total_resources-x_off,
         x_start_r3=(share_reserves+share_demResource)*total_resources-x_off,x_end_r3=total_resources-x_off) %>% 
  # rectangles Y limits
  arrange(desc(c_order_lab)) %>% 
  group_by(Resource_Type) %>% 
  mutate(cum_share_resource=cumsum(share_resource)) %>% 
  mutate(y_start=cum_share_resource,y_end=lag(cum_share_resource,default = 0)) %>% 
  ungroup()
         

p_mosaic <- ggplot(data_fig,aes(x = Resource_Type, y = share_resource, 
                    width = total_resources,
                    group=c_order_lab,
                    fill = Resource_Type)) +
  geom_bar(stat = "identity", position = "fill",aes(alpha=Resource_Type),colour = "black")+
  # RECTANGLES OF STAGES
  # geom_rect(aes(xmin=x_start_r1,xmax=x_end_r1,ymin=y_start,ymax=y_end),
  #           color=NA,fill="white",alpha=0)+ # RESERVES is no transparency
  scale_fill_manual(values=c(unname(resource_colors),"darkgrey"))+
  geom_rect(aes(xmin=x_start_r2,xmax=x_end_r2,ymin=y_start,ymax=y_end),
            color="black",fill="white",alpha=0.15,linewidth=0.1)+
  geom_rect(aes(xmin=x_start_r3,xmax=x_end_r3,ymin=y_start,ymax=y_end),
            color="black",fill="white",alpha=0.3,linewidth=0.1)+
  geom_text(x = -37.5, y = 0.98, label = "Reserves",fontface="italic", data = filter(data_fig, str_detect(Country,"Argentina")),size = 7*5/14 * 0.8, hjust = 0, vjust = 1)+
  geom_text(x = -22.5, y = 0.98, label = "Demonstrated\nResources",fontface="italic", data = filter(data_fig, str_detect(Country,"Argentina")),size = 7*5/14 * 0.8, hjust = 0, vjust = 1)+
  geom_text(x = 13, y = 0.98, label = "Inferred\nResources",fontface="italic",data = filter(data_fig, str_detect(Country,"Argentina")),size = 7*5/14 * 0.8, hjust = 0, vjust = 1)+
  # LABELS
  geom_text(data=filter(data_fig,str_detect(Resource_Type,"Brine")),
            aes(label = r_label), position = position_stack(vjust = 0.5),size=8*5/14 * 0.8) + 
  geom_text(data=filter(data_fig,str_detect(Resource_Type,"Rock")),
            aes(label = r_label), position = position_stack(vjust = 0.5),size=7.5*5/14 * 0.8) + 
  # small label
  geom_text(aes(label = r_label_small), position = position_stack(vjust = 0.5),size=5*5/14 * 0.8) + 
  geom_text(data=filter(data_fig,str_detect(Resource_Type,"Other|Volc")),
            aes(label = r_label), position = position_stack(vjust = 0.5),angle=90,size=6*5/14 * 0.8) + 
  facet_grid(~Resource_Type, scales = "free_x", space = "free_x") +
  scale_alpha_manual(values=c(0.3,0.5,0.5))+
  theme_void(7)+
  labs(title="Lithium Resource Distribution",tag="a")+
  theme(legend.position="none",
        plot.tag = element_text(face = "bold"),
        panel.spacing.x = unit(0, "npc")) # if no spacing preferred between bars
p_mosaic


write.csv(dplyr::select(data_fig,Country,Resource_Type,reserve,resource_demonstrated,
                        resource_inferred),
          "Figures/Data Sources/Fig2a.csv",row.names = F)

# ggsave("Figures/Article/Mosaic.png", ggplot2::last_plot(),
#        units="cm",dpi=600,
#        width=9.2,height=6.2)

# all together
library(cowplot)
plot_grid(
  plot_grid(p_mosaic,p2_tradeoff,rel_widths = c(1,1)),
  p2,nrow=2)


ggsave("Figures/Figure2.png", ggplot2::last_plot(),
       units="cm",dpi=600,
       width=18.4,height=12.4)

pdf("Figures/PDF/Figure2.pdf",width=18.4/2.54,height=12.4/2.54)
ggplot2::last_plot()
dev.off()


ggsave("Figures/Vector/Figure2.svg", ggplot2::last_plot(),
       units="cm",dpi=600,
       width=18.4,height=12.4)

# EoF