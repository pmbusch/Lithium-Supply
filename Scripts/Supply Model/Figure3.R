# 2050 Cumulative supply by scenario
# Need to Run Julia 01-DemandScenarios.jl to generate results 
# PBH July 2024

# Load data -------
source("Scripts/00-Libraries.R", encoding = "UTF-8")
source("Scripts/01-CommonVariables.R", encoding = "UTF-8")


# Get list of all folders inside "Results/Optimization"
(runs <- list.dirs("Results/Optimization/DemandScenario",recursive = F))
(dict_scen <- tibble(Scenario=scens_selected,name=scens_names))
source("Scripts/Supply Model/01-LoadOptimizationResults.R", encoding = "UTF-8")

scen_abr <- tibble(name=scens_names,name_abr=name_abbr)
df_results <- df_results %>% 
  left_join(scen_abr)

# Bar Plot Cumulative Supply --------
# Creates Map showing cumulative (2022-2050) lithium extraction by resource type, and by country
# It shows a 2 barplot merged together

factor=1
# factor=15 # uncomment for 2050 analysis

# By Resource type first, need to add recycling component
recycling_total <- recycling %>%
  filter(t<2051) %>%
  # filter(t==2050) %>% # uncomment for 2050 analysis
  left_join(scen_abr) %>% 
  group_by(name_abr) %>% 
  reframe(mtons=sum(Recycling)/1e3) %>% 
  mutate(Resource_Type="Recycling")

# Data to recreate figure is same as Table1 
# (if decide not to run optimization code, then can preload results, along with the first lines of 
# the script 01-LoadOptimizationResults.R (load other data required)
# df_results <- read.csv("Results/Data_Table1.csv")


# get extraction
data_fig <- df_results %>% 
  left_join(deposit) %>%
  filter(t<2051) %>%
  # filter(t==2050) %>% # uncomment for 2050 analysis
  group_by(name_abr,Resource_Type) %>% 
  reframe(mtons=sum(tons_extracted)/1e3) %>% 
  rbind(recycling_total) %>% 
  mutate(name_abr=factor(name_abr,levels=name_abbr))

# order of plot Resource - custom with labels for legend
resource_legend <- c("Recyc.","Brine","Hard-Rock","Volc.-Sed.")
dict_res <- tibble(Resource_Type=c("Recycling","Brine","Hard Rock","Volcano-Sedimentary"),
                   res_legend=resource_legend)
data_fig <- data_fig %>% 
  left_join(dict_res) %>% 
  mutate(res_legend=factor(res_legend,levels=resource_legend)) %>% 
  mutate(white_font=if_else(res_legend %in% c("Recyc."),"whit","blac"))

# Lithium extraction  - By country
# countries to highlight
key_countries <- c("United States","Australia","Chile","China","EU","Others")
key_countries_legend <- c("USA","AUS","CHL","CHN","EU","Others")

# get recycling at region level
recycling_country <- recycling %>%
  filter(t<2051) %>%
  left_join(scen_abr) %>% 
  # filter(t==2050) %>% # uncomment for 2050 analysis
  mutate(Country=case_when(
    Region %in% key_countries ~ Region,
    Region=="European Union" ~ "EU",
    T ~ "Others")) %>% 
  group_by(name_abr,Country) %>% 
  reframe(mtons=sum(Recycling)/1e3) %>% 
  mutate(Resource_Type="Recycling")

df_results %>% left_join(deposit) %>% pull(Country) %>% unique()

# get extraction
data2 <- df_results %>% 
  left_join(deposit) %>% 
  filter(t<2051) %>% 
  # filter(t==2050) %>% # uncomment for 2050 analysis
  mutate(Country=case_when(
    Country %in% key_countries ~ Country,
    Country %in% c("Spain","Germany","Finland","Czech Republic",
                   "Portugal","Austria","France") ~ "EU",
    T ~ "Others")) %>% 
  group_by(name_abr,Resource_Type,Country) %>% 
  reframe(mtons=sum(tons_extracted)/1e3) %>% 
  mutate(name_abr=factor(name_abr,levels=name_abbr)) %>% 
  rbind(recycling_country) %>% 
  left_join(tibble(Country=key_countries,country_legend=key_countries_legend)) %>% 
  mutate(group_plot=paste0(Resource_Type,country_legend)) %>% 
  # show only labels for country with a lot of extraction
  mutate(country_label=case_when(
    mtons>4/factor & country_legend=="Others" ~ country_legend,
    mtons>3/factor & country_legend!="Others"~country_legend, 
    mtons>2/factor & country_legend %in% c("EU","CAN")~country_legend,
    mtons>1.5/factor & country_legend %in% c("CAN")~country_legend,
    T ~""))

# order plot - by resource and country size - NEED TO MATCH order of first plot
order_plot_country <- data2 %>% 
  left_join(dict_res) %>% 
  mutate(res_legend=factor(res_legend,levels=resource_legend)) %>% 
  arrange(desc(mtons)) %>% arrange(res_legend) %>% pull(group_plot) %>% unique()
data2 <- data2 %>% mutate(group_plot=factor(group_plot,levels=order_plot_country))
data2 <- data2 %>% 
  mutate(white_font=if_else(country_legend %in% c("USA"),"whit","blac"))

# data_fig %>% group_by(name) %>% reframe(x=sum(mtons))
# data2 %>% group_by(name) %>% reframe(x=sum(mtons))

# Bar plot
# 2 bar plots with nudges
p2 <- ggplot(data_fig,aes(as.numeric(name_abr)-0.15,mtons))+
  # Resource plot
  geom_col(aes(fill=res_legend),col="black",width = 0.3,linewidth=0.1)+
  geom_text(aes(label = res_legend,group=res_legend,col=white_font), position = position_stack(vjust = 0.5),
            angle=90,size=5*5/14 * 0.8) +
  # Country plot
  geom_col(data=data2,aes(x=as.numeric(name_abr)+0.15,fill=country_legend,group=group_plot),
           col="black",width = 0.3,linewidth=0.1)+
  geom_text(data=data2,position = position_stack(vjust = 0.5),angle=90,size=5*5/14 * 0.8,
            aes(x=as.numeric(name_abr)+0.15,label = country_label,group=group_plot,col=white_font))+
  # Formatting and colors
  labs(x="Lithium Demand Scenario",
       title="2022-2050 Li metal Supply [million tons]",
       # title="2050 Li metal Supply [million tons]",
       y="")+
  scale_x_continuous(breaks=1:11,labels=name_abbr)+
  scale_fill_manual(values = c("Recyc."="#009E73","Brine"="#0000FF33",
                               "Hard-Rock"="#80008080","Volc.-Sed."="#FF000080",
                               "USA"="#1f78b4","AUS"="#cab2d6","CHL"="#d95f02",
                               "ARG"="#ff7f00","CAN"="#6A3D9A","CHN"="#ff0000",
                               "EU"="#a6cee3","Others"="#808080"))+
  scale_color_manual(values=c( "whit"="#FFFFFF","blac"="#000000"))+
  coord_cartesian(expand = F)+
  theme_bw(8)+ 
  theme(legend.position = "none",
        axis.text.x = element_text(size=5),
        plot.margin = margin(5,7,5,-8),
        panel.grid.major = element_blank(),panel.grid.minor = element_blank())

p2


write.csv(dplyr::select(data2,name_abr,Resource_Type,Country,mtons),
          "Figures/Data Sources/Fig3.csv",row.names = F)


# Save with width size of letter
ggsave("Figures/Figure3.png", ggplot2::last_plot(),
       units="cm",dpi=600,
       width=8.8,height=8.8)

pdf("Figures/PDF/Figure3.pdf",width=8.8/2.54,height=8.8/2.54)
ggplot2::last_plot()
dev.off()

ggsave("Figures/Vector/Figure3.svg", ggplot2::last_plot(),
       units="cm",dpi=600,
       width=8.8,height=8.8)


# to save 2050
# Fig. S3. 2050 Lithium extraction by resource type and country of extraction
# ggsave("Figures/Supply/Figure3_2050.png", ggplot2::last_plot(),
#        units="cm",dpi=600,width=12.1,height=12.1)


# for scenario 4, biggest suppliers by country and type
df_results %>% 
  filter(t<2051) %>% 
  filter(str_detect(Scenario,"NMC")) %>% 
  left_join(deposit) %>% 
  group_by(Resource_Type,Country) %>% 
  reframe(tons=sum(tons_extracted)/1e3) %>% ungroup() %>% 
  group_by(Resource_Type) %>%  slice_max(order_by = tons, n = 5)


# when does USA becomes dominant
df_results %>% 
  filter(t<2051) %>%
  left_join(tibble(Scenario=scens_selected,name=scens_names)) %>%
  filter(str_detect(name,"Ref")) %>% 
  left_join(deposit) %>% 
  group_by(Country,t) %>% 
  reframe(tons=sum(tons_extracted)/1e3) %>% ungroup() %>% 
  group_by(t) %>% mutate(share=tons/sum(tons)) %>% 
  arrange(desc(tons)) %>% slice_max(order_by = tons,n=1) %>% 
  # head(18)
  filter(t==2050)

# Lithium demand by time and region -----

data_region <- df_results %>%
  filter(t<2051) %>%
  left_join(deposit) %>% 
  group_by(name,Country,t) %>%
  reframe(total_prod=sum(tons_extracted)) %>% ungroup() %>% 
  group_by(name,t) %>% mutate(share_prod=total_prod/sum(total_prod)) %>% ungroup() %>% 
  mutate(d=if_else(share_prod>0.05,Country,"Others"))

countries <- unique(data_region$d) # use it they have surpass the threshold at any year

data_region <- data_region %>% 
  mutate(d=if_else(Country %in% countries,Country,"Others")) %>% 
  group_by(name,d,t) %>% 
  mutate(name=factor(name,levels=scens_names)) %>% 
  reframe(total_prod=sum(total_prod)) %>% ungroup() %>% 
  complete(name, t, d, fill = list(total_prod = 0)) # all combinatios for plot (avoid discontinuity)
  
# country order
cont_order <- data_region %>% group_by(name,d) %>%
  reframe(total_prod=sum(total_prod)) %>% ungroup() %>% 
  arrange(desc(total_prod)) %>% pull(d) %>% unique()
cont_order <- c(cont_order[-5],cont_order[5]) # move others to the end

# Fig. S5. Lithium metal extraction
data_region %>% 
  mutate(d=factor(d,levels=rev(cont_order))) %>% 
  ggplot(aes(t,total_prod,fill=d,group=d))+
  geom_area(col="black",linewidth=0.1)+
  facet_wrap(~name)+
  coord_cartesian(expand = F)+
  labs(x="",y="",title="Li Metal Extraction [ktons]",fill="Country")+
  scale_y_continuous(limits = c(0,NA),labels = scales::comma_format(big.mark = ' '))+
  scale_fill_manual(values = c("United States"="#1f78b4","Australia"="#cab2d6","Chile"="#d95f02",
                               "Argentina"="#ff7f00","Canada"="#6A3D9A","China"="#ff0000",
                               "Germany"="#a6cee3","Mali"="#008000","Others"="#808080"))+
  scale_x_continuous(breaks = c(2030, 2040, 2050))+
  theme(axis.text.x = element_text(hjust=1),
        strip.text = element_text(size=6))

ggsave("Figures/Supply/Production.png", ggplot2::last_plot(),
       units="cm",dpi=600,
       width=8.7*2,height=12)

# Bar Plot Only Countries ----------------

factor=1


# Lithium extraction  - By country
# countries to highlight
key_countries <- c("United States","Australia","Chile","China","Rest EU",
                   "Canada","Germany","Argentina","Others")
key_countries_legend <- c("USA","AUS","CHL","CHN","Rest EU",
                          "CAN","GER","ARG","Others")


# get recycling at region level
recycling_country <- recycling %>%
  filter(t<2051) %>%
  left_join(scen_abr) %>% 
  # filter(t==2050) %>% # uncomment for 2050 analysis
  mutate(Country=case_when(
    Region %in% key_countries ~ Region,
    Region=="European Union" ~ "Rest EU",
    T ~ "Others")) %>% 
  group_by(name_abr,Country) %>% 
  reframe(mtons=sum(Recycling)/1e3)

df_results %>% left_join(deposit) %>% pull(Country) %>% unique()

# get extraction
data2 <- df_results %>% 
  left_join(deposit) %>% 
  filter(t<2051) %>% 
  # filter(t==2050) %>% # uncomment for 2050 analysis
  mutate(Country=case_when(
    Country %in% key_countries ~ Country,
    Country %in% c("Spain","Germany","Finland","Czech Republic",
                   "Portugal","Austria","France") ~ "Rest EU",
    T ~ "Others")) %>% 
  group_by(name_abr,Country) %>% 
  reframe(mtons=sum(tons_extracted)/1e3) %>% ungroup() %>% 
  rbind(recycling_country) %>% 
  # SUMMARISE AGAIN WITH RECYCLIGN EFFET
  group_by(name_abr,Country) %>% 
  reframe(mtons=sum(mtons)) %>% 
  left_join(tibble(Country=key_countries,country_legend=key_countries_legend)) %>% 
  mutate(group_plot=paste0(country_legend)) %>% 
  mutate(name_abr=factor(name_abr,levels=name_abbr)) %>% 
  # show only labels for country with a lot of extraction
  mutate(country_label=case_when(
    mtons>4/factor & country_legend=="Others" ~ country_legend,
    mtons>3/factor & country_legend!="Others"~country_legend, 
    mtons>2/factor & country_legend %in% c("EU","CAN")~country_legend,
    mtons>1.5/factor & country_legend %in% c("CAN")~country_legend,
    T ~country_legend))

# order plot - by resource and country size - NEED TO MATCH order of first plot
order_plot_country <- data2 %>% arrange((mtons)) %>% 
  filter(group_plot!="Others") %>% 
  pull(group_plot) %>% unique()
order_plot_country <- c(c("Others"),order_plot_country)

data2 <- data2 %>% mutate(group_plot=factor(group_plot,levels=order_plot_country))
data2 <- data2 %>% 
  mutate(white_font=if_else(country_legend %in% c("USA"),"whit","blac"))


# Bar plot
# 2 bar plots with nudges
p2 <- ggplot(data2,aes(as.numeric(name_abr),mtons))+
  # Country plot
  geom_col(aes(fill=country_legend,group=group_plot),
           col="black",linewidth=0.1)+
  geom_text(position = position_stack(vjust = 0.5),angle=0,size=6*5/14 * 0.8,
            aes(label = country_label,group=group_plot,col=white_font))+
  # Formatting and colors
  labs(x="Lithium Demand Scenario",
       title="2022-2050 Li metal Supply [million tons]",
       # title="2050 Li metal Supply [million tons]",
       y="")+
  scale_x_continuous(breaks=1:11,labels=name_abbr)+
  scale_fill_manual(values = c("USA"="#1f78b4","AUS"="#cab2d6","CHL"="#d95f02",
                               "ARG"="#ff7f00","CAN"="#6A3D9A","CHN"="#ff0000",
                               "GER"="#8B8000",
                               "Rest EU"="#a6cee3","Others"="#808080"))+
  scale_color_manual(values=c( "whit"="#FFFFFF","blac"="#000000"))+
  coord_flip(expand = F)+
  theme_bw(8)+ 
  theme(legend.position = "none",
        panel.grid.major = element_blank(),panel.grid.minor = element_blank())

p2
# Save with width size of letter
ggsave("Figures/Supply/SupplyCountries.png", ggplot2::last_plot(),
       units="cm",dpi=600,
       width=12.1,height=12.1)

# EoF