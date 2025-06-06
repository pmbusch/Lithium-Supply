# Extend the ICCT Forecast up to 2070
# Extension purpose is to avoid horizon effect in supply model
# Data comes from Roadmap model v2.2
# https://theicct.github.io/roadmap-doc/versions/v2.2/
# Data requested on August 2023
# SI Figure 
# PBH April 2024

source("Scripts/00-Libraries.R", encoding = "UTF-8")
source("Scripts/01-CommonVariables.R", encoding = "UTF-8")

# Data from ICCT Roadmap Model v2.2 - Not available to share Raw Data
icct <- readxl::read_excel("Data/Demand Model/ICCT_Country_Sales_Data.xlsx",sheet="Sales_data")
(names(icct) <- names(icct) %>% str_replace_all(" ","_") %>%  # correct names
  str_remove_all("\\(|\\)") %>% str_remove("_group") %>% 
  str_replace("CY","Year"))

# Expand to 2070 base on last 5 years growth -----------

# total sales in 2050
icct %>% 
  filter(Year==2050) %>% 
  filter(Powertrain!="ICE") %>% 
  group_by(Scenario,Vehicle,Powertrain) %>% 
  reframe(Sales=sum(Sales)/1e6) %>% 
  pivot_wider(names_from = Powertrain, values_from = Sales)

# TS Figure
icct %>% 
  filter(Scenario=="Ambitious",Powertrain=="BEV",Vehicle=="Car") %>% 
  mutate(Region=factor(Region,levels=region_level)) %>% 
  group_by(Year,Region) %>% 
  reframe(Sales=sum(Sales)/1e6) %>% ungroup() %>% 
  ggplot(aes(Year, Sales, fill = fct_rev(Region))) +
  geom_area() +
  labs(y="Sales \n [millions]",x="",fill="Vehicle type",
       caption = paste0("Ambitious"," scenario. Only ","BEV"))+  
  coord_cartesian(expand=F)+
  scale_fill_manual(values = region_colors)+
  scale_x_continuous(breaks = c(2022, 2030, 2040, 2050))+
  theme(panel.spacing.x = unit(2, "cm"))

# avg last 5 years
avg_5 <- icct %>% 
  filter(Powertrain!="ICE") %>% 
  filter(Year>2045) %>% 
  rename(x=Sales) %>% 
  group_by(Scenario,Country,Vehicle,Powertrain) %>% 
  summarise(growth = mean((x / lag(x) - 1) * 100,na.rm=T)) %>% 
  filter(!is.nan(growth),!is.infinite(growth)) %>% ungroup()

# Limit growth factors - max 1% per year
avg_5 <- avg_5 %>% mutate(growth=case_when(
  growth < -1 ~ -1,
  growth>-1 & growth<1 ~ 0,
  growth > 1 ~ 1))


# Fill in the dataframe with the projected values
df_fill <- expand.grid(Scenario=unique(icct$Scenario),
                       Country = unique(icct$Country),
                       Vehicle = unique(icct$Vehicle),
                       Powertrain = c("BEV","PHEV"),
                       Year=2050:2070) %>% 
  left_join(icct)

df_fill <- df_fill %>% left_join(avg_5) %>% 
  filter(!is.na(growth)) %>% 
  mutate(Sales=if_else(is.na(Sales),0,Sales)) %>% 
  group_by(Scenario,Vehicle,Powertrain,Country) %>% 
  mutate(Growth_Factor = if_else(Year==2050,1,1 + growth/100),
         Sales=first(Sales) * cumprod(Growth_Factor)) %>% ungroup() %>% 
  filter(Year>2050) # remove initial
df_fill$growth <- df_fill$Growth_Factor <- NULL

# add Region for years after 2050
df_fill$Region <- NULL
reg_dict <- icct %>% group_by(Region,Country) %>% tally() %>% dplyr::select(-n)
df_fill <- df_fill %>% left_join(reg_dict)

# total sales in 2070
df_fill %>% 
  filter(Year==2070) %>% 
  group_by(Scenario,Vehicle,Powertrain) %>% 
  reframe(Sales=sum(Sales)/1e6) %>% 
  pivot_wider(names_from = Powertrain, values_from = Sales)

df <- rbind(icct,df_fill)

# Make it comprehensive by adding 0
df$Region <- NULL
df <- df %>% complete(Country,Year,Powertrain,Vehicle,Scenario,fill = list(Sales=0))
nrow(df)
length(unique(df$Vehicle))*length(unique(df$Powertrain))*length(unique(df$Year))*
  length(unique(df$Country))*length(unique(df$Scenario))
df <- df %>% left_join(reg_dict)


df %>% 
  filter(Scenario=="Ambitious",Powertrain=="BEV",Vehicle=="Car") %>% 
  mutate(Region=factor(Region,levels=region_level)) %>% 
  group_by(Year,Region) %>% 
  reframe(Sales=sum(Sales)/1e6) %>% ungroup() %>% 
  ggplot(aes(Year, Sales, fill = fct_rev(Region))) +
  geom_area() +
  geom_vline(xintercept = 2050)+
  labs(y="Sales \n [millions]",x="",fill="Vehicle type",
       caption = paste0("Ambitious"," scenario. Only ","BEV"))+  
  coord_cartesian(expand=F)+
  scale_fill_manual(values = region_colors)+
  scale_x_continuous(breaks = c(2022, 2030, 2040, 2050))+
  theme(panel.spacing.x = unit(2, "cm"),
        legend.text = element_text(size=6),
        legend.key.height= unit(0.25, 'cm'),
        legend.key.width= unit(0.25, 'cm'))

# SAVE RESULTS
write.csv(df,"Parameters/Demand Intermediate Results/ICCT_demand.csv",row.names = F)

# Figure ----

veh_levels <- c("Two/Three Wheelers","Car","Van","Bus","Medium truck","Heavy truck")
# Fig. S11. BEV Sales forecast by vehicle type from 2022 to 2050 for the Ambitious Scenario
df %>% 
  filter(Year<2051) %>% 
  filter(Scenario=="Ambitious",Powertrain=="BEV") %>% 
  mutate(Region=factor(Region,levels=region_level)) %>% 
  mutate(Vehicle=factor(Vehicle,levels=veh_levels)) %>% 
  group_by(Year,Powertrain,Region,Vehicle) %>% 
  reframe(Sales=sum(Sales)/1e6) %>% ungroup() %>% 
  ggplot(aes(Year, Sales, fill = fct_rev(Region))) +
  geom_area() +
  facet_wrap(~Vehicle,scales="free_y")+
  labs(y="Sales \n [millions]",x="",fill="Region")+  
  coord_cartesian(expand=F)+
  scale_fill_manual(values = region_colors) +
  scale_x_continuous(breaks = c(2022, 2030, 2040, 2050))+
  theme(panel.spacing.x = unit(0.2, "cm"),
        legend.text = element_text(size=6),
        legend.key.height= unit(0.25, 'cm'),
        legend.key.width= unit(0.25, 'cm'))

f.fig.save("Figures/Demand/ICCT.png")

# Figure for poster
data_fig <- df %>% 
  filter(Year<2051) %>% 
  filter(Scenario=="Ambitious",Powertrain=="BEV") %>% 
  filter(Vehicle=="Car") %>% 
  mutate(Region = case_when(
    Region %in% c("Africa") ~ "Africa",
    Region %in% c("Middle East") ~ "Middle East",
    Region %in% c("China") ~ "China",
    Region %in% c("India", "Japan", "South Korea", "ASEAN", "Other Asia Pacific") ~ "Asia",
    Region %in% c("Australia/NZ") ~ "Oceania",
    Region %in% c("Brazil", "Other Latin America and Caribbean") ~ "South America",
    Region %in% c("Canada", "United States","Mexico") ~ "North America",
    Region %in% c("Other Europe", "European Union", "United Kingdom", "EFTA") ~ "Europe",
    TRUE ~ "Other")) %>% 
  mutate(Region=factor(Region,levels=rev(c("Africa", "Middle East", "China", "Asia",
                                       "Oceania", "South America", "North America", "Europe")))) %>% 
  group_by(Year,Region) %>% 
  reframe(Sales=sum(Sales)/1e6) %>% ungroup()

ggplot(data_fig,aes(Year, Sales, fill = fct_rev(Region))) +
  geom_area() +
  labs(y="",x="",fill="",title="Car EV Sales [million units]")+  
  coord_cartesian(expand=F)+
  scale_fill_manual(values = c("Europe"="#a6cee3",
                               "North America" = "#1f78b4",
                               "South America" = "#6a3d9a",
                               "Oceania" = "#cab2d6",
                               "Asia" = "#fdb462",
                               "China" = "#ff0000",
                               "Middle East" = "#8b4513",
                               "Africa" = "#4682b4",
                               "World"="#808080")) +
  scale_x_continuous(breaks = c(2022, 2030, 2040, 2050))+
  theme_bw(16)+
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(hjust=1),
        legend.position = c(0.15,0.65),
        legend.background = element_rect(fill = "transparent"))
        # legend.text = element_text(size=8),
        # legend.key.height= unit(0.25, 'cm'),
        # legend.key.width= unit(0.25, 'cm'))

ggsave("Figures/Demand/ICCT.svg", ggplot2::last_plot(),
       units="cm",dpi=600,
       width=20,height=10)

# EoF