# Compile Parameters for Deposits based on Database of data
# March 2024 PBH


# LOAD DATA ---------
# Load all deposit data
source("Scripts/Supply Model/Prepare Deposit Data/01-LoadDepositData.R", encoding = "UTF-8")

url_fig <- "Figures/Deposit/%s.png"

# Reserves
sum(df$Reserve_Li_ktons,na.rm=T)/1e3 # 21.7 Mt
# Resources Demonstrated (measured+indicated)
sum(df$Resource_Li_ktons,na.rm = T)/1e3 # 58 Mt
# Resource inferred
sum(df$Resource_Inferred_Li_ktons,na.rm = T)/1e3 # 71 Mt

# REGRESSION MODELS ----------

# For Now - reclassify resource types for generalization of costs
table(df$Resource_Detail)
df <- df %>% 
  mutate(Resource_Type_orig=Resource_Detail) %>% 
  mutate(Resource_Type=case_when(
    Resource_Detail %in% c("Brine","Hard Rock","Volcano-Sedimentary") ~ Resource_Detail,
    Resource_Detail %in% c("Other (Oilfield)","Other (Geothermal)") ~ "Brine",
    Resource_Detail %in% c("Other (In Situ)","Other (Waste Rock)") ~ "Hard Rock",
    Resource_Detail %in% c("Other (Volcanic Rock)") ~ "Volcano-Sedimentary"))

# Adjust for inflation towards 2022, using study year
df <- df %>% 
  mutate(USD_pertonne_Li=USD_pertonne_Li*Inflation_Adjusment_2022,
         USD_pertonne_LCE=USD_pertonne_LCE*Inflation_Adjusment_2022,
         Investment=Investment_Original*Inflation_Adjusment_2022)


## Extraction Costs -------

# Brine
# avg cost
df %>% filter(Resource_Type_orig=="Brine") %>% reframe(x=mean(USD_pertonne_LCE,na.rm=T))
# Really bad fit and not so many observations 
mod_brine <- lm(USD_pertonne_Li~Grade_percLi_Reserve,
                weights = Reserve_Li_ktons,
                data=filter(df,Resource_Type_orig=="Brine"))
nobs(mod_brine)
summary(mod_brine)
coef_brine <- coefficients(mod_brine)
r_squared <- summary(mod_brine)$r.squared
eq_y <- paste0("Y = ",round(coef_brine[1]/5.323,0)," - ",
               -round(coef_brine[2]/5.323/1e4,4)," * X")

# Grade cost Brine
# Fig. S18. Linear regression model between OPEX (in USD per ton of LCE extracted, no royalties or taxes) and grade of lithium (in mg/L), for brine projects
df %>% 
  filter(Resource_Type_orig=="Brine") %>%
  # grade in %Li, towards mg/L is 1e4
  mutate(Grade_reserve=Grade_percLi_Reserve*1e4) %>% 
  filter(!is.na(Reserve_Li_ktons),!is.na(Grade_reserve),!is.na(USD_pertonne_LCE)) %>% 
  ggplot(aes(Grade_reserve,USD_pertonne_LCE))+
  geom_smooth(method="lm",se=F,
              # formula = "y~x+I(x^2)",
              aes(weight = Reserve_Li_ktons),
              col="darkred")+
  geom_point(aes(col=Resource_Type,size=Reserve_Li_ktons))+
  geom_text_repel(aes(label=Deposit_Name),col="black",size=10*5/14 * 0.8)+
  annotate(geom="text",label=bquote(R^2==.( round(r_squared,2))),
            x = 1e3, y = 5.1e3 , size = 10*5/14 * 0.8,hjust=0 ,color = "black") +  
  annotate("text", label=eq_y,
           x = 1e3, y = 5.3e3, size = 10*5/14 * 0.8,hjust=0 ,color = "black")+
  labs(y="Extraction Cost \n[USD/t LCE]",
       x="Grade Li [mg/L]",
       col="",size="Reserve [ktons Li]")+
  scale_color_manual(values=resource_colors)+
  guides(col="none")+
  # xlim(0,2200)+ylim(0,6100)+coord_cartesian(expand = F)+
  theme_bw(10)+
  scale_y_continuous(labels = scales::comma_format(big.mark = ' ',prefix = "$"))+
  scale_x_continuous(labels = scales::comma_format(big.mark = ' '))+
  theme(legend.position = c(0.8,0.75),
        panel.grid.major = element_blank(),panel.grid.minor = element_blank())

f.fig.save(sprintf(url_fig,"RegCost_Brine"))

# Hard Rock
# avg cost
df %>% filter(Resource_Type_orig=="Hard Rock") %>% reframe(x=mean(USD_pertonne_LCE,na.rm=T))
mod_rock <- lm(USD_pertonne_Li~Grade_percLi_Reserve,
               weights = Reserve_Li_ktons,
               data=filter(df,Resource_Type_orig=="Hard Rock"))
nobs(mod_rock)
summary(mod_rock)
coef_rock <- coefficients(mod_rock)
r_squared <- summary(mod_rock)$r.squared
eq_y <- paste0("Y = ",round(coef_rock[1]/5.323,0)," - ",
               -round(coef_rock[2]/5.323/2.153,0)," * X")

# Fig. S19. Linear regression model between OPEX (in USD per ton of LCE extracted, no royalties or taxes) and grade of lithium (in Li2O %), for hard rock projects
# Grade cost Hard rock
df %>% 
  filter(Resource_Type_orig=="Hard Rock") %>%
  # grade in %Li, towards %Li2O is 2.153
  mutate(Grade_reserve=Grade_percLi_Reserve*2.153) %>% 
  filter(!is.na(Reserve_Li_ktons),!is.na(Grade_reserve),!is.na(USD_pertonne_LCE)) %>% 
  ggplot(aes(Grade_reserve,USD_pertonne_LCE))+
  geom_smooth(method="lm",se=F,
              aes(weight = Reserve_Li_ktons),
              col="darkred")+
  geom_point(aes(col=Resource_Type,size=Reserve_Li_ktons))+
  annotate(geom="text",label=bquote(R^2==.( round(r_squared,2))),
           x = 1.2, y = 11.7e3 , size = 10*5/14 * 0.8,hjust=0 ,color = "black") +
  annotate("text", label=eq_y,
           x = 1.2, y =12e3, size = 10*5/14 * 0.8,hjust=0 ,color = "black")+
  geom_text_repel(aes(label=Deposit_Name),col="black",size=10*5/14 * 0.8)+
  labs(y="Extraction Cost \n[USD/t LCE]",
       x=expression(paste("Grade Li"[2],"O%")),
       col="",size="Reserve [ktons Li]")+
  scale_color_manual(values=resource_colors)+
  guides(col="none")+
  theme_bw(10)+
  scale_y_continuous(labels = scales::comma_format(big.mark = ' ',prefix = "$"))+
  scale_x_continuous(labels = scales::comma_format(big.mark = ' ',suffix = "%"))+ # for Hard rock
  theme(legend.position = c(0.8,0.75),
        panel.grid.major = element_blank(),panel.grid.minor = element_blank())

f.fig.save(sprintf(url_fig,"RegCost_HardRock"))

## Expansion and Opening Costs --------

# cost per tpa 
df %>% 
  filter(Project_Capacity_Li_ktons>0, Investment>0) %>% 
  mutate(cost_tpa=Investment/Project_Capacity_Li_ktons*1e3/5.323) %>%  # USD/tpa LCE in capacity
  group_by(Resource_Type) %>% 
  dplyr::select(cost_tpa,Resource_Type) %>% 
  skimr::skim_without_charts()

# capacity expansion models
# need to be non linear or favor economies of scale
# Intercept favor economies of scale

mod_cap_brine <- lm(Investment~Project_Capacity_Li_ktons,
                    weights = Reserve_Li_ktons,
                    data=filter(df,
                                # Project_Capacity_Li_ktons<20, # no atacama
                                Resource_Type_orig=="Brine"))
nobs(mod_cap_brine)
summary(mod_cap_brine) # Coefficients are in Million USD per kton, or multiply by 1000 to get USD per ton
r_squared <- summary(mod_cap_brine)$r.squared
eq_y <- paste0("Y = ",round(coef(mod_cap_brine)[1],0)," + ",
               round(coef(mod_cap_brine)[2],0)," * X")


# Production vs Investment
# Fig. S16. Linear regression model between CAPEX (in million USD) and production capacity targets (tonnes per year), for brine projects.
df %>% 
  filter(Resource_Type_orig=="Brine") %>% 
  filter(Investment>0,Project_Capacity_Li_ktons>0,Reserve_Li_ktons>0) %>% 
  # filter(Project_Capacity_Li_ktons<20) %>% # No atacama 
  ggplot(aes(Project_Capacity_Li_ktons,Investment))+
  geom_smooth(method="lm",
              # formula = "y~x+I(x^2)",
              aes(weight=Reserve_Li_ktons),
              se=F,col="darkgreen")+
  geom_point(aes(col=Resource_Type,size=Reserve_Li_ktons))+
  annotate(geom="text",label=bquote(R^2==.( round(r_squared,2))),
           x = 18, y = 2e3 , size = 10*5/14 * 0.8,hjust=0 ,color = "black") +  
  annotate("text", label=eq_y,
           x = 18, y = 2.2e3, size = 10*5/14 * 0.8,hjust=0 ,color = "black")+
  geom_text_repel(aes(label=Deposit_Name),col="black",size=10*5/14 * 0.8)+
  labs(x="Production [ktons Li per year]",y="Investment \n [USD Million]",
       col="Resource \ntype",size="Reserve [ktons Li]")+
  scale_color_manual(values=resource_colors)+
  guides(col="none")+
  theme_bw(10)+
  scale_y_continuous(labels = scales::comma_format(big.mark = ' ',prefix = "$"))+
  scale_x_continuous(labels = scales::comma_format(big.mark = ' '))+
  theme(legend.position = c(0.8,0.3),
        panel.grid.major = element_blank(),panel.grid.minor = element_blank())

f.fig.save(sprintf(url_fig,"Investment_costProd_Brine"))


mod_cap_rock <- lm(Investment~Project_Capacity_Li_ktons,
                   weights = Reserve_Li_ktons,
                   data=filter(df,
                               # Project_Capacity_Li_ktons<60, # no Mt Holland
                               Resource_Type_orig=="Hard Rock"))
nobs(mod_cap_rock)
summary(mod_cap_rock)
r_squared <- summary(mod_cap_rock)$r.squared
eq_y <- paste0("Y = ",round(coef(mod_cap_rock)[1],0)," + ",
               round(coef(mod_cap_rock)[2],0)," * X")

# Fig. S17. Linear regression model between CAPEX (in million USD) and production capacity targets (tonnes per year), for hard-rock projects
# Production vs Investment
df %>% 
  filter(Resource_Type_orig=="Hard Rock") %>% 
  # filter(Project_Capacity_Li_ktons<60) %>%  # no Mt Holland
  filter(Investment>0,Project_Capacity_Li_ktons>0,Reserve_Li_ktons>0) %>% 
  ggplot(aes(Project_Capacity_Li_ktons,Investment))+
  geom_smooth(method="lm",
              # formula = "y~x+I(x^2)",
              aes(weight=Reserve_Li_ktons),
              se=F,col="darkgreen")+
  geom_point(aes(col=Resource_Type,size=Reserve_Li_ktons))+
  annotate(geom="text",label=bquote(R^2==.( round(r_squared,2))),
           x = 18, y = 1200 , size = 10*5/14 * 0.8,hjust=0, color = "black") +  
  annotate("text", label=eq_y,
           x = 18, y = 1300, size = 10*5/14 * 0.8,hjust=0 ,color = "black")+
  geom_text_repel(aes(label=Deposit_Name),col="black",size=10*5/14 * 0.8)+
  labs(x="Production [ktons Li per year]",y="Investment \n [USD Million]",
       col="Resource \ntype",size="Reserve [ktons Li]")+
  scale_color_manual(values=resource_colors)+
  guides(col="none")+
  theme_bw(10)+
  scale_y_continuous(labels = scales::comma_format(big.mark = ' ',prefix = "$"))+
  scale_x_continuous(labels = scales::comma_format(big.mark = ' '))+
  theme(legend.position = c(0.8,0.3),
        panel.grid.major = element_blank(),panel.grid.minor = element_blank())

f.fig.save(sprintf(url_fig,"Investment_costProd_Rock"))

# avoid negative intercept
mod_cap_volcano <- lm(Investment~Project_Capacity_Li_ktons-1,
                   # weights = Reserve_Li_ktons,
                   data=filter(df,Resource_Type_orig=="Volcano-Sedimentary") %>% 
                     mutate(Investment=Investment-250))
nobs(mod_cap_volcano)
summary(mod_cap_volcano)

# DATA CONSOLIDATION -----

# Reserve ---------
df$Resource_Type %>% unique()

# 3 stages of extraction
df <- df %>% 
  mutate(reserve=if_else(is.na(Reserve_Li_ktons),0,Reserve_Li_ktons),
         resource_demostrated=if_else(is.na(Resource_Li_ktons),0,Resource_Li_ktons),
        # substract reserve from resources
        allocate=pmin(resource_demostrated,reserve),
        resource_demostrated=resource_demostrated-allocate,
        res2=reserve-allocate, # substract to inferred
        resource_inferred=if_else(is.na(Resource_Inferred_Li_ktons),0,
                                  Resource_Inferred_Li_ktons-res2),
        resource_inferred=if_else(resource_inferred>0,resource_inferred,0),
        all_resource=reserve+resource_demostrated+resource_inferred)


df %>% reframe(Reserves=sum(reserve,na.rm=T)/1e3,
               Resources_Demonstrated=sum(resource_demostrated,na.rm=T)/1e3,
               Resources_Inferred=sum(resource_inferred,na.rm=T)/1e3,
               all_resource=sum(all_resource,na.rm=T)/1e3)

# Share of resource
df %>% 
  filter(resource_demostrated>0,resource_inferred>0) %>% 
  group_by(Resource_Type) %>% 
  reframe(dem=sum(resource_demostrated),
          infe=sum(resource_inferred)) %>% ungroup() %>% 
  mutate(share=dem/(dem+infe))

# just 37M
df %>% filter(resource_demostrated==0,resource_inferred>0) %>% 
  pull(resource_inferred) %>% sum()/1e3


# Max Production Rate ---------
# 5% max depletion rate for hard rock and 3% for others (DLEc and clay) and 
# 1% for evaporation
dep_rate <- tibble(Resource_Type=unique(df$Resource_Type)) %>% 
  mutate(dep_rate=case_when(
    Resource_Type=="Hard Rock" ~ 0.05,
    Resource_Type=="Brine" ~ 0.01,
    T ~ 0.03))
dep_rate$dle=F
(dep_rate <- rbind(dep_rate,tibble(Resource_Type="Brine",
                                   dep_rate=0.03,dle=T)))


# Max production rate based on all existing resources
df <- df %>% 
  left_join(dep_rate) %>% 
  mutate(max_prod_rate=all_resource*dep_rate)


# Max Ramp Up ----------
# 4 years
df <- df %>% mutate(max_ramp_up=max_prod_rate/4)


# Extraction Costs -----------

#merge coefs
# avoid volcano
(coefs <- tibble(Resource_Type=c(rep("Brine",2),rep("Hard Rock",2),rep("Volcano-Sedimentary",2)),
       var=c(names(coef_brine),names(coef_rock),names(coef_volcano)),
       coef=c(coef_brine,coef_rock,coef_volcano)) %>% 
    mutate(var=str_remove_all(var,"\\(|\\)"),
           var=paste0("cExt_",var)) %>% 
  pivot_wider(names_from = var, values_from = coef))


# number of deposits with cost
nrow(df)
df %>% filter(!is.na(USD_pertonne_Li)) %>% nrow()

# Stage 1
df <- df %>% 
  left_join(coefs) %>%  
  mutate(cost_source=case_when(
    !is.na(USD_pertonne_Li) ~ "Report", 
    !is.na(reserve) & !is.na(Grade_percLi_Reserve) & 
      Resource_Type!="Volcano-Sedimentary" ~ "Regression",
    T ~ "Quantile"),
    cost1=case_when(
      !is.na(USD_pertonne_Li) ~ USD_pertonne_Li, # use cost
      !is.na(reserve) & !is.na(Grade_percLi_Reserve) & 
        Resource_Type!="Volcano-Sedimentary" ~ # avoid reg for volcano
        cExt_Intercept+cExt_Grade_percLi_Reserve*Grade_percLi_Reserve,
         T ~ NA)) %>% 
  mutate(cost1=if_else(cost1>0,cost1,NA))

table(df$cost_source)
ggplot(df,aes(cost1,fill=Resource_Type))+geom_density(alpha=.4)

df %>% filter(!is.na(cost1)) %>% nrow()


# quantile by type and for reserves, resources and inferred resources
# get differences to avoid issues of cost 1 > cost 2
(cost_avg <- df %>% group_by(Resource_Type) %>% 
  reframe(avg_cost1=quantile(cost1,0.75,na.rm=T),
          avg_cost2=quantile(cost1,0.9,na.rm=T)-avg_cost1,
          avg_cost3=quantile(cost1,0.99,na.rm=T)-avg_cost1,
          max_cost=max(cost1,na.rm = T)-avg_cost1,
          n=sum(!is.na(cost1))))

# Note: with weighted quantiles it does not change that much

# For volcano-sedimentary there are not enough data points
# USE quantiles from every deposit then
cost_avg[3,2] <- quantile(df$cost1,0.75,na.rm=T)
cost_avg[3,3] <- quantile(df$cost1,0.9,na.rm=T)-quantile(df$cost1,0.75,na.rm=T)
cost_avg[3,4] <- quantile(df$cost1,0.99,na.rm=T)-quantile(df$cost1,0.75,na.rm=T)
cost_avg[3,5] <- max(df$cost1,na.rm=T)-quantile(df$cost1,0.75,na.rm=T)
cost_avg


# Sample from quantile to upper tail
# Why sample: to avoid tipping point with same values of costs
set.seed(13062024)
# Use quantile 
df <- df %>% 
  left_join(cost_avg) %>% 
  # USD/ton Li
  mutate(cost1=if_else(is.na(cost1),
                       EnvStats::rtri(nrow(df),min=avg_cost1,max=avg_cost1+avg_cost2,mode=avg_cost1+1), # triangular distribution
                       cost1))

ggplot(df,aes(cost1,col=Resource_Type))+stat_ecdf()+coord_flip()

df %>% filter(!is.na(cost1)) %>% nrow()


# Stage 2 - get resources based on Grade difference
df <- df %>% 
  mutate(cost2=cost1+case_when(
    grade_resource<Grade_percLi_Reserve &
      Resource_Type!="Volcano-Sedimentary" ~ (grade_resource-Grade_percLi_Reserve)*
      cExt_Grade_percLi_Reserve,
    T ~ NA))
  
df %>% filter(!is.na(cost2)) %>% nrow()

df %>% filter(cost2<cost1) # to check

# OLD
# get quantiles of difference between cost2 and 1
# (cost_avg2 <- df %>%
#     mutate(diff_cost=cost2-cost1) %>%
#     group_by(Resource_Type) %>%
#     reframe(avg_cost2=quantile(diff_cost,0.8,na.rm=T)))

#df %>% group_by(Resource_Type) %>% dplyr::select(Resource_Type,cost2) %>% skimr::skim_without_charts()  

df <- df %>% 
  dplyr::select(-avg_cost1) %>% 
  # left_join(cost_avg2) %>% 
  mutate(cost2=if_else(is.na(cost2),
                       cost1+EnvStats::rtri(nrow(df),min=avg_cost2,max=avg_cost3,mode=avg_cost2+1),
                       # avg_cost2+cost1+runif(nrow(df))*100,
                       cost2))
df %>% filter(!is.na(cost2)) %>% nrow()
ggplot(df,aes(cost2,col=Resource_Type))+stat_ecdf()

# Stage 3
df <- df %>% 
  mutate(cost3=cost2+case_when(
    grade_resource_inferred<grade_resource & 
      Resource_Type!="Volcano-Sedimentary" ~ (grade_resource_inferred-grade_resource)*
      cExt_Grade_percLi_Reserve,
    T ~ NA)) 

df %>% filter(!is.na(cost3)) %>% nrow()
df %>%  filter(cost3<cost2) # to check

# OLD get quantiles of difference
# (cost_avg3 <- df %>% 
#     mutate(diff_cost=cost3-cost2) %>% 
#     group_by(Resource_Type) %>% 
#     reframe(avg_cost3=quantile(diff_cost,0.8,na.rm=T)))

df <- df %>% 
  dplyr::select(-avg_cost2) %>% 
  # left_join(cost_avg3) %>% 
  mutate(cost3=if_else(is.na(cost3),
                       cost1+EnvStats::rtri(nrow(df),min=avg_cost3,max=max_cost,mode=avg_cost3+1),
                       # avg_cost3+cost1+runif(nrow(df))*100,
                       cost3)) %>% 
  dplyr::select(-avg_cost3)

ggplot(df,aes(cost3,col=Resource_Type))+stat_ecdf()

# Last check up - should be zero
df %>% filter(cost1>cost2) %>% nrow()
df %>% filter(cost2>cost3) %>% nrow()

## Royalties ----------

# Need to have the Table S7 from the Article
royalty <- read_excel(sprintf(url_file,"Data S3.xlsx"),
                      sheet="Taxes",range="A4:D42")
names(royalty) <- c("Country","Corporate_Tax_Rate","Royalty_Rate","Royalty_Based")
royalty <- royalty %>% 
  mutate(Royalty_Rate=as.numeric(Royalty_Rate)) %>% 
  mutate(State=Country)

tax <- royalty %>% dplyr::select(Country,Corporate_Tax_Rate) %>% 
  mutate(Corporate_Tax_Rate=if_else(str_detect(Corporate_Tax_Rate,"United States"),
                 pull(filter(royalty,Country=="United States"),Corporate_Tax_Rate),
                 Corporate_Tax_Rate)) %>% 
  mutate(Corporate_Tax_Rate=as.numeric(Corporate_Tax_Rate))
royalty <- royalty %>% dplyr::select(State,Royalty_Rate,Royalty_Based)

# LCE price assumtpion
li_price <- 20000*5.323 # per ton Li

df <- df %>% 
  mutate(State=if_else(is.na(US_State),Country,US_State)) %>% 
  left_join(royalty) %>% 
  mutate(royalty=case_when(
    Royalty_Based=="Unit" & Country=="France" ~ 62.5*1.0538*2.153, # France, 62.5 Euro/t Li2O
    Royalty_Based=="Unit" & Country=="United Kingdom" ~ 2.03/grade_resource_inferred*100*1.24, # UK, 2.03 $pounds/ ton ore rock
    is.na(Royalty_Rate) ~ 0, # Spain
    US_State=="California" ~ Royalty_Rate*5.323, # per ton LCE
    US_State=="Arkansas" ~  Royalty_Rate*(1/240*1e9)*2.153, # on avg, 240 mg Li2O per Liter 
    Royalty_Based=="Revenue" ~ li_price*Royalty_Rate,
    Royalty_Based=="Profit" ~ (li_price-cost1)*Royalty_Rate,
    T ~ 0)) %>% 
  left_join(tax) %>% 
  mutate(tax_cost=(li_price-cost1)*Corporate_Tax_Rate)

df %>% 
  # filter(Country=="United States") %>% 
  dplyr::select(Resource_Type,Deposit_Name,cost1,royalty,tax_cost) %>% 
  pivot_longer(c(cost1,royalty,tax_cost), names_to = "key", values_to = "value") %>% 
  mutate(value=value/5.323) %>% 
  ggplot(aes(reorder(Deposit_Name,value),value,fill=key))+
  geom_col()+
  facet_grid(Resource_Type~.,scales = "free_y",space = "free_y")+
  coord_flip(expand = F,ylim = c(0,NA))+
  labs(x="",y="Extraction Cost [USD/ton LCE]",fill="")+
  guides(fill= guide_legend(reverse = TRUE))+
  theme(legend.position = "bottom")
  
# for cost 2 and cost 3
df <- df %>% 
  mutate(cost1_noTax=cost1,
         cost1=cost1+royalty+tax_cost) %>% 
  mutate(royalty2=if_else(Royalty_Based=="Profit",
                          (li_price-cost2)*Royalty_Rate,
                          royalty),
         tax_cost2=(li_price-cost2)*Corporate_Tax_Rate,
         cost2_noTax=cost2,
         cost2=cost2+royalty2+tax_cost2,
         royalty3=if_else(Royalty_Based=="Profit",
                          (li_price-cost3)*Royalty_Rate,
                          royalty),
         tax_cost3=(li_price-cost3)*Corporate_Tax_Rate,
         cost3_noTax=cost3,
         cost3=cost3+royalty3+tax_cost3)

# Expansion Costs ----------
# Based on regression

#merge coefs of expansion regression
(coefs_exp <- tibble(Resource_Type=c(rep("Brine",2),rep("Hard Rock",2),rep("Volcano-Sedimentary",1)),
                 var=c(names(coef(mod_cap_brine)),names(coef(mod_cap_rock)),
                       names(coef(mod_cap_volcano))),
                 coef=c(coef(mod_cap_brine),coef(mod_cap_rock),coef(mod_cap_volcano))) %>% 
   mutate(var=str_remove_all(var,"\\(|\\)"),
          var=paste0("cExp_",var)) %>% 
   pivot_wider(names_from = var, values_from = coef))
# df %>% filter(Resource_Type_orig=="Volcano-Sedimentary") %>% reframe(x=mean(Investment_original,na.rm=T))
coefs_exp[3,2] <- 250 # for Volcano

df <- df %>% 
  left_join(coefs_exp) %>% 
  mutate(cost_expansion=cExp_Project_Capacity_Li_ktons*1e3) #USD per tpa in expansion
  # mutate(resource=Resource_Li_ktons) %>% 
  # mutate(cost_expansion=exp((resource-reserve)/resource)*cost_extraction)

# Current production rate -----
# USGS 2022 and 2023 production data by country
(usgs <- read.csv("Data/Supply Model/USGS_Production.csv") %>% 
   mutate(Year=paste0("prod_rate",Year)) %>% 
   mutate(USGS_production=USGS_production/1e3) %>% # to ktons
   pivot_wider(names_from = Year, values_from = USGS_production))


# Give to open mines in countries - shares of 2025 capacity
table(df$Capacity_Forecast_Status)
# only to opend or under construction mines
status_prodRates <- c("Current Lithium Mines","Under Construction")
df <- df %>% 
  mutate(Capacity_2025_Li_ktons=if_else(
    Capacity_Forecast_Status %in% status_prodRates,Capacity_2025_Li_ktons,0),
    Capacity_2030_Li_ktons=if_else(
      Capacity_Forecast_Status %in% status_prodRates,Capacity_2030_Li_ktons,0))

# producing mines
share_pr <- df %>% 
  filter(Capacity_2025_Li_ktons>0) %>%
  filter(Status=="Producing") %>%
  group_by(Country) %>% 
  mutate(share_prod=Capacity_2025_Li_ktons/sum(Capacity_2025_Li_ktons)) %>% 
  ungroup() %>% 
  dplyr::select(Deposit_Name,share_prod)

# divide into mines open
df <- df %>% 
  left_join(usgs) %>% 
  left_join(share_pr) %>% 
  # be aware of NAs
  mutate(Capacity_2025_Li_ktons=if_else(is.na(Capacity_2025_Li_ktons),
                                           0,Capacity_2025_Li_ktons),
         Capacity_2030_Li_ktons=if_else(is.na(Capacity_2030_Li_ktons),
                                           0,Capacity_2030_Li_ktons),
         prod_rate2022=prod_rate2022*share_prod,
         prod_rate2022=if_else(is.na(prod_rate2022),0,prod_rate2022),
         prod_rate2023=prod_rate2023*share_prod,
         prod_rate2023=if_else(is.na(prod_rate2023),0,prod_rate2023),
         prod_rate2025=if_else(Capacity_2025_Li_ktons>prod_rate2023,
                               Capacity_2025_Li_ktons,prod_rate2023),
         prod_rate2030=if_else(Capacity_2030_Li_ktons>prod_rate2023,
                               Capacity_2030_Li_ktons,prod_rate2023))
sum(df$prod_rate2022,na.rm = T)
sum(df$prod_rate2023,na.rm = T)
sum(df$prod_rate2025,na.rm = T)
sum(df$prod_rate2030,na.rm = T)


# Opening Costs -----

# Intercept
table(df$Status)
table(df$Status_Detail)
table(df$Capacity_Forecast_Status)
table(df$open_mine)
df <- df %>% 
  mutate(cost_opening=case_when(
    open_mine==T ~ 0, # if producing or in construction then 0, investment is already commited
    Capacity_Forecast_Status %in% status_prodRates ~ 0,
    T ~ cExp_Intercept*1e6)) # in USD

# Status to delay mines
df <- df %>% 
  mutate(Status_Delay=case_when(
    Status_Detail=="Construction" ~ "Construction", 
    open_mine==T ~ "Open",
    !is.na(Status_Detail) ~ "Evaluation",
    T ~ "No Info"))
table(df$Status_Delay)

# DELAY - years to avoid any expansion (starting from 2022)
df <- df %>% 
  mutate(delay_years=case_when(
    Status_Delay=="Open" ~ 3, # no expansion until 2025
    Status_Delay=="Construction" ~ 5, # until 2027 
    Status_Delay=="Evaluation" ~ 8, # until 2030
    Status_Delay=="No Info" ~ 10, # until 2032
      T ~ 10))
table(df$delay_years)

# Fix issue that current production rate should be less or equal to max prod rate.
df <- df %>%
  ungroup() %>% 
  mutate(max_prod_rate=if_else(max_prod_rate>prod_rate2022,max_prod_rate,prod_rate2022)) %>% 
  mutate(max_prod_rate=if_else(max_prod_rate>prod_rate2023,max_prod_rate,prod_rate2023)) %>% 
  mutate(max_prod_rate=if_else(max_prod_rate>prod_rate2025,max_prod_rate,prod_rate2025)) %>% 
  mutate(max_prod_rate=if_else(max_prod_rate>prod_rate2030,max_prod_rate,prod_rate2030))
  

df <- df %>% mutate(max_ramp_up=max_prod_rate/4)


# Filter only deposits with actual reserves
df <- df %>% filter(reserve+resource_demostrated+resource_inferred>0)

# Non Monetary Index ----------

## Ease of Doing Business
# https://archive.doingbusiness.org/en/data/doing-business-score (2020).
edb <- read.csv("Data/Supply Model/EDB.csv")
names(edb) <- c("Country","edb")

# add by country
df <- df %>% left_join(edb) %>% 
  # mutate(edb=if_else(open_mine==T,100,edb)) %>%  # if opened, no cost
  mutate(edb=100-edb) # from 0 (better) to 100, to minimize

# WGI - Political Stability
# World Bank. Worldwide Governance Indicators, 2024 Update. http://www.govindicators.org/ (2024).
wgi <- read.csv("Data/Supply Model/WGI.csv")
range(wgi$wgi)
df <- df %>% left_join(wgi) %>% 
  mutate(wgi=2.5-wgi) # to minimize, 0 to 5

# WCR - World Competitive Ranking
# IMD World Competitiveness Center. World competitiveness ranking. at https://www.imd.org/ (2024).
# All NAs were filled with 20 (worst)
wcr <- read.csv("Data/Supply Model/WCR.csv")
range(wcr$wcr)
df <- df %>% left_join(wcr) %>% 
  mutate(wcr=100-wcr) # from 0 (better) to 100, to minimize

# correlations of index - included countries
all_indexes <- wcr %>% 
  left_join(wgi) %>% left_join(edb)
cor(all_indexes$edb,all_indexes$wgi,method = "pearson")
cor(all_indexes$wcr,all_indexes$edb,method = "pearson")
cor(all_indexes$wcr,all_indexes$wgi,method = "pearson")


rm(all_indexes)

# save -----
# Select only required columns
df <- df %>% dplyr::select(Country,Deposit_Name,Resource_Type,Latitude,Longitude,
                           Status,Status_Delay,Grade_percLi_Reserve,grade_resource,grade_resource_inferred,
                           open_mine,reserve,resource_demostrated,resource_inferred,all_resource,
                           cost1,cost2,cost3,max_prod_rate,max_ramp_up,cost_expansion,
                           cost_opening,cost_source,
                           prod_rate2022,prod_rate2023,prod_rate2025,prod_rate2030,
                           delay_years,
                           Resource_Type_orig,edb,wgi,wcr,
                           Corporate_Tax_Rate,cost1_noTax,cost2_noTax,cost3_noTax)
# add rownames
df <- df %>% rownames_to_column() %>% rename(d=rowname)
df$d <- as.numeric(df$d)


# Names selection - for map
head(df)
df <- df %>% 
  mutate(label_name=if_else(all_resource>1000,Deposit_Name,"")) %>% 
  mutate(label_name=if_else(all_resource>1000,Deposit_Name,"")) %>% 
  mutate(label_name=if_else(str_detect(Deposit_Name,"Arcadia|Goulamina|Zinnwald|Carolina|LANXESS|Xuxa|Volta|Ostroboth")
                            ,Deposit_Name,label_name)) %>% 
  mutate(label_name=if_else(str_detect(label_name,"Albemarle"),"",label_name)) %>% 
  mutate(label_name=label_name %>% 
  str_remove_all(" \\(All\\)| \\(All Projects\\)| \\(Century\\)| \\(SQM\\)"))

head(df)
write.csv(df,"Parameters/Deposit.csv",row.names = F)

# Scenarios Save ------
url_scen <- "Parameters/Deposit_scenarios/%s.csv"
df_orig <- df

## Ramp up -----
# No ramp up
df <- df_orig %>% mutate(max_ramp_up=max_prod_rate)
write.csv(df,sprintf(url_scen,"NoRampUp"),row.names = F)

# 2 years
df <- df_orig %>% mutate(max_ramp_up=max_prod_rate/2)
write.csv(df,sprintf(url_scen,"2yRampUp"),row.names = F)

# 8 years
df <- df_orig %>% mutate(max_ramp_up=max_prod_rate/8)
write.csv(df,sprintf(url_scen,"8yRampUp"),row.names = F)

## Max Prod Rates -----

# All 5% depletion rate
df <- df_orig %>% mutate(max_prod_rate=all_resource*0.05)
# Ensure current prod rate are ok
df <- df %>%
  mutate(max_prod_rate=if_else(max_prod_rate>prod_rate2022,max_prod_rate,prod_rate2022)) %>% 
  mutate(max_prod_rate=if_else(max_prod_rate>prod_rate2023,max_prod_rate,prod_rate2023)) %>% 
  mutate(max_prod_rate=if_else(max_prod_rate>prod_rate2025,max_prod_rate,prod_rate2025)) %>% 
  mutate(max_prod_rate=if_else(max_prod_rate>prod_rate2030,max_prod_rate,prod_rate2030))
df <- df %>% mutate(max_ramp_up=max_prod_rate/4)
write.csv(df,sprintf(url_scen,"5_prodRate"),row.names = F)

# All 2% depletion rate or 100K
df <- df_orig %>% mutate(max_prod_rate=all_resource*0.02)
# Ensure current prod rate are ok
df <- df %>%
  mutate(max_prod_rate=if_else(max_prod_rate>100,100,max_prod_rate)) %>% 
  mutate(max_prod_rate=if_else(max_prod_rate>prod_rate2022,max_prod_rate,prod_rate2022)) %>% 
  mutate(max_prod_rate=if_else(max_prod_rate>prod_rate2023,max_prod_rate,prod_rate2023)) %>% 
  mutate(max_prod_rate=if_else(max_prod_rate>prod_rate2025,max_prod_rate,prod_rate2025)) %>% 
  mutate(max_prod_rate=if_else(max_prod_rate>prod_rate2030,max_prod_rate,prod_rate2030))
df <- df %>% mutate(max_ramp_up=max_prod_rate/4)
write.csv(df,sprintf(url_scen,"2_prodRate"),row.names = F)

# All DLE - 3% depletion rate
df <- df_orig %>% mutate(max_prod_rate=all_resource*case_when(
  Resource_Type=="Hard Rock" ~ 0.05,
  T ~ 0.03))
# Ensure current prod rate are ok
df <- df %>%
  mutate(max_prod_rate=if_else(max_prod_rate>prod_rate2022,max_prod_rate,prod_rate2022)) %>% 
  mutate(max_prod_rate=if_else(max_prod_rate>prod_rate2023,max_prod_rate,prod_rate2023)) %>% 
  mutate(max_prod_rate=if_else(max_prod_rate>prod_rate2025,max_prod_rate,prod_rate2025)) %>% 
  mutate(max_prod_rate=if_else(max_prod_rate>prod_rate2030,max_prod_rate,prod_rate2030))
df <- df %>% mutate(max_ramp_up=max_prod_rate/4)
# Additional CAPEX and lower OPEX for DLE
# CAPEX: $1500 per tpa LCE
# OPEX: -$900 per ton LCE
df <- df %>% 
  mutate(cost_expansion=cost_expansion+
           if_else(Resource_Type=="Brine",1.5*5.323,0),
         cost1=cost1-if_else(Resource_Type=="Brine",900*5.323,0),
         cost2=cost2-if_else(Resource_Type=="Brine",900*5.323,0),
         cost3=cost3-if_else(Resource_Type=="Brine",900*5.323,0))
write.csv(df,sprintf(url_scen,"AllDLE_prodRate"),row.names = F)

# NO DLE
df <- df_orig %>% mutate(max_prod_rate=all_resource*case_when(
  Resource_Type=="Hard Rock" ~ 0.05,
  Resource_Type=="Brine" ~ 0.01,
  T ~ 0.03))
# Ensure current prod rate are ok
df <- df %>%
  mutate(max_prod_rate=if_else(max_prod_rate>prod_rate2022,max_prod_rate,prod_rate2022)) %>% 
  mutate(max_prod_rate=if_else(max_prod_rate>prod_rate2023,max_prod_rate,prod_rate2023)) %>% 
  mutate(max_prod_rate=if_else(max_prod_rate>prod_rate2025,max_prod_rate,prod_rate2025)) %>% 
  mutate(max_prod_rate=if_else(max_prod_rate>prod_rate2030,max_prod_rate,prod_rate2030))
df <- df %>% mutate(max_ramp_up=max_prod_rate/4)
write.csv(df,sprintf(url_scen,"noDLE_prodRate"),row.names = F)

## Lead Times -----
df <- df_orig %>% 
  mutate(delay_years=case_when(
    Status_Delay=="Open" ~ 3, # no expansion until 2025
    Status_Delay=="Construction" ~ 4, # until 2026 
    Status_Delay=="Evaluation" ~ 6, # until 2028
    Status_Delay=="No Info" ~ 8, # until 2030
    T ~ 8))
table(df$delay_years)
write.csv(df,sprintf(url_scen,"shorter_LeadTime"),row.names = F)

df <- df_orig %>% 
  mutate(delay_years=case_when(
    Status_Delay=="Open" ~ 3, # no expansion until 2025
    Status_Delay=="Construction" ~ 5, # until 2027 
    Status_Delay=="Evaluation" ~ 10, # until 2032
    Status_Delay=="No Info" ~ 14, # until 2036
    T ~ 14))
table(df$delay_years)
write.csv(df,sprintf(url_scen,"longer_LeadTime"),row.names = F)

## Just Demostrated Resources -----
df <- df_orig %>% 
  mutate(dep_rate=max_prod_rate/all_resource) %>% 
  mutate(resource_inferred=0,
         all_resource=reserve+resource_demostrated,
         max_prod_rate=all_resource*dep_rate,dep_rate=NULL)
# Ensure current prod rate are ok
df <- df %>%
  mutate(max_prod_rate=if_else(max_prod_rate>prod_rate2022,max_prod_rate,prod_rate2022)) %>% 
  mutate(max_prod_rate=if_else(max_prod_rate>prod_rate2023,max_prod_rate,prod_rate2023)) %>% 
  mutate(max_prod_rate=if_else(max_prod_rate>prod_rate2025,max_prod_rate,prod_rate2025)) %>% 
  mutate(max_prod_rate=if_else(max_prod_rate>prod_rate2030,max_prod_rate,prod_rate2030))
df <- df %>% mutate(max_ramp_up=max_prod_rate/4)
write.csv(df,sprintf(url_scen,"NoInferredResources"),row.names = F)

# No tax or royalties
df <- df_orig %>% 
  mutate(cost1=cost1_noTax,cost2=cost2_noTax,cost3=cost3_noTax)
write.csv(df,sprintf(url_scen,"NoTax"),row.names = F)

# Hard Rock 20% more expensive due to off-site transport OPEX
df <- df_orig %>% 
  mutate(cost1=cost1*if_else(Resource_Type=="Hard Rock",1.2,1),
         cost2=cost2*if_else(Resource_Type=="Hard Rock",1.2,1),
         cost3=cost3*if_else(Resource_Type=="Hard Rock",1.2,1))
write.csv(df,sprintf(url_scen,"RockTransportCosts"),row.names = F)

# Figure ----

# Fig. S15. Resources by stage (reserve, demonstrated resources and inferred resources) and lithium resource type

df_country <- deposit %>% 
  # mutate(Resource_Type=Resource_Type_orig) %>% 
  group_by(Resource_Type,Country) %>% 
  reframe(reserve=sum(reserve)/1e3,
          resource_demonstrated=sum(resource_demostrated)/1e3,
          resource_inferred=sum(resource_inferred)/1e3) %>% 
  mutate(resource_all=reserve+resource_demonstrated+resource_inferred) %>% 
  arrange(desc(resource_all)) %>% ungroup()
df_country$resource_all %>% sum()

df_country %>% dplyr::select(-Country,-resource_all) %>% 
  pivot_longer(c(-Resource_Type), names_to = "key", values_to = "value") %>% 
  group_by(Resource_Type,key) %>% reframe(value=sum(value)) %>% ungroup() %>% 
  mutate(key=key %>% str_replace("_"," ") %>% str_to_title()) %>% 
  mutate(key=factor(key,levels=c("Resource Inferred","Resource Demonstrated","Reserve"))) %>%
  group_by(Resource_Type) %>% mutate(value_sum=sum(value)) %>% ungroup() %>% 
  ggplot(aes(reorder(Resource_Type,value_sum),value,fill=key))+
  geom_col(col="black")+
  # geom_hline(yintercept = seq(10,60,10),col="white")+
  coord_flip(expand = F,ylim = c(NA,65))+
  scale_fill_manual(values =  c("#6b6b6b", "#ff7f0e", "#1f77b4"))+
  labs(x="",y="Lithium Million tons",fill="")+
  guides(fill= guide_legend(reverse = TRUE))+
  theme(legend.position = "top")

f.fig.save(sprintf(url_fig,"Resource_type"))

# EoF