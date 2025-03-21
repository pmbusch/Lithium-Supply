## Survival Curve based on Normal Distribution
# Calculates dynamics for each region based on survival curves
# Results in detailed outflows of EVs and LIBs additional requirements,
# as well as LIB outflows to EVs, SSPS and recycling
## PBH January 2024

source("Scripts/00-Libraries.R", encoding = "UTF-8")
source("Scripts/01-CommonVariables.R", encoding = "UTF-8")


# Function to get flows (numbers of cars,EV,LIB) depending on the 
# vehicle and battery starting age
# Discretized by year using Normal Distribution
# n vehicles: vehicles currently on stock, 
f.getOutflows <- function(n_veh=1,EV_age,LIB_age, maxEV_age=30, maxLIB_age=30,
                          dist.Age="Logistic"){
  
  # get probability of failure based on CDF of Normal
  # EV
  # y1 = pnorm(EV_age+1, mean = mean_ev, sd = sd_ev)-pnorm(EV_age, mean = mean_ev, sd = sd_ev) 
  # # LIB
  # y2 = pnorm(LIB_age+1, mean = mean_lib, sd = sd_lib)-pnorm(LIB_age, mean = mean_lib, sd = sd_lib)
  
  # option 2: get fraction year to year of survival, based on CDF ratios
  # represent proportion that survives year to year
  
  if(dist.Age=="Normal"){
    y1 = (1-pnorm(EV_age+1, mean = mean_ev, sd = sd_ev))/
      (1-pnorm(EV_age, mean = mean_ev, sd = sd_ev))
    y2 = (1-pnorm(LIB_age+1, mean = mean_lib, sd = sd_lib))/
      (1-pnorm(LIB_age, mean = mean_lib, sd = sd_lib))
  } else{ # Logistic
    y1 = (1-plogis(EV_age+1, mean_ev, sd_ev*sqrt(3)/pi))/ # CONVERT SCALE TO Stand Dev.
      (1-plogis(EV_age, mean_ev, sd_ev*sqrt(3)/pi))
    y2 = (1-plogis(LIB_age+1, mean_lib, sd_lib*sqrt(3)/pi))/
      (1-plogis(LIB_age, mean_lib, sd_lib*sqrt(3)/pi))
  }
  
  
  # max age
  if(EV_age>=maxEV_age) {y1 = 0}
  if(LIB_age>=maxLIB_age) {y2 = 0}
  
  # # get probabilities as independent events
  # ret <- tibble(
  #   both_fail=y1*y2*n_veh,
  #   ev_fail=y1*(1-y2)*n_veh,
  #   lib_fail=(1-y1)*y2*n_veh,
  #   none=(1-y1)*(1-y2)*n_veh) # none fails
  
  # case 2 - independent events to get proportions into 4 cases
  ret <- tibble(
    both_fail=(1-y1)*(1-y2)*n_veh,
    ev_fail=(1-y1)*y2*n_veh,
    lib_fail=y1*(1-y2)*n_veh,
    none=y1*y2*n_veh)
  
  return(ret)
}



# Cohort Outflows --------------

# parameters
# Other parameters
ev_age_newLib <- 8 # year were a new battery is needed, after that an old battery will be sufficient
# 8 years assuming a warranty over this period
max_reuse_lib <- 0.5
# Max age when an EV gets a battery, either 2-hand or new
max_ev_age <- 20
# Max age of LIB to be used in an EV
max_lib_age_ev <- 12

# life time parameters
life_param <- tibble(Vehicle=c("Two/Three Wheelers","Car","Van","Bus",
                               "Medium truck","Heavy truck"),
                     mean_ev=c(12,17,18,16,17,17),
                     sd_ev=rep(4,6),
                     mean_lib=c(8,15,15,8,8,8),
                     sd_lib=rep(4,6),
                     scen_lifetime="Baseline")

# scenario 2
life_param2 <- tibble(Vehicle=c("Two/Three Wheelers","Car","Van","Bus",
                               "Medium truck","Heavy truck"),
                     mean_ev=c(14,20,20,18,19,19),
                     sd_ev=rep(4,6),
                     mean_lib=c(10,20,20,10,10,10),
                     sd_lib=rep(4,6),
                     scen_lifetime="Long duration")
life_param <- rbind(life_param,life_param2)
rm(life_param2)

# Data from ICCT - expanded to 2070
icct <- read.csv("Parameters/Demand Intermediate Results/ICCT_demand.csv")
dict_regions <- icct %>% group_by(Region,Country) %>% tally() %>% mutate(n=NULL)


# Historical EV sales for stock
EV_historical <- read.csv("Parameters/Demand Intermediate Results/historicalEV_sales.csv")

# Whole world
# icct <- icct %>% 
#   filter(Powertrain %in% c("BEV","PHEV")) %>% 
#   group_by(Vehicle, Powertrain,Year,Scenario) %>% summarise(Sales=sum(Sales))

## regional
icct <- icct %>% 
  filter(Powertrain %in% c("BEV","PHEV")) %>% 
  group_by(Region,Vehicle, Powertrain,Year,Scenario) %>% summarise(Sales=sum(Sales))


# add historical
EV_historical <- EV_historical %>% rename(Year=year) %>% 
  filter(Year<2022) %>% 
  left_join(dict_regions,by=c("ICCT_Country"="Country")) %>% 
  rename(Powertrain=Propulsion) %>% 
  filter(Powertrain %in% c("BEV","PHEV")) %>% 
  group_by(Region,Year,Powertrain) %>% reframe(Sales=sum(unit)) %>% 
  mutate(Vehicle="Car") #All EV Volumes is for cars only stock data available


## Loop ------
scenarios <- c("Ambitious")
(vehicles <- icct$Vehicle %>% unique())
(powers <- icct$Powertrain %>% unique())
(lifetime <- life_param$scen_lifetime %>% unique())
# lifetime <- c("Baseline") # run faster
(regions <- icct$Region %>% unique())
icct_orig <- icct
icct_new <- c()
# max_reuse_lib <- 0 # no LIB reuse case

for (reg in regions){
  for (veh in vehicles){
    for (lif in lifetime){
      
      # life params
      mean_ev <- life_param %>% filter(scen_lifetime==lif,Vehicle==veh) %>% pull(mean_ev)
      sd_ev <- life_param %>% filter(scen_lifetime==lif,Vehicle==veh) %>% pull(sd_ev)
      mean_lib <- life_param %>% filter(scen_lifetime==lif,Vehicle==veh) %>% pull(mean_lib)
      sd_lib <- life_param %>% filter(scen_lifetime==lif,Vehicle==veh) %>% pull(sd_lib)
      
      for (pow in powers){
        cat("",veh,"-",pow,"\n")
        
        # if (veh=="Two/Three Wheelers" & pow=="PHEV"){break} # no much sales for this
        
        for (scen in scenarios){
          cat("Scenario ",scen,"\n")
          
          # Filters
          icct <- icct_orig %>% 
            filter(Scenario==scen) %>% 
            filter(Vehicle==veh) %>%
            filter(Region==reg) %>% 
            filter(Powertrain==pow)
          
          icct$scen_lifetime <- lif
          
          start_year <- 2022
          
          # add historical sales - BEV and PHEV
          if(veh=="Car"){
            EV_historical_aux <- EV_historical %>% 
              filter(Region==reg) %>% 
              filter(Powertrain==pow) 
            EV_historical_aux$Scenario <- scen
            EV_historical_aux$scen_lifetime <- lif
            icct <- rbind(EV_historical_aux,icct)
            start_year <- 2015
          }
          
          ## Loop by years 
          # Matrix update idea
          # Key: Update matrix of vehicle age and battery age stock accordingly
          matrix_data <- matrix(0, nrow = 31, ncol = 31)
          rownames(matrix_data) <-paste0("EV_",0:30) # ROWS are EV
          colnames(matrix_data) <- paste0("LIB_",0:30) # COLS are Battery
          
          # Loop through years
          icct$Year %>% range()
          icct$add_LIB <-icct$LIB_Available <- icct$LIB_recycling <- icct$LIB_reuse_EV <- icct$EV_Stock <- 0
          icct$add_LIB_vector <-icct$LIB_Available_vector <- icct$LIB_recycling_vector <- icct$EV_Stock_vector <- c()
            
          for (y in start_year:2070){
            
            # if (y==2043){break} # debug
          
            # Assign new sales to top left cuadrant (0,0)
            matrix_data[1, 1] <- icct$Sales[y-start_year+1]
            
            # clear stock of 10 or less batteries or EVs
            matrix_data[matrix_data < 10] <- 0
            
            # Get new matrix of EV stock with ages, LIBs in good use 
            new_matrix <- matrix_ev <- matrix_lib <- matrix(0, nrow = 31, ncol = 31)
            rownames(new_matrix) <-paste0("EV_",0:30) # ROWS are EV
            colnames(new_matrix) <- paste0("LIB_",0:30) # COLS are Battery
            
            
            for (i in 1:31) { # EV
              for (j in 1:31) { # LIB
                if (matrix_data[i, j] != 0) {
                  result <- f.getOutflows(matrix_data[i, j],i-1,j-1) # age is minus 1 for the index
                  if (i!=31 & j!=31){ # to avoid border case
                    new_matrix[i + 1, j + 1] <- result$none # move 1 age for both EV and LIB
                    matrix_ev[i+1,j+1] <- result$lib_fail+result$both_fail # EVs that need LIB
                    matrix_lib[i+1,j+1] <- result$ev_fail # LIBs available to use
                  } else if (j==31 & i!=31){ # BATTERIES TOO OLD
                    matrix_ev[i+1,j] <- result$lib_fail+result$both_fail # EVs that need LIB, no LIBs available as they died
                  } else if (j!=31 & i==31){ # EV TOO OLD
                    matrix_lib[i,j+1] <- result$ev_fail # LIBs available to use, no EV at border
                  }
                }
              }
            }
            # get vector of outflows of EV and outflows of LIBs
            ev_need <- rowSums(matrix_ev)
            
            # Above certain age simply no LIB required, THEY DIED
            ev_need[(max_ev_age+1):31] <- 0
            
            # move to the left to allow for delay in other part of the code
            lib_failed <- colSums(matrix_ev)[-1] # LIB ready for end life recycling, when the LIB failed
            lib_available <- colSums(matrix_lib)
            
            # assigning old batteries TO EVs
            lib_to_EV <- lib_available*max_reuse_lib
            # limit age of LIB for EV
            lib_to_EV[(max_lib_age_ev+1):31] <- 0
            
            lib_available <- lib_available-lib_to_EV
            
            # first match year to year with offset of years - 8 years
            ev_need <- c(ev_need,rep(0,ev_age_newLib))
            lib_to_EV <- c(rep(0,ev_age_newLib),lib_to_EV)
            allocation <- pmin(ev_need,lib_to_EV)
            
            ev_need <- ev_need - allocation
            lib_to_EV <- lib_to_EV - allocation
            
            # remove offsets
            ev_need <- ev_need[1:31]
            lib_to_EV <- lib_to_EV[-(1:ev_age_newLib)]
            allocation <- allocation[-(1:ev_age_newLib)]
            
            # update new_matrix with stock of EVs and old batteries
            for (i in 1:(31-ev_age_newLib)){
              new_matrix[i+ev_age_newLib,i] <- new_matrix[i+ev_age_newLib,i]+allocation[i]
            }
            
            allocation <- sum(allocation)
            
            # do rest of allocation with LOOP
            start_bat <- 1
            for (i in 31:1) { # start with old
              if (i<=ev_age_newLib){
                # new_matrix[i,0] <- ev_need[i] # new battery DUPLICATED
              } else {
                for (j in start_bat:31) {
                  allocated <- min(ev_need[i], lib_to_EV[j])
                  ev_need[i] <- ev_need[i] - allocated
                  lib_to_EV[j] <- lib_to_EV[j] - allocated
                  # update new_matrix with stock of EVs and old batteries
                  new_matrix[i,j] <- new_matrix[i,j]+allocated
                  allocation <- allocation+allocated
                  start_bat <- j
                  if (ev_need[i] == 0) { break }
                }
              }
            }
            
            # add remaining batteries back to pool
            lib_available <- lib_available+lib_to_EV
            
            # add EVs with new batteries to stock - note, no other battery with 0 age
            new_matrix[,1] <-  ev_need
            
            # assign numbers for Year - totals and vector
            icct$add_LIB[y-start_year+1] <- round(sum(ev_need),0) # additional new LIBs required
            icct$add_LIB_vector[y-start_year+1] <- list(round(ev_need[-1],0)) 
            # LIBs in good condition for SSPS or recycling
            icct$LIB_Available[y-start_year+1] <- round(sum(lib_available),0)  
            icct$LIB_Available_vector[y-start_year+1] <- list(round(lib_available[-1],0))  
            # LIBs that failed but available to recycle
            icct$LIB_recycling[y-start_year+1] <- round(sum(lib_failed),0)
            icct$LIB_recycling_vector[y-start_year+1] <- list(round(lib_failed,0))
            icct$LIB_reuse_EV[y-start_year+1] <- round(allocation,0)
            icct$EV_Stock[y-start_year+1] <- round(sum(new_matrix),0)
            icct$EV_Stock_vector[y-start_year+1] <- list(unname(round(rowSums(new_matrix)[-1],0)))
            
            
            # end for loop, next year
            matrix_data <- new_matrix
            
            # keep balance of removed EV Sales from stock
            
            rm(new_matrix,matrix_ev,matrix_lib,lib_to_EV,lib_available,allocated,allocation,start_bat)
            
          }
          rm(i,j)
          # save data
          icct_new <- rbind(icct_new,icct)
        }
      }
      
    }
  }
}

icct <- icct_new



## save stats as World or region-----
# all as percentage of that year sales
icct <- icct %>% 
  filter(Year>2021) %>% 
  mutate(perc_add_lib=if_else(Sales==0,0,add_LIB/Sales),
         perc_lib_reuse_ev=if_else(Sales==0,0,LIB_reuse_EV/Sales),
         perc_lib_available=if_else(Sales==0,0,LIB_Available/Sales),
         perc_lib_recycling=if_else(Sales==0,0,LIB_recycling/Sales))
icct

# Save vector variables as strings
icct <- icct %>%
  rowwise() %>%
  mutate_if(is.list, ~paste(unlist(.), collapse = '|')) 

# write.csv(icct,"Parameters/Demand Intermediate Results/world_outflows_LIB.csv",row.names = F)
write.csv(icct,"Parameters/Demand Intermediate Results/region_outflows_LIB.csv",row.names = F)


## some analysis stats ----

icct_ev <- icct %>%  
  filter(scen_lifetime=="Baseline") %>% 
  filter(Powertrain=="BEV") %>% 
  filter(Scenario=="Ambitious") %>% 
  # filter(Year<2051) %>% 
  filter(Vehicle=="Car") 

icct_ev %>% group_by(Region) %>% 
  reframe(x=(sum(Sales)+sum(add_LIB))/sum(Sales)-1) # 27% more

  

# battery needs cumulative
sum(icct_ev$Sales)/1e6 # 4329M veh. equivalent 
(sum(icct_ev$Sales)+sum(icct_ev$add_LIB))/1e6 # 5530M
(sum(icct_ev$Sales)+sum(icct_ev$add_LIB))/sum(icct_ev$Sales)-1 # 27% more
sum(icct_ev$add_LIB)/1e6 # 1200M ADDITIONAL
sum(icct_ev$LIB_reuse_EV)/1e6 # 663M were used for reuse
sum(icct_ev$LIB_Available)/1e6 # 1513M
sum(icct_ev$LIB_recycling)/1e6 # 1994M for recycling

# 2050 YEAR
icct_ev$Sales[29]/1e6 # 95M veh. equivalent 
(icct_ev$Sales[29]+icct_ev$add_LIB[29])/1e6 # 117M
(icct_ev$Sales[29]+icct_ev$add_LIB[29])/icct_ev$Sales[29]-1 # 24% more
icct_ev$add_LIB[29]/1e6 # 22M ADDITIONAL
icct_ev$LIB_reuse_EV[29]/1e6 # 7M were used for reuse
icct_ev$LIB_Available[29]/1e6 # 17M
icct_ev$LIB_recycling[29]/1e6 # 30M

# figures
head(icct)
data_fig <- icct %>%
  filter(Year>2021) %>% 
  filter(scen_lifetime=="Baseline") %>% 
  dplyr::select(-EV_Stock,-add_LIB_vector,-LIB_Available_vector,
                -LIB_recycling_vector,-scen_lifetime,
                -perc_add_lib,-perc_lib_reuse_ev,
                -perc_lib_available,-perc_lib_recycling) %>% 
  rename(`Additional LIB \n required`=add_LIB) %>% 
  rename(`LIBs that failed`=LIB_recycling) %>% 
  rename(`2-hand LIBs`=LIB_Available) %>% 
  rename(`2-hand LIBs \n used for EVs`=LIB_reuse_EV) %>% 
  pivot_longer(c(-Year,-Scenario,-Vehicle,-Powertrain,-Region), names_to = "key", values_to = "value") %>% 
  filter(Scenario=="Ambitious") %>% 
  mutate(value=value/1e6) 

data_fig2 <- data_fig %>% filter(Vehicle=="Car", Powertrain=="BEV")

data_fig2 %>%  
  # filter(Year<2051) %>% 
  ggplot(aes(Year,value,col=key,group=key))+
  geom_line(linewidth=1)+
  geom_text(data=filter(data_fig2,Year==2070),x=2072,aes(label=key),
            # nudge_y = c(0,5,-5,2,18),
            lineheight = 0.8,
            size=11*5/14 * 0.8)+
  facet_wrap(~Region,scales = "free_y")+
  labs(x="",y="Units, in millions",col="")+
  theme(legend.position = "none")+
  coord_cartesian(xlim=c(2023.4,2073))+
  scale_x_continuous(breaks = c(2022, seq(2030, 2070, 10)), 
                     labels = c("2022", "2030", "2040", "2050","2060","2070"))

# f.fig.save("Figures/Reuse_Battery/World_outflows_0reuse.png")
f.fig.save("Figures/Reuse_Battery/World_outflows.png")

# All vehicles
data_fig2 <- data_fig %>% filter(Powertrain=="BEV")

data_fig2 %>%  
  ggplot(aes(Year,value,col=key,group=key))+
  geom_line(linewidth=1)+
  facet_wrap(~Vehicle,scales = "free_y")+
  labs(x="",y="Units, in millions",col="Flow",caption="Different scales per panel.")+
  coord_cartesian(xlim=c(2022,2050))+
  guides(col= guide_legend(reverse = TRUE))+
  scale_x_continuous(breaks = c(2022, 2030, 2040, 2050), 
                     labels = c("2022", "2030", "2040", "2050"))

f.fig.save("Figures/Reuse_Battery/World_outflows_vehs.png")

# stock
icct %>% 
  mutate(EV_Stock=EV_Stock/1e6) %>% 
  # filter(Vehicle=="Car") %>% 
  filter(scen_lifetime=="Baseline") %>% 
  filter(Powertrain=="BEV") %>% 
  filter(Scenario=="Ambitious") %>%
  ggplot(aes(Year,EV_Stock))+
  geom_line(linewidth=1)+
  facet_wrap(~Vehicle)+
  labs(x="",y="",title="EV stock [millions]",col="")+
  scale_x_continuous(breaks = c(2022, 2030, 2040, 2050,2060,2070), 
                     labels = c("2022", "2030", "2040", "2050","2060","2070"))
f.fig.save("Figures/Reuse_Battery/World_EVStock.png",w=8.7)

# Battery failure as percentage of stock
icct %>% 
  filter(Year<2051) %>% 
  mutate(EV_Stock=LIB_recycling/EV_Stock) %>% 
  # filter(Vehicle=="Car") %>% 
  filter(scen_lifetime=="Baseline") %>%
  # filter(scen_lifetime=="Long duration") %>% 
  filter(Powertrain=="BEV") %>% 
  filter(Scenario=="Ambitious") %>%
  ggplot(aes(Year,EV_Stock))+
  geom_line(linewidth=1)+
  facet_wrap(~Vehicle)+
  labs(x="",y="",title="Battery failure as % of EV Stock",col="")+
  scale_x_continuous(breaks = c(2022, 2030, 2040, 2050), 
                     labels = c("2022", "2030", "2040", "2050"))+
  scale_y_continuous(labels = scales::percent)
f.fig.save("Figures/Reuse_Battery/World_BatFailure.png",w=8.7)

# EoF