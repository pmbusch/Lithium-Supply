# Electricty Generation data. Used to dissagregate SPS (grid storage) to country level
# Source: https://ourworldindata.org/grapher/electricity-generation
# PBH Sept. 2023

# Code is run from other script, so this Scripts are already loaded. 
# source("Scripts/00-Libraries.R", encoding = "UTF-8")
# source("Scripts/01-CommonVariables.R", encoding = "UTF-8")

gen <- read_excel("Data/Demand Model/ElectricityGeneration.xlsx",
                  sheet="Generation")
(names(gen) <- c("Gen_Country","code","Year","electGen_TWh"))
gen <- gen %>% filter(Year==2022)

# names eqs
eqs <- read_excel("Data/Demand Model/ElectricityGeneration.xlsx",
                   sheet="Eqs")

# Merge names and generate dissagregation
gen <- gen %>% left_join(eqs)

# Generate share per benchmark region
gen <- gen %>% 
  filter(ICCT_Country!="Region") %>% 
  group_by(Benchmark_Region,ICCT_Region,ICCT_Country) %>% 
  reframe(electGen_TWh=sum(electGen_TWh)) %>% ungroup() %>% 
  group_by(Benchmark_Region) %>% 
  mutate(share_gen=electGen_TWh/sum(electGen_TWh)) %>% ungroup()
gen %>% group_by(Benchmark_Region) %>% reframe(sum(share_gen))

rm(eqs)


# EoF