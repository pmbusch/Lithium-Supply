# Load UN Data on global population
# Used mainly to dissagregate

# Libraries
source("Scripts/00-Libraries.R", encoding = "UTF-8")
source("Scripts/01-CommonVariables.R", encoding = "UTF-8")


# Load data --------------
# Heavy file - download from United Nations Database
pop <- read_excel("Data/Demand Model/WPP2022_GEN_F01_DEMOGRAPHIC_INDICATORS_COMPACT_REV1.xlsx",
                  sheet = "Estimates",range="A17:L20613")
names(pop) <- c("Index","Variant","country","Notes","loc_code","iso3","iso2","sdmx",
                "Type","Parent_code","Year","pop")
pop <- pop %>% filter(Type=="Country/Area")
pop <- pop %>% mutate(pop=as.numeric(pop)*1e3) # original comes in thousand
head(pop)

pop %>% filter(Year=="2020") %>% pull(pop) %>% sum()/1e9 # 7.8 billions
pop %>% filter(Year=="2020") %>% mutate(pop=pop/1e6) %>% arrange(desc(pop))


# EoF