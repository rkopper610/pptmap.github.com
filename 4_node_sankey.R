#Sankey diagram with 4 nodes 

#-------------------------------------------------------------------------
library(sankeyD3)
library(dplyr)
library(networkD3)
library(tidyverse)
library(readxl)
library(readr)

#Read in each sheet individually which creates a Tibble for each.
#comment out either the below survey or experiement to use the other
#-------------------------------------------------------------------------

#Experiment paths

# Relationships <- read_excel("/Users/ryankopper/Clark University/Morgan Ruelle - Ryan Kopper Research/Push Pull Systematic Review/experi_4_dummy_sankey.xlsx", sheet = "Relationships")
# #Relationships <- data.frame(lapply(Relationships, as.factor))
# 
# Independent_recode <- read_excel("/Users/ryankopper/Clark University/Morgan Ruelle - Ryan Kopper Research/Push Pull Systematic Review/experi_4_dummy_sankey.xlsx", sheet = "Independent recode")
# #Independent_recode <- data.frame(lapply(Independent_recode, as.factor))
# 
# Dependent_recode <- read_excel("/Users/ryankopper/Clark University/Morgan Ruelle - Ryan Kopper Research/Push Pull Systematic Review/experi_4_dummy_sankey.xlsx", sheet = "Dependent recode")
# #Dependent_recode <- data.frame(lapply(Dependent_recode, as.factor))

#-------------------------------------------------------------------------
#Survey paths

Relationships <- read_excel("/Users/ryankopper/Clark University/Morgan Ruelle - Ryan Kopper Research/Push Pull Systematic Review/Survey_4dummy_sankey.xlsx", sheet = "Relationships")
#Relationships <- data.frame(lapply(Relationships, as.factor))

Independent_recode <- read_excel("/Users/ryankopper/Clark University/Morgan Ruelle - Ryan Kopper Research/Push Pull Systematic Review/Survey_4dummy_sankey.xlsx", sheet = "Independent recode")
#Independent_recode <- data.frame(lapply(Independent_recode, as.factor))

Dependent_recode <- read_excel("/Users/ryankopper/Clark University/Morgan Ruelle - Ryan Kopper Research/Push Pull Systematic Review/Survey_4dummy_sankey.xlsx", sheet = "Dependent recode")
#Dependent_recode <- data.frame(lapply(Dependent_recode, as.factor))


#-------------------------------------------------------------------------

distinct_rel <- left_join( Relationships, Independent_recode,
                           by = "Independent_variable") %>%
  left_join(Dependent_recode, by = "Dependent_variable") %>%
  distinct(Title, Independent_variable_recode,
           Dependent_variable_recode, Independent_broad_category,
           Dependent_broad_category )


BIV_2_BDV <- distinct_rel %>% 
  select(Title, Independent_broad_category, Dependent_broad_category) %>%
  group_by(Independent_broad_category, Dependent_broad_category) %>%
  summarise(count = n()) %>% rename(target = Dependent_broad_category,
                                    source = Independent_broad_category)

IV_2_BIV <- distinct_rel %>%
  group_by(Independent_variable_recode, Independent_broad_category) %>%
  summarise(count = n()) %>%
  rename(source = Independent_variable_recode, target = Independent_broad_category)


DV_2_BDV <- distinct_rel %>%
  group_by(Dependent_variable_recode, Dependent_broad_category) %>%
  summarise(count = n()) %>%
  rename(target = Dependent_variable_recode, source = Dependent_broad_category)





joined_long <- bind_rows(IV_2_BIV , DV_2_BDV, BIV_2_BDV)


#### SANKEY DIAGRAMS

# create nodes dataframe

IV <- unique(Independent_recode$Independent_variable_recode)
DV <- unique(Dependent_recode$Dependent_variable_recode)
BIV <- unique(Independent_recode$Independent_broad_category)
BDV <- unique(Dependent_recode$Dependent_broad_category)
total_var <- length(IV) + length(DV) + length(BIV) + length(BDV) - 1

node <- data.frame(key = 0:total_var,
                   variable = as.factor(c(as.character(IV),as.character(DV),
                                          as.character(BIV), as.character(BDV))))


#create links dataframe and rename 
links <- joined_long %>% inner_join(node,  by = c("source" = "variable")) %>% rename(source_key = key) %>% 
  inner_join(node,  by = c("target" = "variable")) %>% rename(target_key = key)


my_color <- 'd3.scaleOrdinal() .domain(["source","target"]) .range(["red", "green"])'

sankeyD3::sankeyNetwork(Links = links,
                        Nodes = node,
                        Source = 'source_key', 
                        Target = 'target_key',
                        Value = 'count',
                        NodeID = "variable", 
                        #colourScale = my_color,
                        fontSize = 12,
                        fontFamily = "sans-serif",
                        orderByPath = F,
                        showNodeValues = FALSE,
                        nodePadding = 10,
                        nodeWidth = 30,
                        align="center",
                        linkOpacity = 0.4,
                        curvature = 0.4,
                        dragY = TRUE,
                        title = NULL,
                        iterations = 0)
