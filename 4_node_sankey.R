#Sankey diagram with 4 nodes 

#-------------------------------------------------------------------------
library(sankeyD3)
library(dplyr)
library(networkD3)
library(tidyverse)
library(readxl)
library(readr)

#Read in each sheet individually which creates a Tibble for each.
#comment out either the below survey or experiment to use the other
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
# Set working directory for Ryan
setwd("/Users/ryankopper/Clark University/Morgan Ruelle - Ryan Kopper Research/Push Pull Systematic Review/")

# Set working directory for Morgan
#setwd("C:/Users/Morgan/OneDrive - Clark University/Ryan Kopper Research/Push Pull Systematic Review/")

Relationships <- read_excel("Survey_4dummy_sankey.xlsx", sheet = "Relationships")
#Relationships <- data.frame(lapply(Relationships, as.factor))

Independent_recode <- read_excel("Survey_4dummy_sankey.xlsx", sheet = "Independent recode")
#Independent_recode <- data.frame(lapply(Independent_recode, as.factor))

Dependent_recode <- read_excel("Survey_4dummy_sankey.xlsx", sheet = "Dependent recode")
#Dependent_recode <- data.frame(lapply(Dependent_recode, as.factor))


#-------------------------------------------------------------------------

distinct_rel <- left_join( Relationships, Independent_recode,
                           by = "Independent_variable") %>%
  left_join(Dependent_recode, by = "Dependent_variable") %>%
  distinct(Title, Independent_variable_recode,
           Dependent_variable_recode, Independent_broad_category,
           Dependent_broad_category)

# Adding significance the distinct relations
distinct_rel_sig <- Relationships %>% 
  left_join(Independent_recode, by = "Independent_variable") %>%
  left_join(Dependent_recode, by = "Dependent_variable") %>%
  select(Independent_variable_recode, Dependent_variable_recode, Relationship) %>% 
  group_by(Independent_variable_recode, Dependent_variable_recode) %>%
  summarise(sig = sum(Relationship=="significant"),
            not_sig = sum(Relationship=="not significant"),
            mixed = sum(Relationship=="mixed"),
            total = n()) %>%  
  mutate(significance = if_else(sig==total,"sig",
                                if_else(not_sig==total,"not_sig","mixed"))) %>%
  full_join(distinct_rel) %>% 
  select(Title,Independent_variable_recode,Independent_broad_category,
         Dependent_variable_recode,Dependent_broad_category,significance)
  
BIV_2_BDV <- distinct_rel_sig %>%
  select(Title, Independent_broad_category, Dependent_broad_category,significance) %>%
  group_by(Independent_broad_category, Dependent_broad_category,significance) %>%
  summarise(count = n()) %>% rename(target = Dependent_broad_category,
                                    source = Independent_broad_category,
                                    link_group = significance)

IV_2_BIV <- distinct_rel %>%
  group_by(Independent_variable_recode, Independent_broad_category) %>%
  summarise(count = n()) %>%
  rename(source = Independent_variable_recode, target = Independent_broad_category) %>% 
  mutate(link_group="subset")


DV_2_BDV <- distinct_rel %>%
  group_by(Dependent_variable_recode, Dependent_broad_category) %>%
  summarise(count = n()) %>%
  rename(target = Dependent_variable_recode, source = Dependent_broad_category) %>% 
  mutate(link_group="subset")

joined_long <- bind_rows(IV_2_BIV , DV_2_BDV, BIV_2_BDV)


#### SANKEY DIAGRAMS

# create nodes dataframe

IV <- unique(Independent_recode$Independent_variable_recode)
DV <- unique(Dependent_recode$Dependent_variable_recode)
BIV <- unique(Independent_recode$Independent_broad_category)
BDV <- unique(Dependent_recode$Dependent_broad_category)
total_var <- length(IV) + length(DV) + length(BIV) + length(BDV) - 1

nodes <- data.frame(key = 0:total_var,
                   variable = as.factor(c(as.character(IV),as.character(DV),
                                          as.character(BIV), as.character(BDV)))) %>% 
  mutate(node_group="all")


#create links dataframe and rename 
links <- joined_long %>%
  inner_join(nodes,  by = c("source" = "variable"))%>%
  rename(source_key = key) %>% 
  inner_join(nodes,  by = c("target" = "variable")) %>%
  rename(target_key = key) %>% 
  select(source,target,source_key,target_key,count,link_group)


my_color <- 'd3.scaleOrdinal() .domain(["all","mixed","not_sig","sig","subset"]) .range(["tan", "yellow" , "red", "darkgreen","lightgrey"])'

sankeyD3::sankeyNetwork(Links = links,
                        Nodes = nodes,
                        Source = 'source_key', 
                        Target = 'target_key',
                        Value = 'count',
                        NodeID = "variable",
                        LinkGroup = "link_group",
                        NodeGroup = "node_group",
                        colourScale = my_color,
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