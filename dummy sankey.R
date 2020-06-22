library(sankeyD3)
library(networkD3)
library(tidyverse)
library(readxl)
library(readr)
library(scales)
library(RColorBrewer)
library(paletteer)


# Column 1 à Column 2
# Gender, Age, Agricultural Knowledge à Farmer characteristics
# Conventional vs alternate intercrop, Climate-smart vs alternate intercrop à Use of alternate intercrop
# 
# Connect Columns 2 and 3 with fake data
# Farmer characteristicsà Yield
# Use of alternate intercrop à Yield
# Farmer characteristicsà Pest impacts
# Use of alternate intercrop à Pest impacts
# 
# Column 3 à Column 4
# Yield à Maize grain yield, Sorghum grain yield, Maize plant height
# Pest impacts à Stemborer damage, stemborer abundance, striga abundance


#Conventional vs alternate intercrop, Climate-smart vs alternate intercrop
#-> Use of alternate intercrop

#using network D3 package
library(networkD3)
nodes = data.frame("name" = 
                     c("Gender", #1
                       "Age", #1
                       "Agricultural Knowledge", # 1
                       "Conventional vs alternate intercrop", #1
                       "Climate-smart vs alternate intercrop", #1
                       "Farmer characteristics", # 2
                       "Use of alternate intercrop", #2
                       "Yield", #3,
                       "Pest impacts", #3
                       "Maize grain yield", #4
                       "Sorghum grain yield",  #4 
                       "Maize plant height",
                       "Stemborer damage", 
                       "stemborer abundance",
                       "striga abundance"), key = 0:14) #4

# Each row represents a link. The first number represents the node being
# conntected from.

# The second number represents the node connected to.
# The third number is the value of the node.

links = as.data.frame(matrix(c(
  0, 5, 10, #gender
  1, 5, 20, #age
  2, 5, 5,  #ag knowlwdge
  3, 6, 20, #Conventional vs alternate intercrop
  4, 6, 20, #Climate-smart vs alternate intercrop
  5, 7, 10, #farmer characteristics
  6, 7, 35, #use of alt intercrop
  5, 8, 45, 
  6, 8, 40,
  7, 9, 43,
  7,10, 43,
  7,11, 41,
  8,12, 40,
  8,13, 20,
  8,14, 20
  ),
  
  byrow = TRUE, ncol = 3))
names(links) = c("source", "target", "value")

sankeyD3::sankeyNetwork(Links = links, Nodes = nodes,
              Source = "source", Target = "target",
              Value = "value", NodeID = "name", dragY = TRUE,
              fontSize= 12, nodeWidth = 30) 


#-------------------------------------------------------------------------
library(sankeyD3)


#Read in each sheet individually which creates a Tibble for each.

Relationships <- read_excel("/Users/ryankopper/Clark University/Morgan Ruelle - Ryan Kopper Research/Push Pull Systematic Review/experi_4_dummy_sankey.xlsx", sheet = "Relationships")
#Relationships <- data.frame(lapply(Relationships, as.factor))


Independent_recode <- read_excel("/Users/ryankopper/Clark University/Morgan Ruelle - Ryan Kopper Research/Push Pull Systematic Review/experi_4_dummy_sankey.xlsx", sheet = "Independent recode")
#Independent_recode <- data.frame(lapply(Independent_recode, as.factor))

Dependent_recode <- read_excel("/Users/ryankopper/Clark University/Morgan Ruelle - Ryan Kopper Research/Push Pull Systematic Review/experi_4_dummy_sankey.xlsx", sheet = "Dependent recode")
#Dependent_recode <- data.frame(lapply(Dependent_recode, as.factor))

#------
IV_2_BIV <- left_join(Relationships, Independent_recode, by = "Independent_variable") %>% 
  select(Title, Independent_variable_recode, Independent_broad_category) %>% distinct() %>% 
group_by(Independent_variable_recode, Independent_broad_category) %>%
  summarise(count = n()) %>%
  rename(source = Independent_variable_recode, target = Independent_broad_category)

DV_2_BDV <- left_join(Relationships, Dependent_recode, by = "Dependent_variable") %>% 
  select(Title, Dependent_variable_recode, Dependent_broad_category) %>% distinct() %>% 
  group_by(Dependent_variable_recode, Dependent_broad_category) %>%
  summarise(count = n()) %>%
  rename(target = Dependent_variable_recode, source = Dependent_broad_category)


BIV_2_BDV <- left_join( Relationships, Independent_recode, by = "Independent_variable") %>%
  left_join(Dependent_recode, by = "Dependent_variable") %>%
  distinct(Title, Independent_variable_recode,
           Dependent_variable_recode, Independent_broad_category, Dependent_broad_category ) %>% 
  select(Title, Independent_broad_category, Dependent_broad_category) %>%
  group_by(Independent_broad_category, Dependent_broad_category) %>%
  summarise(count = n()) %>% rename(target = Dependent_broad_category,
                                    source = Independent_broad_category)

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


my_color <- 'd3.scaleOrdinal() .domain(["node_group","not_sig","mixed","sig"]) .range(["tan","red","dimgrey", "green"])'

sankeyD3::sankeyNetwork(Links = links,
                        Nodes = node,
                        Source = 'source_key', 
                        Target = 'target_key',
                        Value = 'count',
                        NodeID = "variable", 
                        #colourScale = my_color,
                        fontSize = 12,
                        fontFamily = "sans-serif",
                        orderByPath = TRUE,
                        showNodeValues = FALSE,
                        nodePadding = 10,
                        nodeWidth = 30,
                        align="center",
                        linkOpacity = 0.4,
                        curvature = 0.4,
                        dragY = TRUE,
                        title = NULL,
                        iterations = 0)


#-----------------------

library(networkD3)
library(dplyr)

# Make a connection data frame

links = data.frame(source= c("Gender",#1
                       "Age", "Agricultural Knowledge", # 1
                       "Conventional vs alternate intercrop", #1
                       "Climate-smart vs alternate intercrop", #1
                       "Farmer characteristics", # 2
                       "Use of alternate intercrop"), #2
                   target = c("Yield","Yield", "Yield", #3,
                       "Pest impacts", #3
                       "Maize grain yield", #4
                       "Sorghum grain yield",  #4 
                       "Maize plant height"),
                   value=c(2, 3, 2, 3, 2, 3, 2,2, 2, 3, 3, 2, 2, 1))

#original
links <- data.frame(
  source=c("Gender","Gender", "Age", "group_C", "group_E"), 
  target=c("group_D", "group_E", "group_F", "group_G", "group_H"), 
  value=c(2,3, 2, 3, 1)
)



# From these flows we need to create a node data frame: it lists every entities involved in the flow
nodes <- data.frame(
  name=c(as.character(links$source), as.character(links$target)) %>% 
    unique()
)

# With networkD3, connection must be provided using id, not using real name like in the links dataframe.. So we need to reformat it.
links$IDsource <- match(links$source, nodes$name)-1 
links$IDtarget <- match(links$target, nodes$name)-1

# prepare color scale: I give one specific color for each node.
my_color <- 'd3.scaleOrdinal() .domain(["Age", "Agricultural Knowledge","Conventional vs alternate intercrop","Climate-smart vs alternate intercrop",
"Farmer characteristics","Use of alternate intercrop","Yield","Pest impacts","Maize grain yield",
"Sorghum grain yield", "Maize plant height")]) .range(["blue", "blue" , "blue", "red", "red", "yellow", "purple", "purple,blue", "blue" , "blue", "red", "red", "yellow"])'

# Make the Network. I call my colour scale with the colourScale argument
p <- sankeyNetwork(Links = links, Nodes = nodes, Source = "IDsource", Target = "IDtarget", 
                   Value = "value", NodeID = "name", colourScale=my_color)
p
