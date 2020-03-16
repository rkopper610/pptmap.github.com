#Bibliomatrix work through

library(bibliometrix)
dat <- readFiles("/Users/ryankopper/Desktop/Exported_Items2/Exported_Items2.bib")

#without notes
dat <- readFiles("/Users/ryankopper/Desktop/Export_Item_zotero/Export_Item_zotero.bib")

datdf <- bib2df(D = dat ,dbsource = "generic")

results <- biblioAnalysis(datdf, sep = ";")
S <- summary(object = results, k = 10, pause = FALSE)

# Main Information about data
# 
# Documents                             41 
# Sources (Journals, Books, etc.)       16 
# Keywords Plus (ID)                    5 
# Author's Keywords (DE)                0 
#  Period                                Inf - -Inf 
#  Average citations per documents       NaN 
# 
#  Authors                               95 
#  Author Appearances                    219 
#  Authors of single-authored documents  0 
#  Authors of multi-authored documents   95 
#  Single-authored documents             0 
# 
#  Documents per Author                  0.432 
#  Authors per Document                  2.32 
#  Co-Authors per Documents              5.34 
#  Collaboration Index                   2.32 

plot(x = results, k = 10, pause = FALSE)




