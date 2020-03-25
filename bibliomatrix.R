#Bibliomatrix work through

library(bibliometrix)
dat <- readFiles("/Users/ryankopper/Desktop/R work/biblio_R/combined_ex_surv_4R/combined_ex_surv_4R.bib")

datdf <- bib2df(D = dat ,dbsource = "generic")

results <- biblioAnalysis(datdf, sep = ";")
S <- summary(object = results, k = 10, pause = FALSE)

# Main Information about data
# 
# Documents                             42 
# Sources (Journals, Books, etc.)       0 
# Keywords Plus (ID)                    5 
# Author's Keywords (DE)                0 
#  Period                                Inf - -Inf 
#  Average citations per documents       NaN 
# 
#  Authors                               95 
#  Author Appearances                    224 
#  Authors of single-authored documents  0 
#  Authors of multi-authored documents   95 
#  Single-authored documents             0 
# 
#  Documents per Author                  0.442 
#  Authors per Document                  2.26 
#  Co-Authors per Documents              5.33 
#  Collaboration Index                   2.26 

plot(x = results, k = 10, pause = FALSE)

#dominance
# It calculates the authors’ dominance ranking from an object of the
# class ’bibliometrix’  as pro- posed

x <- dominance(results, k = 10)

plot(y = x$`Rank by DF`, x = x$Author) 


#It estimates Lotka’s law coefficients for scientific productivity
L=lotka(results)

#Returns "Authors’ Productivity frequency table"
#--------------

NetMatrix <- biblioNetwork(results, analysis = "collaboration",
                           network = "authors", sep = ";")
#networkPlot
net <- networkPlot(NetMatrix, n = 30, type = "kamada",
                   Title = "Collaboration",labelsize=0.5)
#-------

NetMatrix <- biblioNetwork(results, analysis = "coupling", network = "uthors", sep = ";")

net=networkPlot(NetMatrix,  normalize = "salton", weighted=NULL, n = 100, Title = "Authors' Coupling",
                type = "fruchterman", size=5,size.cex=T,remove.multiple=TRUE,labelsize=0.8,label.n=10,label.cex=F)


