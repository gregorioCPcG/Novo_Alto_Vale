#latente 
library(lavaan)
library(dplyr)
library(ggplot2)
library(semPlot)
library(readxl)
novo <- read_excel("novo.xlsx")


complete <- function(...) {
  study <- novo %>%
    select(...)
  
  return(study[complete.cases(study),])
}

wvscomplete <- complete(Bolsonaro2018_turno2, Hobus18, Novo_fed_18, Amoedo_2018,Lula_2006,log_pop_18,
                        idh_mun,superior_comp,rural_2010,MDB_pref_16)

nfactors(novo[,2:11], rotate = "varimax")

pf<- fa(novo[,2:11],nfactors = 2, rotate = "varimax")

summary(pf)

#View(pf)

print(pf$loadings,cutoff = 0.001)


h1.1 <- 'novo =~ Novo_fed_18 + Amoedo_2018
novo ~ Hobus18 +Lula_2006+log_pop_18+ idh_mun+superior_comp+rural_2010+MDB_pref_16'


testeh1.1 <- sem(h1.1, data = wvscomplete, orthogonal = TRUE)
semPaths(testeh1.1, what = "paths", whatLabels = "stand",
         rotation = 1)

summary(testeh1.1, standardized = TRUE, fit.measures = TRUE)
fitmeasures(testeh1.1, c("chisq", "df", "pvalue", "cfi","tli", "rmsea","SRMR"))

