#novo federal por município
#setwd(D:\Nova pasta\eleicoes_2018_sc)

library(readr)
votacao_partido_munzona_2018_SC <- read_csv("votacao_partido_munzona_2018_SC.csv")

base_RSL_2014 <- banco2014%>%
  filter(NM_MUNICIPIO == "RIO DO SUL")%>%
  filter(CD_CARGO == "7")
library(tidyverse)
fed <- votacao_partido_munzona_2018_SC%>%
  filter(CD_CARGO =="6")

#
agrol <- fed%>%
  filter(CD_MUNICIPIO =="80039")

agrol <- agrol %>%
  mutate(perc= QT_VOTOS_NOMINAIS/sum(QT_VOTOS_NOMINAIS)) %>%  
  mutate(perc= round(perc*100, 2))
tibble(agrol%>%
         arrange(desc(perc))%>%select(SG_PARTIDO, perc, QT_VOTOS_NOMINAIS))

#
agron <- fed%>%
  filter(CD_MUNICIPIO =="80055")

agron <- agron %>%
  mutate(perc= QT_VOTOS_NOMINAIS/sum(QT_VOTOS_NOMINAIS)) %>%  
  mutate(perc= round(perc*100, 2))
tibble(agron%>%
         arrange(desc(perc))%>%select(SG_PARTIDO, perc, QT_VOTOS_NOMINAIS))

#
atalan <- fed%>%
  filter(CD_MUNICIPIO =="80357")

atalan <- atalan %>%
  mutate(perc= QT_VOTOS_NOMINAIS/sum(QT_VOTOS_NOMINAIS)) %>%  
  mutate(perc= round(perc*100, 2))
tibble(atalan%>%
         arrange(desc(perc))%>%select(SG_PARTIDO, perc, QT_VOTOS_NOMINAIS))

#
auror <- fed%>%
  filter(CD_MUNICIPIO =="80373")

auror<- auror %>%
  mutate(perc= QT_VOTOS_NOMINAIS/sum(QT_VOTOS_NOMINAIS)) %>%  
  mutate(perc= round(perc*100, 2))
tibble(auror%>%
         arrange(desc(perc))%>%select(SG_PARTIDO, perc, QT_VOTOS_NOMINAIS))

#
btromb <- fed%>%
  filter(CD_MUNICIPIO =="80420")

btromb<- btromb %>%
  mutate(perc= QT_VOTOS_NOMINAIS/sum(QT_VOTOS_NOMINAIS)) %>%  
  mutate(perc= round(perc*100, 2))
tibble(btromb%>%
         arrange(desc(perc))%>%select(SG_PARTIDO, perc, QT_VOTOS_NOMINAIS))
#
chaplag <- fed%>%
  filter(CD_MUNICIPIO =="81701")

chaplag<- chaplag %>%
  mutate(perc= QT_VOTOS_NOMINAIS/sum(QT_VOTOS_NOMINAIS)) %>%  
  mutate(perc= round(perc*100, 2))
tibble(chaplag%>%
         arrange(desc(perc))%>%select(SG_PARTIDO, perc, QT_VOTOS_NOMINAIS))
#
donaemma <- fed%>%
  filter(CD_MUNICIPIO =="80993")

donaemma<- donaemma %>%
  mutate(perc= QT_VOTOS_NOMINAIS/sum(QT_VOTOS_NOMINAIS)) %>%  
  mutate(perc= round(perc*100, 2))
tibble(donaemma%>%
         arrange(desc(perc))%>%select(SG_PARTIDO, perc, QT_VOTOS_NOMINAIS))
#
ibir <- fed%>%
  filter(CD_MUNICIPIO =="81353")

ibir<- ibir %>%
  mutate(perc= QT_VOTOS_NOMINAIS/sum(QT_VOTOS_NOMINAIS)) %>%  
  mutate(perc= round(perc*100, 2))
tibble(ibir%>%
         arrange(desc(perc))%>%select(SG_PARTIDO, perc, QT_VOTOS_NOMINAIS))

#
imbuia <- fed%>%
  filter(CD_MUNICIPIO =="81450")

imbuia<- imbuia %>%
  mutate(perc= QT_VOTOS_NOMINAIS/sum(QT_VOTOS_NOMINAIS)) %>%  
  mutate(perc= round(perc*100, 2))
tibble(imbuia%>%
         arrange(desc(perc))%>%select(SG_PARTIDO, perc, QT_VOTOS_NOMINAIS))
library(knitr)
library(kableExtra)

b5 <- imbuia %>% 
  dplyr::select(SG_PARTIDO, perc, QT_VOTOS_NOMINAIS) %>% 
  arrange(desc(perc))
b5 %>%
  kbl(caption = "voto dep fed 2018 por partido em Imbuia") %>%
  kable_classic(full_width = F, html_font = "Garamond")
#
itup <- fed%>%
  filter(CD_MUNICIPIO =="81671")

itup<- itup %>%
  mutate(perc= QT_VOTOS_NOMINAIS/sum(QT_VOTOS_NOMINAIS)) %>%  
  mutate(perc= round(perc*100, 2))
tibble(itup%>%
         arrange(desc(perc))%>%select(SG_PARTIDO, perc, QT_VOTOS_NOMINAIS))
#
jboite <- fed%>%
  filter(CD_MUNICIPIO =="80101")

jboite<- jboite %>%
  mutate(perc= QT_VOTOS_NOMINAIS/sum(QT_VOTOS_NOMINAIS)) %>%  
  mutate(perc= round(perc*100, 2))
tibble(jboite%>%
         arrange(desc(perc))%>%select(SG_PARTIDO, perc, QT_VOTOS_NOMINAIS))

b6 <- jboite %>% 
  dplyr::select(SG_PARTIDO, perc, QT_VOTOS_NOMINAIS) %>% 
  arrange(desc(perc))
b6 %>%
  kbl(caption = "voto dep fed 2018 por partido em José Boiteux") %>%
  kable_classic(full_width = F, html_font = "Garamond")
#
laurent <- fed%>%
  filter(CD_MUNICIPIO =="81876")

laurent<- laurent %>%
  mutate(perc= QT_VOTOS_NOMINAIS/sum(QT_VOTOS_NOMINAIS)) %>%  
  mutate(perc= round(perc*100, 2))
tibble(laurent%>%
         arrange(desc(perc))%>%select(SG_PARTIDO, perc, QT_VOTOS_NOMINAIS))
#
#Lontras
lontras <- fed%>%
  filter(CD_MUNICIPIO =="81957")

lontras<- lontras %>%
  mutate(perc= QT_VOTOS_NOMINAIS/sum(QT_VOTOS_NOMINAIS)) %>%  
  mutate(perc= round(perc*100, 2))
tibble(lontras%>%
         arrange(desc(perc))%>%select(SG_PARTIDO, perc, QT_VOTOS_NOMINAIS))
#
#mirim doce
mirimd <- fed%>%
  filter(CD_MUNICIPIO =="80365")

mirimd<- mirimd %>%
  mutate(perc= QT_VOTOS_NOMINAIS/sum(QT_VOTOS_NOMINAIS)) %>%  
  mutate(perc= round(perc*100, 2))
tibble(mirimd%>%
         arrange(desc(perc))%>%select(SG_PARTIDO, perc, QT_VOTOS_NOMINAIS))
#
#petrolândia
petrolandia <- fed%>%
  filter(CD_MUNICIPIO =="82490")

petrolandia<- petrolandia %>%
  mutate(perc= QT_VOTOS_NOMINAIS/sum(QT_VOTOS_NOMINAIS)) %>%  
  mutate(perc= round(perc*100, 2))
tibble(petrolandia%>%
         arrange(desc(perc))%>%select(SG_PARTIDO, perc, QT_VOTOS_NOMINAIS))
#
#pouso redondo

pouso <- fed%>%
  filter(CD_MUNICIPIO =="82694")

pouso<- pouso %>%
  mutate(perc= QT_VOTOS_NOMINAIS/sum(QT_VOTOS_NOMINAIS)) %>%  
  mutate(perc= round(perc*100, 2))
tibble(pouso%>%
         arrange(desc(perc))%>%select(SG_PARTIDO, perc, QT_VOTOS_NOMINAIS))
#
#pres getulio

getulio <- fed%>%
  filter(CD_MUNICIPIO =="82759")

getulio<- getulio %>%
  mutate(perc= QT_VOTOS_NOMINAIS/sum(QT_VOTOS_NOMINAIS)) %>%  
  mutate(perc= round(perc*100, 2))
tibble(getulio%>%
         arrange(desc(perc))%>%select(SG_PARTIDO, perc, QT_VOTOS_NOMINAIS))

#
#pres nereu

pnereu <- fed%>%
  filter(CD_MUNICIPIO =="82775")

pnereu<- pnereu %>%
  mutate(perc= QT_VOTOS_NOMINAIS/sum(QT_VOTOS_NOMINAIS)) %>%  
  mutate(perc= round(perc*100, 2))
tibble(pnereu%>%
         arrange(desc(perc))%>%select(SG_PARTIDO, perc, QT_VOTOS_NOMINAIS))

#
#Rio do Campo

rcampo <- fed%>%
  filter(CD_MUNICIPIO =="82856")

rcampo<- rcampo %>%
  mutate(perc= QT_VOTOS_NOMINAIS/sum(QT_VOTOS_NOMINAIS)) %>%  
  mutate(perc= round(perc*100, 2))
tibble(rcampo%>%
         arrange(desc(perc))%>%select(SG_PARTIDO, perc, QT_VOTOS_NOMINAIS))
#
#Roeste

roest<- fed%>%
  filter(CD_MUNICIPIO =="82872")

roest<- roest %>%
  mutate(perc= QT_VOTOS_NOMINAIS/sum(QT_VOTOS_NOMINAIS)) %>%  
  mutate(perc= round(perc*100, 2))
tibble(roest%>%
         arrange(desc(perc))%>%select(SG_PARTIDO, perc, QT_VOTOS_NOMINAIS))
#
#Rio do Sul

rsl<- fed%>%
  filter(CD_MUNICIPIO =="82910")

rsl<- rsl %>%
  mutate(perc= QT_VOTOS_NOMINAIS/sum(QT_VOTOS_NOMINAIS)) %>%  
  mutate(perc= round(perc*100, 2))
tibble(rsl%>%
         arrange(desc(perc))%>%select(SG_PARTIDO, perc, QT_VOTOS_NOMINAIS))

#
#Salete

salete<- fed%>%
  filter(CD_MUNICIPIO =="83011")

salete<- salete %>%
  mutate(perc= QT_VOTOS_NOMINAIS/sum(QT_VOTOS_NOMINAIS)) %>%  
  mutate(perc= round(perc*100, 2))
tibble(salete%>%
         arrange(desc(perc))%>%select(SG_PARTIDO, perc, QT_VOTOS_NOMINAIS))
#
#s.terezinha
sterez<- fed%>%
  filter(CD_MUNICIPIO =="80349")

sterez<- sterez %>%
  mutate(perc= QT_VOTOS_NOMINAIS/sum(QT_VOTOS_NOMINAIS)) %>%  
  mutate(perc= round(perc*100, 2))
tibble(sterez%>%
         arrange(desc(perc))%>%select(SG_PARTIDO, perc, QT_VOTOS_NOMINAIS))
#
#Taió
taio<- fed%>%
  filter(CD_MUNICIPIO =="83518")

taio<- taio %>%
  mutate(perc= QT_VOTOS_NOMINAIS/sum(QT_VOTOS_NOMINAIS)) %>%  
  mutate(perc= round(perc*100, 2))
tibble(taio%>%
         arrange(desc(perc))%>%select(SG_PARTIDO, perc, QT_VOTOS_NOMINAIS))
#
##Trombudo Central
tromb<- fed%>%
  filter(CD_MUNICIPIO =="83658")

tromb<- tromb %>%
  mutate(perc= QT_VOTOS_NOMINAIS/sum(QT_VOTOS_NOMINAIS)) %>%  
  mutate(perc= round(perc*100, 2))
tibble(tromb%>%
         arrange(desc(perc))%>%select(SG_PARTIDO, perc, QT_VOTOS_NOMINAIS))

#

#Vidal Ramos
vidal<- fed%>%
  filter(CD_MUNICIPIO =="83771")

vidal<- vidal %>%
  mutate(perc= QT_VOTOS_NOMINAIS/sum(QT_VOTOS_NOMINAIS)) %>%  
  mutate(perc= round(perc*100, 2))
tibble(vidal%>%
         arrange(desc(perc))%>%select(SG_PARTIDO, perc, QT_VOTOS_NOMINAIS))

#
#v.meireles
vitor<- fed%>%
  filter(CD_MUNICIPIO =="80128")

vitor<- vitor %>%
  mutate(perc= QT_VOTOS_NOMINAIS/sum(QT_VOTOS_NOMINAIS)) %>%  
  mutate(perc= round(perc*100, 2))
tibble(vitor%>%
         arrange(desc(perc))%>%select(SG_PARTIDO, perc, QT_VOTOS_NOMINAIS))


b7 <- vitor %>% 
  dplyr::select(SG_PARTIDO, perc, QT_VOTOS_NOMINAIS) %>% 
  arrange(desc(perc))
b7 %>%
  kbl(caption = "voto dep fed 2018 por partido em Vitor Meireles") %>%
  kable_classic(full_width = F, html_font = "Garamond")

#

#Witmarsum
witmarsum <- fed%>%
  filter(CD_MUNICIPIO =="83810")

witmarsum<- witmarsum %>%
  mutate(perc= QT_VOTOS_NOMINAIS/sum(QT_VOTOS_NOMINAIS)) %>%  
  mutate(perc= round(perc*100, 2))
tibble(witmarsum%>%
         arrange(desc(perc))%>%select(SG_PARTIDO, perc, QT_VOTOS_NOMINAIS))
