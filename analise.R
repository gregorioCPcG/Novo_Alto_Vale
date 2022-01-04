#novo no alto vale
library(readxl)
novo <- read_excel("novo.xlsx")
novo$Bolsonaro2018_turno2 <- 100*novo$Bolsonaro2018_turno2

# variavel dep 1 - amoedo pres 2018
hist(novo$Amoedo_2018)

#variavel dep2 - voto pro novo em dep federal
hist(novo$Novo_fed_18)
# unidades 28 cidades do alto vale
table(novo$Cidade)

#indeps:

#pol - hobus18
hist(novo$Hobus18)

#pol - bolsonaro2018
hist(novo$Bolsonaro2018_turno2)
#pol -Lula 2006
hist(novo$Lula_2006)
#pol - mdb_pref2016
summary(novo$MDB_pref_16)
#demogr- %rural
hist(novo$rural_2010)
#demogr- %superior_comp
hist(novo$superior_comp)
#demogr-idh municipal
hist(novo$idh_mun)
#demogr-pop
hist(novo$log_pop_18)
# analise descritiva das duas



matriz <- cor(novo[2:11], method = "pearson") #estamos pondo as colunas que importam 2 a 4
library(corrplot)
corrplot.mixed(matriz)

corrplot(matriz, method="number", 
         type="upper", order="hclust",
         diag=FALSE)

corrplot(matriz, method="shade", 
         type="upper", order="hclust",
         tl.col="black", tl.srt=45)


#bivariadaa
#entre as dep
cor.test(novo$Novo_fed_18, novo$Amoedo_2018)
plot(novo$Novo_fed_18, novo$Amoedo_2018)
#nossa só 0.51

#bivariada #selecionar para modelo

#primeiro amoedo
#bloco pol x dep1
cor.test(novo$Amoedo_2018, novo$Hobus18)#não
cor.test(novo$Amoedo_2018, novo$Bolsonaro2018_turno2)#não
cor.test(novo$Amoedo_2018, novo$Lula_2006)#não
cor.test(novo$Amoedo_2018, novo$MDB_pref_16)#não


#bloco demog x dep1
cor.test(novo$Amoedo_2018, novo$rural_2010)#deu!
plot(novo$Amoedo_2018, novo$rural_2010)
cor.test(novo$Amoedo_2018, novo$superior_comp)#deu!
plot(novo$Amoedo_2018, novo$superior_comp)
cor.test(novo$Amoedo_2018, novo$idh_mun)#deu!
plot(novo$Amoedo_2018, novo$idh_mun)
cor.test(novo$Amoedo_2018, novo$log_pop_18)#não

library(huxtable)

#reglinear dep1 blocopol
#nada
#reglinear dep1 blocodemog
model1 <- lm(Amoedo_2018 ~ rural_2010 + superior_comp + idh_mun, data = novo)
summary(model1)
huxreg(model1, stars = c(`'` = 0.2,`*` = 0.1, `**` = 0.05,
                                         `***` = 0.01), statistics = c("N. obs." = "nobs", "R2" = "r.squared",
                                                                       "AIC" = "AIC"))
#reglinear dep1 full
#NA

#bivariada #selecionar para modelo

#bloco pol x dep2
cor.test(novo$Novo_fed_18, novo$Hobus18)#sim
plot(novo$Novo_fed_18, novo$Hobus18)
cor.test(novo$Novo_fed_18, novo$Bolsonaro2018_turno2)#sim
plot(novo$Novo_fed_18, novo$Bolsonaro2018_turno2)
cor.test(novo$Novo_fed_18, novo$Lula_2006)#nao
cor.test(novo$Novo_fed_18, novo$MDB_pref_16)#não
#bloco demog x dep2
cor.test(novo$Novo_fed_18, novo$rural_2010)#deu!
plot(novo$Novo_fed_18, novo$rural_2010)
cor.test(novo$Novo_fed_18, novo$superior_comp)#deu!
plot(novo$Novo_fed_18, novo$superior_comp)
cor.test(novo$Novo_fed_18, novo$idh_mun)#deu!
plot(novo$Novo_fed_18, novo$idh_mun)
cor.test(novo$Novo_fed_18, novo$log_pop_18)#não
plot(novo$Novo_fed_18, novo$log_pop_18)
#reglinear dep2 blocopol
model2 <- lm(Novo_fed_18 ~ Hobus18 + Bolsonaro2018_turno2, data = novo)
summary(model2)
#reglinear dep2 blocodemog
model3 <- lm(Novo_fed_18 ~ rural_2010+superior_comp+idh_mun+log_pop_18, data = novo)
summary(model3)
huxreg(model3, stars = c(`'` = 0.2,`*` = 0.1, `**` = 0.05,
                         `***` = 0.01), statistics = c("N. obs." = "nobs", "R2" = "r.squared",
                                                       "AIC" = "AIC"))
#reglinear dep2 full
model4 <- lm(Novo_fed_18 ~ rural_2010+superior_comp+idh_mun+log_pop_18+Hobus18+Bolsonaro2018_turno2, data = novo)
summary(model4)
huxreg(model2, model3, model4, stars = c(`*` = 0.1, `**` = 0.05,
                         `***` = 0.01), statistics = c("N. obs." = "nobs", "R2" = "r.squared",
                                                       "AIC" = "AIC"))
# extra - comparar muncipio com  bra e sc
#AMOEDO 18 SC - 4,01
novo$amoedo_sc <- novo$Amoedo_2018-4.01
summary(novo$amoedo_sc)
boxplot(novo$amoedo_sc)
table(novo$amoedo_sc)
#AMOEDO 18 BRA - 2,50
novo$amoedo_br <- novo$Amoedo_2018-2.5
summary(novo$amoedo_br)
boxplot(novo$amoedo_br)
table(novo$amoedo_br)
#novo deputado federal SC - 5,28
novo$novo_sc <- novo$Novo_fed_18-5.28
summary(novo$novo_sc)
boxplot(novo$novo_sc)
table(novo$novo_sc)
#novo deputado federal BRA - 2,79
novo$novo_br <- novo$Novo_fed_18-2.79
summary(novo$novo_br)
boxplot(novo$novo_br)
table(novo$novo_br)

#nova variável soma das distâncias das médias
novo$n_dep <- novo$novo_br + novo$novo_sc + novo$amoedo_br + novo$amoedo_sc
summary(novo$n_dep)

boxplot(novo$n_dep)# remover rio do sul

library(tidyverse)
library(knitr)
library(kableExtra)

b5 <- novo %>% 
  dplyr::select(Cidade, n_dep) %>% 
  arrange(desc(n_dep))
b5 %>%
  kbl(caption = "") %>%
  kable_classic(full_width = F, html_font = "Garamond")

novo2 <- novo[-c(21),] # rm Rio do SUl

summary(novo2$novo_br)
b5 <- novo2 %>% 
  dplyr::select(Cidade, novo_br) %>% 
  arrange(desc(novo_br))
b5 %>%
  kbl(caption = "") %>%
  kable_classic(full_width = F, html_font = "Garamond")


#reglinear dep2 blocopol
model5 <- lm(Novo_fed_18 ~ Hobus18 + Bolsonaro2018_turno2, data = novo2)
summary(model5)
#reglinear dep2 blocodemog
model6 <- lm(Novo_fed_18 ~ rural_2010+superior_comp+idh_mun+log_pop_18, data = novo2)
summary(model6)

#reglinear dep2 full
model7 <- lm(Novo_fed_18 ~ rural_2010+superior_comp+idh_mun+log_pop_18+Hobus18+Bolsonaro2018_turno2, data = novo2)
summary(model7)
huxreg(model5, model6, model7, stars = c(`*` = 0.1, `**` = 0.05,
                                         `***` = 0.01), statistics = c("N. obs." = "nobs", "R2" = "r.squared",
                                                                       "AIC" = "AIC"))


model8 <- lm(n_dep ~ rural_2010+superior_comp+idh_mun+log_pop_18+Hobus18+Bolsonaro2018_turno2, data = novo)
summary(model8)
huxreg(model8, stars = c(`*` = 0.1, `**` = 0.05,
                                         `***` = 0.01), statistics = c("N. obs." = "nobs", "R2" = "r.squared",
                                                                       "AIC" = "AIC"))

plot(novo2$Novo_fed_18, novo2$Amoedo_2018)
cor.test(novo2$Novo_fed_18, novo2$Amoedo_2018)

model9 <- lm(n_dep ~ rural_2010+superior_comp+idh_mun+log_pop_18+Hobus18+Bolsonaro2018_turno2, data = novo2)
summary(model9)
huxreg(model9, stars = c(`*` = 0.1, `**` = 0.05,
                         `***` = 0.01), statistics = c("N. obs." = "nobs", "R2" = "r.squared",
                                                       "AIC" = "AIC"))

##nova variável soma das distâncias das médias -> novo$n_dep
#model8 <- lm(n_dep ~ rural_2010+superior_comp+idh_mun+log_pop_18+Hobus18+Bolsonaro2018_turno2
# sem rio do sul -> model9 <- lm(n_dep ~ rural_2010+superior_comp+idh_mun+log_pop_18+Hobus18+Bolsonaro2018_turno2, data = novo2
# 

huxreg(model8, model9, stars = c(`*` = 0.1, `**` = 0.05,
                         `***` = 0.01), statistics = c("N. obs." = "nobs", "R2" = "r.squared",
                                                       "AIC" = "AIC")) # com ou sem rio do sul

plot(novo2$n_dep, novo2$rural_2010)
cor.test(novo2$n_dep, novo2$rural_2010)

ggplot(data = novo) +
  geom_smooth(mapping = aes(x = Hobus18, y = n_dep)) + theme_bw()#salvo

ggplot(data = novo) +
  geom_smooth(mapping = aes(x = idh_mun, y = n_dep)) + theme_bw()#salvo

ggplot(data = novo2) +
  geom_smooth(mapping = aes(x = rural_2010, y = n_dep)) + theme_bw()#salvo



model10 <- lm(Novo_fed_18 ~ rural_2010+superior_comp+idh_mun+log_pop_18+Bolsonaro2018_turno2, data = novo)
summary(model10)
model11 <- lm(Novo_fed_18 ~ rural_2010+superior_comp+idh_mun+log_pop_18+Bolsonaro2018_turno2, data = novo2)
summary(model11)
model12 <- lm(Amoedo_2018 ~ rural_2010+superior_comp+idh_mun+log_pop_18+Bolsonaro2018_turno2, data = novo)
summary(model12)
model13 <- lm(Amoedo_2018 ~ rural_2010+superior_comp+idh_mun+log_pop_18+Bolsonaro2018_turno2, data = novo2)
summary(model13)

library(caret)
epa32 <- preProcess(novo[,c(16)], method = c("range"))
norm2 <- predict(epa32,novo[,c(16)])
summary(norm2)
epa33 <- preProcess(novo2[,c(16)], method = c("range"))
norm3 <- predict(epa33,novo2[,c(16)])
summary(norm3)

novo$indicador <- norm2$n_dep
novo2$indicador <- norm3$n_dep

model14 <- lm(indicador ~ rural_2010+superior_comp+idh_mun+log_pop_18+Bolsonaro2018_turno2, data = novo)
summary(model14)
model15 <- lm(indicador ~ rural_2010+superior_comp+idh_mun+log_pop_18+Bolsonaro2018_turno2, data = novo2)
summary(model15)

huxreg(model14, model15, stars = c(`*` = 0.1, `**` = 0.05,
                                 `***` = 0.01), statistics = c("N. obs." = "nobs", "R2" = "r.squared")) # com ou sem rio do sul

novo$Novo2018 <- novo$indicador
novo2$Novo2018 <- novo2$indicador


ggplot(data = novo) +
  geom_smooth(mapping = aes(x = idh_mun, y = indicador)) + theme_bw()#salvo
ggplot(data = novo) +
  geom_point(mapping = aes(x = idh_mun, y = Novo2018)) + theme_bw()#salvo
j <- ggplot(novo, aes(idh_mun, Novo2018))
j + geom_text(aes(label = Cidade))



ggplot(data = novo2) +
  geom_smooth(mapping = aes(x = rural_2010, y = indicador)) + theme_bw()#salvo
ggplot(data = novo2) +
  geom_point(mapping = aes(x = rural_2010, y = Novo2018)) + theme_bw()#salvo



