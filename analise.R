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



huxreg(model8, model9, stars = c(`*` = 0.1, `**` = 0.05,
                         `***` = 0.01), statistics = c("N. obs." = "nobs", "R2" = "r.squared",
                                                       "AIC" = "AIC")) # com ou sem rio do sul

plot(novo2$n_dep, novo2$rural_2010)
cor.test(novo2$n_dep, novo2$rural_2010)


ggplot(data = novo) +
  geom_smooth(mapping = aes(x = rural_2010, y = n_dep)) + theme_bw()#salvo


#PCA
base <- subset(novo, select = c(Bolsonaro2018_turno2, Novo_fed_18,Lula_2006,log_pop_18, idh_mun, superior_comp, rural_2010))
require(psych)
cortest.bartlett(base)


KMO(base)

# analise componentes principais
#ACP-> cor = TRUE: as componentes principais serão geradas a partir da matriz de correlação.
fit<-princomp(base,cor=TRUE)
fit

summary(fit)
screeplot(fit)
plot(fit,type="lines")

PCAfit<-principal(base, nfactors=2,
                  n.obs=28,rotate="none", scores=TRUE)

PCAfitvarimax<-principal(base, nfactors=2,
                         n.obs=817,rotate="varimax",scores=TRUE)
PCAfitvarimax

PCAfitvarimax$values

PCAfitvarimax$loadings

biplot(PCAfitvarimax)

#Kmeans
dados <- base

dados<-
  dados %>%
  mutate_at(c(1:7),  ~(scale(.) %>% as.vector)) # reescala (novo objeto dados clusters)

dados.stand <- scale(dados[-1])# To standarize the variables
head(dados)

# K-Means
k.means.fit <- kmeans(dados.stand, 2) # k = 2 dois clusters
head(dados.stand)

attributes(k.means.fit)

# Centroids:
k.means.fit$centers

# Clusters:
k.means.fit$cluster
# Cluster size:
k.means.fit$size


wssplot <- function(data, nc=15, seed=1234){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")}

wssplot(dados.stand, nc=6) 
library(cluster)
clusplot(dados.stand, k.means.fit$cluster, main='2D representation of the Cluster solution',
         color=TRUE, shade=TRUE,
         labels=2, lines=0)

#table(dados[,1],k.means.fit$cluster)# deu erro

d <- dist(dados.stand, method = "euclidean") # Euclidean distance matrix.
H.fit <- hclust(d, method="ward")

plot(H.fit) # display dendogram
groups <- cutree(H.fit, k=2) # cut tree into 5 clusters
# draw dendogram with red borders around the 5 clusters
rect.hclust(H.fit, k=2, border="red")

table(dados[2,3],groups)

# técnica 2 
set.seed(155555559) ## to fix the random starting clusters
grpAntiPt <- kmeans(dados, centers=2, nstart=10)
grpAntiPt

o=order(grpAntiPt$cluster)
data.frame(novo$Cidade[o],grpAntiPt$cluster[o])


plot(dados$Novo_fed_18, dados$rural_2010, type="n", xlim=c(-3,5), ylim = c(-3,2), xlab="novo dep federal 2018", ylab="% rural 2010")
text(x=dados$Novo_fed_18, y=dados$rural_2010, labels=novo$Cidade,col=grpAntiPt$cluster+1)


# com 3
set.seed(155555559) ## to fix the random starting clusters
grpAntiPt <- kmeans(dados, centers=3, nstart=10)
grpAntiPt

o=order(grpAntiPt$cluster)
data.frame(novo$Cidade[o],grpAntiPt$cluster[o])


plot(dados$Novo_fed_18, dados$rural_2010, type="n", xlim=c(-3,5), ylim = c(-3,2), xlab="novo dep federal 2018", ylab="% rural 2010")
text(x=dados$Novo_fed_18, y=dados$rural_2010, labels=novo$Cidade,col=grpAntiPt$cluster+1)





