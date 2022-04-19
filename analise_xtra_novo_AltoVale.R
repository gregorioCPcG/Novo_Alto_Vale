#analise extra

library(readxl)
tiro <- read_excel("D:/ATUALIZA_PASTA_d/analise extra novo_no Alto Vale/dados223333.xlsx")
summary(tiro)
options(scipen=999)
cor.test(tiro$rural_2010, tiro$Novo2018)
fit1 <- lm(Novo2018 ~ rural_2010, data=tiro)

tiro$predicted <- predict(fit1)   # Save the predicted values
tiro$residuals <- residuals(fit1) # Save the residual values

h <- ggplot(tiro, aes(x = rural_2010, y =Novo2018)) +
  geom_smooth(method = "lm", se = FALSE, color = "lightgrey") +        
  geom_segment(aes(xend = rural_2010, yend = predicted), alpha = .2) +      # draw line from point to line
  geom_point(aes(color = abs(residuals), size = abs(residuals))) +  # size of the points
  scale_color_continuous(low = "green", high = "red") +             # colour of the points mapped to residual size - green smaller, red larger
  guides(color = FALSE, size = FALSE) +                             # Size legend removed
  geom_point(aes(y = predicted), shape = 1) +
  theme_bw()
h

Novo <- tiro$Novo2018
Rural <- tiro$rural_2010

# incluindo mais comparadores
Milton27 <- read_excel("D:/ATUALIZA_PASTA_d/A Nova pasta/Milton27/Milton27.xlsx", 
                       col_types = c("text", "skip", "skip", 
                                     "skip", "skip", "numeric", "skip", 
                                     "skip", "skip"))
Cidade <- Milton27$Cidade
Ensino_Superior <- Milton27$superior_comp

rm(tiro, Milton27)


data <- data.frame(Novo, Rural,Ensino_Superior,Cidade)


matriz <- cor(data[1:3], method = "pearson") 
corrplot.mixed(matriz)

corrplot(matriz, method="number", 
         type="upper", order="hclust",
         diag=FALSE)

corrplot(matriz, method="shade", 
         type="upper", order="hclust",
         tl.col="black", tl.srt=45)

model <- lm(Novo ~ Ensino_Superior + Rural,
            data=data)
summary(model)
summary(data$Ensino_Superior)
summary(data$Rural)

library(ggplot2)
h = ggplot(data, aes(Novo,Ensino_Superior))
h + geom_text(label=Cidade) + facet_grid(~ Rural>0)
g <- h + geom_text(label=Cidade) + facet_grid(~ Rural>0)
h + geom_quantile() + geom_jitter() + geom_smooth(model=lm)

h2 = ggplot(data, aes(Novo,Rural))
h2 + geom_quantile() + geom_jitter() + geom_smooth(model=lm)
h2 + geom_quantile() + geom_jitter() + geom_smooth(model=lm)+facet_grid(~ Rural>0)
boxplot(data$Rural)
sd(data$Rural)
sd(data$Novo)
summary(data)
summary(data)
