library(tidyverse) # manipulacoes do BD
library(betareg) # pacote da regressao beta
require(zoo) # acessa o indice das obs
require(ggplot2) # para graficos
library(qqplotr) # Graficos e envelope no ggplot2
library(plotly) # graficos interativos


data("GasolineYield", package = "betareg")
GasolineYield
names(GasolineYield)
summary(GasolineYield$yield)

GasolineYield %>%
  ggplot(aes(x = gravity, y = yield)) +
  geom_point() + 
  stat_smooth(method="lm", se=F)+
  labs(x = "Densidade do petr?leo bruto", 
       y = "Propor??o de petr?leo bruto\n convertido em gasolina")

GasolineYield %>%
  ggplot(aes(x = temp, y = yield)) +
  geom_point() + 
  stat_smooth(method="lm", se=F)+
  labs(x = "Temperatura", 
       y = "Propor??o de petr?leo bruto\n convertido em gasolina")

GasolineYield %>%
  ggplot(aes(x = temp10, y = yield)) +
  geom_point() + 
  stat_smooth(method="lm", se=F)+
  labs(x = "Temperatura para a qual 10% do \n petr?leo bruto transforma-se em vapor", 
       y = "Propor??o de petr?leo bruto\n convertido em gasolina")

GasolineYield %>%
  ggplot(aes(x = pressure, y = yield)) +
  geom_point() + 
  stat_smooth(method="lm", se=FALSE)+
  labs(x = "Press?o", 
       y = "Propor??o de petr?leo bruto\n convertido  em gasolina")

GasolineYield %>%
  ggplot(aes(x = as.factor(batch), y = yield )) +
  geom_boxplot() + 
  labs(x = "Tipos de petr?leo", 
       y = "Propor??o de petr?leo bruto \n convertido  em gasolina")



#M=cor(GasolineYield[,-6])
#corrplot.mixed(M, lower.col = "black", number.cex = .7)

########## Ajuste do modelo de regressao beta
fit1 <- betareg(yield ~ temp +batch, 
                data = GasolineYield, x=TRUE)

summary(fit1)
#para acessar o pseudo-R2
fit1$pseudo.r.squared
# para acessar o valor de AIC e BIC
AIC(fit1)
BIC(fit1)
n=length(fit1$y)
k=1+length(fit1$coefficients$mean)
BICc=-2*fit1$loglik+(n*k*log(n)/(n-k-1))
HQ=-2*fit1$loglik+(2*k*log(n))
HQc=-2*fit1$loglik+(2*k*log(log(n))/(n-k-1))

# Verificando para outros modelos os valores de AIC e BIC
fit2 <- betareg(yield ~ temp, 
                data = GasolineYield, x=TRUE)
AIC(fit2)
BIC(fit2)

# Verificando para outros modelos os valores de AIC e BIC
fit3 <- betareg(yield ~ batch, 
                data = GasolineYield, x=TRUE)
AIC(fit3)
BIC(fit3)

AIC(fit1, fit2, fit3)
BIC(fit1, fit2, fit3)

names(GasolineYield)
fit1 <- betareg(yield ~ batch+temp, 
                data = GasolineYield, x=TRUE)

fit1a <- betareg(yield ~ batch+temp, 
                 data = GasolineYield, x=TRUE,
                 link = "probit")
fit1b <- betareg(yield ~ batch+temp, 
                 data = GasolineYield, x=TRUE,
                 link = "cloglog")
fit1c <- betareg(yield ~ batch+temp, 
                 data = GasolineYield, x=TRUE,
                 link = "cauchit")
fit1d <- betareg(yield ~ batch+temp, 
                 data = GasolineYield, x=TRUE,
                 link = "loglog")

AIC(fit1, fit1a, fit1b, fit1c, fit1d)

fit1=fit1d

## Modelos com loglog passa no teste de correta 
# especificação
library(lmtest)         # Teste reset
lrtest(fit1, . ~ . + 
         I(predict(fit1, type = "link")^2))
lrtest(fit1, . ~ . + 
         I(predict(fit1, type = "link")^3))

#plot(fit1, which = 1:6) gera todos os 6 graficos em 1 comando
#por default residuo padronizado pondera tipo 2
plot(fit1, which = 1:6)

#armazendo informacoes para os graficos
?residuals.betareg
residuot2<- residuals(fit1, type= "sweighted2")
yajust<-fitted.values(fit1)
yhat=hatvalues(fit1)
dcook<- cooks.distance(fit1)

deviance<- sum(residuals(fit1, tipe= "deviance")^2)


#---indices X residuos
g_indexres=ggplot2::ggplot(GasolineYield, aes(x=index(yield),
                                              y=residuot2))+
  geom_point(size=1.5) + 
  geom_hline(yintercept=3, colour="red2", 
             size=0.5,  linetype="dashed") +
  geom_hline(yintercept=0, colour="black", 
             size=0.7, linetype="dashed") +  
  geom_hline(yintercept=-3, colour="red2", 
             size=0.5,  linetype="dashed")+
  labs(x = "Índices das observações", y = "Resíduos")
g_indexres
ggplotly(g_indexres)

#---indices X medida h, hat values
medidah=ggplot(GasolineYield, aes(x=index(yield), y=yhat))+
  geom_point(size=1.5) + 
  labs(x = "Índice das observações", y = "htt")
medidah
ggplotly(medidah)
#--- y vs residuos
yxres=ggplot(GasolineYield, aes(x=yield, y=residuot2))+
  geom_point(size=1.5) + 
  geom_hline(yintercept=3, colour="red2", 
             size=0.5,  linetype="dashed") +
  geom_hline(yintercept=0, colour="black", 
             size=0.7, linetype="dashed") +  
  geom_hline(yintercept=-3, colour="red2", 
             size=0.5,  linetype="dashed")+
  labs(x = "y", y = "Resíduos")
yxres
ggplotly(yxres)

#--- Y ajustado vs resíduos Padronizados 2
g_ajres=ggplot(GasolineYield, aes(x=yajust, y=residuot2))+
  geom_point(size=1.5) + 
  geom_hline(yintercept=3, colour="red2", size=0.5, 
             linetype="dashed") +
  geom_hline(yintercept=0, colour="black", size=0.7,
             linetype="dashed") +  
  geom_hline(yintercept=-3, colour="red2", size=0.5, 
             linetype="dashed")+
  labs(x = "y ajustado", y = "Resíduos")
g_ajres
ggplotly(g_ajres)

#--- Distancia de Cook | influencia de uma observacao em relaçao a todos os n valores
g_cook=ggplot(GasolineYield, aes(x=index(yield), 
                                 y= dcook))+
  geom_point(size=1.5)+
  labs(x = "Índices", y = "Distância de Cook")
ggplotly(g_cook)

#--- y vs y ajustado
ggplot(GasolineYield, aes(x=yield, y=yajust))+
  geom_point(size=1.5) + 
  geom_smooth(method = "lm", linetype ="dashed",
              size=0.5, col = "blue")+
  labs(x = "y observado", y = "y ajustado")

residuot2_df<- data.frame(residuot2)

## grafico envelope
ggplot(data = residuot2_df, 
       mapping = aes(sample = residuot2))+
  geom_qq_band( alpha = 0.5, fill="white", col="black") +
  
  stat_qq_line(size=0.5, linetype="dashed") + 
  stat_qq_point(size=2) +
  scale_fill_discrete("Bandtype")  +
  labs(x = "Quantis teóricos", y = "Quantis amostrais")



par(mfrow = c(1, 1))
plot(fit1, which=1) # Residuos 2 Padronizado vs indices Obs
abline(plot(fit1, which=1),
       col=c("red","blue","red"), h=c(-3,0,3)) # Residuos 2 Padronizado vs indices Obs
identify(1:n, residuals(fit1, tipe= "sweighted2"), n=2)

plot(fit1, which=2) # Dist?ncias de Cook
identify(cooks.distance(fit1), n=1)

plot(fit1, which=3) # Generalized Leverage | Alavancagem estima importancia de observa??es individuais
identify(fitted(fit1), hatvalues(fit1), n=2)

plot(fit1, which=4) # Residuos padronizados vs Y predito
abline(plot(fit1, which=4),
       col=c("red","blue","red"), h=c(-3,0,3)) # Residuos padronizados vs Y predito
plot(fit1, which=5) # Envelope

plot(fit1, which=6) # Valores Preditos vs Observados
