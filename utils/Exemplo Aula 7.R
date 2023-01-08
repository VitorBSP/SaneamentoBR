require(tidyverse)
require(betareg)

data("FoodExpenditure", package = "betareg") # obtencao dos dados
FoodExpenditure
attach(FoodExpenditure)
names(FoodExpenditure)

dados = FoodExpenditure %>%
  mutate(Proporcao=food/income)


dados %>%
  ggplot(aes(x = income, y = Proporcao)) +
  geom_point() + 
  stat_smooth(method="lm", se=F)+
  labs(x = "Nivel de renda", 
       y = "Proporcao de renda gasta com alimentacao")

dados %>%
  ggplot(aes(x = persons, y = Proporcao)) +
  geom_point() + 
  stat_smooth(method="lm", se=F)+
  labs(x = "Numero de pessoas residente em cada domicilio", 
       y = "Proporcao de renda gasta com alimentacao")

M=cor(dados)
corrplot.mixed(M, lower.col = "black", number.cex = .7)

########## Ajuste do modelo de regressao beta
fit1 <- betareg(Proporcao ~ income +food, data = dados)
summary(fit1)


# para funcao de ligacao probit
fit2 <- betareg(Proporcao ~ income + persons, data = dados, 
                link = "probit")
# para funcao de ligacao cloglog
fit3 <- betareg(Proporcao ~ income + persons, data = dados, 
                link = "cloglog")
# para funcao de ligacao cauchit
fit4 <- betareg(Proporcao ~ income + persons, data = dados, 
                link = "cloglog")

