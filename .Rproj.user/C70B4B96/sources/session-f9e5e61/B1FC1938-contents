data = readxl::read_xlsx('./data/data.xlsx')
library(magrittr)
library(dplyr)

data1 = data %>% 
  select(`Territorialidades`,`% dos ocupados com ensino fundamental completo 2010`,
         `% dos ocupados com ensino médio completo 2010`,
         `% dos ocupados com ensino superior completo 2010`,
         `IDEB anos iniciais do ensino fundamental 2015`,
         `IDEB anos finais do ensino fundamental 2015`,
         `% de alunos do ensino médio em escolas com internet 2016`,
         `% de docentes na rede privada do fundamental com formação adequada 2016`,
         `Produto Interno Bruto per capita 2016`,
         `Participação da Agropecuária no Valor Adicionado 2016`,
         `Participação da Indústria no Valor Adicionado 2016`,
         `% de nascidos vivos com pelo menos sete consultas de pré-natal 2016`,
         `% de nascidos vivos com baixo peso ao nascer 2016`,
         `% de cobertura vegetal natural 2016`,
         `% da população urbana residente em domicílios ligados à rede de esgotamento sanitário 2017`,
         `Esperança de vida ao nascer 2016`,
         `Mortalidade infantil 2016`,
         `Taxa de analfabetismo - 15 anos ou mais de idade 2016`,
         `% de pobres 2016`,
         `Índice de Gini 2016`)                                                                       

write.csv(data1, "data_modificado.csv")
