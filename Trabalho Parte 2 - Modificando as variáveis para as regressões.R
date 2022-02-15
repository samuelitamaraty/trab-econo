if(!require(tidylog))
  install.packages("tidylog")
library(tidylog)

if(!require(magrittr))
  install.packages("magrittr")
library(magrittr)

if(!require(readstata13))
  install.packages("readstata13")
library(readstata13)

if(!require(fastDummies))
  install.packages("fastDummies")
library(fastDummies)

if(!require(dplyr))
  install.packages("dplyr")
library(dplyr)

if(!require(psych))
  install.packages("psych")
library(psych)

if(!require(ggplot2))
  install.packages("ggplot2")
library(ggplot2)

if(!require(stargazer))
  install.packages("stargazer")
library(stargazer)

if(!require(scales))
  install.packages("scales")
library(scales)

if(!require(ggthemes))
  install.packages("ggthemes")
library(ggthemes)

if(!require(DT))
  install.packages("DT")
library(DT)

if(!require(car))
  install.packages("car")
library(car)

if(!require(broom))
  install.packages("broom")
library(broom)

if(!require(ivreg))
  install.packages("ivreg", dependencies = TRUE)
library(ivreg)

if(!require(multiwayvcov))
  install.packages("multiwayvcov")
library(multiwayvcov)

if(!require(tidyverse))
  install.packages("tidyverse")
library (tidyverse)

if(!require(lmtest))
  install.packages("lmtest")
library(lmtest)

if(!require(sandwich))
  install.packages("sandwich")
library(sandwich)

if(!require(plm))
  install.packages("plm")
library(plm)

if(!require(arsenal))
  install.packages("arsenal")
library(arsenal)

if(!require(AER))
  install.packages("AER")
library(AER)

#install.packages("magrittr")
#install.packages("readstata13")
#install.packages("fastDummies")
#install.packages("dplyr")

#library(tidylog)
#library(magrittr)
#library(readstata13)
#library(fastDummies)

#### Lendo o arquivo dta 
## mudando o diretório (caso seja necessário)
getwd()
setwd("/Users/samue/OneDrive/Documents/Graduação/4° Semestre/Econometria")
setwd("C:/Users/e-isabella.bianchi/Desktop/Microdados PNAD C")
setwd("C:/Users/bianc/One_Drive/OneDrive/UnB/Atuais/Econometria/Trabalho/Trabalho")

## Lendo a base de dados (demora um bom bucadinho)
pnadc <- read.dta13("PNAD_painel_5_rs.dta")

## Selecionando apenas as observações do SUDESTE
lCO <- c("Minas Gerais", "Espírito Santo", "Rio de Janeiro", "Sao Paulo")
lCO_ids <- c(31, 32, 33, 35)

# Diminuido a base de dados
pnadc <- pnadc[pnadc$UF %in% lCO_ids, ]
View(pnadc)

# criando coluna de data####
pnadc$Trimestre <- as.integer(pnadc$Trimestre)
pnadc$Ano <- as.integer(pnadc$Ano)
pnadc$data <- as.integer(pnadc$Trimestre*10000 + pnadc$Ano)

####criou o id do individuo####

as.integer(pnadc$UPA)

pnadc$iddom <- as.double(paste(pnadc$UPA, pnadc$V1008, pnadc$V1014, sep = ""))
pnadc$idind <- as.double(paste(pnadc$UPA, pnadc$V1008, pnadc$V1014, pnadc$V2003, sep = ""))

##Criando Variável de Educ Conjuge####

pnadc <- arrange(pnadc, iddom, Ano, data, V2005)

pnadc$educconj <- ifelse(pnadc$V2005 %in% c(2,3),lag(pnadc$VD3005), 0)
pnadc$educconj1 <- ifelse(pnadc$V2005 ==1 ,lead(pnadc$VD3005), 0)

pnadc$educconj2 <-  pnadc$educconj+pnadc$educconj1

pnadc$educconj1 <- NULL
pnadc$educconj <- NULL


pnadc$casado2 <- ifelse(lead(pnadc$V2005) %in% c(2,3),1, 0)
pnadc$casado1 <- ifelse(pnadc$V2005 %in% c(2,3),1, 0)

pnadc$casado <- pnadc$casado1 +pnadc$casado2

pnadc$casado2 <- NULL
pnadc$casado1 <- NULL 

##Criando Variável de N° de Filhos####

## ordenando o dataframe
pnadc <- arrange(pnadc, iddom, Ano, V2005)

## dummy de filho e filho na primeira infância
pnadc$filho <-  ifelse(pnadc$V2005 %in% c(4,5,6), 1, 0)
#se tem idade <= 5 e tá nas condições de filho
pnadc$filho06 <-  ifelse(pnadc$V2009 <= 6 & pnadc$V2005 %in% c(4,5,6), 1, 0)

## calculando o total de filhos
# somar filhos agrupando por iddom e data
pnadc$numfilhos <- ave(pnadc$filho, pnadc$iddom, pnadc$data, FUN = sum)
pnadc$numfilhos06 <- ave(pnadc$filho06, pnadc$iddom, pnadc$data, FUN = sum)


## deixando os valores apenas para chefes e conjugues
pnadc$numfilhos <- ifelse(pnadc$V2005 > 3, 0, pnadc$numfilhos)
pnadc$numfilhos06 <- ifelse(pnadc$V2005 > 3, 0, pnadc$numfilhos06)
## a maioria da amostra (66,5%) não possui filhos; o máximo que temos é de TREZE filhos!
table(pnadc$numfilhos)

# removendo as colunas de filhos
pnadc$filho <- NULL
pnadc$filho06 <- NULL

##Criando Variável de Raça####
pnadc <- pnadc %>%
  mutate(cor= case_when(V2010 %in% c(1,3) ~ "Brancos",
                        V2010 %in% c(2,4,5) ~ "PPI"),
         cor = as.factor(cor))

##Criando Variável de Interior ou Capital ####
pnadc <- pnadc %>%
  mutate(local= case_when(V1023 == 1 ~ "Capital",
                          V1023 == 2 ~ "RM",
                          V1023 == 3 ~ "RIDE",  
                          V1023 == 4 ~ "Interior"),
         local = as.factor(local))
pnadc <- pnadc %>%
  mutate(local = relevel(local, ref = "RM"))


##Criando Variável de Rural ou Urbana####
pnadc <- pnadc %>%
  mutate(rural= case_when(V1022 == 1 ~ "0",
                          V1022 ==  2 ~ "1"))



##Criando Variável de Condição no Domicílio####
pnadc <- pnadc %>%
  mutate(conddomi= case_when(VD2002 == 1 ~ "Responsável",
                             VD2002 ==  2 ~ "Cônjuge",
                             VD2002 %in% c(3,4,8,9) ~ "Descendetes", #Filho, Enteado, Neto e Bisneto  
                             VD2002 == 11 ~ "Avó ou Avô",
                             VD2002 %in% c(16,17)~ "Empregadx ou Parente",
                             VD2002 %in% c(5,7,10,12,13,14,15)~ "Outros",
                             VD2002 == 6~ "Pai, mãe, padrasto ou madrasta"),
         conddomi = as.factor(conddomi))
pnadc <- pnadc %>%
  mutate(conddomi = relevel(conddomi, ref = "Outros"))

##Criando Variável de Sexo####
pnadc$mulher <- ifelse(pnadc$V2007 == 2, 1,0)

#criado a variavel idade
pnadc <- pnadc %>%
  mutate(idade = V2009)

#criando a variavel idade ao quadrado
idadeaq <- (pnadc$idade)^2
pnadc$idadeaq <- idadeaq

##Criando Variável nível de educação mais alto####
pnadc <- pnadc %>%
  mutate(niveleduc= case_when(VD3004 == 1 ~ "Sem instrução e menos de 1 ano de estudo",
                              VD3004 == 2  ~ "Fundamental incompleto ou equivalente",
                              VD3004 == 3  ~ "Fundamental completo ou equivalente",
                              VD3004 == 4  ~ "Médio incompleto ou equivalente",
                              VD3004 == 5  ~ "Médio completo ou equivalente",
                              VD3004 == 6  ~ "Superior incompleto ou equivalente",
                              VD3004 == 7  ~ "Superior completo"),
         niveleduc = as.factor(niveleduc))
pnadc <- pnadc %>%
  mutate(niveleduc = relevel(niveleduc, ref = "Sem instrução e menos de 1 ano de estudo"))

pnadc <-rename(pnadc, anoseduc = VD3005)

##Criando variável Condição em relação à força de trabalho na semana de referência para pessoas de 14 anos ou mais de idade####
pnadc$forca_trabalho  <- ifelse(pnadc$VD4001 == 1,1,0)

##Criando variável Condição de ocupação na semana de referência para pessoas de 14 anos ou mais de idade####
pnadc$condicao_ocupado  <- ifelse(pnadc$VD4002 == 1,1,0)

##Criando Variável de Horas de Trabalho#####
pnadc <- pnadc %>%
  mutate(horas_trabalhadas = VD4031*4)  #Horas de Trabalho Habitualmente trabalhadas em TODOS os trabalhos na semana de ref ## multiplicado por 4 para ser horas mensais

pnadc$horas_trabalhadas = as.numeric(pnadc$horas_trabalhadas)

##Criando Variável de Log da Renda#####
pnadc <- pnadc %>%
  mutate(rendahabprin = VD4016/horas_trabalhadas,
         lrendahabprin = log(VD4016/horas_trabalhadas)) #renda mensal Habitual do trabalho principal

pnadc <- pnadc %>%
  mutate(rendaefprin = VD4017*1,
         lrendaefprin = log(VD4017))# Renda mensal Efetiva do trabalho principal
pnadc <- pnadc %>%
  mutate(rendahabtodos = VD4019*1,
         lrendahabtodos = log(VD4019)) #renda mensal Habitual de todos os trabalho 

pnadc <- pnadc %>%
  mutate(rendaeftodos = VD4020*1,
         lrendaeftodos = log(VD4020))# Renda mensal Efetiva de todos os  trabalhos 

#Transformando a variavel em numeric
pnadc$rendahabprin = as.numeric(pnadc$rendahabprin)
pnadc$lrendahabprin = as.numeric(pnadc$lrendahabprin)
pnadc$lrendahabtodos = as.numeric(pnadc$lrendahabtodos)
pnadc$rendahabtodos = as.numeric(pnadc$rendahabtodos)
pnadc$rendaefprin = as.numeric(pnadc$rendaefprin)
pnadc$lrendaefprin = as.numeric(pnadc$lrendaefprin)
pnadc$lrendaeftodos = as.numeric(pnadc$lrendaeftodos)
pnadc$rendaeftodos = as.numeric(pnadc$rendaeftodos)


## variável para casados
pnadc_casado <- pnadc  %>% filter (pnadc$casado == 1)
pnadc_casado_na <- pnadc_casado  %>%drop_na(rendahabprin)

## tirando os NAs da base 
pnadc_na <- pnadc %>% drop_na(rendahabprin)

lColunas1 <- c("V1027","V1028","V1029","posest",
               "V1008","V1014","V1022", "V1023","V2001","V2003","V2005","V2007","V2009","V2010",
               "VD2002","VD2003","VD3004","VD3006","VD4001","VD4002","VD4003",
               "VD4005","VD4008","VD4009","VD4010","VD4016","VD4017","VD4019",
               "VD4020","VD4031","VD4035","VD4036","VD4037")

pnadc <- pnadc %>% select (-lColunas1)
pnadc_na <- pnadc_na %>% select (-lColunas1)
pnadc_casado <- pnadc_casado  %>% select (-lColunas1)
pnadc_casado_na <- pnadc_casado_na %>% select (-lColunas1)

# Salvando
save.dta13(pnadc_na, file="pnadc_semNA.dta")
save.dta13(pnadc, file="pnadc_completa.dta")

save.dta13(pnadc_casado_na, file="pnadc_semNA_casado.dta")
save.dta13(pnadc_casado, file="pnadc_casado.dta")
