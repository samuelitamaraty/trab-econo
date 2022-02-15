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

#if(!require(broom))
  #install.packages("broom")
#library(broom)

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

if(!require(censReg))
  install.packages("censReg")
library(censReg)

if(!require(margins))
  install.packages("margins")
library(margins)

if(!require(sampleSelection))
  install.packages("sampleSelection")
library(sampleSelection)

if(!require(mfx))
  install.packages("mfx")
library(mfx)


getwd()
setwd("/Users/samue/OneDrive/Documents/Graduação/4° Semestre/Econometria")

pnadc_semNA<- read.dta13("pnadc_semNA.dta")
pnadc_semNA_casado<- read.dta13("pnadc_semNA_casado.dta")
pnadc_completa<- read.dta13("pnadc_completa.dta")

# QUESTÃO 2
#a) regressoes
### REG1 ####
summary(reg1_anos_educ <- lm(lrendahabprin ~ anoseduc + idade + idadeaq, data = pnadc_semNA))

summary(reg1_nivel_educ <- lm(lrendahabprin ~ niveleduc + idade + idadeaq, data = pnadc_semNA))


stargazer(reg1_anos_educ, reg1_nivel_educ, type = "html")

### REG2 ####

summary(reg2_anos_educ <- lm(lrendahabprin ~ anoseduc + idade + idadeaq + cor + mulher + horas_trabalhadas , data = pnadc_semNA))

summary(reg2_nivel_educ <- lm(lrendahabprin ~ niveleduc + idade + idadeaq + cor + mulher + horas_trabalhadas, data = pnadc_semNA))

summary(reg2_nivel_educ_inter <- lm(lrendahabprin ~ niveleduc + idade + idadeaq + cor + mulher + horas_trabalhadas + niveleduc*horas_trabalhadas, data = pnadc_semNA))

stargazer(reg2_anos_educ, reg2_nivel_educ, reg2_nivel_educ_inter, type = "html")

### REG3 ####

summary(reg3_anos_educ <- lm(lrendahabprin ~ anoseduc + idade + idadeaq + cor + mulher + horas_trabalhadas + casado + local + rural, data = pnadc_semNA))

summary(reg3_nivel_educ <- lm(lrendahabprin ~ niveleduc + idade + idadeaq + cor + mulher + horas_trabalhadas + casado + local + rural, data = pnadc_semNA))

summary(reg3_nivel_educ_inter <- lm(lrendahabprin ~ niveleduc + idade + idadeaq + cor + mulher + horas_trabalhadas + niveleduc*horas_trabalhadas + casado + local + rural, data = pnadc_semNA))


stargazer(reg3_anos_educ, reg3_nivel_educ, reg3_nivel_educ_inter, type = "html")

### REG4 ####

summary(reg4_anos_educ <- lm(lrendahabprin ~ anoseduc + idade + idadeaq + cor + mulher + horas_trabalhadas + casado + local + rural + numfilhos06, data = pnadc_semNA))

summary(reg4_nivel_educ <- lm(lrendahabprin ~ niveleduc + idade + idadeaq + cor + mulher + horas_trabalhadas + casado + local + rural + numfilhos06, data = pnadc_semNA))

summary(reg4_nivel_educ_inter <- lm(lrendahabprin ~ niveleduc + idade + idadeaq + cor + mulher + horas_trabalhadas + niveleduc*horas_trabalhadas + casado + local + rural + numfilhos06, data = pnadc_semNA))

summary(reg4_nivel_educ_2inter <- lm(lrendahabprin ~ niveleduc + idade + idadeaq + cor + mulher + horas_trabalhadas + niveleduc*horas_trabalhadas + casado + local + rural + numfilhos06 + niveleduc*numfilhos06, data = pnadc_semNA))


stargazer(reg4_anos_educ, reg4_nivel_educ, reg4_nivel_educ_inter, reg4_nivel_educ_2inter, type = "html")


#b) teste de especificacao####
### realizando um teste RESET ####

resettest(reg4_nivel_educ_2inter, power = 2, type = "fitted", data = pnadc_semNA) # p-valor baixo, então foi rejeitado. Tem algum problema na forma funcional, então, falta alguma variável na regressão.

#c)teste para endogeneidade da variável educação ####

#Testa a exogeneidade da variável, ao testar se existe correlação entre o erro e a váriavel.
#A hipótese nula é que a covariância entre a variável e o erro é 0. Por isso, ao rejeitar a hipótese nula,
#mostra-se endogeneidade e a necessidade de variáveis instrumentais.



summary(reg_anos_educ <-ivreg(lrendahabprin ~ anoseduc + idade + idadeaq + cor + mulher + horas_trabalhadas + local + rural 
                               | educconj2 + idade + idadeaq + cor + mulher + horas_trabalhadas + local + rural , data = pnadc_semNA_casado))

#d) ####

summary(reg5_anos_educ <- lm(lrendahabprin ~ anoseduc + idade + idadeaq + cor + mulher + horas_trabalhadas + local + rural , data = pnadc_semNA_casado))

stargazer(reg5_anos_educ, reg_anos_educ, type = "html")

# e) ####
#Para o teste de restrições sobreidentificadoras (teste de Sargan) é necessário mais de uma VI, 
#pois testa a validade da variável instrumental, ao testar se existe correlação entre o erro e a variável instrumental.
#Esse teste somente é válido para instrumentos extras.  
#A hipótese nula é que a covariância entre a variável instrumental e o erro é 0.
#Por isso, ao rejeitar a hipótese nula, mostra-se que ao menos um dos instrumentos não é válido.

summary(reg_anos_educ2 <-ivreg(lrendahabprin ~ anoseduc + idade + idadeaq + cor + mulher + horas_trabalhadas + local + rural 
                               | educconj2 + numfilhos06 + idade + idadeaq + cor + mulher + horas_trabalhadas + local + rural , data = pnadc_semNA_casado))


#Questão 3 ####

pnadc_semNA$d15 <- ifelse(pnadc_semNA$data %in% c(12015,22015,32015,42015),1, 0)
pnadc_semNA$d16 <- ifelse(pnadc_semNA$data %in% c(12016,22016,32016,42016),1, 0)
pnadc_semNA$d17 <- ifelse(pnadc_semNA$data %in% c(12017,22017,32017,42017),1, 0)

#a)####
#feito no stata


#b) So ficam nas regressão as variaveis que variam no tempo ####
#efeitos fixos
summary(reg_painel_fixo <- plm(lrendahabprin ~ anoseduc + idade + idadeaq + cor + casado + mulher + horas_trabalhadas +
                                  rural + d16 + d17,
                          data = pnadc_semNA, index = c("idind","data"), model = "within"))

#efeitos aleatorios
summary(reg_painel_aleatorio <- plm(lrendahabprin ~ anoseduc + idade + idadeaq + cor + casado + mulher + horas_trabalhadas +
                                   rural + d16 + d17,
                          data = pnadc_semNA, index = c("idind","data"), model = "random"))


#teste de hausman
phtest(reg_painel_fixo, data = pnadc_semNA, model = c("within", "random"),index = c("idind", "data"),reg_painel_aleatorio )

#a hipotese nula foi rejeitada
##A hipotése nula é de que os dois modelos são consitentes e tanto faz usá-los. Como o p-valor do teste de Hausman é menot 
#do que 0,05 rejeitamos a hipótese nula, o que significa que um dos modelos é inconsistente.


#é possível ver que a educação pode estar correlacionada com aptidão não observada, pois o teste de hausman foi rejeitada a hipotese nula.
#então, mostra um sinal de endogeniedade

#c)####
#primeiras diferenças
summary(reg_painel_pd <- plm(lrendahabprin ~ anoseduc + idade + idadeaq + cor + casado + mulher + horas_trabalhadas +
                               rural + d16 + d17,
                                    data = pnadc_semNA, index = c("idind","data"), model = "fd", effect = "individual"))

#Breusch-Godfrey teste para autocorrelação dos resíduos
bgtest(reg_painel_pd, order=3, data=pnadc_semNA)

# a hipotese nula tbm foi rejeitada.
#Hipótese nula é que não tem autocorrelação,
##se vc rejeitar h0, os resíduos seguem um passeio aleatório e é preferível pd a ef

#d)####

#criando dummie para cor
pnadc_semNA$cor01 <- ifelse(pnadc_semNA$cor == "PPI",1, 0)

#Fazendo a Média das Variáveis que Variam (anoseduc, idade, idadesq, sexo, cor)
pnadc_semNA <- pnadc_semNA%>%
  group_by(idind)%>%
  mutate (med_anoseduc= mean(anoseduc),
          med_idade = mean(idade),
          med_idadeaq = mean(idadeaq),
          med_casado = mean(casado),
          med_numfilhos06 = mean(numfilhos06),
          med_mulher = mean(mulher),
          med_cor01 = mean (cor01),
          med_horas_trabalhadas = mean(horas_trabalhadas))

#Filtrando os indivíduos com educ fixo ao longo do tempo e add as medias das variáveis na regressão EA
##(Efeitos Aleatórios Correlacionados)

pnadc_semNA_filtrado <- pnadc_semNA%>%
  filter(anoseduc == med_anoseduc)

#efeitos aleatorios
summary(reg_painel_aleatorio_correlacionado <- plm(lrendahabprin ~ anoseduc + idade + idadeaq + cor01 + casado + mulher + horas_trabalhadas +
                                      rural + d16 + d17 + med_idade + med_idadeaq + med_casado +
                                      med_mulher + med_cor01 + med_horas_trabalhadas,
                                    data = pnadc_semNA_filtrado, index = c("idind","data"), model = "random"))

## retorno grande da educação 11%.


## e) ####
pnadc_semNA_casado$d15 <- ifelse(pnadc_semNA_casado$data %in% c(12015,22015,32015,42015),1, 0)
pnadc_semNA_casado$d16 <- ifelse(pnadc_semNA_casado$data %in% c(12016,22016,32016,42016),1, 0)
pnadc_semNA_casado$d17 <- ifelse(pnadc_semNA_casado$data %in% c(12017,22017,32017,42017),1, 0)

pnadc_semNA_casado$cor01 <- ifelse(pnadc_semNA_casado$cor == "PPI",1, 0)

pnadc_semNA_casado <- pnadc_semNA_casado%>%
  group_by(idind)%>%
  mutate (med_anoseduc= mean(anoseduc),
          med_idade = mean(idade),
          med_idadeaq = mean(idadeaq),
          med_casado = mean(casado),
          med_numfilhos06 = mean(numfilhos06),
          med_mulher = mean(mulher),
          med_cor01 = mean (cor01),
          med_horas_trabalhadas = mean(horas_trabalhadas))

#efeitos fixo
summary(reg_painel_fixo <- plm(lrendahabprin ~ anoseduc + idade + idadeaq + cor + mulher + horas_trabalhadas +
                                 rural + d16 + d17 + numfilhos06,
                               data = pnadc_semNA_casado, index = c("idind","data"), model = "within"))

#efeitos aleatorios
summary(reg_painel_aleatorio <- plm(lrendahabprin ~ anoseduc + idade + idadeaq + cor + mulher + horas_trabalhadas +
                                      rural + d16 + d17 + numfilhos06,
                                    data = pnadc_semNA_casado, index = c("idind","data"), model = "random"))

#primeiras diferenças
summary(reg_painel_pd <- plm(lrendahabprin ~ anoseduc + idade + idadeaq + cor + mulher + horas_trabalhadas +
                               rural + d16 + d17 + numfilhos06,
                             data = pnadc_semNA_casado, index = c("idind","data"), model = "fd", effect = "individual"))

#MQ2E -> UTILIZANDO APENAS ADUCAÇÃO DO CONJUGUE COMO VI
summary(reg_anos_educ2 <-ivreg(lrendahabprin ~ anoseduc + idade + idadeaq + cor + mulher + horas_trabalhadas + rural + numfilhos06
                               | educconj2 + idade + idadeaq + cor + mulher + horas_trabalhadas + rural + numfilhos06 ,
                               data = pnadc_semNA_casado))

# MQO
summary(reg5_anos_educ <- lm(lrendahabprin ~ anoseduc + idade + idadeaq + cor + mulher + horas_trabalhadas + rural + numfilhos06 ,
                             data = pnadc_semNA_casado))


stargazer(reg_painel_fixo, reg_painel_aleatorio, reg_painel_pd, reg_anos_educ2, reg5_anos_educ, type = "html")


#questão 4####

#criando variavel de alfabetizado
pnadc_completa <-rename(pnadc_completa, ler_escrever = V3001)
pnadc_completa$n_alfabetizado <- ifelse(pnadc_completa$ler_escrever == "2", 1,0)

#a) fazendo um probit para saber a probabilidade de estar empregado para homens e mulheres####

#homens
summary(probit_empregado_homem <- glm(forca_trabalho ~ niveleduc + idade + idadeaq + cor + casado + rural + numfilhos06 ,
                                       data = pnadc_completa, family = binomial(link = "probit"), subset = (mulher == 0)))

#mulheres

summary(probit_empregado_mulher <- glm(forca_trabalho ~ niveleduc + idade + idadeaq + cor + casado + rural + numfilhos06 ,
                                      data = pnadc_completa, family = binomial(link = "probit"), subset = (mulher == 1)))

# ambos os sexos

summary(probit_empregado <- glm(forca_trabalho ~ niveleduc + idade + idadeaq + cor + casado + rural + numfilhos06 ,
                                      data = pnadc_completa, family = binomial(link = "probit")))

stargazer(probit_empregado_homem, probit_empregado_mulher, probit_empregado, type = "html")

#b) efeitos marginais ####


logitmfx(forca_trabalho ~ niveleduc + idade + idadeaq + cor + casado + rural + numfilhos06 ,
         data = pnadc_completa) #so consegui fazer para a regressão para ambos os sexos juntos

gc()
summary(margins(probit_empregado_homem))
summary(margins(probit_empregado_mulher))
summary(margins(probit_empregado))

#c)####
#tobit

summary(regtobit <- censReg(lrendahabprin ~ anoseduc + idade + idadeaq + cor + mulher + casado + horas_trabalhadas + rural + numfilhos06 ,
                            left = 0, right = 12.20607, data = pnadc_completa ))

# MQO
summary(reg5_anos_educ <- lm(lrendahabprin ~ anoseduc + idade + idadeaq + cor + mulher + casado + horas_trabalhadas + rural + numfilhos06 ,
                             data = pnadc_completa, subset = (condicao_ocupado == 1)))

stargazer(regtobit, reg5_anos_educ, type = "html")

#d) tem q usar uma base somente para mulheres####

summary(poisson_nfilhos <- glm(numfilhos ~ lrendahabprin + anoseduc + idade + idadeaq + cor + casado + rural ,
                                       family = "poisson", data = pnadc_completa, subset = (mulher == 1)))


#e) HECKMAN 2 STEPS####

summary(reg_heckiman <- heckit(forca_trabalho ~  anoseduc + idade + idadeaq + cor + casado + rural + numfilhos06, #1st usando probit
               lrendahabprin ~ anoseduc + idade + idadeaq + cor + casado + rural + numfilhos06 + horas_trabalhadas, #2nd lrendahabprin (MQO)
               data = pnadc_completa), subset = (mulher == 1))
                  
                 
