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


## Selecionando apenas algumas colunas
# ATENÇÃO: COLUNAS QUE NÓS ESCOLHEMOS!
lcolunas <- c("Ano","Trimestre","UF","UPA","Estrato","RM_RIDE","Capital","V1022","V1028","V2001","V2005","V2007","V2009","V2010","V3001",
              "V3002","V3002A","V3003","V3003A","V3006A","V3007","V3009","V3009A","V3012","V3013","V3014","V4001","V4012","V40121",
              "V4039","VD2002","VD2003","VD3004","VD3005","VD3006","V405011","VD4036","VD4037","VD4019","VD4031")
# Reduzimos o tamanho da base :)
pnadc <- pnadc[, lcolunas]
View(pnadc)


#Renomeando as variaveis de interesse para nosso trabalho

pnadc <-rename(pnadc, area = V1022)
pnadc <-rename(pnadc, peso_domicilio = V1028)
pnadc <-rename(pnadc, pessoas_domicilio = V2001)
pnadc <-rename(pnadc, chefe_dom = V2005)
pnadc <-rename(pnadc, sexo = V2007)
pnadc <-rename(pnadc, raca = V2010)
pnadc <-rename(pnadc, condicao_domicilio = VD2002)
pnadc <-rename(pnadc, n_componentes_domicilio = VD2003)
pnadc <-rename(pnadc, anos_estudo = VD3005)
pnadc <-rename(pnadc, grupo_anos_estudo = VD3006)
pnadc <-rename(pnadc, nivel_educ = VD3004)
pnadc <-rename(pnadc, faixa_horast_habituais = VD4036)
pnadc <-rename(pnadc, faixa_horast_efetivas = VD4037)

#Variaveis adicionais que possam ser uteis ao longo do trabalho:

pnadc <-rename(pnadc, ler_escrever = V3001)
pnadc <-rename(pnadc, frequenta_escola = V3002)
pnadc <-rename(pnadc, frequenta_qual_escola = V3002A)
pnadc <-rename(pnadc, frequenta_qual_curso = V3003)
pnadc <-rename(pnadc, frequenta_qual_curso_nc = V3003A)
pnadc <-rename(pnadc, etapa_fundamental = V3006A)
pnadc <-rename(pnadc, concluiu_outro_curso = V3007)
pnadc <-rename(pnadc, curso_mais_elevado_frequentou = V3009)
pnadc <-rename(pnadc, curso_mais_elevado_frequentou_nc = V3009A)
pnadc <-rename(pnadc, aprovado_no_curso = V3012)
pnadc <-rename(pnadc, ultimo_tempo_concluiu_aprovado = V3013)
pnadc <-rename(pnadc, conclusao_curso_elevado = V3014)
pnadc <-rename(pnadc, idade = V2009)
pnadc <-rename(pnadc, faixa_salario = V405011)
pnadc <-rename(pnadc, nesse_trabalho_era = V4012)
pnadc <-rename(pnadc, trabalhador_nao_remunerado = V40121)
pnadc <-rename(pnadc, horas_trabalhada_semana = V4039)
pnadc <-rename(pnadc, salario = VD4019)
pnadc <-rename(pnadc, horas_trabalho = VD4031)
pnadc <-rename(pnadc, trabalho = V4001)
 

## Gerando um novo arquivo dta com a base nova
save.dta13(pnadc, file="pnadc.dta")

#Criando as Dummies
## quando der algum problema começar daqui!!!!!
pnadc <- read.dta13("pnadc.dta")

##Urbana ou Rural
pnadc$urbana <- ifelse(pnadc$area == "1", 1,0)

##Estado
pnadc$MG <- ifelse(pnadc$UF == "31", 1,0)
pnadc$ES <- ifelse(pnadc$UF == "32", 1,0)
pnadc$RJ <- ifelse(pnadc$UF == "33", 1,0)

##Chefe de Domicilio/Conjuge (retiramos do dos códigos pois não entendemos o motivo)

#pnadc$chefe <- ifelse(pnadc$chefe_dom == "1", 1,0)
#pnadc$conjuge <- ifelse(pnadc$chefe_dom == "2" | pnadc$chefe_dom == "3", 1,0)
#pnadc$filho <- ifelse(pnadc$chefe_dom == "4" | pnadc$chefe_dom == "5", 1,0)

##Carracteristicas (considerando Negros = Pretos + Pardos)

pnadc$mulher <- ifelse(pnadc$sexo == "2", 1,0)

pnadc$preta <- ifelse(pnadc$raca == "2",1,0)
pnadc$parda <- ifelse(pnadc$raca == "4", 1,0)
pnadc$amarela <- ifelse(pnadc$raca == "3", 1,0)
pnadc$indigena <- ifelse(pnadc$raca == "5", 1,0)
pnadc$branco <- ifelse(pnadc$raca == "1", 1,0)


# criando faixas de idade
pnadc$faixas_idade2 <- NA
pnadc$faixas_idade2 <- ifelse(pnadc$idade < 14,"Até 13 anos",pnadc$faixas_idade2)
pnadc$faixas_idade2 <- ifelse((pnadc$idade >= 14 & pnadc$idade < 20),"14 a 19 anos",pnadc$faixas_idade2)
pnadc$faixas_idade2 <- ifelse((pnadc$idade >= 20 & pnadc$idade < 30),"20 a 29 anos",pnadc$faixas_idade2)
pnadc$faixas_idade2 <- ifelse((pnadc$idade >= 30 & pnadc$idade < 40),"30 a 39 anos",pnadc$faixas_idade2)
pnadc$faixas_idade2 <- ifelse((pnadc$idade >= 40 & pnadc$idade < 50),"40 a 49 anos",pnadc$faixas_idade2)
pnadc$faixas_idade2 <- ifelse((pnadc$idade >= 50 & pnadc$idade < 60),"50 a 59 anos",pnadc$faixas_idade2)
pnadc$faixas_idade2 <- ifelse((pnadc$idade >= 60 & pnadc$idade < 70),"60 a 69 anos",pnadc$faixas_idade2)
pnadc$faixas_idade2 <- ifelse((pnadc$idade >= 70 & pnadc$idade < 80),"70 a 79 anos",pnadc$faixas_idade2)
pnadc$faixas_idade2 <- ifelse((pnadc$idade >= 80),"80 anos ou mais",pnadc$faixas_idade2)

#Faixas de horas
pnadc$faixas_horas_trabalhadas_efetivas <- NA
pnadc$faixas_horas_trabalhadas_efetivas <- ifelse(pnadc$faixa_horast_efetivas == 1,"Até 14 horas",pnadc$faixas_horas_trabalhadas_efetivas)
pnadc$faixas_horas_trabalhadas_efetivas <- ifelse(pnadc$faixa_horast_efetivas == 2,"15 a 39 horas",pnadc$faixas_horas_trabalhadas_efetivas)
pnadc$faixas_horas_trabalhadas_efetivas <- ifelse(pnadc$faixa_horast_efetivas == 3,"40 a 44 horas",pnadc$faixas_horas_trabalhadas_efetivas)
pnadc$faixas_horas_trabalhadas_efetivas <- ifelse(pnadc$faixa_horast_efetivas == 4,"45 a 48 horas",pnadc$faixas_horas_trabalhadas_efetivas)
pnadc$faixas_horas_trabalhadas_efetivas <- ifelse(pnadc$faixa_horast_efetivas == 5,"Acima de 49 horas",pnadc$faixas_horas_trabalhadas_efetivas)
pnadc$faixa_horast_efetivas <- pnadc$faixas_horas_trabalhadas_efetivas
pnadc <-subset(pnadc, select = -c(faixas_horas_trabalhadas_efetivas))

# faixas de anos de estudo
pnadc$grupo_anos_estudo2 <- NA
pnadc$grupo_anos_estudo2 <- ifelse(pnadc$grupo_anos_estudo == 1, "0 ou Menos de 1 ano",pnadc$grupo_anos_estudo2)
pnadc$grupo_anos_estudo2 <- ifelse(pnadc$grupo_anos_estudo == 2, "1 a 4 anos",pnadc$grupo_anos_estudo2)
pnadc$grupo_anos_estudo2 <- ifelse(pnadc$grupo_anos_estudo == 3, "5 a 8 anos",pnadc$grupo_anos_estudo2)
pnadc$grupo_anos_estudo2 <- ifelse(pnadc$grupo_anos_estudo == 4, "9 a 11 anos",pnadc$grupo_anos_estudo2)
pnadc$grupo_anos_estudo2 <- ifelse(pnadc$grupo_anos_estudo == 5, "12 a 15 anos",pnadc$grupo_anos_estudo2)
pnadc$grupo_anos_estudo2 <- ifelse(pnadc$grupo_anos_estudo == 6, "16 ou Mais anos",pnadc$grupo_anos_estudo2)
pnadc$grupo_anos_estudo <- pnadc$grupo_anos_estudo2
pnadc <-subset(pnadc, select = -c(grupo_anos_estudo2))

#Colocando os nomes da UFs
pnadc$suf <- NA
pnadc$suf <- ifelse(pnadc$UF == 31,"MG",pnadc$suf)
pnadc$suf <- ifelse(pnadc$UF == 32,"ES",pnadc$suf)
pnadc$suf <- ifelse(pnadc$UF == 33,"RJ",pnadc$suf)
pnadc$suf <- ifelse(pnadc$UF == 35,"SP",pnadc$suf)
pnadc$UF <- pnadc$suf
pnadc <-subset(pnadc, select = -c(suf))

##Educacao

pnadc$n_alfabetizado <- ifelse(pnadc$ler_escrever == "2", 1,0)

##Trabalha ou nao

pnadc$n_empregado <- ifelse(pnadc$trabalho == "2", 1,0)

#Como estamos interessados na determinacao do salario, vamos excluir as observacoes dos menores de 14 anos

#pnadc <- subset(pnadc, pnadc$idade >= "14")
#pnadc$salario[is.na(pnadc$salario)] = 0  ->>> retiramos isso pois se declarar 0 para todos os NA's vai geral uma informação falsa

save.dta13(pnadc, file="pnadc_dummies.dta")
pnadc <- read.dta13("pnadc_dummies.dta")


"Tabelas e graficos: Estatisticas descritivas"


#Idade x Salário
#vamos fazer esse gráfico salario por faixas de idade 

media_sal_idade<- aggregate( salario ~ Ano + faixas_idade2 + UF , pnadc, mean)


ggplot(data = media_sal_idade, aes(x = faixas_idade2, y = salario, fill = factor(Ano))) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs( x = "Idade", y = "Salário", subtitle = "2015 até 2017" ,caption = "IBGE" , title = "Salário Médio por Idade",fill="Anos" ) +
  facet_grid(.~UF) + theme_classic() + coord_flip() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

#estatisticas do gráfico para o estado de ES
pnadc_es <- pnadc %>%
  filter(UF == "ES" )
describeBy(pnadc_es$salario, list(pnadc_es$faixas_idade2, pnadc_es$Ano), data = pnadc_es)
estat_sal_alfab_ano_es <- psych::describeBy(pnadc_es$salario, list(pnadc_es$faixas_idade2, pnadc_es$Ano) , skew=FALSE)
estat_sal_alfab_ano2_es <-do.call("rbind",estat_sal_alfab_ano_es)
estat_sal_alfab_ano2_es <- subset(estat_sal_alfab_ano2_es, select = -c(vars))
rownames(estat_sal_alfab_ano2_es) <- c("14 a 19 anos em 2015","20 a 29 anos em 2015","30 a 39 anos em 2015","40 a 49 anos em 2015","50 a 59 anos em 2015","60 a 69 anos em 2015","70 a 79 anos em 2015","80 anos ou mais em 2015","Até 13 anos em 2015",
                                       "14 a 19 anos em 2016","20 a 29 anos em 2016","30 a 39 anos em 2016","40 a 49 anos em 2016","50 a 59 anos em 2016","60 a 69 anos em 2016","70 a 79 anos em 2016","80 anos ou mais em 2016","Até 13 anos em 2016",
                                       "14 a 19 anos em 2017","20 a 29 anos em 2017","30 a 39 anos em 2017","40 a 49 anos em 2017","50 a 59 anos em 2017","60 a 69 anos em 2017","70 a 79 anos em 2017","80 anos ou mais em 2017","Até 13 anos em 2017")

datatable(estat_sal_alfab_ano2_es)
#estatisticas do gráfico para o estado de MG
pnadc_mg <- pnadc %>%
  filter(UF == "MG" )
describeBy(pnadc_mg$salario, list(pnadc_mg$faixas_idade2, pnadc_mg$Ano), data = pnadc_mg)
estat_sal_alfab_ano_mg <- psych::describeBy(pnadc_mg$salario, list(pnadc_mg$faixas_idade2, pnadc_mg$Ano) , skew=FALSE)
estat_sal_alfab_ano2_mg <-do.call("rbind",estat_sal_alfab_ano_mg)
estat_sal_alfab_ano2_mg <- subset(estat_sal_alfab_ano2_mg, select = -c(vars))
rownames(estat_sal_alfab_ano2_mg) <- c("14 a 19 anos em 2015","20 a 29 anos em 2015","30 a 39 anos em 2015","40 a 49 anos em 2015","50 a 59 anos em 2015","60 a 69 anos em 2015","70 a 79 anos em 2015","80 anos ou mais em 2015","Até 13 anos em 2015",
                                       "14 a 19 anos em 2016","20 a 29 anos em 2016","30 a 39 anos em 2016","40 a 49 anos em 2016","50 a 59 anos em 2016","60 a 69 anos em 2016","70 a 79 anos em 2016","80 anos ou mais em 2016","Até 13 anos em 2016",
                                       "14 a 19 anos em 2017","20 a 29 anos em 2017","30 a 39 anos em 2017","40 a 49 anos em 2017","50 a 59 anos em 2017","60 a 69 anos em 2017","70 a 79 anos em 2017","80 anos ou mais em 2017","Até 13 anos em 2017")

datatable(estat_sal_alfab_ano2_mg)
#estatisticas do gráfico para o estado de RJ
pnadc_rj <- pnadc %>%
  filter(UF == "RJ" )
describeBy(pnadc_rj$salario, list(pnadc_rj$faixas_idade2, pnadc_rj$Ano), data = pnadc_rj)
estat_sal_alfab_ano_rj <- psych::describeBy(pnadc_rj$salario, list(pnadc_rj$faixas_idade2, pnadc_rj$Ano) , skew=FALSE)
estat_sal_alfab_ano2_rj <-do.call("rbind",estat_sal_alfab_ano_rj)
estat_sal_alfab_ano2_rj <- subset(estat_sal_alfab_ano2_rj, select = -c(vars))
rownames(estat_sal_alfab_ano2_rj) <- c("14 a 19 anos em 2015","20 a 29 anos em 2015","30 a 39 anos em 2015","40 a 49 anos em 2015","50 a 59 anos em 2015","60 a 69 anos em 2015","70 a 79 anos em 2015","80 anos ou mais em 2015","Até 13 anos em 2015",
                                       "14 a 19 anos em 2016","20 a 29 anos em 2016","30 a 39 anos em 2016","40 a 49 anos em 2016","50 a 59 anos em 2016","60 a 69 anos em 2016","70 a 79 anos em 2016","80 anos ou mais em 2016","Até 13 anos em 2016",
                                       "14 a 19 anos em 2017","20 a 29 anos em 2017","30 a 39 anos em 2017","40 a 49 anos em 2017","50 a 59 anos em 2017","60 a 69 anos em 2017","70 a 79 anos em 2017","80 anos ou mais em 2017","Até 13 anos em 2017")
datatable(estat_sal_alfab_ano2_rj)
#estatisticas do gráfico para o estado de SP
pnadc_sp <- pnadc %>%
  filter(UF == "SP" )
describeBy(pnadc_sp$salario, list(pnadc_sp$faixas_idade2, pnadc_sp$Ano), data = pnadc_sp)
estat_sal_alfab_ano_sp <- psych::describeBy(pnadc_sp$salario, list(pnadc_sp$faixas_idade2, pnadc_sp$Ano) , skew=FALSE)
estat_sal_alfab_ano2_sp <-do.call("rbind",estat_sal_alfab_ano_sp)
estat_sal_alfab_ano2_sp <- subset(estat_sal_alfab_ano2_sp, select = -c(vars))
rownames(estat_sal_alfab_ano2_sp) <- c("14 a 19 anos em 2015","20 a 29 anos em 2015","30 a 39 anos em 2015","40 a 49 anos em 2015","50 a 59 anos em 2015","60 a 69 anos em 2015","70 a 79 anos em 2015","80 anos ou mais em 2015","Até 13 anos em 2015",
                                       "14 a 19 anos em 2016","20 a 29 anos em 2016","30 a 39 anos em 2016","40 a 49 anos em 2016","50 a 59 anos em 2016","60 a 69 anos em 2016","70 a 79 anos em 2016","80 anos ou mais em 2016","Até 13 anos em 2016",
                                       "14 a 19 anos em 2017","20 a 29 anos em 2017","30 a 39 anos em 2017","40 a 49 anos em 2017","50 a 59 anos em 2017","60 a 69 anos em 2017","70 a 79 anos em 2017","80 anos ou mais em 2017","Até 13 anos em 2017")
datatable(estat_sal_alfab_ano2_sp)


#Horas Trabalhadas x Salario ->> trocamos a variavel de hora trabalhada


media_sal_horas <- aggregate( salario ~ faixa_horast_efetivas + Ano + UF, pnadc, mean )

ggplot(data = media_sal_horas, aes(x = faixa_horast_efetivas, y = salario, fill = factor(Ano))) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs( x = "Horas", y = "Salário", subtitle = "2015 até 2017" ,caption = "IBGE" , title = "Salário Médio por Faixas de Horas Trabalhadas",fill="Anos" ) +
  facet_grid(.~UF) + theme_classic() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) 

#estatisticas do gráfico para o estado de ES
describeBy(pnadc_es$salario, list(pnadc_es$faixa_horast_efetivas, pnadc_es$Ano), data = pnadc_es)
estat_sal_fhorat_ano_es <- psych::describeBy(pnadc_es$salario, list(pnadc_es$faixa_horast_efetivas, pnadc_es$Ano) , skew=FALSE)
estat_sal_fhorat_ano2_es <-do.call("rbind",estat_sal_fhorat_ano_es)
estat_sal_fhorat_ano2_es <- subset(estat_sal_fhorat_ano2_es, select = -c(vars))
rownames(estat_sal_fhorat_ano2_es) <- c("15 a 39 horas em 2015","40 a 44 horas em 2015","45 a 48 horas em 2015","Acima de 49 horas em 2015","Até 14 horas em 2015",
                                        "15 a 39 horas em 2016","40 a 44 horas em 2016","45 a 48 horas em 2016","Acima de 49 horas em 2016","Até 14 horas em 2016",
                                        "15 a 39 horas em 2017","40 a 44 horas em 2017","45 a 48 horas em 2017","Acima de 49 horas em 2017","Até 14 horas em 2017")
datatable(estat_sal_fhorat_ano2_es)
#estatisticas do gráfico para o estado de MG
describeBy(pnadc_mg$salario, list(pnadc_mg$faixa_horast_efetivas, pnadc_mg$Ano), data = pnadc_mg)
estat_sal_fhorat_ano_mg <- psych::describeBy(pnadc_mg$salario, list(pnadc_mg$faixa_horast_efetivas, pnadc_mg$Ano) , skew=FALSE)
estat_sal_fhorat_ano2_mg <-do.call("rbind",estat_sal_fhorat_ano_mg)
estat_sal_fhorat_ano2_mg <- subset(estat_sal_fhorat_ano2_mg, select = -c(vars))
rownames(estat_sal_fhorat_ano2_mg) <- c("15 a 39 horas em 2015","40 a 44 horas em 2015","45 a 48 horas em 2015","Acima de 49 horas em 2015","Até 14 horas em 2015",
                                        "15 a 39 horas em 2016","40 a 44 horas em 2016","45 a 48 horas em 2016","Acima de 49 horas em 2016","Até 14 horas em 2016",
                                        "15 a 39 horas em 2017","40 a 44 horas em 2017","45 a 48 horas em 2017","Acima de 49 horas em 2017","Até 14 horas em 2017")
datatable(estat_sal_fhorat_ano2_mg)
#estatisticas do gráfico para o estado de RJ
describeBy(pnadc_rj$salario, list(pnadc_rj$faixa_horast_efetivas, pnadc_rj$Ano), data = pnadc_rj)
estat_sal_fhorat_ano_rj <- psych::describeBy(pnadc_rj$salario, list(pnadc_rj$faixa_horast_efetivas, pnadc_rj$Ano) , skew=FALSE)
estat_sal_fhorat_ano2_rj <-do.call("rbind",estat_sal_fhorat_ano_rj)
estat_sal_fhorat_ano2_rj <- subset(estat_sal_fhorat_ano2_rj, select = -c(vars))
rownames(estat_sal_fhorat_ano2_rj) <- c("15 a 39 horas em 2015","40 a 44 horas em 2015","45 a 48 horas em 2015","Acima de 49 horas em 2015","Até 14 horas em 2015",
                                        "15 a 39 horas em 2016","40 a 44 horas em 2016","45 a 48 horas em 2016","Acima de 49 horas em 2016","Até 14 horas em 2016",
                                        "15 a 39 horas em 2017","40 a 44 horas em 2017","45 a 48 horas em 2017","Acima de 49 horas em 2017","Até 14 horas em 2017")
datatable(estat_sal_fhorat_ano2_rj)
#estatisticas do gráfico para o estado de SP
describeBy(pnadc_sp$salario, list(pnadc_sp$faixa_horast_efetivas, pnadc_sp$Ano), data = pnadc_sp)
estat_sal_fhorat_ano_sp <- psych::describeBy(pnadc_sp$salario, list(pnadc_sp$faixa_horast_efetivas, pnadc_sp$Ano) , skew=FALSE)
estat_sal_fhorat_ano2_sp <-do.call("rbind",estat_sal_fhorat_ano_sp)
estat_sal_fhorat_ano2_sp <- subset(estat_sal_fhorat_ano2_sp, select = -c(vars))
rownames(estat_sal_fhorat_ano2_sp) <- c("15 a 39 horas em 2015","40 a 44 horas em 2015","45 a 48 horas em 2015","Acima de 49 horas em 2015","Até 14 horas em 2015",
                                        "15 a 39 horas em 2016","40 a 44 horas em 2016","45 a 48 horas em 2016","Acima de 49 horas em 2016","Até 14 horas em 2016",
                                        "15 a 39 horas em 2017","40 a 44 horas em 2017","45 a 48 horas em 2017","Acima de 49 horas em 2017","Até 14 horas em 2017")
datatable(estat_sal_fhorat_ano2_sp)



####Educacao

mean(pnadc$anos_estudo, na.rm=TRUE)

media_sal_estudo <- aggregate( salario ~ grupo_anos_estudo + Ano + UF, pnadc, mean )

ggplot(data = media_sal_estudo, aes(x = grupo_anos_estudo, y = salario, fill = factor(Ano))) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs( x = "Anos de estudo", y = "Salário", subtitle = "2015 até 2017" ,caption = "IBGE" , title = "Salário Médio por Faixas de Anos de Estudo",fill="Anos" ) +
  facet_grid(.~UF) + theme_classic() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

#estatisticas do gráfico para o estado de ES
describeBy(pnadc_es$salario, list(pnadc_es$grupo_anos_estudo, pnadc_es$Ano), data = pnadc_es)
estat_sal_ganosest_ano_es <- psych::describeBy(pnadc_es$salario, list(pnadc_es$grupo_anos_estudo, pnadc_es$Ano) , skew=FALSE)
estat_sal_ganosest_ano2_es <-do.call("rbind",estat_sal_ganosest_ano_es)
estat_sal_ganosest_ano2_es <- subset(estat_sal_ganosest_ano2_es, select = -c(vars))
rownames(estat_sal_ganosest_ano2_es) <- c("0 ou menos de 1 ano em 2015","1 a 4 anos em 2015","12 a 15 anos em 2015","16 anos ou mais em 2015","5 a 8 anos em 2015","9 a 11 anos em 2015",
                                          "0 ou menos de 1 ano em 2016","1 a 4 anos em 2016","12 a 15 anos em 2016","16 anos ou mais em 2016","5 a 8 anos em 2016","9 a 11 anos em 2016",
                                          "0 ou menos de 1 ano em 2017","1 a 4 anos em 2017","12 a 15 anos em 2017","16 anos ou mais em 2017","5 a 8 anos em 2017","9 a 11 anos em 2017")
datatable(estat_sal_ganosest_ano2_es)
#estatisticas do gráfico para o estado de MG
describeBy(pnadc_mg$salario, list(pnadc_mg$grupo_anos_estudo, pnadc_mg$Ano), data = pnadc_mg)
estat_sal_ganosest_ano_mg <- psych::describeBy(pnadc_mg$salario, list(pnadc_mg$grupo_anos_estudo, pnadc_mg$Ano) , skew=FALSE)
estat_sal_ganosest_ano2_mg <-do.call("rbind",estat_sal_ganosest_ano_mg)
estat_sal_ganosest_ano2_mg <- subset(estat_sal_ganosest_ano2_mg, select = -c(vars))
rownames(estat_sal_ganosest_ano2_mg) <- c("0 ou menos de 1 ano em 2015","1 a 4 anos em 2015","12 a 15 anos em 2015","16 anos ou mais em 2015","5 a 8 anos em 2015","9 a 11 anos em 2015",
                                          "0 ou menos de 1 ano em 2016","1 a 4 anos em 2016","12 a 15 anos em 2016","16 anos ou mais em 2016","5 a 8 anos em 2016","9 a 11 anos em 2016",
                                          "0 ou menos de 1 ano em 2017","1 a 4 anos em 2017","12 a 15 anos em 2017","16 anos ou mais em 2017","5 a 8 anos em 2017","9 a 11 anos em 2017")

datatable(estat_sal_ganosest_ano2_mg)
#estatisticas do gráfico para o estado de RJ
describeBy(pnadc_rj$salario, list(pnadc_rj$grupo_anos_estudo, pnadc_rj$Ano), data = pnadc_rj)
estat_sal_ganosest_ano_rj <- psych::describeBy(pnadc_rj$salario, list(pnadc_rj$grupo_anos_estudo, pnadc_rj$Ano) , skew=FALSE)
estat_sal_ganosest_ano2_rj <-do.call("rbind",estat_sal_ganosest_ano_rj)
estat_sal_ganosest_ano2_rj <- subset(estat_sal_ganosest_ano2_rj, select = -c(vars))
rownames(estat_sal_ganosest_ano2_rj) <- c("0 ou menos de 1 ano em 2015","1 a 4 anos em 2015","12 a 15 anos em 2015","16 anos ou mais em 2015","5 a 8 anos em 2015","9 a 11 anos em 2015",
                                          "0 ou menos de 1 ano em 2016","1 a 4 anos em 2016","12 a 15 anos em 2016","16 anos ou mais em 2016","5 a 8 anos em 2016","9 a 11 anos em 2016",
                                          "0 ou menos de 1 ano em 2017","1 a 4 anos em 2017","12 a 15 anos em 2017","16 anos ou mais em 2017","5 a 8 anos em 2017","9 a 11 anos em 2017")

datatable(estat_sal_ganosest_ano2_rj)
#estatisticas do gráfico para o estado de SP
describeBy(pnadc_sp$salario, list(pnadc_sp$grupo_anos_estudos, pnadc_sp$Ano), data = pnadc_sp)
estat_sal_ganosest_ano_sp <- psych::describeBy(pnadc_sp$salario, list(pnadc_sp$grupo_anos_estudo, pnadc_sp$Ano) , skew=FALSE)
estat_sal_ganosest_ano2_sp <-do.call("rbind",estat_sal_ganosest_ano_sp)
estat_sal_ganosest_ano2_sp <- subset(estat_sal_ganosest_ano2_sp, select = -c(vars))
rownames(estat_sal_ganosest_ano2_sp) <- c("0 ou menos de 1 ano em 2015","1 a 4 anos em 2015","12 a 15 anos em 2015","16 anos ou mais em 2015","5 a 8 anos em 2015","9 a 11 anos em 2015",
                                          "0 ou menos de 1 ano em 2016","1 a 4 anos em 2016","12 a 15 anos em 2016","16 anos ou mais em 2016","5 a 8 anos em 2016","9 a 11 anos em 2016",
                                          "0 ou menos de 1 ano em 2017","1 a 4 anos em 2017","12 a 15 anos em 2017","16 anos ou mais em 2017","5 a 8 anos em 2017","9 a 11 anos em 2017")
datatable(estat_sal_ganosest_ano2_sp)

#Salario e sexo (contando desempregados)
# ta um pouco zuado pois estamos somando os dados de 3 anos, é melhor falar da proporção

#nrow(pnadc[pnadc$mulher == "1",]) #contando quantas mulheres
#nrow(pnadc[pnadc$mulher == "0",]) #contando quantos homens

media_sal_sexo <- aggregate( salario ~ mulher +UF + Ano, pnadc, mean )
media_sal_sexo$mulher2 <- NA
media_sal_sexo$mulher2 <- ifelse(media_sal_sexo$mulher == 0,"Homem",media_sal_sexo$mulher2)
media_sal_sexo$mulher2 <- ifelse(media_sal_sexo$mulher == 1,"Mulher",media_sal_sexo$mulher2)
media_sal_sexo$mulher <- media_sal_sexo$mulher2
media_sal_sexo <- subset(media_sal_sexo, select = -c(mulher2))

ggplot(data = media_sal_sexo, aes(x = mulher, y = salario, fill = factor(Ano))) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs( x = "Sexo", y = "Salário", subtitle = "2015 até 2017" ,caption = "IBGE" , title = "Salário Médio por Sexo",fill="Anos" ) +
  facet_grid(.~UF) + theme_classic() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

#criando uma coluna com o nome do genero para fazer as estatíticas
pnadc$nome_genero <- NA
pnadc$nome_genero <- ifelse(pnadc$mulher == 1,"Mulher",pnadc$nome_genero)
pnadc$nome_genero <- ifelse(pnadc$mulher == 0,"Homem",pnadc$nome_genero)

#estatisticas do gráfico para o estado de ES
pnadc_es <- pnadc %>%
  filter(UF == "ES" )
describeBy(pnadc_es$salario, list(pnadc_es$nome_genero, pnadc_es$Ano), data = pnadc_es)
estat_sal_sexo_ano_es <- psych::describeBy(pnadc_es$salario, list(pnadc_es$nome_genero, pnadc_es$Ano) , skew=FALSE)
estat_sal_sexo_ano2_es <-do.call("rbind",estat_sal_sexo_ano_es)
estat_sal_sexo_ano2_es <- subset(estat_sal_sexo_ano2_es, select = -c(vars))
rownames(estat_sal_sexo_ano2_es) <- c("Homem em 2015","Mulher em 2015",
                                      "Homem em 2016","Mulher em 2016",
                                      "Homem em 2017","Mulher em 2017")
datatable(estat_sal_sexo_ano2_es)
#estatisticas do gráfico para o estado de MG
pnadc_mg <- pnadc %>%
  filter(UF == "MG" )
describeBy(pnadc_mg$salario, list(pnadc_mg$nome_genero, pnadc_mg$Ano), data = pnadc_mg)
estat_sal_sexo_ano_mg <- psych::describeBy(pnadc_mg$salario, list(pnadc_mg$nome_genero, pnadc_mg$Ano) , skew=FALSE)
estat_sal_sexo_ano2_mg <-do.call("rbind",estat_sal_sexo_ano_mg)
estat_sal_sexo_ano2_mg <- subset(estat_sal_sexo_ano2_mg, select = -c(vars))
rownames(estat_sal_sexo_ano2_mg) <- c("Homem em 2015","Mulher em 2015",
                                      "Homem em 2016","Mulher em 2016",
                                      "Homem em 2017","Mulher em 2017")
datatable(estat_sal_sexo_ano2_mg)
#estatisticas do gráfico para o estado de RJ
pnadc_rj <- pnadc %>%
  filter(UF == "RJ" )
describeBy(pnadc_rj$salario, list(pnadc_rj$nome_genero, pnadc_rj$Ano), data = pnadc_rj)
estat_sal_sexo_ano_rj <- psych::describeBy(pnadc_rj$salario, list(pnadc_rj$nome_genero, pnadc_rj$Ano) , skew=FALSE)
estat_sal_sexo_ano2_rj <-do.call("rbind",estat_sal_sexo_ano_rj)
estat_sal_sexo_ano2_rj <- subset(estat_sal_sexo_ano2_rj, select = -c(vars))
rownames(estat_sal_sexo_ano2_rj) <- c("Homem em 2015","Mulher em 2015",
                                      "Homem em 2016","Mulher em 2016",
                                      "Homem em 2017","Mulher em 2017")
datatable(estat_sal_sexo_ano2_rj)
#estatisticas do gráfico para o estado de SP
pnadc_sp <- pnadc %>%
  filter(UF == "SP" )
describeBy(pnadc_sp$salario, list(pnadc_sp$nome_genero, pnadc_sp$Ano), data = pnadc_sp)
estat_sal_sexo_ano_sp <- psych::describeBy(pnadc_sp$salario, list(pnadc_sp$nome_genero, pnadc_sp$Ano) , skew=FALSE)
estat_sal_sexo_ano2_sp <-do.call("rbind",estat_sal_sexo_ano_sp)
estat_sal_sexo_ano2_sp <- subset(estat_sal_sexo_ano2_sp, select = -c(vars))
rownames(estat_sal_sexo_ano2_sp) <- c("Homem em 2015","Mulher em 2015",
                                      "Homem em 2016","Mulher em 2016",
                                      "Homem em 2017","Mulher em 2017")
datatable(estat_sal_sexo_ano2_sp)


## quantidade de não alfabetizados(NA = 1; A = 0) 

media_sal_alfabet <- aggregate( salario ~ n_alfabetizado + Ano + UF, pnadc, mean)
media_sal_alfabet$n_alfabetizado2 <- NA
media_sal_alfabet$n_alfabetizado2 <- ifelse(media_sal_alfabet$n_alfabetizado == 0,"Alfabetizado",media_sal_alfabet$n_alfabetizado2)
media_sal_alfabet$n_alfabetizado2 <- ifelse(media_sal_alfabet$n_alfabetizado == 1,"Não Alfabetizado",media_sal_alfabet$n_alfabetizado2)
media_sal_alfabet$n_alfabetizado <- media_sal_alfabet$n_alfabetizado2
media_sal_alfabet <- subset(media_sal_alfabet, select = -c(n_alfabetizado2))

ggplot(data = media_sal_alfabet, aes(x = n_alfabetizado, y = salario, fill = factor(Ano))) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs( x = "Alfabetização", y = "Salário", subtitle = "2015 até 2017" ,caption = "IBGE" , title = "Salário Médio por Alfabetização",fill="Anos" ) +
  facet_grid(.~UF) + theme_classic() +
  scale_fill_brewer(name ="Ano", palette = "Dark2") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

#criando uma coluna com a alfabetização do individuo para fazer as estatíticas
pnadc$grau_alfabetização <- NA
pnadc$grau_alfabetização <- ifelse(pnadc$n_alfabetizado == 1,"Não Alfabetizador",pnadc$grau_alfabetização)
pnadc$grau_alfabetização <- ifelse(pnadc$n_alfabetizado == 0,"Alfabetizado",pnadc$grau_alfabetização)

#estatisticas do gráfico para o estado de ES
pnadc_es <- pnadc %>%
  filter(UF == "ES" )
describeBy(pnadc_es$salario, list(pnadc_es$grau_alfabetização, pnadc_es$Ano), data = pnadc_es)
estat_sal_alfabetizacao_ano_es <- psych::describeBy(pnadc_es$salario, list(pnadc_es$grau_alfabetização, pnadc_es$Ano) , skew=FALSE)
estat_sal_alfabetizacao_ano2_es <-do.call("rbind",estat_sal_alfabetizacao_ano_es)
estat_sal_alfabetizacao_ano2_es <- subset(estat_sal_alfabetizacao_ano2_es, select = -c(vars))
rownames(estat_sal_alfabetizacao_ano2_es) <- c("Alfabetizado em 2015","Não Alfabetizado em 2015",
                                               "Alfabetizado em 2016","Não Alfabetizado em 2016",
                                               "Alfabetizado em 2017","Não Alfabetizado em 2017")
datatable(estat_sal_alfabetizacao_ano2_es)
#estatisticas do gráfico para o estado de MG
pnadc_mg <- pnadc %>%
  filter(UF == "MG" )
describeBy(pnadc_mg$salario, list(pnadc_mg$grau_alfabetização, pnadc_mg$Ano), data = pnadc_mg)
estat_sal_alfabetizacao_ano_mg <- psych::describeBy(pnadc_mg$salario, list(pnadc_mg$grau_alfabetização, pnadc_mg$Ano) , skew=FALSE)
estat_sal_alfabetizacao_ano2_mg <-do.call("rbind",estat_sal_alfabetizacao_ano_mg)
estat_sal_alfabetizacao_ano2_mg <- subset(estat_sal_alfabetizacao_ano2_mg, select = -c(vars))
rownames(estat_sal_alfabetizacao_ano2_mg) <- c("Alfabetizado em 2015","Não Alfabetizado em 2015",
                                               "Alfabetizado em 2016","Não Alfabetizado em 2016",
                                               "Alfabetizado em 2017","Não Alfabetizado em 2017")
datatable(estat_sal_alfabetizacao_ano2_mg)
#estatisticas do gráfico para o estado de RJ
pnadc_rj <- pnadc %>%
  filter(UF == "RJ" )
describeBy(pnadc_rj$salario, list(pnadc_rj$grau_alfabetização, pnadc_rj$Ano), data = pnadc_rj)
estat_sal_alfabetizacao_ano_rj <- psych::describeBy(pnadc_rj$salario, list(pnadc_rj$grau_alfabetização, pnadc_rj$Ano) , skew=FALSE)
estat_sal_alfabetizacao_ano2_rj <-do.call("rbind",estat_sal_alfabetizacao_ano_rj)
estat_sal_alfabetizacao_ano2_rj <- subset(estat_sal_alfabetizacao_ano2_rj, select = -c(vars))
rownames(estat_sal_alfabetizacao_ano2_rj) <- c("Alfabetizado em 2015","Não Alfabetizado em 2015",
                                               "Alfabetizado em 2016","Não Alfabetizado em 2016",
                                               "Alfabetizado em 2017","Não Alfabetizado em 2017")
datatable(estat_sal_alfabetizacao_ano2_rj)
#estatisticas do gráfico para o estado de SP
pnadc_sp <- pnadc %>%
  filter(UF == "SP" )
describeBy(pnadc_sp$salario, list(pnadc_sp$grau_alfabetização, pnadc_sp$Ano), data = pnadc_sp)
estat_sal_alfabetizacao_ano_sp <- psych::describeBy(pnadc_sp$salario, list(pnadc_sp$grau_alfabetização, pnadc_sp$Ano) , skew=FALSE)
estat_sal_alfabetizacao_ano2_sp <-do.call("rbind",estat_sal_alfabetizacao_ano_sp)
estat_sal_alfabetizacao_ano2_sp <- subset(estat_sal_alfabetizacao_ano2_sp, select = -c(vars))
rownames(estat_sal_alfabetizacao_ano2_sp) <- c("Alfabetizado em 2015","Não Alfabetizado em 2015",
                                               "Alfabetizado em 2016","Não Alfabetizado em 2016",
                                               "Alfabetizado em 2017","Não Alfabetizado em 2017")
datatable(estat_sal_alfabetizacao_ano2_sp)


#### estatisticas salariais de alfabetizado e não alfabetizado para raças
#nomenado as raças em uma coluna
pnadc$raca_nome <- NA
pnadc$raca_nome <- ifelse(pnadc$raca == 1, "Branca",pnadc$raca_nome)
pnadc$raca_nome <- ifelse(pnadc$raca == 2, "Preta",pnadc$raca_nome)
pnadc$raca_nome <- ifelse(pnadc$raca == 3, "Amarela",pnadc$raca_nome)
pnadc$raca_nome <- ifelse(pnadc$raca == 4, "Parda",pnadc$raca_nome)
pnadc$raca_nome <- ifelse(pnadc$raca == 5, "Indígena",pnadc$raca_nome)
pnadc$raca_nome <- ifelse(pnadc$raca == 9, "Ignorado",pnadc$raca_nome)
# faixas de anos de estudo
pnadc$alfabetização <- NA
pnadc$alfabetização <- ifelse(pnadc$ler_escrever == 1, "Alfabetizado",pnadc$alfabetização)
pnadc$alfabetização <- ifelse(pnadc$ler_escrever == 2, "Não Alfabetizado",pnadc$alfabetização)
## fazendo as estatíticas salarias para dois grupos: raça e alfabetizados
describeBy(pnadc$salario, list(pnadc$alfabetização, pnadc$raca_nome), data = pnadc)
estat_sal_alfab_raca <- psych::describeBy(pnadc$salario, list(pnadc$alfabetização, pnadc$raca_nome) , skew=FALSE)
estat_sal_alfab_raca2 <-do.call("rbind",estat_sal_alfab_raca)
estat_sal_alfab_raca2 <- subset(estat_sal_alfab_raca2, select = -c(vars))
rownames(estat_sal_alfab_raca2) <- c("Alfabetizado e Amarelo","Não Alfabetizado e Amarelo",
                                               "Alfabetizado e Branco","Não Alfabetizado e Branco",
                                               "Alfabetizado e Ignorado","Não Alfabetizado e Ignorado",
                                               "Alfabetizado e Indígena","Não Alfabetizado e Indígena",
                                               "Alfabetizado e Parda","Não Alfabetizado e Parda",
                                               "Alfabetizado e Preta","Não Alfabetizado e Preta")
datatable(estat_sal_alfab_raca2)

### Evolução da alfabetização

prop_alfab <- pnadc %>%
  select(ler_escrever, Ano, UF)

prop_alfab$Alfabetizado <- NA
prop_alfab$Não_Alfabetizado <- NA
prop_alfab$Alfabetizado <- ifelse(prop_alfab$ler_escrever == 1,"1",prop_alfab$Alfabetizado)
prop_alfab$Não_Alfabetizado <- ifelse(prop_alfab$ler_escrever == 2,"1",prop_alfab$Não_Alfabetizado)
prop_alfab <- subset(prop_alfab, select = -c(ler_escrever))

prop_Alfabetizado <-  aggregate(Alfabetizado ~ Ano + UF , data = prop_alfab, length )
prop_Não_Alfabetizado <-  aggregate(Não_Alfabetizado ~ Ano + UF , data = prop_alfab, length )

prop_Alfabetização <- full_join(prop_Não_Alfabetizado,prop_Alfabetizado, by=c("Ano","UF"))

prop_Alfabetização <- prop_Alfabetização %>%
  mutate(prop_alf = (Alfabetizado/(Alfabetizado+Não_Alfabetizado))) %>%
  select(-Não_Alfabetizado,-Alfabetizado)

prop_Alfabetização %>%
  ggplot(aes(x = Ano, y = prop_alf, group = UF, color = UF)) +
  geom_line(size = 1.5) +
  labs(title = "Evolução do Percentual de Alfabetização",subtitle = "2015 até 2017",
       x = "Ano", y = "% de Alfabetização", caption = "IBGE") +
  scale_color_brewer(name = "UF", palette = "Set1") + 
  theme_classic()


#Frequenta escola

freq_escola <- aggregate(frequenta_escola ~ Ano + UF , pnadc, mean )

freq_escola <- pnadc %>%
  select(frequenta_escola, Ano, idade) %>% 
  filter(idade <= 17) %>%
  select(-idade)
freq_escola$frequenta_escola[is.na(freq_escola$frequenta_escola)]=0  
freq_escola$frequenta_escola2 <- freq_escola$frequenta_escola

freq_escola <- aggregate( frequenta_escola ~ Ano + frequenta_escola2 , data = freq_escola, FUN = length ) 

ggplot(data=freq_escola, aes(x=factor(1), y=frequenta_escola, fill= factor(frequenta_escola2))) +
  geom_bar(stat="identity", position="fill") +
  labs( x = "", y = "Anos", subtitle = "2015 até 2017" ,caption = "IBGE" , title = "Adesão Escolar",fill="" ) +
  coord_polar(theta="y") +
    scale_fill_brewer(name = "", palette = "Set1", labels = c("Não Aplicável", 
                                                            "Frequenta Escola",
                                                            "Não Frequenta Escola")) +
  theme(axis.text.x = element_blank(),axis.text.y = element_blank(),
        panel.grid = element_blank(),axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),panel.background = element_blank()) +
  facet_grid(.~Ano) 
# geom_text(aes(label = paste(round(frequenta_escola / sum(frequenta_escola) * 100, 1), "%")), position = position_stack(vjust = 1)) +
  # acabou que esse código deu erro quando foi colocar os rótulos


#curso_mais_elevado_frequentou_nc
#conclus?o_curso_elevado
#frequenta_qual_curso_nc

prop_cursos <- pnadc %>%
  select(curso_mais_elevado_frequentou_nc,conclusao_curso_elevado,
         frequenta_qual_curso_nc, Ano, UF)

prop_cursos$curso_mais_elevado_frequentou_nc[is.na(prop_cursos$curso_mais_elevado_frequentou_nc)]=0
prop_cursos$conclusao_curso_elevado[is.na(prop_cursos$conclusao_curso_elevado)] =0
prop_cursos$frequenta_qual_curso_nc[is.na(prop_cursos$frequenta_qual_curso_nc)] =0
  
prop_cursos$curso_mais_elevado_frequentou_nc2 <- prop_cursos$curso_mais_elevado_frequentou_nc 
prop_cursos$conclusao_curso_elevado2 <- prop_cursos$conclusao_curso_elevado 
prop_cursos$frequenta_qual_curso_nc2 <- prop_cursos$frequenta_qual_curso_nc

prop_cursos <-  aggregate(cbind(curso_mais_elevado_frequentou_nc,conclusao_curso_elevado,
                                frequenta_qual_curso_nc) ~ Ano + UF + curso_mais_elevado_frequentou_nc2 +
                          conclusao_curso_elevado2 + frequenta_qual_curso_nc2,
                          data = prop_cursos, length )
prop_cursos$frequenta_qual_curso_nc2 <- ifelse(prop_cursos$frequenta_qual_curso_nc2 == 0, "Não Aplicável", prop_cursos$frequenta_qual_curso_nc2)
prop_cursos$frequenta_qual_curso_nc2 <- ifelse(prop_cursos$frequenta_qual_curso_nc2 == 1, "Creche", prop_cursos$frequenta_qual_curso_nc2)
prop_cursos$frequenta_qual_curso_nc2 <- ifelse(prop_cursos$frequenta_qual_curso_nc2 == 2, "Pré-escola", prop_cursos$frequenta_qual_curso_nc2)
prop_cursos$frequenta_qual_curso_nc2 <- ifelse(prop_cursos$frequenta_qual_curso_nc2 == 3, "Alfabetização de Jovens Adultos", prop_cursos$frequenta_qual_curso_nc2)
prop_cursos$frequenta_qual_curso_nc2 <- ifelse(prop_cursos$frequenta_qual_curso_nc2 == 4, "Ensino Fundamental Regular", prop_cursos$frequenta_qual_curso_nc2)
prop_cursos$frequenta_qual_curso_nc2 <- ifelse(prop_cursos$frequenta_qual_curso_nc2 == 5, "EJA Ensino Fundamental", prop_cursos$frequenta_qual_curso_nc2)
prop_cursos$frequenta_qual_curso_nc2 <- ifelse(prop_cursos$frequenta_qual_curso_nc2 == 6, "Ensino Médio Regular", prop_cursos$frequenta_qual_curso_nc2)
prop_cursos$frequenta_qual_curso_nc2 <- ifelse(prop_cursos$frequenta_qual_curso_nc2 == 7, "EJA Ensino Médio", prop_cursos$frequenta_qual_curso_nc2)
prop_cursos$frequenta_qual_curso_nc2 <- ifelse(prop_cursos$frequenta_qual_curso_nc2 == 8, "Graduação", prop_cursos$frequenta_qual_curso_nc2)
prop_cursos$frequenta_qual_curso_nc2 <- ifelse(prop_cursos$frequenta_qual_curso_nc2 == 9, "Especilização de Nível Superior", prop_cursos$frequenta_qual_curso_nc2)
prop_cursos$frequenta_qual_curso_nc2 <- ifelse(prop_cursos$frequenta_qual_curso_nc2 == 10, "Mestrado", prop_cursos$frequenta_qual_curso_nc2)
prop_cursos$frequenta_qual_curso_nc2 <- ifelse(prop_cursos$frequenta_qual_curso_nc2 == 11, "Doutorado", prop_cursos$frequenta_qual_curso_nc2)

prop_cursos$curso_mais_elevado_frequentou_nc2 <- ifelse(prop_cursos$curso_mais_elevado_frequentou_nc2 == 0, "Não Aplicável", prop_cursos$curso_mais_elevado_frequentou_nc2)
prop_cursos$curso_mais_elevado_frequentou_nc2 <- ifelse(prop_cursos$curso_mais_elevado_frequentou_nc2 == 1, "Creche", prop_cursos$curso_mais_elevado_frequentou_nc2)
prop_cursos$curso_mais_elevado_frequentou_nc2 <- ifelse(prop_cursos$curso_mais_elevado_frequentou_nc2 == 2, "Pré-escola", prop_cursos$curso_mais_elevado_frequentou_nc2)
prop_cursos$curso_mais_elevado_frequentou_nc2 <- ifelse(prop_cursos$curso_mais_elevado_frequentou_nc2 == 3, "Classe de Alfabetização", prop_cursos$curso_mais_elevado_frequentou_nc2)
prop_cursos$curso_mais_elevado_frequentou_nc2 <- ifelse(prop_cursos$curso_mais_elevado_frequentou_nc2 == 4, "Alfabetização de Jovens Adultos", prop_cursos$curso_mais_elevado_frequentou_nc2)
prop_cursos$curso_mais_elevado_frequentou_nc2 <- ifelse(prop_cursos$curso_mais_elevado_frequentou_nc2 == 5, "Primário (Fundamental 1)", prop_cursos$curso_mais_elevado_frequentou_nc2)
prop_cursos$curso_mais_elevado_frequentou_nc2 <- ifelse(prop_cursos$curso_mais_elevado_frequentou_nc2 == 6, "Ginásio (Fundamental 2)", prop_cursos$curso_mais_elevado_frequentou_nc2)
prop_cursos$curso_mais_elevado_frequentou_nc2 <- ifelse(prop_cursos$curso_mais_elevado_frequentou_nc2 == 7, "Ensino Fundamental Regular", prop_cursos$curso_mais_elevado_frequentou_nc2)
prop_cursos$curso_mais_elevado_frequentou_nc2 <- ifelse(prop_cursos$curso_mais_elevado_frequentou_nc2 == 8, "EJA Ensino Fundamental", prop_cursos$curso_mais_elevado_frequentou_nc2)
prop_cursos$curso_mais_elevado_frequentou_nc2 <- ifelse(prop_cursos$curso_mais_elevado_frequentou_nc2 == 9, "Colegial (CA)", prop_cursos$curso_mais_elevado_frequentou_nc2)
prop_cursos$curso_mais_elevado_frequentou_nc2 <- ifelse(prop_cursos$curso_mais_elevado_frequentou_nc2 == 10, "Ensino Médio Regular", prop_cursos$curso_mais_elevado_frequentou_nc2)
prop_cursos$curso_mais_elevado_frequentou_nc2 <- ifelse(prop_cursos$curso_mais_elevado_frequentou_nc2 == 11, "EJA Ensino Médio", prop_cursos$curso_mais_elevado_frequentou_nc2)
prop_cursos$curso_mais_elevado_frequentou_nc2 <- ifelse(prop_cursos$curso_mais_elevado_frequentou_nc2 == 12, "Graduação", prop_cursos$curso_mais_elevado_frequentou_nc2)
prop_cursos$curso_mais_elevado_frequentou_nc2 <- ifelse(prop_cursos$curso_mais_elevado_frequentou_nc2 == 13, "Especialização de Nível Superior", prop_cursos$curso_mais_elevado_frequentou_nc2)
prop_cursos$curso_mais_elevado_frequentou_nc2 <- ifelse(prop_cursos$curso_mais_elevado_frequentou_nc2 == 14, "Mestrado", prop_cursos$curso_mais_elevado_frequentou_nc2)
prop_cursos$curso_mais_elevado_frequentou_nc2 <- ifelse(prop_cursos$curso_mais_elevado_frequentou_nc2 == 15, "Doutorado", prop_cursos$curso_mais_elevado_frequentou_nc2)

prop_cursos$conclusao_curso_elevado2 <- ifelse(prop_cursos$conclusao_curso_elevado2 == 0, "Não Aplicável", prop_cursos$conclusao_curso_elevado2)
prop_cursos$conclusao_curso_elevado2 <- ifelse(prop_cursos$conclusao_curso_elevado2 == 1, "Sim", prop_cursos$conclusao_curso_elevado2)
prop_cursos$conclusao_curso_elevado2 <- ifelse(prop_cursos$conclusao_curso_elevado2 == 2, "Não", prop_cursos$conclusao_curso_elevado2)

prop_cursos <- prop_cursos %>%
  pivot_wider(names_from = frequenta_qual_curso_nc2, values_from = c(prop_cursos$frequenta_qual_curso_nc,
                                                                     prop_cursos$curso_mais_elevado_frequentou_nc,
                                                                     prop_cursos$conclusao_curso_elevado))


prop_alfab$Alfabetizado <- ifelse(prop_alfab$ler_escrever == 1,"1",prop_alfab$Alfabetizado)


prop_Não_Alfabetizado <-  aggregate(Não_Alfabetizado ~ Ano + UF , data = prop_alfab, length )

prop_Alfabetização <- full_join(prop_Não_Alfabetizado,prop_Alfabetizado, by=c("Ano","UF"))

prop_Alfabetização <- prop_Alfabetização %>%
  mutate(prop_alf = (Alfabetizado/(Alfabetizado+Não_Alfabetizado))) %>%
  select(-Não_Alfabetizado,-Alfabetizado)

