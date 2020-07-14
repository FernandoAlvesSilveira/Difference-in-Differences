### Paper Uber
library(tidyverse)
library(plm)
library(lmtest)
library(robust)
library(sandwich)
library(texreg)

#Importa base / abra e explore os dados
Dados_CNAE <- read_excel("Artigo Uber dif-in-dif/Data/Dados CNAE.xlsx")
dados_cnpj <- Dados_CNAE

#Limpa linhas de cabeçalho
dados_cnpj <- dados_cnpj[5:682,]

tail(dados_cnpj)

#Captura cada tabela/ano e atribui em nova variavel 
x_2010 <- dados_cnpj[1:678,1:10]
x_2011 <- dados_cnpj[1:678,11:19]
x_2012 <- dados_cnpj[1:678,20:28]
x_2013 <- dados_cnpj[1:678,29:37]
x_2014 <- dados_cnpj[1:678,38:46]
x_2015 <- dados_cnpj[1:678,47:55]
x_2016 <- dados_cnpj[1:678,56:64]
x_2017 <- dados_cnpj[1:678,65:73]
x_2018 <- dados_cnpj[1:678,74:82]

#Seta na avariavel "X" para depois preencher o nome das colunas
x <- x_2018

#Copia e adiciona uma coluna "Cidade" para cada nova coluna adicionada
x <- add_column(x, df_cnpjs$Cidade[1:678], .before = "...74")

colnames(x)[1] <- "Cidade";
colnames(x)[2] <- "Total";
colnames(x)[3] <- "Comercio";
colnames(x)[4] <- "Transporte";
colnames(x)[5] <- "Alojamento";
colnames(x)[6] <- "Alimentacao";
colnames(x)[7] <- "Artes";
colnames(x)[8] <- "Lazer";
colnames(x)[9] <- "Populacao";
colnames(x)[10] <- "PIB"

#Adiciona coluna "Ano"
x <- add_column(x, Ano = 2018, .before = "Total")

#df_cnpjs <- x
#Atribui a tabela no data frame alvo
df_cnpjs <-df_cnpjs %>% add_row(
  Cidade = x$Cidade[1:678],
  Ano = x$Ano[1:678],
  Total = x$Total[1:678],
  Comercio = x$Comercio[1:678],
  Transporte = x$Transporte[1:678],
  Alojamento = x$Alojamento[1:678],
  Alimentacao = x$Alimentacao[1:678],
  Artes = x$Artes[1:678],
  Lazer = x$Lazer[1:678],
  Populacao = x$Populacao[1:678],
  PIB = x$PIB[1:678]
)

#Verifique o estado do data frame
head(df_cnpjs)
tail(df_cnpjs)

#Atribui uma variavel backup / df_cnpjs_bkp <- df_cnpjs
#Atribui de forma coercitiva os valores em "numeric"
df_cnpjs$Total <- as.numeric(df_cnpjs$Total)
df_cnpjs$Comercio <- as.numeric(df_cnpjs$Comercio)
df_cnpjs$Transporte <- as.numeric(df_cnpjs$Transporte)
df_cnpjs$Alojamento <- as.numeric(df_cnpjs$Alojamento)
df_cnpjs$Alimentacao <- as.numeric(df_cnpjs$Alimentacao)
df_cnpjs$Artes <- as.numeric(df_cnpjs$Artes)
df_cnpjs$Lazer <- as.numeric(df_cnpjs$Lazer)
df_cnpjs$Populacao <- as.numeric(df_cnpjs$Populacao)
df_cnpjs$PIB <- as.numeric(df_cnpjs$PIB)

df_reg$dummy_time <- as.numeric(df_reg$dummy_time)

df_reg <- df_cnpjs

#Preparação do dataset para dif-in-dif

#cria a dummy time para o periodo da difereça
df_reg$dummy_time = ifelse(df_reg$Ano >= 2016, 1, 0)

#cria a dummy uber para indicar as cidades que possuem o serviço do uber
df_reg$Uber = ifelse(df_reg$Cidade == "Rio de Janeiro (RJ)" | df_reg$Cidade == "Santos (SP)" | df_reg$Cidade == "Brasília (DF)" | 
                       df_reg$Cidade == "Maceió (AL)" | df_reg$Cidade == "Jundiaí (SP)" | df_reg$Cidade == "Juiz de Fora (MG)" |
                       df_reg$Cidade == "Guarulhos (SP)" | df_reg$Cidade == "Mauá (SP)" | df_reg$Cidade == "Mogi das Cruzes (SP)" | df_reg$Cidade == "São José dos Campos (SP)" |
                       df_reg$Cidade == "São Paulo (SP)" | df_reg$Cidade == "Vitória (ES)" | df_reg$Cidade == "Goiânia (GO)" | df_reg$Cidade == "Campo Grande (MS)" | df_reg$Cidade == "Porto Alegre (RS)" |
                       df_reg$Cidade == "Uberlândia (SP)" | df_reg$Cidade == "São Bernardo do Campo (SP)"| df_reg$Cidade == "São Cetano do SUl (SP)" | df_reg$Cidade == "Carapicuíba (SP)" | df_reg$Cidade =="Aracaju (SE)" |
                       df_reg$Cidade == "Londrina (PR)" | df_reg$Cidade == "São Luís (MA)" | df_reg$Cidade == "Belo Horizonte (MG)" | df_reg$Cidade == "Curitiba (PR)" | df_reg$Cidade == "Cuiabá (MT)" |
                       df_reg$Cidade == "Recife (PE)" | df_reg$Cidade == "Osasco (SP)" | df_reg$Cidade == "Santo André (SP)" | df_reg$Cidade == "Cotia (SP)" | df_reg$Cidade == "Florianópolis (SC)" |
                       df_reg$Cidade == "Montes Claros (MG)"| df_reg$Cidade == "Teresina (PI)" | df_reg$Cidade == "Blumenau (SC)" | df_reg$Cidade == "Joinville (SC)" | df_reg$Cidade == "Campinas (SP)" |
                       df_reg$Cidade == "Natal (RN)"| df_reg$Cidade == "Fortaleza (CE)" | df_reg$Cidade == "Salvador (BA)" | df_reg$Cidade == "Sorocaba (SP)" | df_reg$Cidade == "Barueri (SP)" |
                       df_reg$Cidade == "Diadema (SP)" | df_reg$Cidade == "Suzano (SP)" | df_reg$Cidade == "João Pessoa (PB)" | df_reg$Cidade == "Ribeirão Preto (SP)" |
                       df_reg$Cidade == "Uberaba (MG)", 1, 0)


#cria a dummy do efeito uber
df_reg$Efeito_uber = df_reg$dummy_time * df_reg$Uber

#check dataset
head(df_reg)

###Descreve as equações
equacao_01 <- log(Total) ~ Efeito_uber + PIB_percapta + log(Populacao)
equacao_02 <- log(Total) ~ dummy_time*Uber + log(PIB) + log(Populacao)
equacao_03 <- log(Comercio) ~ dummy_time*Uber + log(PIB) + log(Populacao)
equacao_04 <- log(Transporte) ~ dummy_time*Uber + log(PIB) + log(Populacao)
equacao_05 <- log(Alojamento) ~ dummy_time*Uber + log(PIB) + log(Populacao)
equacao_06 <- log(Alimentacao) ~ dummy_time*Uber + log(PIB) + log(Populacao)
equacao_07 <- log(Artes) ~ dummy_time*Uber + log(PIB) + log(Populacao)
equacao_08 <- log(Lazer) ~ dummy_time*Uber + log(PIB) + log(Populacao)

#Escreve os modelos a serem analisados (dados em painel e ols robust)
panel_equacao_01 <- plm(equacao_0001, data = df_reg, model = "within", index = c("Cidade", "Ano"))
panel_equacao_0002 <- plm(equacao_0002, data = df_reg, model = "within", index = c("Cidade", "Ano"))
panel_equacao_0003 <- plm(equacao_0003, data = df_reg, model = "within", index = c("Cidade", "Ano"))
panel_equacao_0004 <- plm(equacao_0004, data = df_reg, model = "within", index = c("Cidade", "Ano"))
panel_equacao_0005 <- plm(equacao_0005, data = df_reg, model = "within", index = c("Cidade", "Ano"))
panel_equacao_0006 <- plm(equacao_0006, data = df_reg, model = "within", index = c("Cidade", "Ano"))
panel_equacao_0007 <- plm(equacao_0007, data = df_reg, model = "within", index = c("Cidade", "Ano"))
panel_equacao_0008 <- plm(equacao_0008, data = df_reg, model = "within", index = c("Cidade", "Ano"))

panel_equacao_01 <- plm(equacao_01, data = df_reg, model = "within", index = c("Cidade", "Ano"))
panel_equacao_02 <- plm(equacao_02, data = df_reg, model = "within", index = c("Cidade", "Ano"))
panel_equacao_03 <- plm(equacao_03, data = df_reg, model = "within", index = c("Cidade", "Ano"))
panel_equacao_04 <- plm(equacao_04, data = df_reg, model = "within", index = c("Cidade", "Ano"))
panel_equacao_05 <- plm(equacao_05, data = df_reg, model = "within", index = c("Cidade", "Ano"))
panel_equacao_06 <- plm(equacao_06, data = df_reg, model = "within", index = c("Cidade", "Ano"))
panel_equacao_07 <- plm(equacao_07, data = df_reg, model = "within", index = c("Cidade", "Ano"))
panel_equacao_08 <- plm(equacao_08, data = df_reg, model = "within", index = c("Cidade", "Ano"))


#Ajuste de heterocedasticidade de white do pacote sandwich
panel_equacao_01_white <- coeftest(panel_equacao_01, vcov.=vcovHC(panel_equacao_01, type="HC1"))
panel_equacao_02_white <- coeftest(panel_equacao_02, vcov.=vcovHC(panel_equacao_02, type="HC1"))
panel_equacao_03_white <- coeftest(panel_equacao_03, vcov.=vcovHC(panel_equacao_03, type="HC1"))
panel_equacao_04_white <- coeftest(panel_equacao_04, vcov.=vcovHC(panel_equacao_04, type="HC1"))
panel_equacao_05_white <- coeftest(panel_equacao_05, vcov.=vcovHC(panel_equacao_05, type="HC1"))
panel_equacao_06_white <- coeftest(panel_equacao_06, vcov.=vcovHC(panel_equacao_06, type="HC1"))
panel_equacao_07_white <- coeftest(panel_equacao_07, vcov.=vcovHC(panel_equacao_07, type="HC1"))
panel_equacao_08_white <- coeftest(panel_equacao_08, vcov.=vcovHC(panel_equacao_08, type="HC1"))

#verifica os residos
shapiro.test(panel_equacao_01$residuals)
hist(panel_equacao_01$residuals)


ols_equacao_01 <- lmRob(equacao_01, data = df_reg)
ols_equacao_02 <- lmRob(equacao_02, data = df_reg)
ols_equacao_03 <- lmRob(equacao_03, data = df_reg)
ols_equacao_04 <- lmRob(equacao_04, data = df_reg)
ols_equacao_05 <- lmRob(equacao_05, data = df_reg)
ols_equacao_06 <- lmRob(equacao_06, data = df_reg)
ols_equacao_07 <- lmRob(equacao_07, data = df_reg)
ols_equacao_08 <- lmRob(equacao_08, data = df_reg)

#Visualiza os modelos
summary(panel_equacao_01)
summary(panel_equacao_02)
summary(panel_equacao_03)
summary(panel_equacao_04)
summary(panel_equacao_05)
summary(panel_equacao_06)
summary(panel_equacao_07)
summary(panel_equacao_08)

summary(ols_equacao_01)
summary(ols_equacao_02)
summary(ols_equacao_03)
summary(ols_equacao_04)
summary(ols_equacao_05)
summary(ols_equacao_06)
summary(ols_equacao_07)
summary(ols_equacao_08)
