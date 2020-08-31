################################################################################################
#
# ANÁLISE DE DADOS
# Por: RICARDO REIS
#
# CASE - FLIGHTS
#
#
################################################################################################


# Carrega Pacotes ---------------------------------------------------------

library(dplyr)
library(tidyr)
library(lubridate)
library(zoo)
library(ggplot2)
library(plotly)
library(wordcloud2)
library(forcats)
library(geobr)
library(sf)
library(tmap)

# Limpando o console.
cat("\014") 
# Limpando o Global Environment.
rm(list = ls())


# Leitura de Dados --------------------------------------------------------

flights <- read.csv("~/R-Projetos/Exploratory-Data-Analysis-Flights/data/raw/resumo_anual_2020.csv", encoding="UTF-8", sep=";")
glimpse(flights)
head(flights)
summary(flights)


# Tidying  ----------------------------------------------------------------

# Renaming
names(flights)
flights <- rename(flights, empresa_sigla = EMPRESA..SIGLA.)
flights <- rename(flights, empresa_nome = EMPRESA..NOME.)
flights <- rename(flights, empresa_nacionalidade = EMPRESA..NACIONALIDADE.)
flights <- rename(flights, ano = ANO)
flights <- rename(flights, mes = M.ca.S)
flights <- rename(flights, aeroporto_origem_sigla = AEROPORTO.DE.ORIGEM..SIGLA.)
flights <- rename(flights, aeroporto_origem_nome = AEROPORTO.DE.ORIGEM..NOME.)
flights <- rename(flights, aeroporto_origem_uf = AEROPORTO.DE.ORIGEM..UF.)
flights <- rename(flights, aeroporto_origem_regiao = AEROPORTO.DE.ORIGEM..REGI.c3.O.)
flights <- rename(flights, aeroporto_origem_pais = AEROPORTO.DE.ORIGEM..PA.cd.S.)
flights <- rename(flights, aeroporto_origem_continente = AEROPORTO.DE.ORIGEM..CONTINENTE.)
flights <- rename(flights, aeroporto_destino_sigla = AEROPORTO.DE.DESTINO..SIGLA.)
flights <- rename(flights, aeroporto_destino_nome = AEROPORTO.DE.DESTINO..NOME.)
flights <- rename(flights, aeroporto_destino_uf = AEROPORTO.DE.DESTINO..UF.)
flights <- rename(flights, aeroporto_destino_regiao = AEROPORTO.DE.DESTINO..REGI.c3.O.)
flights <- rename(flights, aeroporto_destino_pais = AEROPORTO.DE.DESTINO..PA.cd.S.)
flights <- rename(flights, aeroporto_destino_continente = AEROPORTO.DE.DESTINO..CONTINENTE.)
flights <- rename(flights, natureza = NATUREZA)
flights <- rename(flights, grupo_voo = GRUPO.DE.VOO)
flights <- rename(flights, passageiros_pagos = PASSAGEIROS.PAGOS)
flights <- rename(flights, passageiros_gratis = PASSAGEIROS.GR.c1.TIS)
flights <- rename(flights, carga_paga_kg = CARGA.PAGA..KG.)
flights <- rename(flights, carga_gratis_kg = CARGA.GR.c1.TIS..KG.)
flights <- rename(flights, correio_kg = CORREIO..KG.)
flights <- rename(flights, ask = ASK)
flights <- rename(flights, rpk = RPK)
flights <- rename(flights, atk = ATK)
flights <- rename(flights, rtk = RTK)
flights <- rename(flights, combustivel_litros = COMBUST.cd.VEL..LITROS.)
flights <- rename(flights, distancia_voada_km = DIST.c2.NCIA.VOADA..KM.)
flights <- rename(flights, decolagens = DECOLAGENS)
flights <- rename(flights, carga_paga_km = CARGA.PAGA.KM)
flights <- rename(flights, carga_gratis_km = CARGA.GRATIS.KM)
flights <- rename(flights, correio_km = CORREIO.KM)
flights <- rename(flights, assentos = ASSENTOS)
flights <- rename(flights, payload = PAYLOAD)
flights <- rename(flights, horas_voadas = HORAS.VOADAS)
flights <- rename(flights, bagagem_km = BAGAGEM..KG.)

# Correcting encoding
flights$empresa_nome                  <- iconv(flights$empresa_nome, "latin1", "UTF-8")
flights$aeroporto_origem_nome         <- iconv(flights$aeroporto_origem_nome, "latin1", "UTF-8")
flights$aeroporto_origem_pais         <- iconv(flights$aeroporto_origem_pais, "latin1", "UTF-8")
flights$aeroporto_origem_continente   <- iconv(flights$aeroporto_origem_continente, "latin1", "UTF-8")
flights$aeroporto_destino_nome        <- iconv(flights$aeroporto_destino_nome, "latin1", "UTF-8")
flights$aeroporto_destino_pais        <- iconv(flights$aeroporto_destino_pais, "latin1", "UTF-8")
flights$aeroporto_destino_continente  <- iconv(flights$aeroporto_destino_continente, "latin1", "UTF-8")
flights$grupo_voo                     <- iconv(flights$grupo_voo, "latin1", "UTF-8")
flights$natureza                      <- iconv(flights$natureza, "latin1", "UTF-8")

# Transforming character columns in factor
flights$empresa_nome                  <- factor(flights$empresa_nome)
flights$aeroporto_origem_nome         <- factor(flights$aeroporto_origem_nome)
flights$aeroporto_origem_pais         <- factor(flights$aeroporto_origem_pais)
flights$aeroporto_origem_continente   <- factor(flights$aeroporto_origem_continente)
flights$aeroporto_destino_nome        <- factor(flights$aeroporto_destino_nome)
flights$aeroporto_destino_pais        <- factor(flights$aeroporto_destino_pais)
flights$aeroporto_destino_continente  <- factor(flights$aeroporto_destino_continente)
flights$grupo_voo                     <- factor(flights$grupo_voo)
flights$natureza                      <- factor(flights$natureza)

levels(flights$grupo_voo)

# Formating horas_voadas
flights$horas_voadas <- gsub(",", ".", flights$horas_voadas)
flights$horas_voadas <- as.numeric(flights$horas_voadas,digits=15)


# Filtering data
brazilian_domestic_flights <- subset(flights, empresa_nacionalidade == "BRASILEIRA" & natureza == "DOMÉSTICA" & grupo_voo != "IMPRODUTIVO" & empresa_sigla %in% c("AZU", "GLO", "TAM"))


# Análise -----------------------------------------------------------------

# Decolagens por mês por companhia
decolagens_mes_companhia <- brazilian_domestic_flights %>%
  group_by(empresa_sigla, mes) %>%
  summarise(n = sum(decolagens, na.rm = TRUE))

ggplot(decolagens_mes_companhia, aes(x = mes, y = n, color = empresa_sigla)) +
  geom_line()

# Passageiros pagos por mês por companhia
passageiros_pagos_mes_companhia <- brazilian_domestic_flights %>%
  group_by(empresa_sigla, mes) %>%
  summarise(n = sum(passageiros_pagos, na.rm = TRUE))

ggplot(passageiros_pagos_mes_companhia, aes(x = mes, y = n, color = empresa_sigla)) +
  geom_line() +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))

# Passageiros gratis por mês por companhia
passageiros_gratis_mes_companhia <- brazilian_domestic_flights %>%
  group_by(empresa_sigla, mes) %>%
  summarise(n = sum(passageiros_gratis, na.rm = TRUE))

ggplot(passageiros_gratis_mes_companhia, aes(x = mes, y = n, color = empresa_sigla)) +
  geom_line() +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))

# Passageiros total por mês por companhia
brazilian_domestic_flights$passageiros_total <- brazilian_domestic_flights$passageiros_pagos + brazilian_domestic_flights$passageiros_gratis

passageiros_total_mes_companhia <- brazilian_domestic_flights %>%
  group_by(empresa_sigla, mes) %>%
  summarise(n = sum(passageiros_total, na.rm = TRUE))

ggplot(passageiros_total_mes_companhia, aes(x = mes, y = n, color = empresa_sigla)) +
  geom_line() +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))

# Carga paga (em kg) por mês por companhia
carga_paga_mes_companhia <- brazilian_domestic_flights %>%
  group_by(empresa_sigla, mes) %>%
  summarise(n = sum(carga_paga_kg, na.rm = TRUE))

ggplot(carga_paga_mes_companhia, aes(x = mes, y = n, color = empresa_sigla)) +
  geom_line() +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))

# Carga gratis (em kg) por mês por companhia
carga_gratis_mes_companhia <- brazilian_domestic_flights %>%
  group_by(empresa_sigla, mes) %>%
  summarise(n = sum(carga_gratis_kg, na.rm = TRUE))

ggplot(carga_gratis_mes_companhia, aes(x = mes, y = n, color = empresa_sigla)) +
  geom_line() +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))

# Combustível (em litros) por mês por companhia
combustivel_mes_companhia <- brazilian_domestic_flights %>%
  group_by(empresa_sigla, mes) %>%
  summarise(n = sum(combustivel_litros, na.rm = TRUE))

ggplot(combustivel_mes_companhia, aes(x = mes, y = n, color = empresa_sigla)) +
  geom_line() +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))

# Distância voada (em kg) por mês por companhia
distancia_mes_companhia <- brazilian_domestic_flights %>%
  group_by(empresa_sigla, mes) %>%
  summarise(n = sum(distancia_voada_km, na.rm = TRUE))

ggplot(distancia_mes_companhia, aes(x = mes, y = n, color = empresa_sigla)) +
  geom_line() +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))

# ASK por mês por companhia
ask_mes_companhia <- brazilian_domestic_flights %>%
  group_by(empresa_sigla, mes) %>%
  summarise(n = sum(ask, na.rm = TRUE))

ggplot(ask_mes_companhia, aes(x = mes, y = n, color = empresa_sigla)) +
  geom_line() +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))

# RPK por mês por companhia
rpk_mes_companhia <- brazilian_domestic_flights %>%
  group_by(empresa_sigla, mes) %>%
  summarise(n = sum(rpk, na.rm = TRUE))

ggplot(rpk_mes_companhia, aes(x = mes, y = n, color = empresa_sigla)) +
  geom_line() +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))

# Horas voadas por mês por companhia
horas_voadas_mes_companhia <- brazilian_domestic_flights %>%
  group_by(empresa_sigla, mes) %>%
  summarise(n = sum(horas_voadas, na.rm = TRUE))

ggplot(horas_voadas_mes_companhia, aes(x = mes, y = n, color = empresa_sigla)) +
  geom_line() +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))


# Análise Espacial --------------------------------------------------------

uf <- read_state(code_state="all", year=2018)
plot(uf) # Plota todas as colunas
plot(st_geometry(uf)) # Plota apenas a geometria
head(uf)
class(uf$geom)

# Decolagens por estado
decolagens_estado <- brazilian_domestic_flights %>%
  group_by(aeroporto_origem_uf) %>%
  summarise(n = sum(decolagens, na.rm = TRUE)) 

uf_decolagens <- inner_join(uf, decolagens_estado, by= c("abbrev_state" = "aeroporto_origem_uf"))

#Seta o título do gráfico
titulo = "Decolagens por UF"

#Seta o os valores das quebras na legenda
breaks = c(500, 1000, 5000, 10000, 20000, 50000) 

#Primeira layer = Mapa ddo Brasil
tm_shape(uf_decolagens) +
  
  #Utiliza as cores para montar o mapa de calor baseado na coluna n
  tm_fill(col = "n", title = titulo, palette = "BuGn", breaks = breaks) +
  
  #Insere bordas para facilitar a visualização das áreas
  tm_borders() 
