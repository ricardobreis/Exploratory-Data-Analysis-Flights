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

# Limpando o console.
cat("\014") 
# Limpando o Global Environment.
rm(list = ls())


# Leitura de Dados --------------------------------------------------------

flights <- read.csv("~/R-Projetos/Exploratory-Data-Analysis-Flights/data/raw/resumo_anual_2020.csv", encoding="UTF-8", sep=";", stringsAsFactors = T)
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

# Transforming character columns in factor
flights$empresa_nome                  <- factor(flights$empresa_nome)
flights$aeroporto_origem_nome         <- factor(flights$aeroporto_origem_nome)
flights$aeroporto_origem_pais         <- factor(flights$aeroporto_origem_pais)
flights$aeroporto_origem_continente   <- factor(flights$aeroporto_origem_continente)
flights$aeroporto_destino_nome        <- factor(flights$aeroporto_destino_nome)
flights$aeroporto_destino_pais        <- factor(flights$aeroporto_destino_pais)
flights$aeroporto_destino_continente  <- factor(flights$aeroporto_destino_continente)
flights$grupo_voo                     <- factor(flights$grupo_voo)

levels(flights$aeroporto_destino_pais)
