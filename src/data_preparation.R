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
library(RColorBrewer)
display.brewer.all()

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

# Changing months label
brazilian_domestic_flights$mes_factor <- factor(brazilian_domestic_flights$mes, levels = c(1, 2, 3, 4, 5, 6, 7), labels = c("Jan", "Fev", "Mar", "Abr", "Mai", "Jun", "Jul"))


# Dados Espaciais ---------------------------------------------------------
# uf <- read_state(code_state="all", year=2018)
# plot(uf) # Plota todas as colunas
# plot(st_geometry(uf)) # Plota apenas a geometria
# head(uf)
# class(uf$geom)

# Gera o shapefile do dataframe sf
#st_write(uf, "~/R-Projetos/Exploratory-Data-Analysis-Flights/data/raw/uf.shp")

# Carrega o shapefile
uf <- st_read("~/R-Projetos/Exploratory-Data-Analysis-Flights/data/raw/uf.shp")

# Gol, Latam, Azul
uf_aereas <- brazilian_domestic_flights %>%
  group_by(empresa_sigla, aeroporto_origem_uf, mes_factor) %>%
  summarise(decolagens = sum(decolagens, na.rm = TRUE), 
            passageiros_pagos = sum(passageiros_pagos, na.rm = TRUE),
            passageiros_gratis = sum(passageiros_gratis, na.rm = TRUE),
            carga_paga_kg = sum(carga_paga_kg, na.rm = TRUE),
            carga_gratis_kg = sum(carga_gratis_kg, na.rm = TRUE),
            ask = sum(ask, na.rm = TRUE),
            rpk = sum(rpk, na.rm = TRUE),
            combustivel_litros = sum(combustivel_litros, na.rm = TRUE),
            assentos = sum(assentos, na.rm = TRUE))

uf_aereas_shp <- inner_join(uf, uf_aereas, by= c("abbrv_s" = "aeroporto_origem_uf"))

st_write(uf_aereas_shp, "~/R-Projetos/Exploratory-Data-Analysis-Flights/data/processed/uf_aereas_shp.shp")

# Gol
uf_gol <- brazilian_domestic_flights %>%
  filter(empresa_sigla == "GLO") %>%
  group_by(aeroporto_origem_uf, mes_factor) %>%
  summarise(decolagens = sum(decolagens, na.rm = TRUE), 
            passageiros_pagos = sum(passageiros_pagos, na.rm = TRUE),
            passageiros_gratis = sum(passageiros_gratis, na.rm = TRUE),
            carga_paga_kg = sum(carga_paga_kg, na.rm = TRUE),
            carga_gratis_kg = sum(carga_gratis_kg, na.rm = TRUE),
            ask = sum(ask, na.rm = TRUE),
            rpk = sum(rpk, na.rm = TRUE),
            combustivel_litros = sum(combustivel_litros, na.rm = TRUE),
            assentos = sum(assentos, na.rm = TRUE))

uf_gol_shp <- inner_join(uf, uf_gol, by= c("abbrv_s" = "aeroporto_origem_uf"))

st_write(uf_gol_shp, "~/R-Projetos/Exploratory-Data-Analysis-Flights/data/processed/uf_gol_shp.shp")
