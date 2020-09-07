################################################################################################
#
# ANÁLISE DE DADOS
# Por: RICARDO REIS
#
# CASE - FLIGHTS
#
#
################################################################################################

source("~/R-Projetos/Exploratory-Data-Analysis-Flights/src/data_preparation.R")


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


# Decolagens por estado
decolagens_estado <- brazilian_domestic_flights %>%
  group_by(aeroporto_origem_uf) %>%
  summarise(n = sum(decolagens, na.rm = TRUE)) 

uf_decolagens <- inner_join(uf, decolagens_estado, by= c("abbrv_s" = "aeroporto_origem_uf"))

tm_shape(uf_decolagens) +
  tm_fill(palette = "YlOrRd", col = "n", title = "Decolagens por UF", breaks = c(0, 500, 1000, 5000, 10000, 20000, 50000)) +
  tm_borders(alpha = 0.2) +
  tm_layout(legend.title.size = 1.2, legend.position = c("left", "bottom"), frame = FALSE)
  
  #tm_layout(bg.color = "lightblue") + 
  
  #tm_layout(frame = FALSE)

# Decolagens por estado por mes
decolagens_estado_mes <- brazilian_domestic_flights %>%
  group_by(aeroporto_origem_uf, mes) %>%
  summarise(n = sum(decolagens, na.rm = TRUE)) 

uf_mes_decolagens <- inner_join(uf, decolagens_estado_mes, by= c("abbrv_s" = "aeroporto_origem_uf"))

tm_shape(uf_mes_decolagens) +
  tm_fill(palette = "YlOrRd", col = "n", title = "Decolagens por UF por Mês", breaks = c(0, 500, 1000, 5000, 10000, 20000, 50000)) +
  tm_borders(alpha = 0.2) +
  tm_facets(by = "mes", free.coords = FALSE)

# Decolagens por estado por mes Gol
decolagens_estado_mes_gol <- brazilian_domestic_flights %>%
  filter(empresa_sigla == "GLO") %>%
  group_by(aeroporto_origem_uf, mes_factor) %>%
  summarise(n = sum(decolagens, na.rm = TRUE)) 

uf_mes_decolagens_gol <- inner_join(uf, decolagens_estado_mes_gol, by= c("abbrv_s" = "aeroporto_origem_uf"))

tm_shape(uf_mes_decolagens_gol) +
  tm_fill(palette = "YlOrRd", col = "n", title = "Partidas - 2020 (Gol)", breaks = c(0, 50, 100, 500, 1000, 5000, 10000)) +
  tm_borders(alpha = 0.2) +
  tm_facets(by = "mes_factor", free.coords = FALSE) +
  tm_layout(legend.title.size = 1.3)

# Decolagens por estado por mes Latam
decolagens_estado_mes_tam <- brazilian_domestic_flights %>%
  filter(empresa_sigla == "TAM") %>%
  group_by(aeroporto_origem_uf, mes_factor) %>%
  summarise(n = sum(decolagens, na.rm = TRUE)) 

uf_mes_decolagens_tam <- inner_join(uf, decolagens_estado_mes_tam, by= c("abbrv_s" = "aeroporto_origem_uf"))

tm_shape(uf_mes_decolagens_tam) +
  tm_fill(palette = "YlOrRd", col = "n", title = "Partidas - 2020 (Tam)", breaks = c(0, 50, 100, 500, 1000, 5000, 10000)) +
  tm_borders(alpha = 0.2) +
  tm_facets(by = "mes_factor", free.coords = FALSE) +
  tm_layout(legend.title.size = 1.3)

# Decolagens por estado por mes Azul
decolagens_estado_mes_azu <- brazilian_domestic_flights %>%
  filter(empresa_sigla == "AZU") %>%
  group_by(aeroporto_origem_uf, mes_factor) %>%
  summarise(n = sum(decolagens, na.rm = TRUE)) 

uf_mes_decolagens_azu <- inner_join(uf, decolagens_estado_mes_azu, by= c("abbrv_s" = "aeroporto_origem_uf"))

tm_shape(uf_mes_decolagens_azu) +
  tm_fill(palette = "YlOrRd", col = "n", title = "Partidas - 2020 (Azul)", breaks = c(0, 50, 100, 500, 1000, 5000, 10000)) +
  tm_borders(alpha = 0.2) +
  tm_facets(by = "mes_factor", free.coords = FALSE) +
  tm_layout(legend.title.size = 1.3)

# Passageiros pagos por estado por mes Gol
passageiros_pagos_estado_mes_gol <- brazilian_domestic_flights %>%
  filter(empresa_sigla == "GLO") %>%
  group_by(aeroporto_origem_uf, mes_factor) %>%
  summarise(n = sum(passageiros_pagos, na.rm = TRUE)) 

uf_mes_passageiros_pagos_gol <- inner_join(uf, passageiros_pagos_estado_mes_gol, by= c("abbrv_s" = "aeroporto_origem_uf"))

tm_shape(uf_mes_passageiros_pagos_gol) +
  tm_fill(palette = "YlOrRd", col = "n", title = "Passageiros Pagos - Gol", breaks = c(0, 10000, 50000, 100000, 500000, 1000000)) +
  tm_borders(alpha = 0.2) +
  tm_facets(by = "mes_factor", free.coords = FALSE) +
  tm_layout(legend.title.size = 1.1)

# Passageiros pagos por estado por mes Latam
passageiros_pagos_estado_mes_tam <- brazilian_domestic_flights %>%
  filter(empresa_sigla == "TAM") %>%
  group_by(aeroporto_origem_uf, mes_factor) %>%
  summarise(n = sum(passageiros_pagos, na.rm = TRUE)) 

uf_mes_passageiros_pagos_tam <- inner_join(uf, passageiros_pagos_estado_mes_tam, by= c("abbrv_s" = "aeroporto_origem_uf"))

tm_shape(uf_mes_passageiros_pagos_tam) +
  tm_fill(palette = "YlOrRd", col = "n", title = "Passageiros Pagos - Tam", breaks = c(0, 10000, 50000, 100000, 500000, 1000000)) +
  tm_borders(alpha = 0.2) +
  tm_facets(by = "mes_factor", free.coords = FALSE) +
  tm_layout(legend.title.size = 1.1)

# Passageiros pagos por estado por mes Azul
passageiros_pagos_estado_mes_azu <- brazilian_domestic_flights %>%
  filter(empresa_sigla == "AZU") %>%
  group_by(aeroporto_origem_uf, mes_factor) %>%
  summarise(n = sum(passageiros_pagos, na.rm = TRUE)) 

uf_mes_passageiros_pagos_azu <- inner_join(uf, passageiros_pagos_estado_mes_azu, by= c("abbrv_s" = "aeroporto_origem_uf"))

tm_shape(uf_mes_passageiros_pagos_azu) +
  tm_fill(palette = "YlOrRd", col = "n", title = "Passageiros Pagos - Azul", breaks = c(0, 10000, 50000, 100000, 500000, 1000000)) +
  tm_borders(alpha = 0.2) +
  tm_facets(by = "mes_factor", free.coords = FALSE) +
  tm_layout(legend.title.size = 1.1)










# Gera o shapefile do dataframe sf
st_write(uf_mes_decolagens, "~/R-Projetos/Exploratory-Data-Analysis-Flights/data/raw/a.shp")
