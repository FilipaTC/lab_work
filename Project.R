# =============================================================================
# PIPELINE SARIMAX — Análise de Absentismo por Conveniência
# Autores: Ana Correia, André Vicente, Filipa Carneiro
# =============================================================================

# -----------------------------------------------------------------------------
# 0. PACOTES NECESSÁRIOS
# -----------------------------------------------------------------------------
# install.packages(c("tidyverse", "lubridate", "forecast", "tseries", "xts", "openxlsx"))

library(tidyverse)    # manipulação de dados
library(lubridate)    # datas
library(forecast)     # ARIMA / SARIMAX (auto.arima)
library(tseries)      # testes ADF (estacionaridade)
library(xts)          # séries temporais indexadas por data
library(openxlsx)     # exportar tabela mestra para Excel (opcional)

# -----------------------------------------------------------------------------
# 1. CARREGAR OS DOIS CSVs
# -----------------------------------------------------------------------------
add_raw <- read_csv2(                     # read_csv2 usa ";" como separador (padrão PT)
  "C:/Users/andre/OneDrive/Ambiente de Trabalho/Doutoramento/Lab/autodeclaracoes-de-doenca-dos-utentes.csv",
  locale = locale(decimal_mark = ",", grouping_mark = ".")
)

gripe_raw <- read_csv2(
  "C:/Users/andre/OneDrive/Ambiente de Trabalho/Doutoramento/Lab/atendimentos-nos-csp-gripe.csv",
  locale = locale(decimal_mark = ",", grouping_mark = ".")
)

# Inspeciona as primeiras linhas para confirmar nomes das colunas:
glimpse(add_raw)
glimpse(gripe_raw)

# -----------------------------------------------------------------------------
# 2. AGREGAR PARA NÍVEL DIÁRIO
# -----------------------------------------------------------------------------
# Ambos os datasets estão desagregados (ADD por origem/sexo/grupo etário;
# Gripe por região). Somamos tudo por dia antes de juntar.

# --- ADD: agrupar por Data, somar Nº ADD Emitidas ---
# A coluna Data já está em formato <date> (lida automaticamente pelo read_csv2).

add_clean <- add_raw %>%
  group_by(Data) %>%
  summarise(ADD_Total = sum(`Nº ADD Emitidas`, na.rm = TRUE), .groups = "drop") %>%
  arrange(Data)

cat("\n--- ADD agregado ---\n")
cat("Período:", format(min(add_clean$Data)), "a", format(max(add_clean$Data)), "\n")
cat("Nº dias:", nrow(add_clean), "\n")
print(head(add_clean))

# --- Gripe: agrupar por Período (= Data), somar Nº Consultas Gripe nos CSP ---
# A coluna "Nº Consultas Gripe nos CSP" vem como texto ("8.0", "39.0", …)
# → convertemos para numérico antes de somar.

gripe_clean <- gripe_raw %>%
  mutate(Gripe_CSP_num = as.numeric(`Nº Consultas Gripe nos CSP`)) %>%
  group_by(Data = Período) %>%
  summarise(Gripe_CSP = sum(Gripe_CSP_num, na.rm = TRUE), .groups = "drop") %>%
  arrange(Data)

cat("\n--- Gripe agregado ---\n")
cat("Período:", format(min(gripe_clean$Data)), "a", format(max(gripe_clean$Data)), "\n")
cat("Nº dias:", nrow(gripe_clean), "\n")
print(head(gripe_clean))


# -----------------------------------------------------------------------------
# 3. JUNTAR OS DOIS DATASETS (INNER JOIN → só dias comuns = 2023–2026)
# -----------------------------------------------------------------------------
# inner_join garante que só ficam dias com dados nos dois ficheiros.
# O modelo trabalhará sobre o período dos ADD (2023-2026).

tabela_mestra <- inner_join(add_clean, gripe_clean, by = "Data") %>%
  arrange(Data)

cat("\n--- Tabela mestra ---\n")
cat("Período:", format(min(tabela_mestra$Data)), "a",
    format(max(tabela_mestra$Data)), "\n")
cat("Nº de dias:", nrow(tabela_mestra), "\n")

# Verificar se há dias sem dados de gripe após o join:
dias_na_gripe <- sum(is.na(tabela_mestra$Gripe_CSP))
if (dias_na_gripe > 0) {
  cat("⚠️  Atenção:", dias_na_gripe, "dias sem dados de Gripe_CSP.\n")
  cat("   Esses dias serão excluídos ou imputados — avalia antes de continuar.\n")
} else {
  cat("✅ Sem valores em falta na coluna Gripe_CSP.\n")
}


# -----------------------------------------------------------------------------
# 4. FERIADOS NACIONAIS PORTUGUESES (2023–2026)
# -----------------------------------------------------------------------------
# Lista completa dos feriados nacionais obrigatórios.
# Páscoa é calculada; os restantes são fixos.

calcular_pascoa <- function(ano) {
  # Algoritmo de Butcher/Oudin
  a <- ano %% 19
  b <- ano %/% 100
  c <- ano %% 100
  d <- b %/% 4
  e <- b %% 4
  f <- (b + 8) %/% 25
  g <- (b - f + 1) %/% 3
  h <- (19 * a + b - d - g + 15) %% 30
  i <- c %/% 4
  k <- c %% 4
  l <- (32 + 2 * e + 2 * i - h - k) %% 7
  m <- (a + 11 * h + 22 * l) %/% 451
  mes <- (h + l - 7 * m + 114) %/% 31
  dia <- ((h + l - 7 * m + 114) %% 31) + 1
  as.Date(paste(ano, mes, dia, sep = "-"))
}

feriados_fixos <- function(ano) {
  as.Date(c(
    paste0(ano, "-01-01"),  # Ano Novo
    paste0(ano, "-04-25"),  # Liberdade
    paste0(ano, "-05-01"),  # Trabalho
    paste0(ano, "-06-10"),  # Portugal
    paste0(ano, "-08-15"),  # Assunção
    paste0(ano, "-10-05"),  # República
    paste0(ano, "-11-01"),  # Todos os Santos
    paste0(ano, "-12-01"),  # Restauração
    paste0(ano, "-12-08"),  # Imaculada
    paste0(ano, "-12-25")   # Natal
  ))
}

feriados_moveis <- function(ano) {
  pascoa <- calcular_pascoa(ano)
  c(
    pascoa - 2,   # Sexta-feira Santa
    pascoa,       # Páscoa
    pascoa + 60   # Corpo de Deus
  )
}

anos_modelo <- unique(year(tabela_mestra$Data))

todos_feriados <- map(anos_modelo, ~ c(feriados_fixos(.x), feriados_moveis(.x))) %>%
  unlist() %>%
  as.Date(origin = "1970-01-01") %>%
  unique() %>%
  sort()

cat("\nFeriados identificados no período:\n")
print(todos_feriados)


# -----------------------------------------------------------------------------
# 5. TOLERÂNCIAS DE PONTO E CARNAVAL (2023–2026)
# -----------------------------------------------------------------------------
# Tolerâncias oficiais: verificar cada ano (Governo PT publica em Diário da República)


# Tolerâncias de ponto oficiais da Função Pública (Diário da República)
tolerancias <- as.Date(c(
  "2023-02-21",  # Carnaval 2023
  "2023-04-06",  # Quinta-feira santa 2023
  "2023-12-26",  # 26 dezembro 2023
  "2024-01-02",  # 2 janeiro 2024
  "2024-02-13",  # Carnaval 2024
  "2023-03-28",  # Quinta-feira santa 2024
  "2024-12-24",  # véspera de Natal 2024
  "2024-12-31",  # véspera de Ano Novo 2024
  "2025-03-04",  # Carnaval 2025
  "2023-04-17",  # Quinta-feira santa 2025
  "2025-12-24",  # véspera de Natal 2025
  "2025-12-26",  # 26 dezembro 2025
  "2025-12-31",  # véspera de Ano Novo 2025
  "2026-02-17",  # Carnaval 2026
  "2023-04-02",  # Quinta-feira santa 2026
))

cat("\nTolerâncias:\n")
print(tolerancias)


# -----------------------------------------------------------------------------
# 6. CONSTRUIR AS DUMMIES COM HIERARQUIA (regra do overlap)
# -----------------------------------------------------------------------------
# Prioridade: Feriado > Tolerância > Ponte > Segunda_Comum / Sexta_Comum

detectar_ponte <- function(data_vec, feriados, tolerancias) {
  # Um dia útil (Seg–Sex) é "ponte" se:
  #   - não é feriado nem tolerância
  #   - está entre um feriado/tolerância e um fim de semana (ou vice-versa)
  todas_especiais <- c(feriados, tolerancias)
  
  map_int(data_vec, function(d) {
    dow <- wday(d, week_start = 1)  # 1=Seg, 7=Dom
    if (dow %in% c(6, 7)) return(0L)             # fim de semana não é ponte
    if (d %in% todas_especiais) return(0L)        # feriado/tolerância já classificado
    
    # Janela de ±3 dias úteis
    vizinhos <- d + (-3:3)
    tem_especial <- any(vizinhos %in% todas_especiais)
    tem_fds      <- any(wday(vizinhos, week_start = 1) %in% c(6, 7))
    
    if (tem_especial && tem_fds) 1L else 0L
  })
}

tabela_mestra <- tabela_mestra %>%
  mutate(
    dow = wday(Data, week_start = 1),  # 1=Seg … 7=Dom (auxiliar)
    
    Feriado      = if_else(Data %in% todos_feriados, 1L, 0L),
    Tolerancia   = if_else(Data %in% todas_tolerancias & Feriado == 0, 1L, 0L),
    Ponte        = detectar_ponte(Data, todos_feriados, todas_tolerancias),
    Segunda_Comum = if_else(dow == 1 & Feriado == 0 & Tolerancia == 0 & Ponte == 0, 1L, 0L),
    Sexta_Comum   = if_else(dow == 5 & Feriado == 0 & Tolerancia == 0 & Ponte == 0, 1L, 0L)
  ) %>%
  select(-dow)  # remove coluna auxiliar

# Verificação rápida — nenhuma linha deve ter soma > 1:
check_overlap <- tabela_mestra %>%
  mutate(soma = Feriado + Tolerancia + Ponte + Segunda_Comum + Sexta_Comum) %>%
  filter(soma > 1)

if (nrow(check_overlap) == 0) {
  cat("\n✅ Sem overlaps nas dummies.\n")
} else {
  cat("\n⚠️  ATENÇÃO: overlaps detectados em", nrow(check_overlap), "dias!\n")
  print(check_overlap)
}

# Distribuição das dummies:
tabela_mestra %>%
  summarise(across(c(Feriado, Tolerancia, Ponte, Segunda_Comum, Sexta_Comum), sum)) %>%
  print()


# -----------------------------------------------------------------------------
# 7. GUARDAR A TABELA MESTRA
# -----------------------------------------------------------------------------

write_csv(tabela_mestra, "tabela_mestra_ADD.csv")

# Opcional — exportar para Excel com formatação:
# write.xlsx(tabela_mestra, "tabela_mestra_ADD.xlsx", overwrite = TRUE)

cat("\n✅ Tabela mestra guardada: tabela_mestra_ADD.csv\n")
cat("   Colunas:", paste(names(tabela_mestra), collapse = ", "), "\n")


# -----------------------------------------------------------------------------
# 8. PREPARAR SÉRIES TEMPORAIS PARA O SARIMAX
# -----------------------------------------------------------------------------

# Série dependente (Y)
y_ts <- ts(tabela_mestra$ADD_Total,
           start     = c(year(min(tabela_mestra$Data)),
                         yday(min(tabela_mestra$Data))),
           frequency = 365)

# Regressores externos (matriz xreg)
xreg_matrix <- tabela_mestra %>%
  select(Gripe_CSP, Feriado, Tolerancia, Ponte, Segunda_Comum, Sexta_Comum) %>%
  as.matrix()


# -----------------------------------------------------------------------------
# 9. TESTE DE ESTACIONARIDADE (ADF)
# -----------------------------------------------------------------------------

cat("\n--- Teste ADF para ADD_Total ---\n")
adf_result <- adf.test(tabela_mestra$ADD_Total, alternative = "stationary")
print(adf_result)

# Se p-value > 0.05 → série não estacionária → auto.arima vai diferenciar (d≥1)


# -----------------------------------------------------------------------------
# 10. MODELO SARIMAX
# -----------------------------------------------------------------------------
# auto.arima encontra automaticamente os melhores parâmetros (p,d,q)(P,D,Q)[7]
# A sazonalidade semanal (frequency=7) é a mais relevante para absentismo diário.

cat("\n⏳ A correr auto.arima (pode demorar 1-3 min)...\n")

y_semanal <- ts(tabela_mestra$ADD_Total, frequency = 7)  # sazonalidade semanal

modelo_sarimax <- auto.arima(
  y_semanal,
  xreg          = xreg_matrix,
  seasonal      = TRUE,
  stepwise      = FALSE,   # exploração completa (mais lento mas melhor)
  approximation = FALSE,
  trace         = TRUE     # mostra os modelos testados
)

cat("\n✅ Modelo selecionado:\n")
summary(modelo_sarimax)


# -----------------------------------------------------------------------------
# 11. DIAGNÓSTICO DOS RESÍDUOS
# -----------------------------------------------------------------------------

cat("\n--- Diagnóstico dos resíduos ---\n")
checkresiduals(modelo_sarimax)

# Teste de Ljung-Box — se p > 0.05, resíduos são ruído branco (bom sinal):
lb_test <- Box.test(residuals(modelo_sarimax), lag = 14, type = "Ljung-Box")
print(lb_test)


# -----------------------------------------------------------------------------
# 12. EXTRAIR E INTERPRETAR OS COEFICIENTES β
# -----------------------------------------------------------------------------

coefs <- coef(modelo_sarimax)
se    <- sqrt(diag(vcov(modelo_sarimax)))

resultados_beta <- tibble(
  Variavel  = names(coefs),
  Beta      = coefs,
  SE        = se,
  t_stat    = Beta / SE,
  p_value   = 2 * pnorm(-abs(t_stat)),
  Sig       = case_when(
    p_value < 0.001 ~ "***",
    p_value < 0.01  ~ "**",
    p_value < 0.05  ~ "*",
    p_value < 0.10  ~ ".",
    TRUE            ~ ""
  )
) %>%
  filter(Variavel %in% c("Gripe_CSP", "Feriado", "Tolerancia",
                         "Ponte", "Segunda_Comum", "Sexta_Comum"))

cat("\n=== COEFICIENTES DE INTERESSE ===\n")
print(resultados_beta, n = Inf)


# -----------------------------------------------------------------------------
# 13. TRADUÇÃO ECONÓMICA
# -----------------------------------------------------------------------------
# Ajusta os parâmetros abaixo à tua realidade.

custo_dia_euros <- 120   # € — custo médio diário por trabalhador (ajusta)

# Contagem de dias por categoria no período analisado:
contagem_dias <- tabela_mestra %>%
  summarise(
    Feriado       = sum(Feriado),
    Tolerancia    = sum(Tolerancia),
    Ponte         = sum(Ponte),
    Segunda_Comum = sum(Segunda_Comum),
    Sexta_Comum   = sum(Sexta_Comum)
  ) %>%
  pivot_longer(everything(), names_to = "Variavel", values_to = "N_dias")

impacto_economico <- resultados_beta %>%
  filter(Variavel != "Gripe_CSP") %>%
  left_join(contagem_dias, by = "Variavel") %>%
  mutate(
    ADD_excedentarias = Beta * N_dias,
    Custo_euros       = ADD_excedentarias * custo_dia_euros,
    Nota              = if_else(p_value < 0.05, "Significativo", "Não significativo")
  ) %>%
  select(Variavel, Beta, p_value, Sig, N_dias, ADD_excedentarias, Custo_euros, Nota)

cat("\n=== IMPACTO ECONÓMICO ESTIMADO ===\n")
print(impacto_economico, n = Inf)

# Guardar resultados:
write_csv(resultados_beta,    "resultados_coeficientes.csv")
write_csv(impacto_economico,  "impacto_economico.csv")

cat("\n✅ Resultados guardados:\n")
cat("   resultados_coeficientes.csv\n")
cat("   impacto_economico.csv\n")


# -----------------------------------------------------------------------------
# 14. GRÁFICO FINAL — Real vs Previsto "apenas com gripe"
# -----------------------------------------------------------------------------

# Valores ajustados pelo modelo completo:
fitted_completo <- fitted(modelo_sarimax)

# Previsão "só com gripe" — zero nas dummies de conveniência:
xreg_so_gripe <- xreg_matrix
xreg_so_gripe[, c("Feriado", "Tolerancia", "Ponte",
                  "Segunda_Comum", "Sexta_Comum")] <- 0

# Refit com regressores neutralizados para obter baseline clínico:
# (usamos os coeficientes do modelo original aplicados manualmente)
beta_gripe <- coefs["Gripe_CSP"]
baseline_clinico <- fitted_completo -
  (xreg_matrix[, "Feriado"]       * coefs["Feriado"])       -
  (xreg_matrix[, "Tolerancia"]    * coefs["Tolerancia"])     -
  (xreg_matrix[, "Ponte"]         * coefs["Ponte"])          -
  (xreg_matrix[, "Segunda_Comum"] * coefs["Segunda_Comum"])  -
  (xreg_matrix[, "Sexta_Comum"]   * coefs["Sexta_Comum"])

plot_data <- tabela_mestra %>%
  mutate(
    Fitted_Completo   = as.numeric(fitted_completo),
    Baseline_Clinico  = as.numeric(baseline_clinico),
    Tipo_Dia = case_when(
      Feriado    == 1 ~ "Feriado",
      Tolerancia == 1 ~ "Tolerância",
      Ponte      == 1 ~ "Ponte",
      TRUE            ~ "Normal"
    )
  )

p <- ggplot(plot_data, aes(x = Data)) +
  geom_line(aes(y = ADD_Total,          colour = "Observado"),    linewidth = 0.5, alpha = 0.7) +
  geom_line(aes(y = Baseline_Clinico,   colour = "Prev. só Gripe"), linewidth = 0.8, linetype = "dashed") +
  geom_line(aes(y = Fitted_Completo,    colour = "Prev. Modelo Completo"), linewidth = 0.7) +
  geom_point(
    data = filter(plot_data, Tipo_Dia != "Normal"),
    aes(y = ADD_Total, shape = Tipo_Dia, colour = Tipo_Dia),
    size = 2.5
  ) +
  scale_colour_manual(values = c(
    "Observado"              = "grey40",
    "Prev. só Gripe"         = "steelblue",
    "Prev. Modelo Completo"  = "tomato",
    "Feriado"                = "navy",
    "Tolerância"             = "darkorchid",
    "Ponte"                  = "darkorange"
  )) +
  scale_shape_manual(values = c("Feriado" = 17, "Tolerância" = 18, "Ponte" = 15)) +
  labs(
    title    = "ADD Observadas vs Previstas (Modelo SARIMAX)",
    subtitle = "Separação entre efeito clínico (Gripe) e efeito de conveniência (calendário)",
    x        = "Data",
    y        = "Nº de ADD emitidas",
    colour   = NULL,
    shape    = "Tipo de dia"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

print(p)
ggsave("grafico_ADD_vs_previsto.png", p, width = 14, height = 6, dpi = 300)
cat("\n✅ Gráfico guardado: grafico_ADD_vs_previsto.png\n")

cat("\n============================================================\n")
cat(" PIPELINE CONCLUÍDA\n")
cat("============================================================\n")