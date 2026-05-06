# =============================================================================
# PIPELINE SARIMAX v2 — Análise de Absentismo por Conveniência
# Autores: Ana Correia, André Vicente, Filipa Carneiro
# =============================================================================
# ALTERAÇÕES vs v1:
#   [FIX-1]  Typo openxlxs → openxlsx
#   [FIX-2]  Removed orphan y_ts; single consistent ts object
#   [FIX-3]  Termos de Fourier (K=3, freq=365.25) para sazonalidade anual
#   [FIX-4]  Ljung-Box manual com fitdf correcto
#   [FIX-5]  set.seed() antes do auto.arima para reprodutibilidade
#   [FIX-6]  Ordens máximas alargadas no auto.arima
#   [FIX-7]  Verificação de alinhamento xreg vs y antes do modelo
#   [FIX-8]  install.packages protegido por requireNamespace
# =============================================================================


# -----------------------------------------------------------------------------
# 0. PACOTES NECESSÁRIOS
# -----------------------------------------------------------------------------
# [FIX-8] Só instala se não estiver disponível
pkgs <- c("tidyverse", "lubridate", "forecast", "tseries", "xts", "openxlsx")
new_pkgs <- pkgs[!sapply(pkgs, requireNamespace, quietly = TRUE)]
if (length(new_pkgs) > 0) install.packages(new_pkgs)

library(tidyverse)
library(lubridate)
library(forecast)
library(tseries)
library(xts)
library(openxlsx)


# -----------------------------------------------------------------------------
# 1. CARREGAR OS DOIS CSVs
# -----------------------------------------------------------------------------
add_raw <- read_csv2(
  "autodeclaracoes-de-doenca-dos-utentes.csv",
  locale = locale(decimal_mark = ",", grouping_mark = ".")
)

gripe_raw <- read_csv2(
  "atendimentos-nos-csp-gripe.csv",
  locale = locale(decimal_mark = ",", grouping_mark = ".")
)

glimpse(add_raw)
glimpse(gripe_raw)


# -----------------------------------------------------------------------------
# 2. AGREGAR PARA NÍVEL DIÁRIO
# -----------------------------------------------------------------------------
add_clean <- add_raw %>%
  group_by(Data) %>%
  summarise(ADD_Total = sum(`Nº ADD Emitidas`, na.rm = TRUE), .groups = "drop") %>%
  arrange(Data)

gripe_clean <- gripe_raw %>%
  mutate(Gripe_CSP_num = as.numeric(`Nº Consultas Gripe nos CSP`)) %>%
  group_by(Data = Período) %>%
  summarise(Gripe_CSP = sum(Gripe_CSP_num, na.rm = TRUE), .groups = "drop") %>%
  arrange(Data)

cat("\n--- ADD agregado ---\n")
cat("Período:", format(min(add_clean$Data)), "a", format(max(add_clean$Data)), "\n")
cat("Nº dias:", nrow(add_clean), "\n")

cat("\n--- Gripe agregado ---\n")
cat("Período:", format(min(gripe_clean$Data)), "a", format(max(gripe_clean$Data)), "\n")
cat("Nº dias:", nrow(gripe_clean), "\n")


# -----------------------------------------------------------------------------
# 3. JUNTAR OS DOIS DATASETS
# -----------------------------------------------------------------------------
# [FIX] Auditar dias de ADD excluídos pelo inner_join antes de prosseguir
dias_add_excluidos <- anti_join(add_clean, gripe_clean, by = "Data")
if (nrow(dias_add_excluidos) > 0) {
  cat("\n⚠️  inner_join vai excluir", nrow(dias_add_excluidos),
      "dias com ADD mas sem registo de Gripe.\n")
  cat("   Distribuição por dia da semana dos dias excluídos:\n")
  dias_add_excluidos %>%
    mutate(dow = wday(Data, label = TRUE, week_start = 1)) %>%
    count(dow) %>%
    print()
  cat("   → Avaliar se exclusão introduz viés antes de continuar.\n")
}

tabela_mestra <- inner_join(add_clean, gripe_clean, by = "Data") %>%
  arrange(Data)

cat("\n--- Tabela mestra ---\n")
cat("Período:", format(min(tabela_mestra$Data)), "a",
    format(max(tabela_mestra$Data)), "\n")
cat("Nº de dias:", nrow(tabela_mestra), "\n")

dias_na_gripe <- sum(is.na(tabela_mestra$Gripe_CSP))
if (dias_na_gripe > 0) {
  cat("⚠️  Atenção:", dias_na_gripe, "dias sem dados de Gripe_CSP.\n")
} else {
  cat("✅ Sem valores em falta na coluna Gripe_CSP.\n")
}


# -----------------------------------------------------------------------------
# 4. FERIADOS NACIONAIS PORTUGUESES (2023–2026)
# -----------------------------------------------------------------------------
calcular_pascoa <- function(ano) {
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
tolerancias <- as.Date(c(
  "2023-02-21",  # Carnaval 2023
  "2023-04-06",  # Quinta-feira santa 2023
  "2023-12-26",  # 26 dezembro 2023
  "2024-01-02",  # 2 janeiro 2024
  "2024-02-13",  # Carnaval 2024
  "2024-03-28",  # Quinta-feira santa 2024
  "2024-12-24",  # véspera de Natal 2024
  "2024-12-31",  # véspera de Ano Novo 2024
  "2025-03-04",  # Carnaval 2025
  "2025-04-17",  # Quinta-feira santa 2025
  "2025-12-24",  # véspera de Natal 2025
  "2025-12-26",  # 26 dezembro 2025
  "2025-12-31",  # véspera de Ano Novo 2025
  "2026-02-17",  # Carnaval 2026
  "2026-04-02"   # Quinta-feira santa 2026
))

cat("\nTolerâncias:\n")
print(tolerancias)


# -----------------------------------------------------------------------------
# 6. CONSTRUIR AS DUMMIES COM HIERARQUIA (regra do overlap)
# -----------------------------------------------------------------------------
# Prioridade: Feriado > Tolerância > Ponte > Segunda_Comum / Sexta_Comum

# [FIX] Lógica de ponte corrigida: só dias DIRECTAMENTE adjacentes a feriado/
#       tolerância com fim-de-semana do outro lado (não janela ±3 dias)
detectar_ponte <- function(data_vec, feriados, tolerancias) {
  todas_especiais <- c(feriados, tolerancias)

  map_int(data_vec, function(d) {
    dow <- wday(d, week_start = 1)  # 1=Seg, 7=Dom

    if (dow %in% c(6, 7)) return(0L)       # fim-de-semana
    if (d %in% todas_especiais) return(0L) # já é feriado/tolerância

    ontem <- d - 1
    amanha <- d + 1

    # Caso 1: feriado/tolerância ontem + sábado amanhã
    #         ex: feriado 4ª → ponte 5ª → sábado
    caso1 <- (ontem %in% todas_especiais) &&
              wday(amanha, week_start = 1) == 6

    # Caso 2: domingo ontem + feriado/tolerância amanhã
    #         ex: domingo → ponte 2ª → feriado 3ª
    caso2 <- wday(ontem, week_start = 1) == 7 &&
              (amanha %in% todas_especiais)

    if (caso1 || caso2) 1L else 0L
  })
}

tabela_mestra <- tabela_mestra %>%
  mutate(
    dow = wday(Data, week_start = 1),

    Feriado       = if_else(Data %in% todos_feriados, 1L, 0L),
    Tolerancia    = if_else(Data %in% tolerancias & Feriado == 0, 1L, 0L),
    Ponte         = detectar_ponte(Data, todos_feriados, tolerancias),
    Segunda_Comum = if_else(dow == 1 & Feriado == 0 & Tolerancia == 0 & Ponte == 0, 1L, 0L),
    Sexta_Comum   = if_else(dow == 5 & Feriado == 0 & Tolerancia == 0 & Ponte == 0, 1L, 0L)
  ) %>%
  select(-dow)

# Verificação de overlaps:
check_overlap <- tabela_mestra %>%
  mutate(soma = Feriado + Tolerancia + Ponte + Segunda_Comum + Sexta_Comum) %>%
  filter(soma > 1)

if (nrow(check_overlap) == 0) {
  cat("\n✅ Sem overlaps nas dummies.\n")
} else {
  cat("\n⚠️  ATENÇÃO: overlaps detectados em", nrow(check_overlap), "dias!\n")
  print(check_overlap)
}

cat("\nDistribuição das dummies:\n")
tabela_mestra %>%
  summarise(across(c(Feriado, Tolerancia, Ponte, Segunda_Comum, Sexta_Comum), sum)) %>%
  print()

# [FIX] Verificação de colinearidade entre regressores (deve ser inspeccionado)
cat("\nMatriz de correlação dos regressores:\n")
tabela_mestra %>%
  select(Gripe_CSP, Feriado, Tolerancia, Ponte, Segunda_Comum, Sexta_Comum) %>%
  cor() %>%
  round(3) %>%
  print()


# -----------------------------------------------------------------------------
# 7. GUARDAR A TABELA MESTRA
# -----------------------------------------------------------------------------
write_csv(tabela_mestra, "tabela_mestra_ADD.csv")
cat("\n✅ Tabela mestra guardada: tabela_mestra_ADD.csv\n")
cat("   Colunas:", paste(names(tabela_mestra), collapse = ", "), "\n")


# -----------------------------------------------------------------------------
# 8. PREPARAR SÉRIES TEMPORAIS PARA O SARIMAX
# -----------------------------------------------------------------------------
# [FIX-2] Um único objecto ts com frequency=7 (sazonalidade semanal).
#         A sazonalidade anual será capturada pelos termos de Fourier (secção 9).
n_obs <- nrow(tabela_mestra)

y_semanal <- ts(tabela_mestra$ADD_Total, frequency = 7)

# Regressores base (calendário + gripe)
xreg_base <- tabela_mestra %>%
  select(Gripe_CSP, Feriado, Tolerancia, Ponte, Segunda_Comum, Sexta_Comum) %>%
  as.matrix()


# -----------------------------------------------------------------------------
# 9. TERMOS DE FOURIER PARA SAZONALIDADE ANUAL   [FIX-3 — novo]
# -----------------------------------------------------------------------------
# O SARIMA[7] captura a sazonalidade semanal mas não o ciclo anual
# (picos de inverno, vales de verão) que é forte em dados de absentismo.
# Adicionamos pares seno/coseno com frequência 365.25 dias.
#
# K=3 → 6 colunas (sin1,cos1,sin2,cos2,sin3,cos3).
# Aumentar K se os resíduos ainda mostrarem padrão anual; K≤5 é razoável.

K_fourier <- 3  # nº de harmónicos — ajustar se Ljung-Box ainda falhar

# fourier() da {forecast} requer um objecto ts com a frequência correcta
y_anual_ref <- ts(tabela_mestra$ADD_Total, frequency = 365.25)
fourier_terms <- fourier(y_anual_ref, K = K_fourier)

cat("\n--- Termos de Fourier (primeiras linhas) ---\n")
print(head(fourier_terms))
cat("Dimensão:", dim(fourier_terms), "\n")

# Matriz xreg completa: regressores originais + Fourier
xreg_matrix <- cbind(xreg_base, fourier_terms)

# [FIX-7] Verificar alinhamento de dimensões antes de entrar no modelo
stopifnot(
  "xreg e y têm nº de linhas diferentes!" = nrow(xreg_matrix) == length(y_semanal)
)
cat("\n✅ Alinhamento xreg/y confirmado:", nrow(xreg_matrix), "observações.\n")


# -----------------------------------------------------------------------------
# 10. TESTE DE ESTACIONARIDADE (ADF)
# -----------------------------------------------------------------------------
cat("\n--- Teste ADF para ADD_Total ---\n")
adf_result <- adf.test(tabela_mestra$ADD_Total, alternative = "stationary")
print(adf_result)
# Se p-value > 0.05 → série não estacionária → auto.arima vai diferenciar (d≥1)


# -----------------------------------------------------------------------------
# 11. MODELO SARIMAX   [FIX-5, FIX-6]
# -----------------------------------------------------------------------------
# [FIX-5] set.seed() para reprodutibilidade
# [FIX-6] Ordens máximas alargadas para dar mais espaço ao optimizador

set.seed(42)

cat("\n⏳ A correr auto.arima com Fourier (pode demorar 3-8 min)...\n")

modelo_sarimax <- auto.arima(
  y_semanal,
  xreg          = xreg_matrix,
  seasonal      = TRUE,
  max.p         = 3,
  max.q         = 3,
  max.P         = 2,
  max.Q         = 2,
  stepwise      = FALSE,   # exploração completa
  approximation = FALSE,
  trace         = TRUE
)

cat("\n✅ Modelo selecionado:\n")
summary(modelo_sarimax)

# Extrair nº de parâmetros ARMA para o Ljung-Box (usado na secção 12)
arma_ord  <- arimaorder(modelo_sarimax)
# fitdf = p + q + P + Q (não conta d/D nem regressores externos)
n_params_arma <- arma_ord["p"] + arma_ord["q"] + arma_ord["P"] + arma_ord["Q"]
cat("\nParâmetros ARMA do modelo (fitdf para Ljung-Box):", n_params_arma, "\n")


# -----------------------------------------------------------------------------
# 12. DIAGNÓSTICO DOS RESÍDUOS   [FIX-4]
# -----------------------------------------------------------------------------
cat("\n--- Diagnóstico dos resíduos ---\n")
checkresiduals(modelo_sarimax)
# checkresiduals() já usa o fitdf correcto internamente.

# [FIX-4] Box.test manual com fitdf correcto (descontando parâmetros ARMA)
lb_test <- Box.test(
  residuals(modelo_sarimax),
  lag   = 14,
  type  = "Ljung-Box",
  fitdf = n_params_arma   # ← correcção dos graus de liberdade
)
cat("\n--- Ljung-Box corrigido (lag=14, fitdf =", n_params_arma, ") ---\n")
print(lb_test)

if (lb_test$p.value > 0.05) {
  cat("✅ Resíduos consistentes com ruído branco (p =",
      round(lb_test$p.value, 4), ")\n")
} else {
  cat("⚠️  Autocorrelação residual detectada (p =",
      round(lb_test$p.value, 4), ")\n")
  cat("   Sugestões: aumentar K_fourier (linha ~130) ou alargar max.p/max.q.\n")
}


# -----------------------------------------------------------------------------
# 13. EXTRAIR E INTERPRETAR OS COEFICIENTES β
# -----------------------------------------------------------------------------
# Nota metodológica: p-values calculados por aproximação normal assimptótica
# (distribuição t com n→∞). Adequado para n > 500; conservador para amostras
# menores. Para inferência mais robusta considerar bootstrap ou intervals delta.

coefs <- coef(modelo_sarimax)
se    <- sqrt(diag(vcov(modelo_sarimax)))

# Variáveis de interesse (excluir termos ARMA e Fourier da tabela principal)
vars_interesse <- c("Gripe_CSP", "Feriado", "Tolerancia",
                    "Ponte", "Segunda_Comum", "Sexta_Comum")

resultados_beta <- tibble(
  Variavel = names(coefs),
  Beta     = coefs,
  SE       = se,
  t_stat   = Beta / SE,
  p_value  = 2 * pnorm(-abs(t_stat)),
  Sig      = case_when(
    p_value < 0.001 ~ "***",
    p_value < 0.01  ~ "**",
    p_value < 0.05  ~ "*",
    p_value < 0.10  ~ ".",
    TRUE            ~ ""
  )
) %>%
  filter(Variavel %in% vars_interesse)

cat("\n=== COEFICIENTES DE INTERESSE ===\n")
print(resultados_beta, n = Inf)

# Verificar se algum regressor foi removido por colinearidade (coef = NA):
nas_coef <- resultados_beta %>% filter(is.na(Beta))
if (nrow(nas_coef) > 0) {
  cat("\n⚠️  Os seguintes regressores têm coeficiente NA (possível colinearidade):\n")
  print(nas_coef$Variavel)
}


# -----------------------------------------------------------------------------
# 14. TRADUÇÃO ECONÓMICA
# -----------------------------------------------------------------------------
# PRESSUPOSTOS (documentar na metodologia):
#   - custo_dia_euros: custo médio diário por trabalhador ausente.
#     Fonte sugerida: INE — Inquérito ao Emprego, custo médio hora × 8h,
#     ou relatório de recursos humanos da entidade em estudo.
#   - Assume-se que cada ADD de conveniência corresponde a 1 dia de ausência.
#   - O intervalo de confiança do custo propaga a incerteza do Beta (±1.96×SE).
#   - Apenas variáveis estatisticamente significativas (p<0.05) são interpretadas
#     causalmente; as restantes são reportadas mas marcadas como "Não significativo".

custo_dia_euros <- 120  # ← ajustar com fonte explícita na metodologia

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
    ADD_excedentarias   = Beta * N_dias,
    # [FIX] Intervalos de confiança propagados do Beta
    ADD_exc_lb          = (Beta - 1.96 * SE) * N_dias,
    ADD_exc_ub          = (Beta + 1.96 * SE) * N_dias,
    Custo_euros         = ADD_excedentarias * custo_dia_euros,
    Custo_euros_lb      = ADD_exc_lb * custo_dia_euros,
    Custo_euros_ub      = ADD_exc_ub * custo_dia_euros,
    Nota                = if_else(p_value < 0.05, "Significativo", "Não significativo")
  ) %>%
  select(Variavel, Beta, SE, p_value, Sig, N_dias,
         ADD_excedentarias, ADD_exc_lb, ADD_exc_ub,
         Custo_euros, Custo_euros_lb, Custo_euros_ub, Nota)

cat("\n=== IMPACTO ECONÓMICO ESTIMADO (custo/dia =", custo_dia_euros, "€) ===\n")
print(impacto_economico, n = Inf)


# -----------------------------------------------------------------------------
# 15. GRÁFICO FINAL — Real vs Previsto
# -----------------------------------------------------------------------------
fitted_completo <- fitted(modelo_sarimax)

# Baseline clínico: remove efeito das dummies de conveniência
# (indexação por nome para ser robusta a reordenação)
coefs_conv <- coefs[vars_interesse[vars_interesse != "Gripe_CSP"]]
coefs_conv[is.na(coefs_conv)] <- 0  # proteger contra NAs por colinearidade

baseline_clinico <- as.numeric(fitted_completo) -
  (tabela_mestra$Feriado       * coefs_conv["Feriado"])       -
  (tabela_mestra$Tolerancia    * coefs_conv["Tolerancia"])     -
  (tabela_mestra$Ponte         * coefs_conv["Ponte"])          -
  (tabela_mestra$Segunda_Comum * coefs_conv["Segunda_Comum"])  -
  (tabela_mestra$Sexta_Comum   * coefs_conv["Sexta_Comum"])

plot_data <- tabela_mestra %>%
  mutate(
    Fitted_Completo  = as.numeric(fitted_completo),
    Baseline_Clinico = baseline_clinico,
    Tipo_Dia = case_when(
      Feriado    == 1 ~ "Feriado",
      Tolerancia == 1 ~ "Tolerância",
      Ponte      == 1 ~ "Ponte",
      TRUE            ~ "Normal"
    )
  )

p <- ggplot(plot_data, aes(x = Data)) +
  geom_line(aes(y = ADD_Total,        colour = "Observado"),
            linewidth = 0.5, alpha = 0.7) +
  geom_line(aes(y = Baseline_Clinico, colour = "Prev. só Gripe"),
            linewidth = 0.8, linetype = "dashed") +
  geom_line(aes(y = Fitted_Completo,  colour = "Prev. Modelo Completo"),
            linewidth = 0.7) +
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
    title    = "ADD Observadas vs Previstas (SARIMAX + Fourier)",
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


# -----------------------------------------------------------------------------
# 16. GUARDAR RESULTADOS
# -----------------------------------------------------------------------------
write_csv(resultados_beta,   "resultados_coeficientes.csv")
write_csv(impacto_economico, "impacto_economico.csv")

cat("\n✅ Resultados guardados:\n")
cat("   resultados_coeficientes.csv\n")
cat("   impacto_economico.csv\n")

cat("\n============================================================\n")
cat(" PIPELINE v2 CONCLUÍDA\n")
cat("============================================================\n")
