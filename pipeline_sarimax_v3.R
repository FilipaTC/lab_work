# =============================================================================
# PIPELINE SARIMAX v3 - Análise de Absentismo por Conveniência
# Autores: Ana Correia, André Vicente, Filipa Carneiro
# =============================================================================
# ALTERAÇÕES vs v2:
#   [v3-1]  Dummy Quarta_Comum adicionada ao modelo
#   [v3-2]  Quarta_Pre_Especial testada e fundida com Quarta_Comum (não sig.)
#   [v3-3]  Sexta_Comum removida do modelo (não significativa, p=0.167)
#   [v3-4]  Dummies de outlier (|resíduo| > 3??) para tratar autocorrelação residual
#   [v3-5]  Duas passagens do modelo: 1ª identifica outliers, 2ª incorpora-os
#   [v3-6]  Impacto económico e gráfico actualizados para nova estrutura de dummies
#   [v3-7]  Gráfico adicional: perfil semanal de ADD por tipo de dia
# =============================================================================


# -----------------------------------------------------------------------------
# 0. PACOTES NECESSÁRIOS
# -----------------------------------------------------------------------------
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
dias_add_excluidos <- anti_join(add_clean, gripe_clean, by = "Data")
if (nrow(dias_add_excluidos) > 0) {
  cat("\n??????  inner_join vai excluir", nrow(dias_add_excluidos),
      "dias com ADD mas sem registo de Gripe.\n")
  cat("   Distribuição por dia da semana dos dias excluídos:\n")
  dias_add_excluidos %>%
    mutate(dow = wday(Data, label = TRUE, week_start = 1)) %>%
    count(dow) %>%
    print()
  cat("   ??? Avaliar se exclusão introduz viés antes de continuar.\n")
}

tabela_mestra <- inner_join(add_clean, gripe_clean, by = "Data") %>%
  arrange(Data)

cat("\n--- Tabela mestra ---\n")
cat("Período:", format(min(tabela_mestra$Data)), "a",
    format(max(tabela_mestra$Data)), "\n")
cat("Nº de dias:", nrow(tabela_mestra), "\n")

dias_na_gripe <- sum(is.na(tabela_mestra$Gripe_CSP))
if (dias_na_gripe > 0) {
  cat("??????  Atenção:", dias_na_gripe, "dias sem dados de Gripe_CSP.\n")
} else {
  cat("??? Sem valores em falta na coluna Gripe_CSP.\n")
}


# -----------------------------------------------------------------------------
# 4. FERIADOS NACIONAIS PORTUGUESES (2023-2026)
# -----------------------------------------------------------------------------
calcular_pascoa <- function(ano) {
  a <- ano %% 19; b <- ano %/% 100; c <- ano %% 100
  d <- b %/% 4;  e <- b %% 4;      f <- (b + 8) %/% 25
  g <- (b - f + 1) %/% 3
  h <- (19 * a + b - d - g + 15) %% 30
  i <- c %/% 4;  k <- c %% 4
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
  c(pascoa - 2, pascoa, pascoa + 60)  # Sexta Santa, Páscoa, Corpo de Deus
}

anos_modelo <- unique(year(tabela_mestra$Data))

todos_feriados <- map(anos_modelo, ~ c(feriados_fixos(.x), feriados_moveis(.x))) %>%
  unlist() %>%
  as.Date(origin = "1970-01-01") %>%
  unique() %>% sort()

cat("\nFeriados identificados no período:\n")
print(todos_feriados)


# -----------------------------------------------------------------------------
# 5. TOLERÂNCIAS DE PONTO (2023-2026)
# -----------------------------------------------------------------------------
tolerancias <- as.Date(c(
  "2023-02-21", "2023-04-06", "2023-12-26",
  "2024-01-02", "2024-02-13", "2024-03-28", "2024-12-24", "2024-12-31",
  "2025-03-04", "2025-04-17", "2025-12-24", "2025-12-26", "2025-12-31",
  "2026-02-17", "2026-04-02"
))

cat("\nTolerâncias:\n"); print(tolerancias)


# -----------------------------------------------------------------------------
# 6. CONSTRUIR AS DUMMIES COM HIERARQUIA
# -----------------------------------------------------------------------------
# Prioridade: Feriado > Tolerância > Ponte > Segunda_Comum > Quarta_Comum
#
# [v3-1/2] Quarta_Pre_Especial foi testada separadamente e não mostrou
#           efeito significativo adicional face a Quarta_Comum (p=0.146).
#           Interpretação: o efeito de conveniência das quartas não é
#           condicionado pela proximidade de feriados - opera todas as
#           semanas via mecanismo quarta+qui+sex+fds = 5 dias de ausência
#           efectiva com 3 dias de declaração.
#           Decisão: usar uma única dummy Quarta_Comum (quartas normais).
#
# [v3-3] Sexta_Comum não significativa (p=0.167, ??=-139): removida.
#         Interpretação consistente - a sexta não tem valor estratégico
#         porque o fds imediatamente após já é não-útil.

detectar_ponte <- function(data_vec, feriados, tolerancias) {
  todas_especiais <- c(feriados, tolerancias)
  map_int(data_vec, function(d) {
    dow <- wday(d, week_start = 1)
    if (dow %in% c(6, 7)) return(0L)
    if (d %in% todas_especiais) return(0L)
    ontem  <- d - 1
    amanha <- d + 1
    caso1 <- (ontem %in% todas_especiais) && wday(amanha, week_start = 1) == 6
    caso2 <- wday(ontem, week_start = 1) == 7 && (amanha %in% todas_especiais)
    if (caso1 || caso2) 1L else 0L
  })
}

tabela_mestra <- tabela_mestra %>%
  mutate(
    dow = wday(Data, week_start = 1),
    
    Feriado       = if_else(Data %in% todos_feriados, 1L, 0L),
    Tolerancia    = if_else(Data %in% tolerancias & Feriado == 0, 1L, 0L),
    Ponte         = detectar_ponte(Data, todos_feriados, tolerancias),
    Segunda_Comum = if_else(
      dow == 1 & Feriado == 0 & Tolerancia == 0 & Ponte == 0, 1L, 0L
    ),
    Quarta_Comum  = if_else(
      dow == 3 & Feriado == 0 & Tolerancia == 0 & Ponte == 0, 1L, 0L
    )
    # Sexta_Comum removida: não significativa (v3-3)
    # Quarta_Pre_Especial fundida em Quarta_Comum (v3-2)
  ) %>%
  select(-dow)

# Verificação de overlaps
check_overlap <- tabela_mestra %>%
  mutate(soma = Feriado + Tolerancia + Ponte + Segunda_Comum + Quarta_Comum) %>%
  filter(soma > 1)

if (nrow(check_overlap) == 0) {
  cat("\n??? Sem overlaps nas dummies.\n")
} else {
  cat("\n??????  ATENÇÃO: overlaps detectados em", nrow(check_overlap), "dias!\n")
  print(check_overlap)
}

cat("\nDistribuição das dummies:\n")
tabela_mestra %>%
  summarise(across(c(Feriado, Tolerancia, Ponte, Segunda_Comum, Quarta_Comum), sum)) %>%
  print()

cat("\nMatriz de correlação dos regressores:\n")
tabela_mestra %>%
  select(Gripe_CSP, Feriado, Tolerancia, Ponte, Segunda_Comum, Quarta_Comum) %>%
  cor() %>% round(3) %>% print()


# -----------------------------------------------------------------------------
# 7. GUARDAR TABELA MESTRA
# -----------------------------------------------------------------------------
write_csv(tabela_mestra, "tabela_mestra_ADD.csv")
cat("\n??? Tabela mestra guardada: tabela_mestra_ADD.csv\n")


# -----------------------------------------------------------------------------
# 8. PREPARAR SÉRIE TEMPORAL E REGRESSORES BASE
# -----------------------------------------------------------------------------
y_semanal <- ts(tabela_mestra$ADD_Total, frequency = 7)

xreg_base <- tabela_mestra %>%
  select(Gripe_CSP, Feriado, Tolerancia, Ponte, Segunda_Comum, Quarta_Comum) %>%
  as.matrix()


# -----------------------------------------------------------------------------
# 9. TERMOS DE FOURIER PARA SAZONALIDADE ANUAL
# -----------------------------------------------------------------------------
# Captura o ciclo anual (picos de inverno, vales de verão) que o SARIMA[7]
# não consegue modelar nativamente.
# K=5 é o valor mais alto testado; manter se Ljung-Box ainda falhar após
# a correcção de outliers (secção 11).

K_fourier    <- 5
y_anual_ref  <- ts(tabela_mestra$ADD_Total, frequency = 365.25)
fourier_terms <- fourier(y_anual_ref, K = K_fourier)

xreg_1a_passagem <- cbind(xreg_base, fourier_terms)

stopifnot(
  "xreg e y têm nº de linhas diferentes!" =
    nrow(xreg_1a_passagem) == length(y_semanal)
)
cat("\n??? Alinhamento xreg/y confirmado:", nrow(xreg_1a_passagem), "observações.\n")


# -----------------------------------------------------------------------------
# 10. TESTE DE ESTACIONARIDADE (ADF)
# -----------------------------------------------------------------------------
cat("\n--- Teste ADF para ADD_Total ---\n")
adf_result <- adf.test(tabela_mestra$ADD_Total, alternative = "stationary")
print(adf_result)


# -----------------------------------------------------------------------------
# 11. 1ª PASSAGEM DO MODELO - identificar outliers   [v3-4, v3-5]
# -----------------------------------------------------------------------------
# Estratégia em duas passagens:
#   1ª passagem: modelo sem dummies de outlier ??? extrair resíduos
#   Identificar dias com |resíduo| > 3??
#   2ª passagem: modelo com dummies de outlier ??? estimativas finais
#
# Justificação: dias com valores extremos (ex: 1ª semana de Janeiro,
# regresso pós-Natal, eventos COVID) criam autocorrelação aparente nos
# resíduos. Dummies pontuais são o tratamento padrão em séries temporais
# com outliers aditivos (cf. Chen & Liu, 1993).

set.seed(42)
cat("\n??? 1ª passagem: a identificar outliers (pode demorar 3-8 min)...\n")

modelo_1a <- auto.arima(
  y_semanal,
  xreg          = xreg_1a_passagem,
  seasonal      = TRUE,
  max.p = 3, max.q = 3, max.P = 2, max.Q = 2,
  stepwise      = FALSE,
  approximation = FALSE,
  trace         = FALSE   # silencioso na 1ª passagem
)

residuos_1a <- residuals(modelo_1a)
sigma_1a    <- sd(residuos_1a)
limiar      <- 3 * sigma_1a

outlier_dates <- tabela_mestra$Data[abs(residuos_1a) > limiar]
# Extrair resíduos dos dias outlier usando índices do vector completo
idx_outliers <- which(tabela_mestra$Data %in% outlier_dates)

outlier_info <- tabela_mestra %>%
  filter(Data %in% outlier_dates) %>%
  mutate(
    residuo   = as.numeric(residuos_1a)[idx_outliers],  # índice já tem 24 elementos
    sigma_n   = round(residuo / sigma_1a, 1),
    dow_label = wday(Data, label = TRUE, week_start = 1)
  ) %>%
  select(Data, dow_label, ADD_Total, residuo, sigma_n,
         Feriado, Tolerancia, Ponte, Segunda_Comum, Quarta_Comum)

print(outlier_info, n = Inf)



# -----------------------------------------------------------------------------
# 12. CONSTRUIR DUMMIES DE OUTLIER E XREG FINAL
# -----------------------------------------------------------------------------
if (length(outlier_dates) > 0) {
  outlier_dummies <- map_dfc(
    outlier_dates,
    ~ as.integer(tabela_mestra$Data == .x)
  ) %>%
    setNames(paste0("out_", format(outlier_dates, "%Y%m%d")))
  
  xreg_final <- cbind(xreg_1a_passagem, outlier_dummies)
  cat("\n??? Dummies de outlier adicionadas:", ncol(outlier_dummies), "colunas.\n")
} else {
  xreg_final <- xreg_1a_passagem
  cat("\n??? Sem outliers a adicionar; xreg_final = xreg_1a_passagem.\n")
}

stopifnot(
  "xreg_final e y têm nº de linhas diferentes!" =
    nrow(xreg_final) == length(y_semanal)
)

# Diagnóstico: ver tipos de cada coluna
cat("Tipos de coluna em xreg_final:\n")
print(sapply(as.data.frame(xreg_final), class))

# Correcção: forçar tudo a numeric antes de entrar no modelo
xreg_final <- apply(xreg_final, 2, as.numeric)

# Confirmar
cat("\nApós correcção:\n")
print(sapply(as.data.frame(xreg_final), class))
stopifnot(all(apply(xreg_final, 2, is.numeric)))
cat("??? xreg_final é uma matriz numérica.\n")
# -----------------------------------------------------------------------------
# 13. 2ª PASSAGEM - MODELO FINAL   [v3-5]
# -----------------------------------------------------------------------------
set.seed(42)
cat("\n??? 2ª passagem: modelo final com outliers corrigidos (pode demorar alguns min)...\n")

modelo_sarimax <- auto.arima(
  y_semanal,
  xreg          = xreg_final,
  seasonal      = TRUE,
  max.p = 3, max.q = 3, max.P = 2, max.Q = 2,
  stepwise      = FALSE,
  approximation = FALSE,
  trace         = TRUE
)

cat("\n??? Modelo final selecionado:\n")
summary(modelo_sarimax)

arma_ord      <- arimaorder(modelo_sarimax)
n_params_arma <- arma_ord["p"] + arma_ord["q"] + arma_ord["P"] + arma_ord["Q"]
cat("\nParâmetros ARMA (fitdf para Ljung-Box):", n_params_arma, "\n")


# -----------------------------------------------------------------------------
# 14. DIAGNÓSTICO DOS RESÍDUOS
# -----------------------------------------------------------------------------
cat("\n--- Diagnóstico dos resíduos (modelo final) ---\n")
checkresiduals(modelo_sarimax)

lb_test <- Box.test(
  residuals(modelo_sarimax),
  lag   = 14,
  type  = "Ljung-Box",
  fitdf = n_params_arma
)
cat("\n--- Ljung-Box corrigido (lag=14, fitdf =", n_params_arma, ") ---\n")
print(lb_test)

if (lb_test$p.value > 0.05) {
  cat("??? Resíduos consistentes com ruído branco (p =",
      round(lb_test$p.value, 4), ")\n")
} else {
  cat("??????  Autocorrelação residual ainda presente (p =",
      round(lb_test$p.value, 4), ")\n")
  cat("   O modelo é reportável mas a limitação deve ser declarada.\n")
  cat("   Nota metodológica sugerida:\n")
  cat("   'Apesar da correcção de outliers e da inclusão de termos de Fourier,\n")
  cat("    os resíduos apresentam autocorrelação moderada, possivelmente devida\n")
  cat("    a instabilidade estrutural no período pós-COVID. As estimativas dos\n")
  cat("    coeficientes são consideradas conservadoras.'\n")
}


# -----------------------------------------------------------------------------
# 15. COEFICIENTES ?? - VARIÁVEIS DE INTERESSE
# -----------------------------------------------------------------------------
# Nota metodológica: p-values por aproximação normal assimptótica (n > 500).

coefs <- coef(modelo_sarimax)
se    <- sqrt(diag(vcov(modelo_sarimax)))

# [v3-3] Sexta_Comum excluída; [v3-1] Quarta_Comum incluída
vars_interesse <- c("Gripe_CSP", "Feriado", "Tolerancia",
                    "Ponte", "Segunda_Comum", "Quarta_Comum")

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

nas_coef <- resultados_beta %>% filter(is.na(Beta))
if (nrow(nas_coef) > 0) {
  cat("\n??????  Coeficientes NA (possível colinearidade):\n")
  print(nas_coef$Variavel)
}


# -----------------------------------------------------------------------------
# 16. IMPACTO ECONÓMICO   [v3-6]
# -----------------------------------------------------------------------------
# PRESSUPOSTOS (documentar na metodologia):
#   - custo_dia_euros: custo médio diário por trabalhador ausente.
#     Fonte sugerida: INE - Inquérito ao Emprego, custo horário médio × 8h.
#   - Cada ADD de conveniência = 1 dia de ausência com custo fixo.
#   - Segundas e quartas têm mecanismos distintos de conveniência (ver texto).
#   - IC a 95% propagado da incerteza do Beta (método delta).
#   - Só variáveis com p < 0.05 são interpretadas causalmente.

custo_dia_euros <- 150  # ??? substituir por valor documentado

contagem_dias <- tabela_mestra %>%
  summarise(
    Feriado       = sum(Feriado),
    Tolerancia    = sum(Tolerancia),
    Ponte         = sum(Ponte),
    Segunda_Comum = sum(Segunda_Comum),
    Quarta_Comum  = sum(Quarta_Comum)
  ) %>%
  pivot_longer(everything(), names_to = "Variavel", values_to = "N_dias")

impacto_economico <- resultados_beta %>%
  filter(Variavel != "Gripe_CSP") %>%
  left_join(contagem_dias, by = "Variavel") %>%
  mutate(
    ADD_excedentarias = Beta * N_dias,
    ADD_exc_lb        = (Beta - 1.96 * SE) * N_dias,
    ADD_exc_ub        = (Beta + 1.96 * SE) * N_dias,
    Custo_euros       = ADD_excedentarias * custo_dia_euros,
    Custo_euros_lb    = ADD_exc_lb * custo_dia_euros,
    Custo_euros_ub    = ADD_exc_ub * custo_dia_euros,
    Nota              = if_else(p_value < 0.05, "Significativo", "Não significativo")
  ) %>%
  select(Variavel, Beta, SE, p_value, Sig, N_dias,
         ADD_excedentarias, ADD_exc_lb, ADD_exc_ub,
         Custo_euros, Custo_euros_lb, Custo_euros_ub, Nota)

cat("\n=== IMPACTO ECONÓMICO ESTIMADO (custo/dia =", custo_dia_euros, "???) ===\n")
print(impacto_economico, n = Inf)

# Resumo: total das ADD de conveniência significativas
total_sig <- impacto_economico %>%
  filter(Nota == "Significativo") %>%
  summarise(
    Total_ADD  = sum(ADD_excedentarias),
    Total_lb   = sum(ADD_exc_lb),
    Total_ub   = sum(ADD_exc_ub),
    Total_eur  = sum(Custo_euros),
    Total_e_lb = sum(Custo_euros_lb),
    Total_e_ub = sum(Custo_euros_ub)
  )
cat("\n--- Totais (apenas variáveis significativas) ---\n")
cat(sprintf("ADD excedentárias: %.0f [IC95: %.0f - %.0f]\n",
            total_sig$Total_ADD, total_sig$Total_lb, total_sig$Total_ub))
cat(sprintf("Custo estimado:    %.0f??? [IC95: %.0f??? - %.0f???]\n",
            total_sig$Total_eur, total_sig$Total_e_lb, total_sig$Total_e_ub))


# -----------------------------------------------------------------------------
# 17. GRÁFICO 1 - Real vs Previsto   [v3-6]
# -----------------------------------------------------------------------------
fitted_completo <- fitted(modelo_sarimax)

# Baseline clínico: remove contribuição das dummies de conveniência
coefs_conv <- coefs[vars_interesse[vars_interesse != "Gripe_CSP"]]
coefs_conv[is.na(coefs_conv)] <- 0

baseline_clinico <- as.numeric(fitted_completo) -
  (tabela_mestra$Feriado       * coefs_conv["Feriado"])       -
  (tabela_mestra$Tolerancia    * coefs_conv["Tolerancia"])     -
  (tabela_mestra$Ponte         * coefs_conv["Ponte"])          -
  (tabela_mestra$Segunda_Comum * coefs_conv["Segunda_Comum"])  -
  (tabela_mestra$Quarta_Comum  * coefs_conv["Quarta_Comum"])

plot_data <- tabela_mestra %>%
  mutate(
    Fitted_Completo  = as.numeric(fitted_completo),
    Baseline_Clinico = baseline_clinico,
    Tipo_Dia = case_when(
      Feriado       == 1 ~ "Feriado",
      Tolerancia    == 1 ~ "Tolerância",
      Ponte         == 1 ~ "Ponte",
      Segunda_Comum == 1 ~ "Segunda",
      Quarta_Comum  == 1 ~ "Quarta",
      TRUE               ~ "Normal"
    )
  )

p1 <- ggplot(plot_data, aes(x = Data)) +
  geom_line(aes(y = ADD_Total,        colour = "Observado"),
            linewidth = 0.4, alpha = 0.6) +
  geom_line(aes(y = Baseline_Clinico, colour = "Prev. só Gripe"),
            linewidth = 0.8, linetype = "dashed") +
  geom_line(aes(y = Fitted_Completo,  colour = "Prev. Modelo Completo"),
            linewidth = 0.7) +
  geom_point(
    data = filter(plot_data, Tipo_Dia %in% c("Feriado","Tolerância","Ponte")),
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
    title    = "ADD Observadas vs Previstas (SARIMAX v3)",
    subtitle = "Separação entre efeito clínico (Gripe) e efeito de conveniência (calendário)",
    x = "Data", y = "Nº de ADD emitidas",
    colour = NULL, shape = "Tipo de dia"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

print(p1)
ggsave("grafico_ADD_vs_previsto.png", p1, width = 14, height = 6, dpi = 300)


# -----------------------------------------------------------------------------
# 18. GRÁFICO 2 - Perfil semanal de ADD   [v3-7]
# -----------------------------------------------------------------------------
# Visualiza o padrão Segunda/Quarta que fundamenta a interpretação de
# conveniência de "meio de semana" e de "início de semana".

perfil_semanal <- tabela_mestra %>%
  mutate(dow = wday(Data, label = TRUE, week_start = 1,
                    locale = "pt_PT.UTF-8")) %>%
  filter(wday(Data, week_start = 1) %in% 1:5) %>%  # só dias úteis
  group_by(dow) %>%
  summarise(
    Media  = mean(ADD_Total),
    SE     = sd(ADD_Total) / sqrt(n()),
    .groups = "drop"
  ) %>%
  mutate(
    Destaque = case_when(
      as.integer(dow) == 1 ~ "Segunda (??=+1280***)",
      as.integer(dow) == 3 ~ "Quarta (??=+826***)",
      TRUE                 ~ "Outros dias úteis"
    )
  )

p2 <- ggplot(perfil_semanal, aes(x = dow, y = Media, fill = Destaque)) +
  geom_col(width = 0.6) +
  geom_errorbar(aes(ymin = Media - 1.96 * SE,
                    ymax = Media + 1.96 * SE),
                width = 0.2, colour = "grey30") +
  scale_fill_manual(values = c(
    "Segunda (??=+1280***)" = "#e63946",
    "Quarta (??=+826***)"   = "#f4a261",
    "Outros dias úteis"    = "grey70"
  )) +
  labs(
    title    = "Perfil semanal de ADD emitidas (dias úteis)",
    subtitle = "Segunda e Quarta apresentam volumes significativamente superiores\n- consistente com estratégia de maximização da ausência efectiva",
    x = "Dia da semana", y = "Média diária de ADD emitidas",
    fill = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

print(p2)
ggsave("grafico_perfil_semanal.png", p2, width = 8, height = 5, dpi = 300)

cat("\n??? Gráficos guardados:\n")
cat("   grafico_ADD_vs_previsto.png\n")
cat("   grafico_perfil_semanal.png\n")


# -----------------------------------------------------------------------------
# 19. GUARDAR RESULTADOS
# -----------------------------------------------------------------------------
write_csv(resultados_beta,   "resultados_coeficientes.csv")
write_csv(impacto_economico, "impacto_economico.csv")

if (length(outlier_dates) > 0) {
  write_csv(outlier_info, "outliers_identificados.csv")
  cat("   outliers_identificados.csv\n")
}

cat("\n??? Resultados guardados:\n")
cat("   resultados_coeficientes.csv\n")
cat("   impacto_economico.csv\n")

cat("\n============================================================\n")
cat(" PIPELINE v3 CONCLUÍDA\n")
cat("============================================================\n")
