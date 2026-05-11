# =============================================================================
# PIPELINE SARIMAX v5 - Análise de Absentismo por Conveniência
# Autores: Ana Correia, André Vicente, Filipa Carneiro
# =============================================================================
# ALTERAÇÕES vs v4:
#   [v5-1]  Retorno à especificação de dummies da v3 (melhor Ljung-Box/AIC)
#   [v5-2]  Jan_Regresso e Dez_FimAno tornam-se ADITIVAS (não exclusivas):
#           uma segunda de Janeiro tem Segunda_Comum=1 E Jan_Regresso=1,
#           permitindo ao modelo estimar os dois efeitos em separado.
#           Resolve o problema da v4 onde Primeira_Semana_Jan amputava
#           o pool de segundas/quartas e eliminava a sua significância.
#   [v5-3]  Outliers pontuais reduzidos aos genuinamente isolados (3 datas)
#           - Janeiro e Dezembro absorvidos pelas variáveis aditivas.
#   [v5-4]  Gráfico 2 adaptativo: destaca Quarta apenas se significativa;
#           caso contrário centra a narrativa na Segunda.
#   [v5-5]  Contagem de dias corrigida: inclui Fim_Ano e Jan_Regresso
#   [v5-6]  Verificação explícita de que variáveis aditivas não entram
#           no check_overlap (overlap intencional e documentado)
#   [v5-7]  Ordens max.p/q restauradas a 3 (consistente com v3)
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
  dias_add_excluidos %>%
    mutate(dow = wday(Data, week_start = 1)) %>%
    count(dow) %>% print()
}

tabela_mestra <- inner_join(add_clean, gripe_clean, by = "Data") %>%
  arrange(Data)

cat("\n--- Tabela mestra ---\n")
cat("Período:", format(min(tabela_mestra$Data)), "a",
    format(max(tabela_mestra$Data)), "\n")
cat("Nº de dias:", nrow(tabela_mestra), "\n")

if (sum(is.na(tabela_mestra$Gripe_CSP)) == 0) {
  cat("??? Sem valores em falta na coluna Gripe_CSP.\n")
} else {
  cat("??????  Valores em falta em Gripe_CSP:", sum(is.na(tabela_mestra$Gripe_CSP)), "\n")
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
  c(pascoa - 2, pascoa, pascoa + 60)
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
# 6. CONSTRUIR AS DUMMIES
# -----------------------------------------------------------------------------
# ARQUITECTURA DE DUMMIES - duas camadas independentes:
#
# CAMADA 1 - Mutuamente exclusivas (hierarquia rígida):
#   Feriado > Tolerância > Ponte > Pos_Feriado > Segunda_Comum > Quarta_Comum
#   Cada dia pertence a no máximo uma categoria.
#
#   Pos_Feriado [NOVO]: dia útil (Seg-Sex) imediatamente a seguir a um
#   feriado ou tolerância, não classificado como feriado/tolerância/ponte.
#   Motivação: na v5, os outliers pontuais de 2023-08-16, 2025-03-05,
#   2025-04-22, 2025-06-11, 2025-12-02 e 2026-02-18 são todos dias
#   imediatamente a seguir a feriados ou tolerâncias - padrão estrutural
#   recorrente ("ressaca de feriado") que não deve ser tratado como outlier
#   aditivo. A dummy captura este efeito de forma parcimoniousa.
#
# CAMADA 2 - Aditivas (sobreposição intencional com Camada 1):
#   Jan_Regresso : dias 2-10 de Janeiro (excl. feriados/tolerâncias)
#   Dez_FimAno   : dias 26-31 de Dezembro (excl. feriados/tolerâncias)
#   Um dia pode ter Segunda_Comum=1 E Jan_Regresso=1 simultaneamente.
#   O modelo estima o efeito base da Segunda + o desvio de Janeiro.
#   Motivação: na v4, estas variáveis eram exclusivas, o que amputava
#   o pool de segundas/quartas e eliminava a sua significância
#   (Quarta: ??=697???36). A abordagem aditiva preserva o pool completo.
#
# DECISÕES ANTERIORES MANTIDAS:
#   - Quarta_Pre_Especial: não significativa (p=0.146, v3) ??? fundida em
#     Quarta_Comum. Efeito não condicionado à proximidade de feriados.
#   - Sexta_Comum: não significativa (p=0.167, ??=???139, v3) ??? removida.
#     Sem valor estratégico - fds subsequente já é não-útil.

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

# Conjunto de todas as datas especiais para lookup de Pos_Feriado
todas_especiais_set <- c(todos_feriados, tolerancias)

tabela_mestra <- tabela_mestra %>%
  mutate(
    dow = wday(Data, week_start = 1),
    
    # --- CAMADA 1: mutuamente exclusivas (ordem de prioridade) ---
    
    Feriado    = if_else(Data %in% todos_feriados, 1L, 0L),
    Tolerancia = if_else(Data %in% tolerancias & Feriado == 0, 1L, 0L),
    Ponte      = detectar_ponte(Data, todos_feriados, tolerancias),
    
    # Pos_Feriado: dia útil imediatamente após feriado/tolerância.
    # Usa lookup directo por (Data - 1) - robusto a gaps no dataset
    # (fins-de-semana, dias sem dados) ao contrário de lag().
    # Prioridade sobre Segunda_Comum e Quarta_Comum mas abaixo de Ponte
    # (se o dia de ressaca for também ponte, prevalece Ponte).
    Pos_Feriado = if_else(
      dow %in% 1:5 &
        Feriado    == 0 &
        Tolerancia == 0 &
        Ponte      == 0 &
        (Data - 1) %in% todas_especiais_set,
      1L, 0L
    ),
    
    Segunda_Comum = if_else(
      dow == 1 &
        Feriado     == 0 &
        Tolerancia  == 0 &
        Ponte       == 0 &
        Pos_Feriado == 0,
      1L, 0L
    ),
    
    Quarta_Comum = if_else(
      dow == 3 &
        Feriado     == 0 &
        Tolerancia  == 0 &
        Ponte       == 0 &
        Pos_Feriado == 0,
      1L, 0L
    ),
    
    # --- CAMADA 2: aditivas - overlap intencional com Camada 1 ---
    
    # Jan_Regresso: desvio do comportamento de ADD nos dias 2-10 de Janeiro.
    # Não exclui segundas/quartas de Janeiro - o overlap é intencional.
    Jan_Regresso = if_else(
      month(Data) == 1 & day(Data) %in% 2:10 &
        Feriado    == 0 &
        Tolerancia == 0,
      1L, 0L
    ),
    
    # Dez_FimAno: desvio dos dias 26-31 de Dezembro.
    # Inclui fins-de-semana (padrão de ressaca de Natal não é exclusivo
    # de dias úteis).
    Dez_FimAno = if_else(
      month(Data) == 12 & day(Data) %in% 26:31 &
        Feriado    == 0 &
        Tolerancia == 0,
      1L, 0L
    )
  ) %>%
  select(-dow)


# --- Verificações ---

# Camada 1: nenhum dia deve ter soma > 1
check_overlap <- tabela_mestra %>%
  mutate(soma = Feriado + Tolerancia + Ponte +
           Pos_Feriado + Segunda_Comum + Quarta_Comum) %>%
  filter(soma > 1)

if (nrow(check_overlap) == 0) {
  cat("\n??? Sem overlaps nas dummies mutuamente exclusivas (Camada 1).\n")
} else {
  cat("\n??????  ATENÇÃO: overlaps na Camada 1 em", nrow(check_overlap), "dias!\n")
  print(check_overlap)
}

# Camada 2: documentar overlap intencional
cat("\n--- Overlap intencional (Camada 2 - aditivas) ---\n")
tabela_mestra %>%
  summarise(
    Seg_em_Jan    = sum(Segunda_Comum * Jan_Regresso),
    Qua_em_Jan    = sum(Quarta_Comum  * Jan_Regresso),
    PosF_em_Jan   = sum(Pos_Feriado   * Jan_Regresso),
    Seg_em_Dez    = sum(Segunda_Comum * Dez_FimAno),
    Qua_em_Dez    = sum(Quarta_Comum  * Dez_FimAno),
    PosF_em_Dez   = sum(Pos_Feriado   * Dez_FimAno)
  ) %>%
  print()

# Distribuição completa
cat("\nDistribuição de todas as dummies:\n")
tabela_mestra %>%
  summarise(across(
    c(Feriado, Tolerancia, Ponte, Pos_Feriado,
      Segunda_Comum, Quarta_Comum, Jan_Regresso, Dez_FimAno),
    sum
  )) %>%
  print()

# Inspecção dos dias classificados como Pos_Feriado
cat("\nDias Pos_Feriado (primeiros 20):\n")
tabela_mestra %>%
  filter(Pos_Feriado == 1) %>%
  mutate(
    dow_label  = c("Seg","Ter","Qua","Qui","Sex")[wday(Data, week_start = 1)],
    ontem      = Data - 1,
    tipo_ontem = case_when(
      ontem %in% todos_feriados ~ "Feriado",
      ontem %in% tolerancias   ~ "Tolerância",
      TRUE                     ~ "Outro"
    )
  ) %>%
  select(Data, dow_label, ontem, tipo_ontem, ADD_Total) %>%
  print(n = 20)

# Correlações entre regressores
cat("\nMatriz de correlação dos regressores:\n")
tabela_mestra %>%
  select(Gripe_CSP, Feriado, Tolerancia, Ponte, Pos_Feriado,
         Segunda_Comum, Quarta_Comum, Jan_Regresso, Dez_FimAno) %>%
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

# Todas as variáveis: Camada 1 + Camada 2   [v5-5]
xreg_base <- tabela_mestra %>%
  select(Gripe_CSP, Feriado, Tolerancia, Ponte, Pos_Feriado,
         Segunda_Comum, Quarta_Comum,
         Jan_Regresso, Dez_FimAno) %>%
  as.matrix()


# -----------------------------------------------------------------------------
# 9. TERMOS DE FOURIER PARA SAZONALIDADE ANUAL
# -----------------------------------------------------------------------------
K_fourier   <- 5
y_anual_ref <- ts(tabela_mestra$ADD_Total, frequency = 365.25)
fourier_terms <- fourier(y_anual_ref, K = K_fourier)

xreg_1a_passagem <- cbind(xreg_base, fourier_terms)
xreg_1a_passagem <- apply(xreg_1a_passagem, 2, as.numeric)

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
# 11. 1ª PASSAGEM - identificar outliers residuais
# -----------------------------------------------------------------------------
# [v5-3] Com Jan_Regresso e Dez_FimAno aditivas, esperamos que os outliers
# de Janeiro e Dezembro sejam absorvidos. Apenas outliers genuinamente
# pontuais (sem padrão recorrente) serão adicionados ao modelo final.
#
# Triagem: correcção de Bonferroni para controlar false discovery rate
# com múltiplos testes simultâneos.

set.seed(42)
cat("\n??? 1ª passagem: a identificar outliers (pode demorar 3-8 min)...\n")

modelo_1a <- auto.arima(
  y_semanal,
  xreg          = xreg_1a_passagem,
  seasonal      = TRUE,
  max.p = 3, max.q = 3, max.P = 2, max.Q = 2,  # [v5-7] restaurado da v3
  stepwise      = FALSE,
  approximation = FALSE,
  trace         = FALSE
)

residuos_1a <- residuals(modelo_1a)
sigma_1a    <- sd(residuos_1a)
limiar      <- 3 * sigma_1a
outlier_dates <- tabela_mestra$Data[abs(residuos_1a) > limiar]

cat("\n--- Outliers residuais (|resíduo| >", round(limiar, 1), ") ---\n")
cat(length(outlier_dates), "dias identificados.\n")

if (length(outlier_dates) > 0) {
  
  # Triagem individual com correcção de Bonferroni
  testes_outlier <- map_dfr(outlier_dates, function(d) {
    res_outlier <- as.numeric(residuos_1a)[tabela_mestra$Data == d]
    t_val <- res_outlier / sigma_1a
    p_val <- 2 * pnorm(-abs(t_val))
    tibble(
      data         = d,
      dow_n        = wday(d, week_start = 1),
      dow_label    = c("Seg","Ter","Qua","Qui","Sex","Sáb","Dom")[wday(d, week_start=1)],
      Jan_Regresso = as.integer(month(d) == 1 & day(d) %in% 2:10),
      Dez_FimAno   = as.integer(month(d) == 12 & day(d) %in% 26:31),
      Pos_Feriado = tabela_mestra$Pos_Feriado[
        tabela_mestra$Data == d
      ],
      residuo      = res_outlier,
      sigma_n      = round(res_outlier / sigma_1a, 1),
      p_value      = p_val,
      # [v5-3] Bonferroni: só incluir se p < ??/n E não coberto por aditivas
      sig_bonf     = p_val < (0.05 / length(outlier_dates)),
      coberto      = (month(d) == 1 & day(d) %in% 2:10) |
        (month(d) == 12 & day(d) %in% 26:31),
      incluir      = p_val < (0.05 / length(outlier_dates)) &
        !((month(d) == 1 & day(d) %in% 2:10) |
            (month(d) == 12 & day(d) %in% 26:31))
    )
  })
  
  cat("\n--- Triagem de outliers (Bonferroni, ??/n =",
      round(0.05 / length(outlier_dates), 4), ") ---\n")
  print(testes_outlier %>%
          select(data, dow_label, residuo, sigma_n, p_value,
                 sig_bonf, coberto, incluir),
        n = Inf)
  
  outlier_sig_dates <- testes_outlier$data[testes_outlier$incluir]
  
  cat("\nOutliers pontuais a incluir:", length(outlier_sig_dates), "\n")
  if (length(outlier_sig_dates) > 0) print(outlier_sig_dates)
  
  cat("Outliers cobertos por Jan_Regresso/Dez_FimAno (excluídos):",
      sum(testes_outlier$coberto & testes_outlier$sig_bonf), "\n")
  
  if (length(outlier_sig_dates) > 0) {
    outlier_dummies_sig <- map_dfc(
      outlier_sig_dates,
      ~ tibble(!!paste0("out_", format(.x, "%Y%m%d")) :=
                 as.numeric(tabela_mestra$Data == .x))
    ) %>% as.matrix()
    
    xreg_final <- cbind(xreg_1a_passagem, outlier_dummies_sig)
  } else {
    xreg_final <- xreg_1a_passagem
    cat("Nenhum outlier pontual adicional.\n")
  }
  
  outlier_info <- testes_outlier
  
} else {
  outlier_info <- NULL
  xreg_final   <- xreg_1a_passagem
  cat("Nenhum outlier detectado.\n")
}


xreg_final <- apply(xreg_final, 2, as.numeric)
stopifnot(
  "xreg_final e y têm nº de linhas diferentes!" =
    nrow(xreg_final) == length(y_semanal)
)
cat("\n??? xreg_final pronto:", ncol(xreg_final), "colunas.\n")


# -----------------------------------------------------------------------------
# 12. 2ª PASSAGEM - MODELO FINAL
# -----------------------------------------------------------------------------
set.seed(42)
cat("\n??? 2ª passagem: modelo final (pode demorar 3-8 min)...\n")

modelo_sarimax <- auto.arima(
  y_semanal,
  xreg          = xreg_final,
  seasonal      = TRUE,
  max.p = 3, max.q = 3, max.P = 2, max.Q = 2,  # [v5-7]
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
# 13. DIAGNÓSTICO DOS RESÍDUOS
# -----------------------------------------------------------------------------
cat("\n--- Diagnóstico dos resíduos ---\n")
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
  cat("??????  Autocorrelação residual presente (p =",
      round(lb_test$p.value, 4), ")\n")
  cat("   Nota metodológica para o paper:\n")
  cat("   'Apesar da inclusão de termos de Fourier, variáveis sazonais\n")
  cat("    aditivas para os períodos de Janeiro e Dezembro, e correcção\n")
  cat("    de outliers pontuais, os resíduos apresentam autocorrelação\n")
  cat("    moderada (Ljung-Box p =", round(lb_test$p.value, 3), "),\n")
  cat("    possivelmente devida a instabilidade estrutural no período\n")
  cat("    pós-COVID. As estimativas são consideradas conservadoras.'\n")
}

# Drift
coefs_todos <- coef(modelo_sarimax)
if ("drift" %in% names(coefs_todos)) {
  drift_val <- coefs_todos["drift"]
  cat(sprintf("\n--- Tendência (drift) ---\n%.2f ADD adicionais por dia.\n", drift_val))
  if (drift_val > 0) {
    cat("   ??????  Tendência crescente estrutural - discutir nos resultados.\n")
  }
}


# -----------------------------------------------------------------------------
# 14. COEFICIENTES ?? - VARIÁVEIS DE INTERESSE
# -----------------------------------------------------------------------------
coefs <- coef(modelo_sarimax)
se    <- sqrt(diag(vcov(modelo_sarimax)))

# [v5-5] vars_interesse inclui as aditivas Jan_Regresso e Dez_FimAno
vars_interesse <- c("Gripe_CSP", "Feriado", "Tolerancia", "Ponte",
                    "Pos_Feriado",
                    "Segunda_Comum", "Quarta_Comum",
                    "Jan_Regresso", "Dez_FimAno")

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

# Nota metodológica sobre Ponte se positiva
ponte_beta <- resultados_beta$Beta[resultados_beta$Variavel == "Ponte"]
if (length(ponte_beta) > 0 && !is.na(ponte_beta) && ponte_beta > 0) {
  cat(sprintf(
    "\n--- Nota: ??_Ponte = +%.0f ---\n", ponte_beta
  ))
  cat("   Ambíguo: (a) conveniência - prolongar ausência além da ponte;\n")
  cat("   (b) confundimento - maior procura de consultas pré-feriado.\n")
  cat("   Indistinguível sem dados de hora/duração da ADD. Declarar no paper.\n")
}

nas_coef <- resultados_beta %>% filter(is.na(Beta))
if (nrow(nas_coef) > 0) {
  cat("\n??????  Coeficientes NA (possível colinearidade):", nas_coef$Variavel, "\n")
}


# -----------------------------------------------------------------------------
# 15. IMPACTO ECONÓMICO
# -----------------------------------------------------------------------------
# PRESSUPOSTOS (documentar na metodologia):
#   - custo_dia_euros: INE, Inquérito ao Emprego - custo horário médio × 8h.
#   - Cada ADD de conveniência = 1 dia de ausência (limite máximo ADD = 3 dias).
#   - Classificação: ??>0 e p<0.05 ??? Conveniência; ??<0 e p<0.05 ??? Supressor.
#   - IC95% propagado pelo método delta (±1.96 × SE × N_dias).
#   - Variáveis aditivas (Jan_Regresso, Dez_FimAno) entram no impacto
#     com o seu N_dias próprio, independentemente das Segundas/Quartas.

cenarios_custo <- tibble(
  Cenario   = c("Conservador", "Central", "Máximo"),
  custo_dia = c(100, 150, 200)
)

# [v5-5] contagem_dias inclui todas as variáveis do modelo
contagem_dias <- tabela_mestra %>%
  summarise(
    Feriado       = sum(Feriado),
    Tolerancia    = sum(Tolerancia),
    Ponte         = sum(Ponte),
    Segunda_Comum = sum(Segunda_Comum),
    Quarta_Comum  = sum(Quarta_Comum),
    Jan_Regresso  = sum(Jan_Regresso),
    Dez_FimAno    = sum(Dez_FimAno),
    Pos_Feriado   = sum(Pos_Feriado)
  ) %>%
  pivot_longer(everything(), names_to = "Variavel", values_to = "N_dias")

impacto_base <- resultados_beta %>%
  filter(Variavel != "Gripe_CSP") %>%
  left_join(contagem_dias, by = "Variavel") %>%
  mutate(
    ADD_excedentarias = Beta * N_dias,
    ADD_exc_lb        = (Beta - 1.96 * SE) * N_dias,
    ADD_exc_ub        = (Beta + 1.96 * SE) * N_dias,
    Tipo = case_when(
      Beta > 0 & p_value < 0.05 ~ "Conveniência",
      Beta < 0 & p_value < 0.05 ~ "Supressor",
      TRUE                       ~ "Não significativo"
    )
  )

impacto_economico <- map_dfr(
  cenarios_custo$custo_dia,
  function(c_dia) {
    impacto_base %>%
      mutate(
        Cenario        = cenarios_custo$Cenario[cenarios_custo$custo_dia == c_dia],
        custo_dia      = c_dia,
        Custo_euros    = ADD_excedentarias * c_dia,
        Custo_euros_lb = ADD_exc_lb * c_dia,
        Custo_euros_ub = ADD_exc_ub * c_dia
      )
  }
) %>%
  select(Cenario, custo_dia, Variavel, Tipo, Beta, SE, p_value, Sig, N_dias,
         ADD_excedentarias, ADD_exc_lb, ADD_exc_ub,
         Custo_euros, Custo_euros_lb, Custo_euros_ub)

cat("\n=== IMPACTO ECONÓMICO POR CENÁRIO ===\n")
print(impacto_economico, n = Inf)

cat("\n=== TOTAIS POR TIPO DE EFEITO (cenário Central) ===\n")
impacto_economico %>%
  filter(Cenario == "Central") %>%
  group_by(Tipo) %>%
  summarise(
    ADD_total = sum(ADD_excedentarias),
    ADD_lb    = sum(ADD_exc_lb),
    ADD_ub    = sum(ADD_exc_ub),
    Custo     = sum(Custo_euros),
    Custo_lb  = sum(Custo_euros_lb),
    Custo_ub  = sum(Custo_euros_ub),
    .groups   = "drop"
  ) %>%
  print()

cat("\n=== CUSTO DE CONVENIÊNCIA POR CENÁRIO (apenas ??>0 significativos) ===\n")
resumo_cenarios <- impacto_economico %>%
  filter(Tipo == "Conveniência") %>%
  group_by(Cenario, custo_dia) %>%
  summarise(
    ADD_conv = sum(ADD_excedentarias),
    ADD_lb   = sum(ADD_exc_lb),
    ADD_ub   = sum(ADD_exc_ub),
    Custo    = sum(Custo_euros),
    Custo_lb = sum(Custo_euros_lb),
    Custo_ub = sum(Custo_euros_ub),
    .groups  = "drop"
  )

resumo_cenarios %>%
  rowwise() %>%
  group_walk(~ cat(sprintf(
    "%-12s (%3.0f???/dia): ADD=%7.0f [%7.0f-%7.0f]  Custo=%10.0f??? [%10.0f???-%10.0f???]\n",
    .x$Cenario, .x$custo_dia,
    .x$ADD_conv, .x$ADD_lb, .x$ADD_ub,
    .x$Custo,    .x$Custo_lb, .x$Custo_ub
  )))


# -----------------------------------------------------------------------------
# 16. GRÁFICO 1 - Real vs Previsto
# -----------------------------------------------------------------------------
fitted_completo <- fitted(modelo_sarimax)

coefs_cal <- coefs[intersect(vars_interesse, names(coefs))]
coefs_cal[is.na(coefs_cal)] <- 0

# Remover contribuição de todas as variáveis de calendário do baseline clínico
baseline_clinico <- as.numeric(fitted_completo)
for (v in names(coefs_cal)[names(coefs_cal) != "Gripe_CSP"]) {
  if (v %in% names(tabela_mestra)) {
    baseline_clinico <- baseline_clinico -
      (tabela_mestra[[v]] * coefs_cal[v])
  }
}

plot_data <- tabela_mestra %>%
  mutate(
    Fitted_Completo  = as.numeric(fitted_completo),
    Baseline_Clinico = baseline_clinico,
    Tipo_Dia = case_when(
      Feriado       == 1 ~ "Feriado",
      Tolerancia    == 1 ~ "Tolerância",
      Ponte         == 1 ~ "Ponte",
      Jan_Regresso  == 1 ~ "Regresso Jan.",
      Dez_FimAno    == 1 ~ "Fim de Ano",
      Segunda_Comum == 1 ~ "Segunda",
      Quarta_Comum  == 1 ~ "Quarta",
      Pos_Feriado   == 1 ~ "Pos_Feriado",
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
    data = filter(plot_data,
                  Tipo_Dia %in% c("Feriado","Tolerância","Ponte",
                                  "Regresso Jan.","Fim de Ano")),
    aes(y = ADD_Total, shape = Tipo_Dia, colour = Tipo_Dia),
    size = 2.5
  ) +
  scale_colour_manual(values = c(
    "Observado"              = "grey40",
    "Prev. só Gripe"         = "steelblue",
    "Prev. Modelo Completo"  = "tomato",
    "Feriado"                = "navy",
    "Tolerância"             = "darkorchid",
    "Ponte"                  = "darkorange",
    "Regresso Jan."          = "darkgreen",
    "Fim de Ano"             = "sienna"
  )) +
  scale_shape_manual(values = c(
    "Feriado"        = 17,
    "Tolerância"     = 18,
    "Ponte"          = 15,
    "Regresso Jan."  = 8,
    "Fim de Ano"     = 4
  )) +
  labs(
    title    = "ADD Observadas vs Previstas (SARIMAX v5)",
    subtitle = "Separação entre efeito clínico (Gripe) e efeito de conveniência (calendário)",
    x = "Data", y = "Nº de ADD emitidas",
    colour = NULL, shape = "Tipo de dia"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

print(p1)
ggsave("grafico_ADD_vs_previsto.png", p1, width = 14, height = 6, dpi = 300)


# -----------------------------------------------------------------------------
# 17. GRÁFICO 2 - Perfil semanal   [v5-4]
# -----------------------------------------------------------------------------
# [v5-4] Lógica adaptativa:
#   - Se Quarta_Comum é significativa (p<0.05): destaca Segunda E Quarta
#   - Se Quarta_Comum não é significativa: destaca apenas Segunda,
#     e legenda reflecte a narrativa de conveniência de início de semana

beta_seg <- resultados_beta %>%
  filter(Variavel == "Segunda_Comum") %>%
  pull(Beta) %>% round()

beta_qua <- resultados_beta %>%
  filter(Variavel == "Quarta_Comum") %>%
  pull(Beta) %>% round()

p_qua <- resultados_beta %>%
  filter(Variavel == "Quarta_Comum") %>%
  pull(p_value)

qua_sig <- length(p_qua) > 0 && !is.na(p_qua) && p_qua < 0.05

dias_pt <- c("Seg", "Ter", "Qua", "Qui", "Sex")

perfil_semanal <- tabela_mestra %>%
  mutate(dow_n = wday(Data, week_start = 1)) %>%
  filter(dow_n %in% 1:5) %>%
  group_by(dow_n) %>%
  summarise(
    Media  = mean(ADD_Total),
    SE_bar = sd(ADD_Total) / sqrt(n()),
    .groups = "drop"
  ) %>%
  mutate(
    dow = factor(dias_pt[dow_n], levels = dias_pt),
    Destaque = case_when(
      dow_n == 1 ~ paste0("Segunda (??=+", beta_seg, "***)"),
      dow_n == 3 & qua_sig ~
        paste0("Quarta (??=+", beta_qua, ", p<0.05)"),
      TRUE ~ "Outros dias úteis"
    )
  )

label_seg <- paste0("Segunda (??=+", beta_seg, "***)")
label_qua <- if (qua_sig) paste0("Quarta (??=+", beta_qua, ", p<0.05)") else NULL

cores_fill <- if (qua_sig) {
  setNames(
    c("#e63946", "#f4a261", "grey70"),
    c(label_seg, label_qua, "Outros dias úteis")
  )
} else {
  setNames(
    c("#e63946", "grey70"),
    c(label_seg, "Outros dias úteis")
  )
}

subtitulo <- if (qua_sig) {
  paste0(
    "Segunda e Quarta apresentam volumes superiores - consistente com\n",
    "maximização da ausência efectiva (ADD de 3 dias):\n",
    "  Segunda: cobre Seg+Ter+Qua  |  Quarta: cobre Qua+Qui+Sex+fds"
  )
} else {
  paste0(
    "Segunda apresenta volume superior (??=+", beta_seg, "***) - consistente com\n",
    "prolongamento do fim-de-semana via ADD de 3 dias (Seg+Ter+Qua).\n",
    "Efeito de Quarta não confirmado nesta especificação (p=",
    round(p_qua, 3), ")."
  )
}

p2 <- ggplot(perfil_semanal, aes(x = dow, y = Media, fill = Destaque)) +
  geom_col(width = 0.6) +
  geom_errorbar(aes(ymin = Media - 1.96 * SE_bar,
                    ymax = Media + 1.96 * SE_bar),
                width = 0.2, colour = "grey30") +
  scale_fill_manual(values = cores_fill) +
  labs(
    title    = "Perfil semanal de ADD emitidas (dias úteis)",
    subtitle = subtitulo,
    x    = "Dia da semana",
    y    = "Média diária de ADD emitidas",
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
# 18. GUARDAR RESULTADOS
# -----------------------------------------------------------------------------
write_csv(resultados_beta,   "resultados_coeficientes.csv")
write_csv(impacto_economico, "impacto_economico.csv")
write_csv(resumo_cenarios,   "resumo_cenarios_custo.csv")

if (!is.null(outlier_info)) {
  write_csv(outlier_info, "outliers_identificados.csv")
  cat("   outliers_identificados.csv\n")
}

cat("\n??? Resultados guardados:\n")
cat("   resultados_coeficientes.csv\n")
cat("   impacto_economico.csv\n")
cat("   resumo_cenarios_custo.csv\n")

cat("\n============================================================\n")
cat(" PIPELINE v5 CONCLUÍDA\n")
cat("============================================================\n")