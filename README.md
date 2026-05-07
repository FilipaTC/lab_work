# lab_work
Lab work for PLCDS


## 🛠️ Resumo da Pipeline Final Definida
A estratégia de análise seguiu uma abordagem de **Economia da Saúde** e **Econometria de Séries Temporais**:
1. **Modelo:** SARIMAX-X (Seasonal AutoRegressive Integrated Moving Average com regressores exógenos).
2. **Variável Dependente:** Volume diário de emissão de ADD.
3. **Controlo Clínico (Baseline):** Número de consultas por síndrome gripal nos Cuidados de Saúde Primários (CSP), servindo como proxy para a morbilidade real.
4. **Variáveis de Conveniência (Dummies):** - Hierarquia de variáveis binárias para isolar o impacto de Feriados Nacionais, Tolerâncias de Ponto e "Pontes".
   - Controlo de sazonalidade semanal (efeito Segunda e Sexta-feira).
5. **Objetivo de Output:** Estimar o coeficiente de "utilização não-clínica" e convertê-lo em perda de produtividade económica (Custo de Oportunidade) baseada no xxxx.
