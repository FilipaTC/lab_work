# 🤖 Registo de Interação com IA (Consultoria Metodológica)

Este documento regista os prompts utilizados no projeto. Foram consultados diferentes LLMs, nomeadamente Claude, ChatGPT e Gemini, o que permitiu comparar abordagens estatísticas, validar a hierarquia de variáveis e refinar a pipeline. 

## 📝 Prompts Utilizados

## 🤖 Gemini (Foco: definição da questão de investigação e pipeline de análise)
### 1. Definição da Questão de Investigação e Controlo de Confundimento
**Prompt:**
> "Estou a desenhar um estudo sobre a emissão de Autodeclarações de Doença (ADD) em Portugal e a sua relação com dias festivos. Utilizo dados diários de maio/2023 a janeiro/2026. Identifiquei que a sazonalidade das infeções respiratórias no inverno é um fator confundidor crítico. Como posso estruturar um modelo estatístico que isole o efeito de conveniência social (feriados/pontes/tolerâncias) do efeito clínico (proxy gripe), garantindo que os picos de ADD não são atribuídos erroneamente a conveniência quando podem ser causados por surtos epidemiológicos expectáveis?"

### 2. Estrutura de Dados e Hierarquia de Variáveis (Pipeline)
**Prompt:**
> "Para a pipeline de análise, tenho disponível uma base de dados diária com as seguintes variáveis: ADD (variável dependente), Consultas de Gripe nos CSP (controlo clínico), Feriados Nacionais, Tolerâncias de Ponto, Pontes, Segundas e Sextas-feiras. Como devo gerir a hierarquia destas variáveis binárias (dummies) para evitar multicolinearidade, garantindo que o modelo SARIMAX interpreta corretamente uma segunda-feira que é também uma ponte ou um feriado?"

### 3. Modelo de Análise e Impacto Económico
**Prompt:**
> "Explica a pipeline técnica para a execução de um modelo SARIMAX $(p,d,q) \times (P,D,Q)s$ com regressores externos para este estudo. Como posso utilizar os coeficientes ($\beta$) obtidos para as variáveis de conveniência para calcular o excesso de ADD (valores observados vs. esperados pelo modelo clínico) e converter esse valor numa estimativa de perda de produtividade económica baseada no salário médio diário em Portugal?"

---

## 🤖 Claude (Foco: Atualização das pipelines)
### 1. Correção da deteção de pontes
**Prompt:**
> "[Secções 4 e 5 da pipeline original]" "Este código devolve 147 pontes. É demasiado? (considera Portugal)"

---
### 2. Deteção de erros na pipeline original
**Prompt:**
> "És um revisor académico com background de data science e quero que revejas este código que tenta estimar economicamente o custo das autodeclarações de doença por coveniência"
> "[Resultados do Ljung-Box test e do Box-Ljung test]"

---
### 3. Segunda versão da pipeline
**Prompt:**
> "Gera um código com as correções anteriormente mencionadas"

---
### 4. Teste de resíduos da v2
**Prompt:**
> "O teste de resíduos continua a não se ajustar ao modelo, mesmo quando se aumenta o k para 4 ou 5. Achas que uma reconstrução do modelo, adicionando as quartas feiras comuns e retirando as variáveis não significativas, poderia melhorar os testes de resíduos?"

---
### 5. Efeito das quartas-feiras
**Prompt:**
> "No entanto, acrescentei as quartas feiras ao modelo para testar e obtive valores significativos. Já que o máximo de dias das autodeclarações de doença são 3, esta relação pode indicar que as pessoas que pedem a declaração às quartas feiras o fazem por conveniência. Concordas?"
> "Como enquadrar as quartas comuns e as quartas pre especiais na tabela mestra sem que façam overlap?"
> "[Resultados dos coeficientes de interesse com a adição de quartas comuns e quartas pré especiais]"

---
### 6. Construção da v3
**Prompt:**
> "Reescreve o código tendo em conta estes resultados e o problema dos resíduos."


## 🤖 ChatGPT (Foco: )
### 1. K-fourier
**Prompt:**
> [Resultados do Ljung-Box test e do Box-Ljung test] Sugere melhorias para estes resultados.
---
