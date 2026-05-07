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

## 🤖 Claude (Foco: )

---

## 🤖 ChatGPT (Foco: )

---
