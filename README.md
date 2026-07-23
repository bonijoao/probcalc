# ProbCalc - Calculadora de Distribuições de Probabilidade

[![Test and deploy](https://github.com/bonijoao/probcalc/actions/workflows/deploy.yml/badge.svg)](https://github.com/bonijoao/probcalc/actions/workflows/deploy.yml)

**🌐 Use online: <https://bonijoao.github.io/probcalc/>** (roda 100% no navegador via [shinylive](https://posit-dev.github.io/r-shinylive/)/webR — o primeiro carregamento demora um pouco)

## Sobre o Projeto
ProbCalc é uma aplicação web interativa desenvolvida para auxiliar estudantes e profissionais no cálculo e visualização de distribuições de probabilidade. A aplicação oferece uma interface intuitiva para trabalhar com várias distribuições estatísticas comuns, em três idiomas (português, inglês e espanhol).

## Funcionalidades

### Distribuições Suportadas
- Normal
- t-Student
- Qui-quadrado
- F
- Gamma
- Log-normal
- Pareto
- Weibull
- Binomial
- Geométrica (tentativas e fracassos)
- Hipergeométrica
- Beta
- Binomial Negativa (tentativas e fracassos)
- Exponencial
- Poisson

### Recursos
- Cálculo de probabilidades: P(X > x), P(X < x), 2P(|X| > x), P(X ≥ x), P(X ≤ x), P(X = x)
- Cálculo inverso (valor de x para uma probabilidade dada)
- Visualização gráfica com destaque da área/barras correspondentes
- Fórmulas matemáticas detalhadas para cada distribuição (MathJax)
- Interface trilíngue e responsiva

## Tecnologias Utilizadas
- R Shiny para o backend e frontend
- Bootstrap 5 (bslib) para estilização
- MathJax para renderização de fórmulas matemáticas
- ggplot2 para visualizações gráficas

## Estrutura do Projeto
```
├── app.R                 # UI e server
├── R/
│   ├── dist_registry.R   # registry central: parâmetros, funções d/p/q,
│   │                     #   validação e faixa de plot por distribuição
│   ├── calc.R            # funções puras de cálculo (testáveis)
│   ├── ui_helpers.R      # componentes de UI gerados a partir do registry
│   ├── i18n.R            # infraestrutura de tradução
│   └── translations_*.R  # textos da UI em pt/en/es
├── www/help_*.html       # fórmulas (HTML + LaTeX) por idioma
└── tests/testthat/       # testes automatizados
```
Para adicionar uma distribuição, basta criar uma entrada em `R/dist_registry.R` e as chaves de tradução correspondentes — UI, cálculo, validação e reset derivam do registry.

## Como Usar

### Pré-requisitos
- R instalado
- Pacotes necessários:
  ```R
  install.packages(c("shiny", "bslib", "ggplot2", "bsicons"))
  ```

### Executando a Aplicação
1. Clone o repositório
2. Abra o R ou RStudio na pasta do projeto
3. Execute:
   ```R
   shiny::runApp(".")
   ```

### Rodando os Testes
```R
install.packages("testthat")
testthat::test_dir("tests/testthat")
```

## Exemplos de Uso
1. Selecione uma distribuição
2. Insira os parâmetros necessários
3. Escolha o tipo de probabilidade
4. Insira o valor de x ou a probabilidade desejada
5. Visualize o resultado no gráfico e o valor calculado

## Contribuindo
Contribuições são bem-vindas! Sinta-se à vontade para:
- Reportar bugs
- Sugerir novas funcionalidades
- Melhorar a documentação
- Submeter pull requests

## Licença
Este projeto está sob a licença [MIT](LICENSE).
