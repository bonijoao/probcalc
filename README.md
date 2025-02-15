# ProbCalc - Calculadora de Distribuições de Probabilidade

## Sobre o Projeto
ProbCalc é uma aplicação web interativa desenvolvida para auxiliar estudantes e profissionais no cálculo e visualização de distribuições de probabilidade. A aplicação oferece uma interface intuitiva para trabalhar com várias distribuições estatísticas comuns.

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
- Binomial Negativa (tentativas e fracassos)
- Exponencial
- Poisson

### Recursos
- Cálculo de probabilidades
- Visualização gráfica interativa
- Suporte a diferentes tipos de cálculos:
  - P(X > x)
  - P(X < x)
  - P(X = x)
  - Cálculo de valores críticos
- Fórmulas matemáticas detalhadas para cada distribuição
- Interface responsiva e moderna

## Tecnologias Utilizadas
- R Shiny para o backend e frontend
- Bootstrap 5 para estilização
- MathJax para renderização de fórmulas matemáticas
- ggplot2 para visualizações gráficas

## Como Usar

### Pré-requisitos
- R instalado
- Pacotes necessários:
  ```R
  install.packages(c("shiny", "bslib", "ggplot2", "bsicons"))
  ```

### Executando a Aplicação
1. Clone o repositório
2. Abra o R ou RStudio
3. Execute:
   ```R
   shiny::runApp("caminho/para/o/projeto")
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
Este projeto está sob a licença MIT.