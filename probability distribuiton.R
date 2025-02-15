library(shiny)
library(bslib)
library(ggplot2)
library(bsicons)

options(shiny.autoreload = TRUE)
options(shiny.launch.browser = TRUE)

ui <- function(request) {
  page_fillable(
    theme = bs_theme(version = 5, preset = "shiny"),
    
    # Atualizar suporte ao MathJax
    tags$head(
      tags$script(src = "https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js"),
      tags$script(type = "text/javascript",
        "window.MathJax = {
          tex: {
            inlineMath: [['\\\\(', '\\\\)']],
            displayMath: [['\\\\[', '\\\\]']],
            processEscapes: true,
            packages: ['base', 'ams']
          },
          loader: {load: ['[tex]/ams']},
          startup: {
            pageReady: () => {
              return MathJax.startup.defaultPageReady();
            }
          }
        };"
      )
    ),
    
    # Título e controles de língua
    layout_columns(
      col_widths = c(8, 4),
      
      # Título principal
      h1(textOutput("title"), class = "text-center"),
      
      div(
        style = "text-align: right;",
        actionButton(
          "settings_btn",
          label = NULL,
          icon = icon("gear"),
                     class = "btn btn-link", 
          style = "font-size: 24px; padding: 0px 4px; color: #666;"
        )
      )
    ),
    
    layout_columns(
      col_widths = c(4, 8),
      
      uiOutput("params_card"),
      uiOutput("plot_card")
    )
  )
}

server <- function(input, output, session) {
translations <- list(
  pt = list(
      title = "ProbCalc",
    params = "Parâmetros",
    dist = "Distribuição:",
    mean = "Média:",
    sd = "Desvio Padrão:",
    df = "Graus de Liberdade:",
    prob_type = "Tipo de Probabilidade:",
    x_value = "Valor de x:",
    prob_value = "Probabilidade:",
    calculate = "Calcular",
    visualization = "Visualização",
    result_x = "X para P = %.4f: %.4f",
      result_p = "P(X = %.4f) = %.4f",
      input_type = "Entrada por probabilidade:",
      settings = "Configurações",
      language = "Idioma",
      about = "Sobre",
      about_text = "Desenvolvido por João Bonifácio <br>Versão 1.0<br>2024",
      close = "Fechar",
      alpha = "α (alfa):",
      beta = "β (beta):",
      lambda = "λ (taxa):",
      df1 = "Graus de Liberdade 1:",
      df2 = "Graus de Liberdade 2:",
      gamma_shape = "α (shape):",
      gamma_rate = "β (rate):",
      meanlog = "μ (média log):",
      sdlog = "σ (desvio padrão log):",
      pareto_m = "m (mínimo):",
      pareto_alpha = "α (shape):",
      pareto_error = "Erro: m deve ser > 0, α deve ser > 0, e x deve ser ≥ m",
      weibull_alpha = "α (shape):",
      weibull_beta = "β (scale):",
      weibull_error = "Erro: α deve ser > 0, β deve ser > 0, e x deve ser > 0",
      binom_n = "n (tentativas):",
      binom_p = "p (probabilidade):",
      binom_error = "Erro: n deve ser inteiro ≥ 1, 0 ≤ p ≤ 1, e x deve ser inteiro entre 0 e n",
      geom1_p = "p (probabilidade de sucesso):",
      geom2_p = "p (probabilidade de sucesso):",
      geom1_error = "Erro: 0 < p ≤ 1, e x deve ser inteiro ≥ 1",
      geom2_error = "Erro: 0 < p ≤ 1, e x deve ser inteiro ≥ 0",
      geom1_name = "Geométrica (tentativas)",
      geom2_name = "Geométrica (fracassos)",
      hyper_N = "N (tamanho da população):",
      hyper_K = "K (número de sucessos na população):",
      hyper_n = "n (tamanho da amostra):",
      hyper_error = "Erro: N, K, n devem ser inteiros positivos, K ≤ N, n ≤ N, e x deve ser inteiro entre max(0, n-(N-K)) e min(n,K)",
      nbin1_r = "r (sucessos desejados):",
      nbin1_p = "p (probabilidade de sucesso):",
      nbin2_r = "r (sucessos desejados):",
      nbin2_p = "p (probabilidade de sucesso):",
      nbin1_error = "Erro: r deve ser inteiro > 0, 0 < p ≤ 1, e x deve ser inteiro ≥ r",
      nbin2_error = "Erro: r deve ser inteiro > 0, 0 < p ≤ 1, e x deve ser inteiro ≥ 0",
      nbin1_name = "Binomial Negativa (tentativas)",
      nbin2_name = "Binomial Negativa (fracassos)",
      norm_error = "Erro: desvio padrão deve ser > 0",
      t_error = "Erro: graus de liberdade deve ser > 0",
      chisq_error = "Erro: graus de liberdade deve ser > 0",
      f_error = "Erro: graus de liberdade devem ser > 0",
      gamma_error = "Erro: α e β devem ser > 0",
      lnorm_error = "Erro: σ deve ser > 0",
      beta_error = "Erro: α e β devem ser > 0, e 0 ≤ x ≤ 1",
      exp_error = "Erro: λ deve ser > 0 e x ≥ 0",
      pois_error = "Erro: λ deve ser > 0 e x deve ser inteiro ≥ 0",
      help = "Fórmulas",
      help_text = '
        <h1>Resumo das Distribuições de Probabilidade</h1>

        <h2>1. Distribuição Normal</h2>
        <p><strong>Notação:</strong> \\(X \\sim N(\\mu, \\sigma^2)\\)</p>
        
        <p><strong>Parâmetros:</strong></p>
        <ul>
          <li>\\(\\mu \\in \\mathbb{R}\\) (média)</li>
          <li>\\(\\sigma > 0\\) (desvio padrão)</li>
        </ul>
        
        <p><strong>Suporte:</strong> \\(x \\in \\mathbb{R}\\)</p>
        
        <p><strong>Função Densidade:</strong></p>
        \\[f(x) = \\frac{1}{\\sigma \\sqrt{2\\pi}} e^{-\\frac{(x-\\mu)^2}{2\\sigma^2}}\\]
        
        <p><strong>Esperança:</strong></p>
        \\[E(X) = \\mu\\]
        
        <p><strong>Variância:</strong></p>
        \\[Var(X) = \\sigma^2\\]

        <h2>2. Distribuição t-Student</h2>
        <p><strong>Notação:</strong> \\(X \\sim t_{\\nu}\\)</p>
        
        <p><strong>Parâmetros:</strong></p>
        <ul>
          <li>\\(\\nu > 0\\) (graus de liberdade)</li>
        </ul>
        
        <p><strong>Suporte:</strong> \\(x \\in \\mathbb{R}\\)</p>
        
        <p><strong>Função Densidade:</strong></p>
        \\[f(x) = \\frac{\\Gamma(\\frac{\\nu+1}{2})}{\\sqrt{\\nu\\pi}\\,\\Gamma(\\frac{\\nu}{2})} \\left(1+\\frac{x^2}{\\nu}\\right)^{-\\frac{\\nu+1}{2}}\\]
        
        <p><strong>Esperança:</strong></p>
        \\[E(X) = 0 \\quad \\text{(se } \\nu > 1\\text{)}\\]
        
        <p><strong>Variância:</strong></p>
        \\[Var(X) = \\frac{\\nu}{\\nu-2} \\quad \\text{(se } \\nu > 2\\text{)}\\]

        <h2>3. Distribuição Qui-Quadrado</h2>
        <p><strong>Notação:</strong> \\(X \\sim \\chi^2_k\\)</p>

        <p><strong>Parâmetros:</strong></p>
        <ul>
          <li>\\(k > 0\\) (graus de liberdade)</li>
        </ul>

        <p><strong>Suporte:</strong> \\(x > 0\\)</p>

        <p><strong>Função Densidade:</strong></p>
        \\[f(x) = \\frac{1}{2^{k/2} \\Gamma(k/2)} x^{(k/2)-1} e^{-x/2}\\]

        <p><strong>Esperança:</strong></p>
        \\[E(X) = k\\]

        <p><strong>Variância:</strong></p>
        \\[Var(X) = 2k\\]

        <h2>4. Distribuição F</h2>
        <p><strong>Notação:</strong> \\(X \\sim F_{d_1,d_2}\\)</p>

        <p><strong>Parâmetros:</strong></p>
        <ul>
          <li>\\(d_1 > 0\\) (graus de liberdade do numerador)</li>
          <li>\\(d_2 > 0\\) (graus de liberdade do denominador)</li>
        </ul>

        <p><strong>Suporte:</strong> \\(x > 0\\)</p>

        <p><strong>Função Densidade:</strong></p>
        \\[f(x) = \\frac{\\left(\\frac{d_1}{d_2}\\right)^{d_1/2} x^{(d_1/2)-1}}{B(d_1/2, d_2/2) \\left( 1 + \\frac{d_1}{d_2} x \\right)^{(d_1+d_2)/2}}\\]

        <p><strong>Esperança:</strong></p>
        \\[E(X) = \\frac{d_2}{d_2-2} \\quad \\text{(se } d_2 > 2\\text{)}\\]

        <p><strong>Variância:</strong></p>
        \\[Var(X) = \\frac{2d_2^2(d_1+d_2-2)}{d_1(d_2-2)^2(d_2-4)} \\quad \\text{(se } d_2 > 4\\text{)}\\]

        <h2>5. Distribuição Gamma</h2>
        <p><strong>Notação:</strong> \\(X \\sim Gamma(\\alpha, \\beta)\\)</p>

        <p><strong>Parâmetros:</strong></p>
        <ul>
          <li>\\(\\alpha > 0\\) (parâmetro de forma)</li>
          <li>\\(\\beta > 0\\) (parâmetro de taxa)</li>
        </ul>

        <p><strong>Suporte:</strong> \\(x > 0\\)</p>

        <p><strong>Função Densidade:</strong></p>
        \\[f(x) = \\frac{\\beta^\\alpha}{\\Gamma(\\alpha)} x^{\\alpha-1} e^{-\\beta x}\\]

        <p><strong>Esperança:</strong></p>
        \\[E(X) = \\frac{\\alpha}{\\beta}\\]

        <p><strong>Variância:</strong></p>
        \\[Var(X) = \\frac{\\alpha}{\\beta^2}\\]

        <h2>6. Distribuição Log-normal</h2>
        <p><strong>Notação:</strong> \\(X \\sim LogN(\\mu, \\sigma^2)\\)</p>

        <p><strong>Parâmetros:</strong></p>
        <ul>
          <li>\\(\\mu \\in \\mathbb{R}\\) (média do logaritmo)</li>
          <li>\\(\\sigma > 0\\) (desvio padrão do logaritmo)</li>
        </ul>

        <p><strong>Suporte:</strong> \\(x > 0\\)</p>

        <p><strong>Função Densidade:</strong></p>
        \\[f(x) = \\frac{1}{x\\sigma\\sqrt{2\\pi}} e^{-\\frac{(\\ln x - \\mu)^2}{2\\sigma^2}}\\]

        <p><strong>Esperança:</strong></p>
        \\[E(X) = e^{\\mu + \\sigma^2/2}\\]

        <p><strong>Variância:</strong></p>
        \\[Var(X) = (e^{\\sigma^2}-1)e^{2\\mu + \\sigma^2}\\]

        <h2>7. Distribuição Pareto</h2>
        <p><strong>Notação:</strong> \\(X \\sim Pareto(m, \\alpha)\\)</p>

        <p><strong>Parâmetros:</strong></p>
        <ul>
          <li>\\(m > 0\\) (parâmetro de escala/mínimo)</li>
          <li>\\(\\alpha > 0\\) (parâmetro de forma)</li>
        </ul>

        <p><strong>Suporte:</strong> \\(x \\geq m\\)</p>

        <p><strong>Função Densidade:</strong></p>
        \\[f(x) = \\frac{\\alpha m^\\alpha}{x^{\\alpha+1}}\\]

        <p><strong>Esperança:</strong></p>
        \\[E(X) = \\frac{\\alpha m}{\\alpha-1} \\quad \\text{(se } \\alpha > 1\\text{)}\\]

        <p><strong>Variância:</strong></p>
        \\[Var(X) = \\frac{\\alpha m^2}{(\\alpha-1)^2(\\alpha-2)} \\quad \\text{(se } \\alpha > 2\\text{)}\\]

        <h2>8. Distribuição Weibull</h2>
        <p><strong>Notação:</strong> \\(X \\sim Weibull(\\alpha, \\beta)\\)</p>

        <p><strong>Parâmetros:</strong></p>
        <ul>
          <li>\\(\\alpha > 0\\) (parâmetro de forma)</li>
          <li>\\(\\beta > 0\\) (parâmetro de escala)</li>
        </ul>

        <p><strong>Suporte:</strong> \\(x > 0\\)</p>

        <p><strong>Função Densidade:</strong></p>
        \\[f(x) = \\frac{\\alpha}{\\beta}\\left(\\frac{x}{\\beta}\\right)^{\\alpha-1}e^{-(x/\\beta)^\\alpha}\\]

        <p><strong>Esperança:</strong></p>
        \\[E(X) = \\beta\\Gamma(1+\\frac{1}{\\alpha})\\]

        <p><strong>Variância:</strong></p>
        \\[Var(X) = \\beta^2\\left[\\Gamma(1+\\frac{2}{\\alpha})-\\Gamma^2(1+\\frac{1}{\\alpha})\\right]\\]

        <h2>9. Distribuição Binomial</h2>
        <p><strong>Notação:</strong> \\(X \\sim B(n,p)\\)</p>

        <p><strong>Parâmetros:</strong></p>
        <ul>
          <li>\\(n \\in \\mathbb{N}^*\\) (número de tentativas)</li>
          <li>\\(p \\in [0,1]\\) (probabilidade de sucesso)</li>
        </ul>

        <p><strong>Suporte:</strong> \\(x \\in \\{0,1,\\ldots,n\\}\\)</p>

        <p><strong>Função de Massa:</strong></p>
        \\[P(X = k) = \\binom{n}{k} p^k (1-p)^{n-k}\\]

        <p><strong>Esperança:</strong></p>
        \\[E(X) = np\\]

        <p><strong>Variância:</strong></p>
        \\[Var(X) = np(1-p)\\]

        <h2>10. Distribuição Geométrica (tentativas)</h2>
        <p><strong>Notação:</strong> \\(X \\sim Geom_1(p)\\)</p>

        <p><strong>Parâmetros:</strong></p>
        <ul>
          <li>\\(p \\in (0,1]\\) (probabilidade de sucesso)</li>
        </ul>

        <p><strong>Suporte:</strong> \\(x \\in \\mathbb{N}^*\\)</p>

        <p><strong>Função de Massa:</strong></p>
        \\[P(X = k) = p(1-p)^{k-1}\\]

        <p><strong>Esperança:</strong></p>
        \\[E(X) = \\frac{1}{p}\\]

        <p><strong>Variância:</strong></p>
        \\[Var(X) = \\frac{1-p}{p^2}\\]

        <h2>11. Distribuição Geométrica (fracassos)</h2>
        <p><strong>Notação:</strong> \\(X \\sim Geom_0(p)\\)</p>

        <p><strong>Parâmetros:</strong></p>
        <ul>
          <li>\\(p \\in (0,1]\\) (probabilidade de sucesso)</li>
        </ul>

        <p><strong>Suporte:</strong> \\(x \\in \\mathbb{N}_0\\)</p>

        <p><strong>Função de Massa:</strong></p>
        \\[P(X = k) = p(1-p)^k\\]

        <p><strong>Esperança:</strong></p>
        \\[E(X) = \\frac{1-p}{p}\\]

        <p><strong>Variância:</strong></p>
        \\[Var(X) = \\frac{1-p}{p^2}\\]

        <h2>12. Distribuição Hipergeométrica</h2>
        <p><strong>Notação:</strong> \\(X \\sim H(N,K,n)\\)</p>

        <p><strong>Parâmetros:</strong></p>
        <ul>
          <li>\\(N \\in \\mathbb{N}^*\\) (tamanho da população)</li>
          <li>\\(K \\in \\{0,\\ldots,N\\}\\) (número de sucessos na população)</li>
          <li>\\(n \\in \\{0,\\ldots,N\\}\\) (tamanho da amostra)</li>
        </ul>

        <p><strong>Suporte:</strong> \\(x \\in \\{\\max(0,n-N+K),\\ldots,\\min(n,K)\\}\\)</p>

        <p><strong>Função de Massa:</strong></p>
        \\[P(X = k) = \\frac{\\binom{K}{k}\\binom{N-K}{n-k}}{\\binom{N}{n}}\\]

        <p><strong>Esperança:</strong></p>
        \\[E(X) = n\\frac{K}{N}\\]

        <p><strong>Variância:</strong></p>
        \\[Var(X) = n\\frac{K}{N}\\left(1-\\frac{K}{N}\\right)\\frac{N-n}{N-1}\\]

        <h2>13. Distribuição Binomial Negativa (tentativas)</h2>
        <p><strong>Notação:</strong> \\(X \\sim NB_1(r,p)\\)</p>

        <p><strong>Parâmetros:</strong></p>
        <ul>
          <li>\\(r \\in \\mathbb{N}^*\\) (número de sucessos desejados)</li>
          <li>\\(p \\in (0,1]\\) (probabilidade de sucesso)</li>
        </ul>

        <p><strong>Suporte:</strong> \\(x \\in \\{r,r+1,\\ldots\\}\\)</p>

        <p><strong>Função de Massa:</strong></p>
        \\[P(X = k) = \\binom{k-1}{r-1}p^r(1-p)^{k-r}\\]

        <p><strong>Esperança:</strong></p>
        \\[E(X) = \\frac{r}{p}\\]

        <p><strong>Variância:</strong></p>
        \\[Var(X) = \\frac{r(1-p)}{p^2}\\]

        <h2>14. Distribuição Binomial Negativa (fracassos)</h2>
        <p><strong>Notação:</strong> \\(X \\sim NB_0(r,p)\\)</p>

        <p><strong>Parâmetros:</strong></p>
        <ul>
          <li>\\(r \\in \\mathbb{N}^*\\) (número de sucessos desejados)</li>
          <li>\\(p \\in (0,1]\\) (probabilidade de sucesso)</li>
        </ul>

        <p><strong>Suporte:</strong> \\(x \\in \\mathbb{N}_0\\)</p>

        <p><strong>Função de Massa:</strong></p>
        \\[P(X = k) = \\binom{k+r-1}{r-1}p^r(1-p)^k\\]

        <p><strong>Esperança:</strong></p>
        \\[E(X) = \\frac{r(1-p)}{p}\\]

        <p><strong>Variância:</strong></p>
        \\[Var(X) = \\frac{r(1-p)}{p^2}\\]

        <h2>15. Distribuição Exponencial</h2>
        <p><strong>Notação:</strong> \\(X \\sim Exp(\\lambda)\\)</p>

        <p><strong>Parâmetros:</strong></p>
        <ul>
          <li>\\(\\lambda > 0\\) (taxa)</li>
        </ul>

        <p><strong>Suporte:</strong> \\(x \\geq 0\\)</p>

        <p><strong>Função Densidade:</strong></p>
        \\[f(x) = \\lambda e^{-\\lambda x}\\]

        <p><strong>Esperança:</strong></p>
        \\[E(X) = \\frac{1}{\\lambda}\\]

        <p><strong>Variância:</strong></p>
        \\[Var(X) = \\frac{1}{\\lambda^2}\\]

        <h2>16. Distribuição Poisson</h2>
        <p><strong>Notação:</strong> \\(X \\sim Poisson(\\lambda)\\)</p>

        <p><strong>Parâmetros:</strong></p>
        <ul>
          <li>\\(\\lambda > 0\\) (taxa média)</li>
        </ul>

        <p><strong>Suporte:</strong> \\(x \\in \\mathbb{N}_0\\)</p>

        <p><strong>Função de Massa:</strong></p>
        \\[P(X = k) = \\frac{\\lambda^k e^{-\\lambda}}{k!}\\]

        <p><strong>Esperança:</strong></p>
        \\[E(X) = \\lambda\\]

        <p><strong>Variância:</strong></p>
        \\[Var(X) = \\lambda\\]
      '
  ),
  en = list(
      title = "ProbCalc",
    params = "Parameters",
    dist = "Distribution:",
    mean = "Mean:",
    sd = "Standard Deviation:",
    df = "Degrees of Freedom:",
    prob_type = "Probability Type:",
    x_value = "x value:",
    prob_value = "Probability:",
    calculate = "Calculate",
    visualization = "Visualization",
    result_x = "X for P = %.4f: %.4f",
      result_p = "P(X = %.4f) = %.4f",
      input_type = "Input by probability:",
      settings = "Settings",
      language = "Language",
      about = "About",
      about_text = "Developed by João Bonifácio<br>Version 1.0<br>2024",
      close = "Close",
      alpha = "α (alpha):",
      beta = "β (beta):",
      lambda = "λ (rate):",
      df1 = "Degrees of Freedom 1:",
      df2 = "Degrees of Freedom 2:",
      gamma_shape = "α (shape):",
      gamma_rate = "β (rate):",
      meanlog = "μ (meanlog):",
      sdlog = "σ (sdlog):",
      pareto_m = "m (minimum):",
      pareto_alpha = "α (shape):",
      pareto_error = "Error: m must be > 0, α must be > 0, and x must be ≥ m",
      weibull_alpha = "α (shape):",
      weibull_beta = "β (scale):",
      weibull_error = "Error: α must be > 0, β must be > 0, and x must be > 0",
      binom_n = "n (trials):",
      binom_p = "p (probability):",
      binom_error = "Error: n must be integer ≥ 1, 0 ≤ p ≤ 1, and x must be integer between 0 and n",
      geom1_p = "p (success probability):",
      geom2_p = "p (success probability):",
      geom1_error = "Error: 0 < p ≤ 1, and x must be integer ≥ 1",
      geom2_error = "Error: 0 < p ≤ 1, and x must be integer ≥ 0",
      geom1_name = "Geometric (trials)",
      geom2_name = "Geometric (failures)",
      hyper_N = "N (population size):",
      hyper_K = "K (number of successes in population):",
      hyper_n = "n (sample size):",
      hyper_error = "Error: N, K, n must be positive integers, K ≤ N, n ≤ N, and x must be integer between max(0, n-(N-K)) and min(n,K)",
      nbin1_r = "r (desired successes):",
      nbin1_p = "p (success probability):",
      nbin2_r = "r (desired successes):",
      nbin2_p = "p (success probability):",
      nbin1_error = "Error: r must be integer > 0, 0 < p ≤ 1, and x must be integer ≥ r",
      nbin2_error = "Error: r must be integer > 0, 0 < p ≤ 1, and x must be integer ≥ 0",
      nbin1_name = "Negative Binomial (trials)",
      nbin2_name = "Negative Binomial (failures)",
      norm_error = "Error: standard deviation must be > 0",
      t_error = "Error: degrees of freedom must be > 0",
      chisq_error = "Error: degrees of freedom must be > 0",
      f_error = "Error: degrees of freedom must be > 0",
      gamma_error = "Error: α and β must be > 0",
      lnorm_error = "Error: σ must be > 0",
      beta_error = "Error: α and β must be > 0, and 0 ≤ x ≤ 1",
      exp_error = "Error: λ must be > 0 and x ≥ 0",
      pois_error = "Error: λ must be > 0 and x must be integer ≥ 0",
      help = "Formulas",
      help_text = '
        <h1>Probability Distributions Summary</h1>

        <h2>1. Normal Distribution</h2>
        <p><strong>Density Function:</strong></p>
        <p class="formula">
            f(x) = \\frac{1}{\\sigma \\sqrt{2\\pi}} e^{-\\frac{(x - \\mu)^2}{2\\sigma^2}}
        </p>
        <p><strong>Mean:</strong> \\( \\mu \\) &nbsp;&nbsp; <strong>Variance:</strong> \\( \\sigma^2 \\)</p>

        <h2>2. t-Student Distribution</h2>
        <p><strong>Density Function:</strong></p>
        <p class="formula">f(x) = \\frac{\\Gamma(\\frac{\\nu+1}{2})}{\\sqrt{\\nu\\pi} \\Gamma(\\frac{\\nu}{2})} \\left(1 + \\frac{x^2}{\\nu} \\right)^{-\\frac{\\nu+1}{2}}</p>
        <p><strong>Mean:</strong> 0 (if \\( \\nu > 1 \\)) &nbsp;&nbsp; <strong>Variance:</strong> \\( \\frac{\\nu}{\\nu-2} \\) (if \\( \\nu > 2 \\))</p>

        <h2>3. Chi-Square Distribution</h2>
        <p><strong>Density Function:</strong></p>
        <p class="formula">f(x) = \\frac{1}{2^{k/2} \\Gamma(k/2)} x^{(k/2)-1} e^{-x/2}, \\quad x > 0</p>
        <p><strong>Mean:</strong> \\( k \\) &nbsp;&nbsp; <strong>Variance:</strong> \\( 2k \\)</p>

        <h2>4. F Distribution</h2>
        <p><strong>Density Function:</strong></p>
        <p class="formula">f(x) = \\frac{\\left(\\frac{d_1}{d_2}\\right)^{d_1/2} x^{(d_1/2)-1}}{B(d_1/2, d_2/2) \\left( 1 + \\frac{d_1}{d_2} x \\right)^{(d_1+d_2)/2}}</p>
        <p><strong>Mean:</strong> \\( \\frac{d_2}{d_2-2} \\) (if \\( d_2 > 2 \\))</p>

        <h2>5. Gamma Distribution</h2>
        <p><strong>Density Function:</strong></p>
        <p class="formula">f(x) = \\frac{\\beta^\\alpha}{\\Gamma(\\alpha)} x^{\\alpha-1} e^{-\\beta x}, \\quad x > 0</p>
        <p><strong>Mean:</strong> \\( \\frac{\\alpha}{\\beta} \\) &nbsp;&nbsp; <strong>Variance:</strong> \\( \\frac{\\alpha}{\\beta^2} \\)</p>

        <h2>6. Log-Normal Distribution</h2>
        <p><strong>Density Function:</strong></p>
        <p class="formula">f(x) = \\frac{1}{x\\sigma\\sqrt{2\\pi}} e^{-\\frac{(\\ln x - \\mu)^2}{2\\sigma^2}}, \\quad x > 0</p>
        <p><strong>Mean:</strong> \\( e^{\\mu + \\sigma^2/2} \\) &nbsp;&nbsp; <strong>Variance:</strong> \\( (e^{\\sigma^2}-1)e^{2\\mu + \\sigma^2} \\)</p>

        <h2>7. Pareto Distribution</h2>
        <p><strong>Density Function:</strong></p>
        <p class="formula">f(x) = \\frac{\\alpha m^\\alpha}{x^{\\alpha+1}}, \\quad x \\geq m</p>
        <p><strong>Mean:</strong> \\( \\frac{\\alpha m}{\\alpha-1} \\) (if \\( \\alpha > 1 \\)) &nbsp;&nbsp; <strong>Variance:</strong> \\( \\frac{\\alpha m^2}{(\\alpha-1)^2(\\alpha-2)} \\) (if \\( \\alpha > 2 \\))</p>

        <h2>8. Weibull Distribution</h2>
        <p><strong>Density Function:</strong></p>
        <p class="formula">f(x) = \\frac{\\alpha}{\\beta}\\left(\\frac{x}{\\beta}\\right)^{\\alpha-1}e^{-(x/\\beta)^\\alpha}, \\quad x > 0</p>
        <p><strong>Mean:</strong> \\( \\beta\\Gamma(1+\\frac{1}{\\alpha})\\) &nbsp;&nbsp; <strong>Variance:</strong> \\( \\beta^2[\\Gamma(1+\\frac{2}{\\alpha})-\\Gamma^2(1+\\frac{1}{\\alpha})] \\)</p>

        <h2>9. Binomial Distribution</h2>
        <p><strong>Probability Mass:</strong></p>
        <p class="formula">P(X = k) = \\binom{n}{k} p^k (1-p)^{n-k}, \\quad k = 0,1,\\ldots,n</p>
        <p><strong>Mean:</strong> \\( np \\) &nbsp;&nbsp; <strong>Variance:</strong> \\( np(1-p) \\)</p>

        <h2>10. Geometric (trials) Distribution</h2>
        <p><strong>Probability Mass:</strong></p>
        <p class="formula">P(X = k) = p(1-p)^{k-1}, \\quad k = 1,2,\\ldots</p>
        <p><strong>Mean:</strong> \\( \\frac{1}{p} \\) &nbsp;&nbsp; <strong>Variance:</strong> \\( \\frac{1-p}{p^2} \\)</p>

        <h2>11. Geometric (failures) Distribution</h2>
        <p><strong>Probability Mass:</strong></p>
        <p class="formula">P(X = k) = p(1-p)^k, \\quad k = 0,1,2,\\ldots</p>
        <p><strong>Mean:</strong> \\( \\frac{1-p}{p} \\) &nbsp;&nbsp; <strong>Variance:</strong> \\( \\frac{1-p}{p^2} \\)</p>

        <h2>12. Hypergeometric Distribution</h2>
        <p><strong>Probability Mass:</strong></p>
        <p class="formula">P(X = k) = \\frac{\\binom{K}{k}\\binom{N-K}{n-k}}{\\binom{N}{n}}, \\quad \\max(0,n-N+K) \\leq k \\leq \\min(n,K)</p>
        <p><strong>Mean:</strong> \\( n\\frac{K}{N} \\) &nbsp;&nbsp; <strong>Variance:</strong> \\( n\\frac{K}{N}(1-\\frac{K}{N})\\frac{N-n}{N-1} \\)</p>

        <h2>13. Negative Binomial (trials) Distribution</h2>
        <p><strong>Probability Mass:</strong></p>
        <p class="formula">P(X = k) = \\binom{k-1}{r-1}p^r(1-p)^{k-r}, \\quad k = r,r+1,\\ldots</p>
        <p><strong>Mean:</strong> \\( \\frac{r}{p} \\) &nbsp;&nbsp; <strong>Variance:</strong> \\( \\frac{r(1-p)}{p^2} \\)</p>

        <h2>14. Negative Binomial (failures) Distribution</h2>
        <p><strong>Probability Mass:</strong></p>
        <p class="formula">P(X = k) = \\binom{k+r-1}{r-1}p^r(1-p)^k, \\quad k = 0,1,2,\\ldots</p>
        <p><strong>Mean:</strong> \\( \\frac{r(1-p)}{p} \\) &nbsp;&nbsp; <strong>Variance:</strong> \\( \\frac{r(1-p)}{p^2} \\)</p>

        <h2>15. Exponential Distribution</h2>
        <p><strong>Density Function:</strong></p>
        <p class="formula">f(x) = \\lambda e^{-\\lambda x}, \\quad x \\geq 0</p>
        <p><strong>Mean:</strong> \\( \\frac{1}{\\lambda} \\) &nbsp;&nbsp; <strong>Variance:</strong> \\( \\frac{1}{\\lambda^2} \\)</p>

        <h2>16. Poisson Distribution</h2>
        <p><strong>Probability Mass:</strong></p>
        <p class="formula">P(X = k) = \\frac{\\lambda^k e^{-\\lambda}}{k!}, \\quad k = 0,1,2,\\ldots</p>
        <p><strong>Mean:</strong> \\( \\lambda \\) &nbsp;&nbsp; <strong>Variance:</strong> \\( \\lambda \\)</p>
      '
    ),
    es = list(
      title = "ProbCalc",
      params = "Parámetros",
      dist = "Distribución:",
      mean = "Media:",
      sd = "Desviación Estándar:",
      df = "Grados de Libertad:",
      prob_type = "Tipo de Probabilidad:",
      x_value = "Valor de x:",
      prob_value = "Probabilidad:",
      calculate = "Calcular",
      visualization = "Visualización",
      result_x = "X para P = %.4f: %.4f",
      result_p = "P(X = %.4f) = %.4f",
      input_type = "Entrada por probabilidad:",
      settings = "Configuración",
      language = "Idioma",
      about = "Acerca de",
      about_text = "Desarrollado por João Bonifácio <br>Versión 1.0<br>2024",
      close = "Cerrar",
      alpha = "α (alfa):",
      beta = "β (beta):",
      lambda = "λ (tasa):",
      df1 = "Grados de Libertad 1:",
      df2 = "Grados de Libertad 2:",
      gamma_shape = "α (shape):",
      gamma_rate = "β (rate):",
      meanlog = "μ (media log):",
      sdlog = "σ (desviación estándar log):",
      pareto_m = "m (mínimo):",
      pareto_alpha = "α (shape):",
      pareto_error = "Error: m debe ser > 0, α debe ser > 0, y x debe ser ≥ m",
      weibull_alpha = "α (shape):",
      weibull_beta = "β (scale):",
      weibull_error = "Error: α debe ser > 0, β debe ser > 0, y x debe ser > 0",
      binom_n = "n (intentos):",
      binom_p = "p (probabilidad):",
      binom_error = "Error: n debe ser entero ≥ 1, 0 ≤ p ≤ 1, y x debe ser entero entre 0 y n",
      geom1_p = "p (probabilidad de éxito):",
      geom2_p = "p (probabilidad de éxito):",
      geom1_error = "Error: 0 < p ≤ 1, y x debe ser entero ≥ 1",
      geom2_error = "Error: 0 < p ≤ 1, y x debe ser entero ≥ 0",
      geom1_name = "Geométrica (intentos)",
      geom2_name = "Geométrica (fracasos)",
      hyper_N = "N (tamaño de la población):",
      hyper_K = "K (número de éxitos en la población):",
      hyper_n = "n (tamaño de la muestra):",
      hyper_error = "Error: N, K, n deben ser enteros positivos, K ≤ N, n ≤ N, y x debe ser entero entre max(0, n-(N-K)) y min(n,K)",
      nbin1_r = "r (éxitos deseados):",
      nbin1_p = "p (probabilidad de éxito):",
      nbin2_r = "r (éxitos deseados):",
      nbin2_p = "p (probabilidad de éxito):",
      nbin1_error = "Error: r debe ser entero > 0, 0 < p ≤ 1, y x debe ser entero ≥ r",
      nbin2_error = "Error: r debe ser entero > 0, 0 < p ≤ 1, y x debe ser entero ≥ 0",
      nbin1_name = "Binomial Negativa (intentos)",
      nbin2_name = "Binomial Negativa (fracasos)",
      norm_error = "Error: desviación estándar debe ser > 0",
      t_error = "Error: grados de libertad debe ser > 0",
      chisq_error = "Error: grados de libertad debe ser > 0",
      f_error = "Error: grados de libertad deben ser > 0",
      gamma_error = "Error: α y β deben ser > 0",
      lnorm_error = "Error: σ debe ser > 0",
      beta_error = "Error: α y β deben ser > 0, y 0 ≤ x ≤ 1",
      exp_error = "Error: λ debe ser > 0 y x ≥ 0",
      pois_error = "Error: λ debe ser > 0 y x debe ser entero ≥ 0",
      help = "Fórmulas",
      help_text = '
        <h1>Resumen de Distribuciones de Probabilidad</h1>

        <h2>1. Distribución Normal</h2>
        <p><strong>Función de Densidad:</strong></p>
        <p class="formula">
            f(x) = \\frac{1}{\\sigma \\sqrt{2\\pi}} e^{-\\frac{(x - \\mu)^2}{2\\sigma^2}}
        </p>
        <p><strong>Media:</strong> \\( \\mu \\) &nbsp;&nbsp; <strong>Varianza:</strong> \\( \\sigma^2 \\)</p>

        <h2>2. Distribución t-Student</h2>
        <p><strong>Función de Densidad:</strong></p>
        <p class="formula">f(x) = \\frac{\\Gamma(\\frac{\\nu+1}{2})}{\\sqrt{\\nu\\pi} \\Gamma(\\frac{\\nu}{2})} \\left(1 + \\frac{x^2}{\\nu} \\right)^{-\\frac{\\nu+1}{2}}</p>
        <p><strong>Media:</strong> 0 (si \\( \\nu > 1 \\)) &nbsp;&nbsp; <strong>Varianza:</strong> \\( \\frac{\\nu}{\\nu-2} \\) (si \\( \\nu > 2 \\))</p>

        <h2>3. Distribución Chi-Cuadrado</h2>
        <p><strong>Función de Densidad:</strong></p>
        <p class="formula">f(x) = \\frac{1}{2^{k/2} \\Gamma(k/2)} x^{(k/2)-1} e^{-x/2}, \\quad x > 0</p>
        <p><strong>Media:</strong> \\( k \\) &nbsp;&nbsp; <strong>Varianza:</strong> \\( 2k \\)</p>

        <h2>4. Distribución F</h2>
        <p><strong>Función de Densidad:</strong></p>
        <p class="formula">f(x) = \\frac{\\left(\\frac{d_1}{d_2}\\right)^{d_1/2} x^{(d_1/2)-1}}{B(d_1/2, d_2/2) \\left( 1 + \\frac{d_1}{d_2} x \\right)^{(d_1+d_2)/2}}</p>
        <p><strong>Media:</strong> \\( \\frac{d_2}{d_2-2} \\) (si \\( d_2 > 2 \\))</p>

        <h2>5. Distribución Gamma</h2>
        <p><strong>Función de Densidad:</strong></p>
        <p class="formula">f(x) = \\frac{\\beta^\\alpha}{\\Gamma(\\alpha)} x^{\\alpha-1} e^{-\\beta x}, \\quad x > 0</p>
        <p><strong>Media:</strong> \\( \\frac{\\alpha}{\\beta} \\) &nbsp;&nbsp; <strong>Varianza:</strong> \\( \\frac{\\alpha}{\\beta^2} \\)</p>

        <h2>6. Distribución Log-Normal</h2>
        <p><strong>Función de Densidad:</strong></p>
        <p class="formula">f(x) = \\frac{1}{x\\sigma\\sqrt{2\\pi}} e^{-\\frac{(\\ln x - \\mu)^2}{2\\sigma^2}}, \\quad x > 0</p>
        <p><strong>Media:</strong> \\( e^{\\mu + \\sigma^2/2} \\) &nbsp;&nbsp; <strong>Varianza:</strong> \\( (e^{\\sigma^2}-1)e^{2\\mu + \\sigma^2} \\)</p>

        <h2>7. Distribución Pareto</h2>
        <p><strong>Función de Densidad:</strong></p>
        <p class="formula">f(x) = \\frac{\\alpha m^\\alpha}{x^{\\alpha+1}}, \\quad x \\geq m</p>
        <p><strong>Media:</strong> \\( \\frac{\\alpha m}{\\alpha-1} \\) (si \\( \\alpha > 1 \\)) &nbsp;&nbsp; <strong>Varianza:</strong> \\( \\frac{\\alpha m^2}{(\\alpha-1)^2(\\alpha-2)} \\) (si \\( \\alpha > 2 \\))</p>

        <h2>8. Distribución Weibull</h2>
        <p><strong>Función de Densidad:</strong></p>
        <p class="formula">f(x) = \\frac{\\alpha}{\\beta}\\left(\\frac{x}{\\beta}\\right)^{\\alpha-1}e^{-(x/\\beta)^\\alpha}, \\quad x > 0</p>
        <p><strong>Media:</strong> \\( \\beta\\Gamma(1+\\frac{1}{\\alpha})\\) &nbsp;&nbsp; <strong>Varianza:</strong> \\( \\beta^2[\\Gamma(1+\\frac{2}{\\alpha})-\\Gamma^2(1+\\frac{1}{\\alpha})] \\)</p>

        <h2>9. Distribución Binomial</h2>
        <p><strong>Función de Masa:</strong></p>
        <p class="formula">P(X = k) = \\binom{n}{k} p^k (1-p)^{n-k}, \\quad k = 0,1,\\ldots,n</p>
        <p><strong>Media:</strong> \\( np \\) &nbsp;&nbsp; <strong>Varianza:</strong> \\( np(1-p) \\)</p>

        <h2>10. Distribución Geométrica (intentos)</h2>
        <p><strong>Función de Masa:</strong></p>
        <p class="formula">P(X = k) = p(1-p)^{k-1}, \\quad k = 1,2,\\ldots</p>
        <p><strong>Media:</strong> \\( \\frac{1}{p} \\) &nbsp;&nbsp; <strong>Varianza:</strong> \\( \\frac{1-p}{p^2} \\)</p>

        <h2>11. Distribución Geométrica (fracasos)</h2>
        <p><strong>Función de Masa:</strong></p>
        <p class="formula">P(X = k) = p(1-p)^k, \\quad k = 0,1,2,\\ldots</p>
        <p><strong>Media:</strong> \\( \\frac{1-p}{p} \\) &nbsp;&nbsp; <strong>Varianza:</strong> \\( \\frac{1-p}{p^2} \\)</p>

        <h2>12. Distribución Hipergeométrica</h2>
        <p><strong>Función de Masa:</strong></p>
        <p class="formula">P(X = k) = \\frac{\\binom{K}{k}\\binom{N-K}{n-k}}{\\binom{N}{n}}, \\quad \\max(0,n-N+K) \\leq k \\leq \\min(n,K)</p>
        <p><strong>Media:</strong> \\( n\\frac{K}{N} \\) &nbsp;&nbsp; <strong>Varianza:</strong> \\( n\\frac{K}{N}(1-\\frac{K}{N})\\frac{N-n}{N-1} \\)</p>

        <h2>13. Distribución Binomial Negativa (intentos)</h2>
        <p><strong>Función de Masa:</strong></p>
        <p class="formula">P(X = k) = \\binom{k-1}{r-1}p^r(1-p)^{k-r}, \\quad k = r,r+1,\\ldots</p>
        <p><strong>Media:</strong> \\( \\frac{r}{p} \\) &nbsp;&nbsp; <strong>Varianza:</strong> \\( \\frac{r(1-p)}{p^2} \\)</p>

        <h2>14. Distribución Binomial Negativa (fracasos)</h2>
        <p><strong>Función de Masa:</strong></p>
        <p class="formula">P(X = k) = \\binom{k+r-1}{r-1}p^r(1-p)^k, \\quad k = 0,1,2,\\ldots</p>
        <p><strong>Media:</strong> \\( \\frac{r(1-p)}{p} \\) &nbsp;&nbsp; <strong>Varianza:</strong> \\( \\frac{r(1-p)}{p^2} \\)</p>

        <h2>15. Distribución Exponencial</h2>
        <p><strong>Función de Densidad:</strong></p>
        <p class="formula">f(x) = \\lambda e^{-\\lambda x}, \\quad x \\geq 0</p>
        <p><strong>Media:</strong> \\( \\frac{1}{\\lambda} \\) &nbsp;&nbsp; <strong>Varianza:</strong> \\( \\frac{1}{\\lambda^2} \\)</p>

        <h2>16. Distribución Poisson</h2>
        <p><strong>Función de Masa:</strong></p>
        <p class="formula">P(X = k) = \\frac{\\lambda^k e^{-\\lambda}}{k!}, \\quad k = 0,1,2,\\ldots</p>
        <p><strong>Media:</strong> \\( \\lambda \\) &nbsp;&nbsp; <strong>Varianza:</strong> \\( \\lambda \\)</p>
      '
    )
  )
  
  # Controle de língua
  current_lang <- reactiveVal("pt")
  
  # Função helper para obter traduções
  t <- reactive({
    translations[[current_lang()]]
  })
  
  observeEvent(input$settings_btn, {
    showModal(modalDialog(
      title = t()$settings,
      
      tabsetPanel(
        tabPanel(
          t()$language,
          div(
            style = "margin: 20px 0;",
            actionButton("modal_lang_pt", "🇧🇷 Português", 
                        class = "btn btn-outline-primary me-2"),
            actionButton("modal_lang_en", "🇺🇸 English", 
                        class = "btn btn-outline-primary me-2"),
            actionButton("modal_lang_es", "🇪🇸 Español", 
                        class = "btn btn-outline-primary")
          )
        ),
        
        tabPanel(
          t()$help,
          div(
            style = "margin: 20px 0;",
            tags$head(
              tags$style("
                .formula {
                  font-family: 'Times New Roman', serif;
                  background-color: #f8f9fa;
                  padding: 15px;
                  margin: 10px 0;
                  display: block;
                  border-radius: 5px;
                  border: 1px solid #dee2e6;
                  overflow-x: auto;
                  text-align: center;
                  font-size: 1.1em;
                }
                .MathJax_Display {
                  overflow-x: auto;
                  overflow-y: hidden;
                }
              ")
            ),
            tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.5/MathJax.js?config=TeX-AMS_HTML"),
            tags$script(HTML("
              MathJax.Hub.Config({
                tex2jax: {
                  inlineMath: [['\\\\(','\\\\)']],
                  displayMath: [['\\\\[','\\\\]']],
                  processEscapes: true
                }
              });
            ")),
            HTML(t()$help_text)
          )
        ),
        
        tabPanel(
          t()$about,
          div(
            style = "margin: 20px 0;",
            HTML(t()$about_text)
          )
        )
      ),
      
      footer = tagList(
        modalButton(t()$close)
      ),
      size = "l",
      easyClose = TRUE
    ))
  })
  
  observeEvent(input$modal_lang_pt, {
    current_lang("pt")
    removeModal()
  })
  
  observeEvent(input$modal_lang_en, {
    current_lang("en")
    removeModal()
  })
  
  observeEvent(input$modal_lang_es, {
    current_lang("es")
    removeModal()
  })
  
  # Título reativo
  output$title <- renderText({
    t()$title
  })
  
  # Card de parâmetros
  output$params_card <- renderUI({
    t <- t()
    
    card(
      card_header(t$params),
      
      selectInput("dist", t$dist,
                  choices = c("Normal" = "norm", 
                              "t-Student" = "t",
                              "Qui-quadrado" = "chisq",
                              "F" = "f",
                              "Gamma" = "gamma",
                              "Log-normal" = "lnorm",
                              "Pareto" = "pareto",
                              "Weibull" = "weibull",
                              "Binomial" = "binom",
                              "Geométrica (tentativas)" = "geom1",
                              "Geométrica (fracassos)" = "geom2",
                              "Hipergeométrica" = "hyper",
                              "Beta" = "beta",
                              "Exponencial" = "exp",
                              "Poisson" = "pois",
                              "Binomial Negativa (tentativas)" = "nbin1",
                              "Binomial Negativa (fracassos)" = "nbin2")),
      
      conditionalPanel(
        condition = "input.dist == 'norm'",
        numericInput("mean", t$mean, value = 0),
        numericInput("sd", t$sd, value = 1, min = 0.1)
      ),
      
      conditionalPanel(
        condition = "input.dist == 't'",
        numericInput("df", t$df, value = 5, min = 1)
      ),
      
      conditionalPanel(
        condition = "input.dist == 'pois'",
        numericInput("lambda", "λ:", value = 5, min = 0.1),
        selectInput("prob_type_disc", t$prob_type,
                    choices = c("P(X ≥ x)" = "greater_eq",
                                "P(X ≤ x)" = "less_eq",
                                "P(X = x)" = "equal"))
      ),
      
      conditionalPanel(
        condition = "input.dist == 'chisq'",
        numericInput("df_chisq", t$df, value = 1, min = 1),
        selectInput("prob_type", t$prob_type,
                    choices = c("P(X > x)" = "greater",
                                "P(X < x)" = "less"))
      ),
      
      conditionalPanel(
        condition = "input.dist == 'beta'",
        numericInput("alpha", t$alpha, value = 2, min = 0.1),
        numericInput("beta", t$beta, value = 2, min = 0.1),
        selectInput("prob_type", t$prob_type,
                    choices = c("P(X > x)" = "greater",
                                "P(X < x)" = "less",
                                "2P(|X| > x)" = "both"))
      ),
      
      conditionalPanel(
        condition = "input.dist == 'exp'",
        numericInput("lambda_exp", t$lambda, value = 1, min = 0.1),
        selectInput("prob_type", t$prob_type,
                    choices = c("P(X > x)" = "greater",
                                "P(X < x)" = "less"))
      ),
      
      conditionalPanel(
        condition = "input.dist == 'f'",
        numericInput("df1", t$df1, value = 1, min = 1),
        numericInput("df2", t$df2, value = 1, min = 1),
        selectInput("prob_type", t$prob_type,
                    choices = c("P(X > x)" = "greater",
                                "P(X < x)" = "less"))
      ),
      
      conditionalPanel(
        condition = "input.dist == 'gamma'",
        numericInput("gamma_shape", t$gamma_shape, value = 2, min = 0.1),
        numericInput("gamma_rate", t$gamma_rate, value = 1, min = 0.1),
        selectInput("prob_type", t$prob_type,
                    choices = c("P(X > x)" = "greater",
                                "P(X < x)" = "less"))
      ),
      
      conditionalPanel(
        condition = "input.dist == 'lnorm'",
        numericInput("meanlog", t$meanlog, value = 0),
        numericInput("sdlog", t$sdlog, value = 1, min = 0.1),
        selectInput("prob_type", t$prob_type,
                    choices = c("P(X > x)" = "greater",
                                "P(X < x)" = "less"))
      ),
      
      conditionalPanel(
        condition = "input.dist == 'pareto'",
        numericInput("pareto_m", t$pareto_m, value = 1, min = 0.1),
        numericInput("pareto_alpha", t$pareto_alpha, value = 3, min = 0.1),
        selectInput("prob_type", t$prob_type,
                    choices = c("P(X > x)" = "greater",
                                "P(X < x)" = "less"))
      ),
      
      conditionalPanel(
        condition = "input.dist == 'weibull'",
        numericInput("weibull_alpha", t$weibull_alpha, value = 2, min = 0.1),
        numericInput("weibull_beta", t$weibull_beta, value = 1, min = 0.1),
        selectInput("prob_type", t$prob_type,
                    choices = c("P(X > x)" = "greater",
                                "P(X < x)" = "less"))
      ),
      
      conditionalPanel(
        condition = "input.dist == 'binom'",
        numericInput("binom_n", t$binom_n, value = 10, min = 1, step = 1),
        numericInput("binom_p", t$binom_p, value = 0.5, min = 0, max = 1),
        selectInput("prob_type_disc", t$prob_type,
                    choices = c("P(X ≥ x)" = "greater_eq",
                              "P(X ≤ x)" = "less_eq",
                              "P(X = x)" = "equal"))
      ),
      
      conditionalPanel(
        condition = "input.dist == 'geom1'",
        numericInput("geom1_p", t$geom1_p, value = 0.5, min = 0.001, max = 1),
        selectInput("prob_type_disc", t$prob_type,
                    choices = c("P(X ≥ x)" = "greater_eq",
                              "P(X ≤ x)" = "less_eq",
                              "P(X = x)" = "equal"))
      ),
      
      conditionalPanel(
        condition = "input.dist == 'geom2'",
        numericInput("geom2_p", t$geom2_p, value = 0.5, min = 0.001, max = 1),
        selectInput("prob_type_disc", t$prob_type,
                    choices = c("P(X ≥ x)" = "greater_eq",
                              "P(X ≤ x)" = "less_eq",
                              "P(X = x)" = "equal"))
      ),
      
      conditionalPanel(
        condition = "input.dist == 'hyper'",
        numericInput("hyper_N", t$hyper_N, value = 100, min = 1, step = 1),
        numericInput("hyper_K", t$hyper_K, value = 50, min = 1, step = 1),
        numericInput("hyper_n", t$hyper_n, value = 10, min = 1, step = 1),
        selectInput("prob_type_disc", t$prob_type,
                    choices = c("P(X ≥ x)" = "greater_eq",
                              "P(X ≤ x)" = "less_eq",
                              "P(X = x)" = "equal"))
      ),
      
      conditionalPanel(
        condition = "input.dist == 'nbin1'",
        numericInput("nbin1_r", t$nbin1_r, value = 5, min = 1, step = 1),
        numericInput("nbin1_p", t$nbin1_p, value = 0.5, min = 0.001, max = 1),
        selectInput("prob_type_disc", t$prob_type,
                    choices = c("P(X ≥ x)" = "greater_eq",
                              "P(X ≤ x)" = "less_eq",
                              "P(X = x)" = "equal"))
      ),
      
      conditionalPanel(
        condition = "input.dist == 'nbin2'",
        numericInput("nbin2_r", t$nbin2_r, value = 5, min = 1, step = 1),
        numericInput("nbin2_p", t$nbin2_p, value = 0.5, min = 0.001, max = 1),
        selectInput("prob_type_disc", t$prob_type,
                    choices = c("P(X ≥ x)" = "greater_eq",
                              "P(X ≤ x)" = "less_eq",
                              "P(X = x)" = "equal"))
      ),
      
      conditionalPanel(
        condition = "input.dist != 'pois' && input.dist != 'chisq' && input.dist != 'beta' && input.dist != 'exp' && input.dist != 'f' && input.dist != 'gamma' && input.dist != 'lnorm' && input.dist != 'pareto' && input.dist != 'weibull' && input.dist != 'binom' && input.dist != 'geom1' && input.dist != 'geom2' && input.dist != 'hyper' && input.dist != 'nbin1' && input.dist != 'nbin2'",
        selectInput("prob_type", t$prob_type,
                    choices = c("P(X > x)" = "greater",
                                "P(X < x)" = "less",
                                "2P(|X| > x)" = "both"))
      ),
      
      switchInput("prob_input", t$input_type, value = FALSE),
      
      conditionalPanel(
        condition = "!input.prob_input",
        numericInput("x_value", t$x_value, value = 0)
      ),
      
      conditionalPanel(
        condition = "input.prob_input",
        numericInput("prob_value", t$prob_value, value = 0.95, min = 0, max = 1)
      ),
      
      actionButton("calculate", t$calculate, class = "btn-primary")
    )
  })
  
  # Card do gráfico
  output$plot_card <- renderUI({
    t <- t()
    
    card(
      card_header(t$visualization),
      plotOutput("dist_plot", height = "400px"),
      card_footer(
        uiOutput("result_text")
      )
    )
  })
  
  dist_data <- reactive({
    if (input$dist %in% c("binom", "pois", "geom1", "geom2", "hyper", "nbin1", "nbin2")) {
      if (input$dist == "nbin1") {
        validate(
          need(input$nbin1_r >= 1 && input$nbin1_r == round(input$nbin1_r), t()$nbin1_error),
          need(input$nbin1_p > 0 && input$nbin1_p <= 1, t()$nbin1_error)
        )
        x <- input$nbin1_r:qnbinom(0.999, size = input$nbin1_r, prob = input$nbin1_p)
        y <- dnbinom(x - input$nbin1_r, size = input$nbin1_r, prob = input$nbin1_p)
        data.frame(x = x, y = y)
      } else if (input$dist == "nbin2") {
        validate(
          need(input$nbin2_r >= 1 && input$nbin2_r == round(input$nbin2_r), t()$nbin2_error),
          need(input$nbin2_p > 0 && input$nbin2_p <= 1, t()$nbin2_error)
        )
        x <- 0:qnbinom(0.999, size = input$nbin2_r, prob = input$nbin2_p)
        y <- dnbinom(x, size = input$nbin2_r, prob = input$nbin2_p)
        data.frame(x = x, y = y)
      } else if (input$dist == "hyper") {
        validate(
          need(input$hyper_N >= 1 && input$hyper_N == round(input$hyper_N), t()$hyper_error),
          need(input$hyper_K >= 1 && input$hyper_K <= input$hyper_N && input$hyper_K == round(input$hyper_K), t()$hyper_error),
          need(input$hyper_n >= 1 && input$hyper_n <= input$hyper_N && input$hyper_n == round(input$hyper_n), t()$hyper_error)
        )
        x <- max(0, input$hyper_n - (input$hyper_N - input$hyper_K)):min(input$hyper_n, input$hyper_K)
        y <- dhyper(x, input$hyper_K, input$hyper_N - input$hyper_K, input$hyper_n)
      } else if (input$dist == "geom1") {
        validate(
          need(input$geom1_p > 0 && input$geom1_p <= 1, t()$geom1_error)
        )
        x <- 1:qgeom(0.999, prob = input$geom1_p)
        y <- dgeom(x - 1, prob = input$geom1_p)
      } else if (input$dist == "geom2") {
        validate(
          need(input$geom2_p > 0 && input$geom2_p <= 1, t()$geom2_error)
        )
        x <- 0:qgeom(0.999, prob = input$geom2_p)
        y <- dgeom(x, prob = input$geom2_p)
      } else {
        x <- 0:input$binom_n
        if (input$dist == "binom") {
          y <- dbinom(x, size = input$binom_n, prob = input$binom_p)
        } else if (input$dist == "pois") {
      x <- 0:max(20, qpois(0.999, input$lambda))
      y <- dpois(x, lambda = input$lambda)
        } else {
          y <- dhyper(x, input$hyper_K, input$hyper_N - input$hyper_K, input$hyper_n)
        }
      }
      data.frame(x = x, y = y)
    } else {
      if (input$dist == "pareto") {
        validate(
          need(input$pareto_m > 0 && input$pareto_alpha > 0, 
               t()$pareto_error)
        )
        x <- seq(input$pareto_m, qpareto(0.999, input$pareto_m, input$pareto_alpha), length.out = 200)
        y <- dpareto(x, input$pareto_m, input$pareto_alpha)
      } else if (input$dist == "lnorm") {
        validate(
          need(input$sdlog > 0, t()$lnorm_error)
        )
        x <- seq(0, qlnorm(0.999, meanlog = input$meanlog, sdlog = input$sdlog), length.out = 200)
        y <- dlnorm(x, meanlog = input$meanlog, sdlog = input$sdlog)
      } else if (input$dist == "f") {
        validate(
          need(input$df1 > 0 && input$df2 > 0, t()$f_error)
        )
        x <- seq(0, qf(0.999, df1 = input$df1, df2 = input$df2), length.out = 200)
        y <- df(x, df1 = input$df1, df2 = input$df2)
      } else if (input$dist == "chisq") {
        validate(
          need(input$df_chisq > 0, t()$chisq_error)
        )
        x <- seq(0, qchisq(0.999, df = input$df_chisq), length.out = 200)
        y <- dchisq(x, df = input$df_chisq)
      } else if (input$dist == "beta") {
        validate(
          need(input$alpha > 0 && input$beta > 0, t()$beta_error)
        )
        x <- seq(0, 1, length.out = 200)
        y <- dbeta(x, shape1 = input$alpha, shape2 = input$beta)
      } else if (input$dist == "exp") {
        validate(
          need(input$lambda_exp > 0, t()$exp_error)
        )
        x <- seq(0, qexp(0.999, rate = input$lambda_exp), length.out = 200)
        y <- dexp(x, rate = input$lambda_exp)
      } else if (input$dist == "gamma") {
        validate(
          need(input$gamma_shape > 0 && input$gamma_rate > 0, t()$gamma_error)
        )
        x <- seq(0, qgamma(0.999, shape = input$gamma_shape, rate = input$gamma_rate), length.out = 200)
        y <- dgamma(x, shape = input$gamma_shape, rate = input$gamma_rate)
      } else if (input$dist == "weibull") {
        validate(
          need(input$weibull_alpha > 0 && input$weibull_beta > 0, 
               t()$weibull_error)
        )
        x <- seq(0, qweibull(0.999, shape = input$weibull_alpha, scale = input$weibull_beta), length.out = 200)
        y <- dweibull(x, shape = input$weibull_alpha, scale = input$weibull_beta)
    } else {
      x <- seq(-4, 4, length.out = 200)
      if (input$dist == "norm") {
          validate(
            need(input$sd > 0, t()$norm_error)
          )
        y <- dnorm(x, mean = input$mean, sd = input$sd)
      } else {
          validate(
            need(input$df > 0, t()$t_error)
          )
        y <- dt(x, df = input$df)
        }
      }
      data.frame(x = x, y = y)
    }
  })
  
  result_values <- reactiveVal(list(x = 0, prob = 0, x_from_p = 0))
  
  observe({
    req(input$calculate)
    
    # Inicializar prob e x_from_p
    prob <- 0
    x_from_p <- 0
    
    if (!input$prob_input) {
      # Cálculo quando o usuário fornece x
      if (input$dist == "norm") {
        prob <- if (input$prob_type == "greater") {
          1 - pnorm(input$x_value, input$mean, input$sd)
        } else if (input$prob_type == "less") {
          pnorm(input$x_value, input$mean, input$sd)
        } else {
          2 * (1 - pnorm(abs(input$x_value), input$mean, input$sd))
        }
      } else if (input$dist == "t") {
        prob <- if (input$prob_type == "greater") {
          1 - pt(input$x_value, df = input$df)
        } else if (input$prob_type == "less") {
          pt(input$x_value, df = input$df)
        } else {
          2 * (1 - pt(abs(input$x_value), df = input$df))
        }
      }
      # ... outros casos de distribuição ...
      
    } else {
      # Cálculo quando o usuário fornece probabilidade
      if (input$dist == "norm") {
        x_from_p <- if (input$prob_type == "greater") {
          qnorm(1 - input$prob_value, input$mean, input$sd)
        } else if (input$prob_type == "less") {
          qnorm(input$prob_value, input$mean, input$sd)
        } else {
          qnorm(1 - input$prob_value/2, input$mean, input$sd)
        }
        prob <- input$prob_value
      }
      # ... outros casos de distribuição ...
    }
    
    # Atualizar os valores
    result_values(list(
      x = if (!input$prob_input) input$x_value else x_from_p,
      prob = prob,
      x_from_p = x_from_p
    ))
  })
  
  output$dist_plot <- renderPlot({
    data <- dist_data()
    x_value <- result_values()$x
    
    if (input$dist %in% c("pois", "binom", "geom1", "geom2", "hyper", "nbin1", "nbin2")) {  # Adicione todas as distribuições discretas aqui
      p <- ggplot(data, aes(x = x, y = y)) +
        geom_col(fill = "lightblue", color = "black") +
        theme_minimal() +
        labs(x = "X", y = "Probabilidade") +
        theme(text = element_text(size = 14))
      
      highlight_data <- data
      highlight_data$highlight <- FALSE
      if (input$prob_type_disc == "greater_eq") {
        highlight_data$highlight <- highlight_data$x >= x_value
      } else if (input$prob_type_disc == "less_eq") {
        highlight_data$highlight <- highlight_data$x <= x_value
      } else {
        highlight_data$highlight <- highlight_data$x == x_value
      }
      
      p <- p + geom_col(data = subset(highlight_data, highlight),
                        aes(x = x, y = y),
                        fill = "blue", alpha = 0.3)
    } else {
      p <- ggplot(data, aes(x = x, y = y)) +
        geom_line(size = 1) +
        theme_minimal() +
        labs(x = "X", y = "Densidade") +
        theme(text = element_text(size = 14))
      
      if (input$prob_type == "greater") {
        shade_data <- data[data$x >= x_value, ]
        p <- p + geom_area(data = shade_data, fill = "blue", alpha = 0.3)
      } else if (input$prob_type == "less") {
        shade_data <- data[data$x <= x_value, ]
        p <- p + geom_area(data = shade_data, fill = "blue", alpha = 0.3)
      } else {
        # Área bilateral: sombrear as duas caudas
        shade_data_left <- data[data$x <= -abs(x_value), ]
        shade_data_right <- data[data$x >= abs(x_value), ]
      
        p <- p + 
          geom_area(data = shade_data_left, fill = "blue", alpha = 0.3) +
          geom_area(data = shade_data_right, fill = "blue", alpha = 0.3)
    }
    
    p <- p + geom_vline(xintercept = x_value, 
                        linetype = "dashed", 
                        color = "red")
      
      # Adicionar linha vertical em -x para o caso bilateral
      if (input$prob_type == "both") {
        p <- p + geom_vline(xintercept = -x_value, 
                           linetype = "dashed", 
                           color = "red")
      }
    }
    
    p
  })
  
  output$result_text <- renderUI({
    t <- t()
    res <- result_values()
    HTML(sprintf(
      "<h4>%s</h4>",
      sprintf(t$result_p, res$x, res$prob)
    ))
  })
}

switchInput <- function(inputId, label, value = FALSE) {
  div(
    class = "form-check form-switch mb-3",
    tags$input(
      class = "form-check-input",
      type = "checkbox",
      id = inputId,
      checked = if(value) "checked" else NULL
    ),
    tags$label(
      class = "form-check-label",
      `for` = inputId,
      label
    )
  )
}

dpareto <- function(x, m, alpha) {
  ifelse(x >= m, alpha * m^alpha / x^(alpha + 1), 0)
}

ppareto <- function(q, m, alpha) {
  ifelse(q >= m, 1 - (m/q)^alpha, 0)
}

qpareto <- function(p, m, alpha) {
  m / (1 - p)^(1/alpha)
}

shinyApp(ui, server)
