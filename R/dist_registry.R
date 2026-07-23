# Registry central de distribuições.
#
# Cada entrada descreve completamente uma distribuição:
#   label     - nome exibido no seletor
#   discrete  - TRUE para pmf (barras), FALSE para pdf (curva)
#   two_sided - oferece o tipo de probabilidade bilateral "both"
#   params    - por parâmetro: label_key (chave de tradução), default e
#               restrições (min/max/step) repassadas ao numericInput
#   dfun/pfun/qfun - densidade/acumulada/quantil, sempre no espaço do x
#               exibido ao usuário (offsets de geom1/nbin1 ficam aqui dentro)
#   validate  - recebe a lista de parâmetros; retorna a chave de tradução do
#               erro ou NULL se válidos
#   plot_x    - vetor de x para o gráfico, dado os parâmetros
#
# UI, cálculo, validação e reset iteram sobre esta lista — para adicionar uma
# distribuição basta adicionar uma entrada aqui (e as chaves de tradução).

DISTRIBUTIONS <- list(
  norm = list(
    label = "Normal", discrete = FALSE, two_sided = TRUE,
    params = list(
      mean = list(label_key = "mean", default = 0),
      sd   = list(label_key = "sd", default = 1, min = 0.1)
    ),
    dfun = function(x, p) dnorm(x, mean = p$mean, sd = p$sd),
    pfun = function(q, p) pnorm(q, mean = p$mean, sd = p$sd),
    qfun = function(prob, p) qnorm(prob, mean = p$mean, sd = p$sd),
    validate = function(p) if (p$sd <= 0) "norm_error" else NULL,
    plot_x = function(p) seq(-4, 4, length.out = 200)
  ),

  t = list(
    label = "t-Student", discrete = FALSE, two_sided = TRUE,
    params = list(
      df = list(label_key = "df", default = 5, min = 1)
    ),
    dfun = function(x, p) dt(x, df = p$df),
    pfun = function(q, p) pt(q, df = p$df),
    qfun = function(prob, p) qt(prob, df = p$df),
    validate = function(p) if (p$df <= 0) "t_error" else NULL,
    plot_x = function(p) seq(-4, 4, length.out = 200)
  ),

  chisq = list(
    label = "Qui-quadrado", discrete = FALSE, two_sided = FALSE,
    params = list(
      df = list(label_key = "df", default = 1, min = 1)
    ),
    dfun = function(x, p) dchisq(x, df = p$df),
    pfun = function(q, p) pchisq(q, df = p$df),
    qfun = function(prob, p) qchisq(prob, df = p$df),
    validate = function(p) if (p$df <= 0) "chisq_error" else NULL,
    plot_x = function(p) seq(0, qchisq(0.999, df = p$df), length.out = 200)
  ),

  f = list(
    label = "F", discrete = FALSE, two_sided = FALSE,
    params = list(
      df1 = list(label_key = "df1", default = 1, min = 1),
      df2 = list(label_key = "df2", default = 1, min = 1)
    ),
    dfun = function(x, p) df(x, df1 = p$df1, df2 = p$df2),
    pfun = function(q, p) pf(q, df1 = p$df1, df2 = p$df2),
    qfun = function(prob, p) qf(prob, df1 = p$df1, df2 = p$df2),
    validate = function(p) if (p$df1 <= 0 || p$df2 <= 0) "f_error" else NULL,
    plot_x = function(p) seq(0, qf(0.999, df1 = p$df1, df2 = p$df2), length.out = 200)
  ),

  gamma = list(
    label = "Gamma", discrete = FALSE, two_sided = FALSE,
    params = list(
      shape = list(label_key = "gamma_shape", default = 2, min = 0.1),
      rate  = list(label_key = "gamma_rate", default = 1, min = 0.1)
    ),
    dfun = function(x, p) dgamma(x, shape = p$shape, rate = p$rate),
    pfun = function(q, p) pgamma(q, shape = p$shape, rate = p$rate),
    qfun = function(prob, p) qgamma(prob, shape = p$shape, rate = p$rate),
    validate = function(p) if (p$shape <= 0 || p$rate <= 0) "gamma_error" else NULL,
    plot_x = function(p) seq(0, qgamma(0.999, shape = p$shape, rate = p$rate), length.out = 200)
  ),

  lnorm = list(
    label = "Log-normal", discrete = FALSE, two_sided = FALSE,
    params = list(
      meanlog = list(label_key = "meanlog", default = 0),
      sdlog   = list(label_key = "sdlog", default = 1, min = 0.1)
    ),
    dfun = function(x, p) dlnorm(x, meanlog = p$meanlog, sdlog = p$sdlog),
    pfun = function(q, p) plnorm(q, meanlog = p$meanlog, sdlog = p$sdlog),
    qfun = function(prob, p) qlnorm(prob, meanlog = p$meanlog, sdlog = p$sdlog),
    validate = function(p) if (p$sdlog <= 0) "lnorm_error" else NULL,
    plot_x = function(p) seq(0, qlnorm(0.999, meanlog = p$meanlog, sdlog = p$sdlog), length.out = 200)
  ),

  pareto = list(
    label = "Pareto", discrete = FALSE, two_sided = FALSE,
    params = list(
      m     = list(label_key = "pareto_m", default = 1, min = 0.1),
      alpha = list(label_key = "pareto_alpha", default = 3, min = 0.1)
    ),
    dfun = function(x, p) dpareto(x, m = p$m, alpha = p$alpha),
    pfun = function(q, p) ppareto(q, m = p$m, alpha = p$alpha),
    qfun = function(prob, p) qpareto(prob, m = p$m, alpha = p$alpha),
    validate = function(p) if (p$m <= 0 || p$alpha <= 0) "pareto_error" else NULL,
    plot_x = function(p) seq(p$m, qpareto(0.999, m = p$m, alpha = p$alpha), length.out = 200)
  ),

  weibull = list(
    label = "Weibull", discrete = FALSE, two_sided = FALSE,
    params = list(
      alpha = list(label_key = "weibull_alpha", default = 2, min = 0.1),
      beta  = list(label_key = "weibull_beta", default = 1, min = 0.1)
    ),
    dfun = function(x, p) dweibull(x, shape = p$alpha, scale = p$beta),
    pfun = function(q, p) pweibull(q, shape = p$alpha, scale = p$beta),
    qfun = function(prob, p) qweibull(prob, shape = p$alpha, scale = p$beta),
    validate = function(p) if (p$alpha <= 0 || p$beta <= 0) "weibull_error" else NULL,
    plot_x = function(p) seq(0, qweibull(0.999, shape = p$alpha, scale = p$beta), length.out = 200)
  ),

  binom = list(
    label = "Binomial", discrete = TRUE, two_sided = FALSE,
    params = list(
      n = list(label_key = "binom_n", default = 10, min = 1, step = 1),
      p = list(label_key = "binom_p", default = 0.5, min = 0, max = 1)
    ),
    dfun = function(x, p) dbinom(x, size = p$n, prob = p$p),
    pfun = function(q, p) pbinom(q, size = p$n, prob = p$p),
    qfun = function(prob, p) qbinom(prob, size = p$n, prob = p$p),
    validate = function(p) {
      if (p$n < 1 || p$n != round(p$n) || p$p < 0 || p$p > 1) "binom_error" else NULL
    },
    plot_x = function(p) 0:p$n
  ),

  geom1 = list(
    label = "Geométrica (tentativas)", discrete = TRUE, two_sided = FALSE,
    params = list(
      p = list(label_key = "geom1_p", default = 0.5, min = 0.001, max = 1)
    ),
    dfun = function(x, p) dgeom(x - 1, prob = p$p),
    pfun = function(q, p) pgeom(q - 1, prob = p$p),
    qfun = function(prob, p) qgeom(prob, prob = p$p) + 1,
    validate = function(p) if (p$p <= 0 || p$p > 1) "geom1_error" else NULL,
    plot_x = function(p) 1:qgeom(0.999, prob = p$p)
  ),

  geom2 = list(
    label = "Geométrica (fracassos)", discrete = TRUE, two_sided = FALSE,
    params = list(
      p = list(label_key = "geom2_p", default = 0.5, min = 0.001, max = 1)
    ),
    dfun = function(x, p) dgeom(x, prob = p$p),
    pfun = function(q, p) pgeom(q, prob = p$p),
    qfun = function(prob, p) qgeom(prob, prob = p$p),
    validate = function(p) if (p$p <= 0 || p$p > 1) "geom2_error" else NULL,
    plot_x = function(p) 0:qgeom(0.999, prob = p$p)
  ),

  hyper = list(
    label = "Hipergeométrica", discrete = TRUE, two_sided = FALSE,
    params = list(
      N = list(label_key = "hyper_N", default = 100, min = 1, step = 1),
      K = list(label_key = "hyper_K", default = 50, min = 1, step = 1),
      n = list(label_key = "hyper_n", default = 10, min = 1, step = 1)
    ),
    dfun = function(x, p) dhyper(x, p$K, p$N - p$K, p$n),
    pfun = function(q, p) phyper(q, p$K, p$N - p$K, p$n),
    qfun = function(prob, p) qhyper(prob, p$K, p$N - p$K, p$n),
    validate = function(p) {
      vals <- c(p$N, p$K, p$n)
      ok <- all(vals >= 1) && all(vals == round(vals)) && p$K <= p$N && p$n <= p$N
      if (!ok) "hyper_error" else NULL
    },
    plot_x = function(p) max(0, p$n - (p$N - p$K)):min(p$n, p$K)
  ),

  beta = list(
    label = "Beta", discrete = FALSE, two_sided = TRUE,
    params = list(
      alpha = list(label_key = "alpha", default = 2, min = 0.1),
      beta  = list(label_key = "beta", default = 2, min = 0.1)
    ),
    dfun = function(x, p) dbeta(x, shape1 = p$alpha, shape2 = p$beta),
    pfun = function(q, p) pbeta(q, shape1 = p$alpha, shape2 = p$beta),
    qfun = function(prob, p) qbeta(prob, shape1 = p$alpha, shape2 = p$beta),
    validate = function(p) if (p$alpha <= 0 || p$beta <= 0) "beta_error" else NULL,
    plot_x = function(p) seq(0, 1, length.out = 200)
  ),

  exp = list(
    label = "Exponencial", discrete = FALSE, two_sided = FALSE,
    params = list(
      lambda = list(label_key = "lambda", default = 1, min = 0.1)
    ),
    dfun = function(x, p) dexp(x, rate = p$lambda),
    pfun = function(q, p) pexp(q, rate = p$lambda),
    qfun = function(prob, p) qexp(prob, rate = p$lambda),
    validate = function(p) if (p$lambda <= 0) "exp_error" else NULL,
    plot_x = function(p) seq(0, qexp(0.999, rate = p$lambda), length.out = 200)
  ),

  pois = list(
    label = "Poisson", discrete = TRUE, two_sided = FALSE,
    params = list(
      lambda = list(label_key = "lambda", default = 5, min = 0.1)
    ),
    dfun = function(x, p) dpois(x, lambda = p$lambda),
    pfun = function(q, p) ppois(q, lambda = p$lambda),
    qfun = function(prob, p) qpois(prob, lambda = p$lambda),
    validate = function(p) if (p$lambda <= 0) "pois_error" else NULL,
    plot_x = function(p) 0:max(20, qpois(0.999, lambda = p$lambda))
  ),

  nbin1 = list(
    label = "Binomial Negativa (tentativas)", discrete = TRUE, two_sided = FALSE,
    params = list(
      r = list(label_key = "nbin1_r", default = 5, min = 1, step = 1),
      p = list(label_key = "nbin1_p", default = 0.5, min = 0.001, max = 1)
    ),
    dfun = function(x, p) dnbinom(x - p$r, size = p$r, prob = p$p),
    pfun = function(q, p) pnbinom(q - p$r, size = p$r, prob = p$p),
    qfun = function(prob, p) qnbinom(prob, size = p$r, prob = p$p) + p$r,
    validate = function(p) {
      if (p$r < 1 || p$r != round(p$r) || p$p <= 0 || p$p > 1) "nbin1_error" else NULL
    },
    plot_x = function(p) p$r:(qnbinom(0.999, size = p$r, prob = p$p) + p$r)
  ),

  nbin2 = list(
    label = "Binomial Negativa (fracassos)", discrete = TRUE, two_sided = FALSE,
    params = list(
      r = list(label_key = "nbin2_r", default = 5, min = 1, step = 1),
      p = list(label_key = "nbin2_p", default = 0.5, min = 0.001, max = 1)
    ),
    dfun = function(x, p) dnbinom(x, size = p$r, prob = p$p),
    pfun = function(q, p) pnbinom(q, size = p$r, prob = p$p),
    qfun = function(prob, p) qnbinom(prob, size = p$r, prob = p$p),
    validate = function(p) {
      if (p$r < 1 || p$r != round(p$r) || p$p <= 0 || p$p > 1) "nbin2_error" else NULL
    },
    plot_x = function(p) 0:qnbinom(0.999, size = p$r, prob = p$p)
  )
)

# Pareto não existe no R base
dpareto <- function(x, m, alpha) {
  ifelse(x >= m, alpha * m^alpha / x^(alpha + 1), 0)
}

ppareto <- function(q, m, alpha) {
  ifelse(q >= m, 1 - (m / q)^alpha, 0)
}

qpareto <- function(p, m, alpha) {
  m / (1 - p)^(1 / alpha)
}
