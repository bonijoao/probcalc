test_that("P dado x funciona para TODAS as distribuições (regressão: 7 dists sem cálculo)", {
  for (id in names(DISTRIBUTIONS)) {
    spec <- DISTRIBUTIONS[[id]]
    params <- default_params(id)
    x <- spec$plot_x(params)[2]
    types <- if (spec$discrete) {
      c("greater_eq", "less_eq", "equal")
    } else {
      c("greater", "less")
    }
    for (pt in types) {
      prob <- calc_prob_from_x(id, params, x, pt)
      expect_true(
        is.finite(prob) && prob >= 0 && prob <= 1,
        info = paste(id, pt)
      )
    }
  }
})

test_that("x dado P funciona para todas as distribuições", {
  for (id in names(DISTRIBUTIONS)) {
    spec <- DISTRIBUTIONS[[id]]
    params <- default_params(id)
    types <- if (spec$discrete) c("greater_eq", "less_eq") else c("greater", "less")
    for (pt in types) {
      x <- calc_x_from_prob(id, params, 0.9, pt)
      expect_true(is.finite(x), info = paste(id, pt))
    }
  }
})

test_that("round-trip x -> P -> x nas contínuas", {
  for (id in names(DISTRIBUTIONS)) {
    spec <- DISTRIBUTIONS[[id]]
    if (spec$discrete) next
    params <- default_params(id)
    x0 <- median(spec$plot_x(params))
    pr <- calc_prob_from_x(id, params, x0, "less")
    expect_equal(
      calc_x_from_prob(id, params, pr, "less"), x0,
      tolerance = 1e-6, info = id
    )
  }
})

test_that("valores de referência", {
  expect_equal(
    calc_prob_from_x("norm", list(mean = 0, sd = 1), 1.96, "less"),
    pnorm(1.96)
  )
  expect_equal(
    calc_x_from_prob("norm", list(mean = 0, sd = 1), 0.975, "less"),
    qnorm(0.975)
  )
  expect_equal(
    calc_prob_from_x("norm", list(mean = 0, sd = 1), 1.96, "both"),
    2 * (1 - pnorm(1.96))
  )
  # regressão: qui-quadrado lia input$df (da t-Student) em vez do próprio df
  expect_equal(
    calc_prob_from_x("chisq", list(df = 4), 3, "greater"),
    1 - pchisq(3, df = 4)
  )
  expect_equal(
    calc_prob_from_x("geom1", list(p = 0.5), 3, "equal"),
    dgeom(2, prob = 0.5)
  )
  expect_equal(
    calc_prob_from_x("nbin1", list(r = 5, p = 0.5), 7, "equal"),
    dnbinom(2, size = 5, prob = 0.5)
  )
  expect_equal(
    calc_prob_from_x("hyper", list(N = 100, K = 50, n = 10), 5, "less_eq"),
    phyper(5, 50, 50, 10)
  )
  # inverso discreto: regra unificada qfun(1 - prob), sem o +1 extra que o
  # binomial aplicava (inconsistente com Poisson)
  expect_equal(
    calc_x_from_prob("binom", list(n = 10, p = 0.5), 0.9, "greater_eq"),
    qbinom(0.1, size = 10, prob = 0.5)
  )
  expect_equal(
    calc_x_from_prob("pois", list(lambda = 5), 0.9, "greater_eq"),
    qpois(0.1, lambda = 5)
  )
  # geom1/nbin1 continuam no espaço de tentativas (offset dentro do qfun)
  expect_equal(
    calc_x_from_prob("geom1", list(p = 0.5), 0.5, "less_eq"),
    qgeom(0.5, prob = 0.5) + 1
  )
  expect_true(is.na(calc_x_from_prob("binom", list(n = 10, p = 0.5), 0.5, "equal")))
})

test_that("validate_params retorna as chaves de erro corretas", {
  expect_null(validate_params("norm", list(mean = 0, sd = 1)))
  expect_equal(validate_params("norm", list(mean = 0, sd = -1)), "norm_error")
  expect_equal(validate_params("norm", list(mean = NULL, sd = 1)), "norm_error")
  expect_equal(validate_params("norm", list(mean = NA, sd = 1)), "norm_error")
  expect_equal(validate_params("hyper", list(N = 10, K = 20, n = 5)), "hyper_error")
  expect_equal(validate_params("binom", list(n = 2.5, p = 0.5)), "binom_error")
  expect_equal(validate_params("geom1", list(p = 1.5)), "geom1_error")
  for (id in names(DISTRIBUTIONS)) {
    expect_null(validate_params(id, default_params(id)), info = id)
  }
})

test_that("make_plot_data retorna dados válidos para todas as distribuições", {
  for (id in names(DISTRIBUTIONS)) {
    data <- make_plot_data(id, default_params(id))
    expect_true(nrow(data) > 1, info = id)
    # densidades com singularidade em x = 0 (chisq df=1, F df1=1) podem ter
    # Inf no primeiro ponto do grid — o ggplot descarta esse ponto
    y_interior <- data$y[-1]
    expect_true(all(is.finite(y_interior)) && all(y_interior >= 0), info = id)
  }
})

test_that("chaves de erro do registry existem nas traduções", {
  for (id in names(DISTRIBUTIONS)) {
    key <- paste0(id, "_error")
    expect_true(key %in% names(translations_pt), info = key)
  }
})

test_that("os 3 idiomas têm exatamente as mesmas chaves", {
  expect_equal(sort(names(translations_en)), sort(names(translations_pt)))
  expect_equal(sort(names(translations_es)), sort(names(translations_pt)))
})

test_that("label_key de cada parâmetro existe nas traduções", {
  for (id in names(DISTRIBUTIONS)) {
    for (pdef in DISTRIBUTIONS[[id]]$params) {
      expect_true(pdef$label_key %in% names(translations_pt),
                  info = paste(id, pdef$label_key))
    }
  }
})
