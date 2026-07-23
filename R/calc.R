# Funções puras de cálculo — sem dependência de input/session, testáveis.

# ID do numericInput de um parâmetro: "<dist>_<param>" (ex.: "chisq_df")
param_input_id <- function(dist_id, param_name) {
  paste(dist_id, param_name, sep = "_")
}

default_params <- function(dist) {
  lapply(DISTRIBUTIONS[[dist]]$params, `[[`, "default")
}

# Retorna a chave de tradução do erro, ou NULL se os parâmetros são válidos
validate_params <- function(dist, params) {
  if (any(vapply(params, is.null, logical(1))) || anyNA(unlist(params))) {
    return(paste0(dist, "_error"))
  }
  DISTRIBUTIONS[[dist]]$validate(params)
}

# P(...) dado x. prob_type: "greater"|"less"|"both" (contínuas),
# "greater_eq"|"less_eq"|"equal" (discretas)
calc_prob_from_x <- function(dist, params, x, prob_type) {
  spec <- DISTRIBUTIONS[[dist]]
  if (spec$discrete) {
    switch(prob_type,
      greater_eq = 1 - spec$pfun(x - 1, params),
      less_eq    = spec$pfun(x, params),
      equal      = spec$dfun(x, params)
    )
  } else {
    switch(prob_type,
      greater = 1 - spec$pfun(x, params),
      less    = spec$pfun(x, params),
      both    = 2 * (1 - spec$pfun(abs(x), params))
    )
  }
}

# x dado P. Para discretas, "equal" não tem inverso único -> NA
calc_x_from_prob <- function(dist, params, prob, prob_type) {
  spec <- DISTRIBUTIONS[[dist]]
  if (spec$discrete) {
    switch(prob_type,
      greater_eq = spec$qfun(1 - prob, params),
      less_eq    = spec$qfun(prob, params),
      equal      = NA_real_
    )
  } else {
    switch(prob_type,
      greater = spec$qfun(1 - prob, params),
      less    = spec$qfun(prob, params),
      both    = spec$qfun(1 - prob / 2, params)
    )
  }
}

make_plot_data <- function(dist, params) {
  spec <- DISTRIBUTIONS[[dist]]
  x <- spec$plot_x(params)
  data.frame(x = x, y = spec$dfun(x, params))
}
