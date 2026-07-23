# Componentes de UI derivados do registry de distribuições.

# Toggle Bootstrap (form-switch) — o Shiny o trata como checkbox
switchInput <- function(inputId, label, value = FALSE) {
  div(
    class = "form-check form-switch mb-3",
    tags$input(
      class = "form-check-input",
      type = "checkbox",
      id = inputId,
      checked = if (value) "checked" else NULL
    ),
    tags$label(
      class = "form-check-label",
      `for` = inputId,
      label
    )
  )
}

# Opções do selectInput de distribuições, na ordem do registry
dist_choices <- function() {
  setNames(
    names(DISTRIBUTIONS),
    vapply(DISTRIBUTIONS, function(spec) spec$label, character(1))
  )
}

# Opções de tipo de probabilidade conforme a distribuição
prob_type_choices <- function(spec) {
  if (spec$discrete) {
    c("P(X ≥ x)" = "greater_eq",
      "P(X ≤ x)" = "less_eq",
      "P(X = x)" = "equal")
  } else {
    choices <- c("P(X > x)" = "greater", "P(X < x)" = "less")
    if (spec$two_sided) {
      choices <- c(choices, "2P(|X| > x)" = "both")
    }
    choices
  }
}

# Um conditionalPanel por distribuição, com os numericInputs de seus
# parâmetros gerados a partir do registry
dist_param_panels <- function(labels) {
  panels <- lapply(names(DISTRIBUTIONS), function(dist_id) {
    spec <- DISTRIBUTIONS[[dist_id]]
    inputs <- lapply(names(spec$params), function(pn) {
      pdef <- spec$params[[pn]]
      do.call(numericInput, c(
        list(
          inputId = param_input_id(dist_id, pn),
          label = labels[[pdef$label_key]],
          value = pdef$default
        ),
        pdef[intersect(names(pdef), c("min", "max", "step"))]
      ))
    })
    conditionalPanel(
      condition = sprintf("input.dist == '%s'", dist_id),
      inputs
    )
  })
  tagList(panels)
}
