library(shiny)
library(bslib)
library(ggplot2)
library(bsicons)

# Registry, cálculo, helpers de UI e traduções são carregados
# automaticamente pelo Shiny a partir de R/

ui <- function(request) {
  page_fillable(
    theme = bs_theme(version = 5, preset = "shiny"),

    tags$head(
      tags$script(src = "https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js"),
      tags$script(
        type = "text/javascript",
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
      ),
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

    layout_columns(
      col_widths = c(8, 4),

      h1(textOutput("title"), class = "text-center"),

      div(
        style = "text-align: right;",
        actionButton(
          "reset_btn",
          label = NULL,
          icon = icon("rotate-left"),
          class = "btn btn-link",
          style = "font-size: 24px; padding: 0px 4px; color: #666; margin-right: 8px;"
        ),
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
  current_lang <- reactiveVal("pt")

  tr <- reactive({
    all_translations()[[current_lang()]]
  })

  observeEvent(input$settings_btn, {
    labels <- tr()
    showModal(modalDialog(
      title = labels$settings,

      tabsetPanel(
        tabPanel(
          labels$language,
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
          labels$help,
          div(
            style = "margin: 20px 0;",
            HTML(load_help_html(current_lang())),
            tags$script(HTML(
              "if (window.MathJax && MathJax.typesetPromise) { MathJax.typesetPromise(); }"
            ))
          )
        ),

        tabPanel(
          labels$about,
          div(
            style = "margin: 20px 0;",
            HTML(labels$about_text)
          )
        )
      ),

      footer = tagList(
        modalButton(labels$close)
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

  output$title <- renderText({
    tr()$title
  })

  output$params_card <- renderUI({
    labels <- tr()
    # isolate: o card só re-renderiza na troca de idioma, preservando a
    # distribuição selecionada
    dist_selected <- isolate(input$dist) %||% names(DISTRIBUTIONS)[1]

    card(
      card_header(labels$params),

      selectInput("dist", labels$dist,
                  choices = dist_choices(),
                  selected = dist_selected),

      dist_param_panels(labels),

      selectInput("prob_type", labels$prob_type,
                  choices = prob_type_choices(DISTRIBUTIONS[[dist_selected]])),

      switchInput("prob_input", labels$input_type, value = FALSE),

      conditionalPanel(
        condition = "!input.prob_input",
        numericInput("x_value", labels$x_value, value = 0)
      ),

      conditionalPanel(
        condition = "input.prob_input",
        numericInput("prob_value", labels$prob_value, value = 0.95, min = 0, max = 1)
      ),

      actionButton("calculate", labels$calculate, class = "btn-primary")
    )
  })

  output$plot_card <- renderUI({
    labels <- tr()

    card(
      card_header(labels$visualization),
      plotOutput("dist_plot", height = "400px"),
      card_footer(
        uiOutput("result_text")
      )
    )
  })

  # Parâmetros da distribuição atual, lidos dos inputs "<dist>_<param>"
  current_params <- reactive({
    spec <- DISTRIBUTIONS[[input$dist]]
    setNames(
      lapply(names(spec$params), function(pn) {
        input[[param_input_id(input$dist, pn)]]
      }),
      names(spec$params)
    )
  })

  dist_data <- reactive({
    req(input$dist)
    params <- current_params()
    err <- validate_params(input$dist, params)
    if (!is.null(err)) {
      validate(need(FALSE, tr()[[err]]))
    }
    make_plot_data(input$dist, params)
  })

  result_values <- reactiveVal(list(x = 0, prob = 0, mode = "prob"))

  # Após o primeiro clique em Calcular, recalcula a cada mudança de input
  # (semântica original do app)
  observe({
    req(input$calculate)
    req(input$dist, input$prob_type)
    spec <- DISTRIBUTIONS[[input$dist]]
    req(input$prob_type %in% prob_type_choices(spec))

    params <- current_params()
    req(is.null(validate_params(input$dist, params)))

    if (!input$prob_input) {
      req(input$x_value)
      prob <- calc_prob_from_x(input$dist, params, input$x_value, input$prob_type)
      result_values(list(x = input$x_value, prob = prob, mode = "prob"))
    } else {
      req(input$prob_value)
      x <- calc_x_from_prob(input$dist, params, input$prob_value, input$prob_type)
      result_values(list(x = x, prob = input$prob_value, mode = "quantile"))
    }
  })

  output$dist_plot <- renderPlot({
    req(input$dist, input$prob_type)
    spec <- DISTRIBUTIONS[[input$dist]]
    req(input$prob_type %in% prob_type_choices(spec))

    data <- dist_data()
    x_value <- result_values()$x

    if (spec$discrete) {
      p <- ggplot(data, aes(x = x, y = y)) +
        geom_col(fill = "lightblue", color = "black") +
        theme_minimal() +
        labs(x = "X", y = "Probabilidade") +
        theme(text = element_text(size = 14))

      if (!is.na(x_value)) {
        highlight_data <- data
        highlight_data$highlight <- switch(input$prob_type,
          greater_eq = highlight_data$x >= x_value,
          less_eq    = highlight_data$x <= x_value,
          equal      = highlight_data$x == x_value
        )

        p <- p + geom_col(data = subset(highlight_data, highlight),
                          aes(x = x, y = y),
                          fill = "blue", alpha = 0.3)
      }
    } else {
      p <- ggplot(data, aes(x = x, y = y)) +
        geom_line(linewidth = 1) +
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
        shade_data_left <- data[data$x <= -abs(x_value), ]
        shade_data_right <- data[data$x >= abs(x_value), ]

        p <- p +
          geom_area(data = shade_data_left, fill = "blue", alpha = 0.3) +
          geom_area(data = shade_data_right, fill = "blue", alpha = 0.3)
      }

      p <- p + geom_vline(xintercept = x_value,
                          linetype = "dashed",
                          color = "red")

      if (input$prob_type == "both") {
        p <- p + geom_vline(xintercept = -x_value,
                            linetype = "dashed",
                            color = "red")
      }
    }

    p
  })

  output$result_text <- renderUI({
    labels <- tr()
    res <- result_values()
    text <- if (identical(res$mode, "quantile")) {
      sprintf(labels$result_x, res$prob, res$x)
    } else {
      sprintf(labels$result_p, res$x, res$prob)
    }
    HTML(sprintf("<h4>%s</h4>", text))
  })

  resetValues <- function() {
    spec <- DISTRIBUTIONS[[input$dist]]
    for (pn in names(spec$params)) {
      updateNumericInput(session, param_input_id(input$dist, pn),
                         value = spec$params[[pn]]$default)
    }

    updateNumericInput(session, "x_value", value = 0)
    updateNumericInput(session, "prob_value", value = 0.95)
    updateCheckboxInput(session, "prob_input", value = FALSE)

    result_values(list(x = 0, prob = 0, mode = "prob"))
  }

  observeEvent(input$reset_btn, {
    req(input$dist)
    resetValues()
  })

  # Trocar de distribuição reseta os inputs e ajusta os tipos de
  # probabilidade disponíveis
  observeEvent(input$dist, {
    spec <- DISTRIBUTIONS[[input$dist]]
    updateSelectInput(session, "prob_type",
                      choices = prob_type_choices(spec))
    resetValues()
  })
}

shinyApp(ui, server)
