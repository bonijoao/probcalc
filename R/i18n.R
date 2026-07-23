# Infraestrutura de i18n. As listas translations_* vivem em R/translations_*.R;
# como o Shiny carrega R/ em ordem alfabética, este arquivo só as referencia
# dentro de funções (avaliação adiada), nunca no top-level.

all_translations <- function() {
  list(
    pt = translations_pt,
    en = translations_en,
    es = translations_es
  )
}

# Texto de ajuda (fórmulas) em HTML+LaTeX, um arquivo por idioma em www/
load_help_html <- function(lang) {
  path <- file.path("www", sprintf("help_%s.html", lang))
  paste(readLines(path, encoding = "UTF-8", warn = FALSE), collapse = "\n")
}
