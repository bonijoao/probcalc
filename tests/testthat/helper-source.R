for (f in list.files(testthat::test_path("..", "..", "R"),
                     full.names = TRUE, pattern = "\\.R$")) {
  source(f, encoding = "UTF-8")
}
