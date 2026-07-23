test_that("dpareto/ppareto/qpareto são consistentes", {
  expect_equal(ppareto(2, m = 1, alpha = 3), 1 - (1 / 2)^3)
  expect_equal(ppareto(0.5, m = 1, alpha = 3), 0)
  expect_equal(dpareto(0.5, m = 1, alpha = 3), 0)
  expect_equal(qpareto(ppareto(5, m = 2, alpha = 3), m = 2, alpha = 3), 5)
  expect_equal(
    integrate(dpareto, 1, Inf, m = 1, alpha = 3)$value, 1,
    tolerance = 1e-6
  )
})
