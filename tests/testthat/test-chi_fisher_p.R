# test-chi_fisher_p.R

context("test-chi_fisher_p.R")

test_that("returns chi-squared p value if no warnings are thrown", {
  expect_silent(chisq.test(treatment$gender, treatment$treatment))
  expect_equal(chi_fisher_p(treatment, "gender", "treatment"), chisq.test(treatment$gender, treatment$treatment)$p.value)
})

test_that("returns fisher p value if chi-squared warnings are thrown", {
  expect_warning(chisq.test(treatment$outcome, treatment$treatment))
  expect_equal(chi_fisher_p(treatment, "outcome", "treatment"), fisher.test(treatment$outcome, treatment$treatment)$p.value)
})
