# Generated by roxytest: Do not edit by hand!

# File R/get_value.R: @tests

test_that("Function gedcom_value() @ L19", {
  expect_equal(gedcom_value(tidyged::sample555, "HD", "FORM", 2), "LINEAGE-LINKED")
  expect_equal(gedcom_value(tidyged::sample555, "HD", "TEST", 1), "")
  expect_equal(gedcom_value(tidyged::sample555, "HD", "VERS", 2), "5.5.5")
  expect_equal(gedcom_value(tidyged::sample555, "HD", "VERS", 3), "5.5.5")
  expect_equal(gedcom_value(tidyged::sample555, "@I1@", "VERS", 3), "")
})

