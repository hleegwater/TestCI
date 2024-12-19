# treatment-data.R

treatment <- tibble::tibble(
  treatment = c(rep("old", 50), rep("new", 50)),
  gender    = c(rep("male", 30), rep("female", 20), rep("male", 20), rep("female", 30)),
  outcome   = c(rep("failure", 95), rep("success", 5))
)
