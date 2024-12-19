# chi_fisher_p.R

#' Function which calculates p-value via Chi-square or Fisher exact test.
#'
#' @param tbl (`tbl`) Dataframe that has variable and treatment columns of interest
#' @param var (`character`) Name of variable column
#' @param treatment (`character`) Name of treatment column
#'
#' @return (`numeric`) p-value
#'
#' @examples
#'
#' chi_fisher_p(treatment, "outcome", "treatment")
#' chi_fisher_p(treatment, "gender", "treatment")
#'
#' @export
chi_fisher_p <- function(tbl, var, treatment) {

  chisq_wrapper <- function(tbl, var, treatment) {

    var       <- tbl %>% dplyr::pull(var) %>% as.factor()
    treatment <- tbl %>% dplyr::pull(treatment) %>% as.factor()

    p <- stats::chisq.test(var, treatment)$p.value
    return(p)
  }

  fisher_wrapper <- function(tbl, var, treatment) {

    var       <- tbl %>% dplyr::pull(var) %>% as.factor()
    treatment <- tbl %>% dplyr::pull(treatment) %>% as.factor()

    p <- stats::fisher.test(var, treatment)$p.value
    return(p)
  }

  chisq_wrapper <- purrr::quietly(chisq_wrapper)
  chisq <- chisq_wrapper(tbl, var, treatment)

  if (length(chisq$warnings) == 0) {
    return(chisq$result)
  } else {
    return(fisher_wrapper(tbl, var, treatment))
  }

}
