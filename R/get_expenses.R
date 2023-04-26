#' Title
#'
#' @param accountApiKey token api
#' @param startDateTS start as epoch
#' @param endDateTS end as epoch
#'
#' @export
#' @import httr
#' @import purrr
get_arrayExpenses <-
  function(accountApiKey = getOption("accountApiKey"),
           startDateTS = 0,
           endDateTS = 2451915527) {
    message("deprecated use XXX instead")
    stop("deprecated")
    
  }

#' Title
#'
#' @param arrayExpenses 
#'
#' @return
#' @export
#' @importFrom  jsonlite toJSON fromJSON
get_expenses <- function(arrayExpenses = get_arrayExpenses()){
arrayExpenses %>%
  toJSON() %>%
    fromJSON()
  }
