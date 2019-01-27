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
    POST(
      "https://axonaut.com/api/post/expense/list",
      body = list(
        "accountApiKey" = accountApiKey,
        "startDateTS" = startDateTS,
        "endDateTS " = endDateTS
      )
    ) %>% content() %>%
      pluck("arrayExpenses")
    
  }

#' Title
#'
#' @param arrayExpenses 
#'
#' @return
#' @export
#' @import jsonlit
get_expenses <- function(arrayExpenses = get_arrayExpenses()){
arrayExpenses %>%
  toJSON() %>%
    fromJSON()
  }
