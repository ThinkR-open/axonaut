#' Title
#'
#' @param accountApiKey token api
#' @param startDateTS start as epoch
#' @param endDateTS end as epoch
#'
#' @export
#' @import httr
#' @import purrr
get_arrayInvoices <-
  function(accountApiKey = getOption("accountApiKey"),
           startDateTS = 0,
           endDateTS = 2451915527) {
    POST(
      "https://axonaut.com/api/post/invoice/list",
      body = list(
        "accountApiKey" = accountApiKey,
        "startDateTS" = startDateTS,
        "endDateTS " = endDateTS
      )
    ) %>% content() %>%
      pluck("arrayInvoices")

  }

#' Title
#'
#' @param arrayInvoices arrayInvoices
#'
#' @export
#'
#' @import purrr
#' @import dplyr
#' @importFrom lubridate ymd
get_facture_compact <- function(arrayInvoices=get_arrayInvoices()){

  arrayInvoices %>%
  map( unlist ) %>%
    map(t) %>%
    map(as_tibble) %>%
    bind_rows()%>%
    # outstandingAmount
  # number	invoiceDate	paidDate	client	HT	FRAIS
    select(number,
           invoiceDate,
           paidDate,
           client = company.name,
           HT = preTaxAmount,
           `reste_a_recevoir (TTC)` = outstandingAmount) %>%
    mutate_at(vars(HT,`reste_a_recevoir (TTC)`),as.numeric) %>% 
    mutate_at(vars(invoiceDate ,paidDate),lubridate::ymd) %>% 
    as.tbl()

}

#' Title
#'
#' @param arrayInvoices arrayInvoices
#'
#' @export
#' @import purrr
get_facture_detail <- function(arrayInvoices=get_arrayInvoices()){
  arrayInvoices %>%
    map(sur_plusieurs_ligne) %>%
    map(mutate_if,is.factor,as.character) %>% 
    map(~mutate(.,margin_p=as.numeric(margin_p))) %>% 
    # map(~.x %>% rename_all(str_replace_all,pattern =  "\\.",replacement = "_dd")) %>%
    bind_rows() %>% 
    as.tbl() %>% mutate_at(vars(invoiceDate, sentDate, paidDate),lubridate::ymd) %>% 
  mutate_at(vars(contains("Amount"),unitPrice_p , quantity_p , taxRate_p),thinkr::as_mon_numeric)  



}

#' Title
#'
#' @param invoice an invoice
#'
#' @export
#'
#' @import purrr
#' @import jsonlite
#' @import dplyr
#' @importFrom stringr str_c
extract_product_from_an_invoice <- function(invoice){
  invoice %>%
  toJSON() %>%
    fromJSON() %>%
    pluck("arrayProducts") %>%
    select(-productInternalId,-starts_with("line"))  %>%
    # mutate(number=invoice$number) %>%
    # select(number,everything()) %>%
    mutate_all(unlist) %>%
    rename_all(str_c,"_p")

}


#' Title
#'
#' @param invoice an invoice
#'
#' @export
#' @import purrr
#' @import jsonlite
#' @import dplyr
sur_plusieurs_ligne <- function(invoice){
  products <- invoice %>%
    extract_product_from_an_invoice()

  invoice %>%
    toJSON() %>%
    fromJSON() %>%
    purrr::discard((names(.) == "arrayProducts")) %>%
    unlist() %>%
    t() %>%
    as.data.frame() %>%
    list(.) %>%
    rep(nrow(products)) %>%
    bind_rows() %>%
    cbind(products)
}
