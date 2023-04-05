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
message("deprecated use get_all_invoices instead")
    stop("deprecated")
  }

# deprecated












#' Title
#'
#' @param arrayInvoices arrayInvoices
#'
#' @export
#'
#' @import purrr
#' @import dplyr
#' @importFrom lubridate ymd
get_facture_compact <- function(invoice=axonaut:::get_all_invoices()){


  
  invoice %>% 
    map_dfr(info_from_invoice) %>% 
    select(number,
           invoiceDate =date,
           paidDate=paid_date,
           client = client,
           HT = pre_tax_amount,
           `reste_a_recevoir (TTC)` = outstanding_amount) %>%
    mutate_at(vars(HT,`reste_a_recevoir (TTC)`),as.numeric) %>% 
    mutate_at(vars(invoiceDate ,paidDate),lubridate::ymd) %>% 
    as.tbl()

}

info_from_invoice <- function(inv){
  
  inv$client <- inv$company$name
  base <- inv[c("number","date","paid_date","client" ,"pre_tax_amount","outstanding_amount")]
  base[sapply(base,is.null)]<-NA
  data.frame(base)
}



#' Title
#'
#' @param arrayInvoices arrayInvoices
#'
#' @export
#' @import purrr
get_facture_detail <- function(invoice=axonaut:::get_all_invoices()){
  
  invoice %>% 
    map_dfr(get_product_info_from_inv) %>% 
    
    as.tbl() %>% 
    
    mutate_at(vars(invoiceDate, sentDate, paidDate),lubridate::ymd) %>% 
  mutate_at(vars(contains("Amount"),unitPrice_p , quantity_p , taxRate_p),thinkr::as_mon_numeric)  



}


get_product_info_from_inv <- function(inv){
  
  
  inv$company_id <-inv$company$id
  inv$company_name <- inv$company$name
  inv$company_is_supplier <- inv$company$is_supplier
  inv$company_is_prospect <- inv$company$is_prospect
  inv$company_is_customer <- inv$company$is_customer
  inv$discounts_amount <-  inv$discounts$amount
  inv$discounts_amount_with_tax <-  inv$discounts$amount_with_tax
  inv$discounts_comments <-  inv$discounts$comments
  
  socle <- inv[c("id","number","date",
                 "sent_date","paid_date","pre_tax_amount","tax_amount","total","outstanding_amount",
                 "company_id","company_name","company_is_supplier","company_is_prospect","company_is_customer","discounts_amount","discounts_amount_with_tax","discounts_comments",
                 "project_id","public_path")] 
  
  socle[sapply(socle,is.null)]<-NA
  
  inv$invoice_lines %>%
    purrr::map_depth(2, ~ifelse(is.null(.x), NA, .x) ) %>% 
    bind_rows()%>% 
    mutate(tax_rates     = tax_rates %>% map_dbl("rate")) %>%
    select(
      product_code ,
      "productCodeAndName_p" = product_name,
      "unitPrice_p" = price,
      "quantity_p" =   quantity,
      "taxRate_p"=   tax_rates,
      "title_p"=  name,
      "details_p" = details,
      "taxAmount_p"= total_tax_amount,
      "pretaxAmount_p" = total_pre_tax_amount,
      "totalAmount_p" =  total_amount
    ) %>% bind_cols(data.frame(socle),.) %>%
    mutate(discounts_comments = as.character(discounts_comments)) %>%
    rename(
      
      invoiceDate = date, 
      sentDate = sent_date, 
      paidDate= paid_date
      
      
    )
  
  
}


#' Title
#'
#' @param invoice an invoice
#'
#' @export
#'
#' @import purrr
#' @importFrom  jsonlite toJSON fromJSON
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
#' @importFrom  jsonlite toJSON fromJSON
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
