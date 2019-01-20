
<!-- README.md is generated from README.Rmd. Please edit that file -->
axonaut
=======

The goal of axonaut is to connect to [axonaut](http://axonaut.com/) API

Installation
------------

You can install the released version of axonaut with:

``` r
#install.packages("remotes")
remotes::install_github("VincentGuyader/axonaut")
```

Example
-------

This is a basic example which shows you how to use the Package:

``` r
library(axonaut)
library(dplyr)
options("accountApiKey"="your_api_key")
```

``` r
get_arrayInvoices() %>% 
  get_facture_compact() 
```

``` r
get_arrayInvoices() %>% 
  get_facture_detail() 
```

``` r
get_arrayInvoices() %>% 
  get_facture_compact() %>% 
  summarise(sum(HT))
```
