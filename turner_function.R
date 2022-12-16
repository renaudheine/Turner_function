
#### load packages ####
if(!require("pacman")) install.packages("pacman")
library("pacman")
p_load("wbstats", "WDI", "tidyverse", "here")

#### Purchasing Power Parities (PPP) ####
# usd_ppp <- wb_data(indicator = "PA.NUS.PPP")
# write_csv(usd_ppp, here("data/ppp/usd_ppp"))

if(!file.exists(here("data/ppp/usd_ppp.csv"))){
  usd_ppp <- wb_data(indicator = "PA.NUS.PPP")
} else {
  usd_ppp <- read.csv(here("data/ppp/usd_ppp.csv"),
                      stringsAsFactors = FALSE) %>%
    as_tibble()
}

#### CPI ####
# wb_cpi <- wb_data(indicator = "FP.CPI.TOTL")
# write_csv(wb_cpi, here("data/cpi/wb_cpi.csv"))

if(!file.exists(here("data/cpi/wb_cpi.csv"))){
  wb_cpi <- wb_data(indicator = "FP.CPI.TOTL")
} else {
  wb_cpi <- read.csv(here("data/cpi/wb_cpi.csv"),
                     stringsAsFactors = FALSE) %>%
    as_tibble()
}

#### usd exchange ####
# usd_exchange <- WDI(country = "all", indicator =  "PA.NUS.FCRF")
# write_csv(usd_exchange, here("data/ppp/usd_exchange"))

if(!file.exists(here("data/ppp/usd_exchange.csv"))){
  usd_exchange <-  WDI(country = "all", indicator =  "PA.NUS.FCRF")
} else {
  usd_exchange <- read.csv(here("data/ppp/usd_exchange.csv"),
                           stringsAsFactors = FALSE) %>%
    as_tibble()
}

EMU_ex <- usd_exchange %>% filter(iso3c %in% c("EMU") & year >= 1999) %>% select(year, PA.NUS.FCRF) %>% rename(exchange_EMU = PA.NUS.FCRF)
EMU_countries <- c("AUT", "BEL", "CYP", "EST", "FIN", "FRA", "DEU", "GRC", "IRL", "ITA", "LVA", "LTU", "LUX", "MLT", "NLD", "PRT", "SVK", "SVN", "ESP")

usd_exchange <- left_join(usd_exchange, EMU_ex, by = c("year")) %>% 
  mutate(PA.NUS.FCRF = if_else(iso3c %in% EMU_countries & year >= 1999 & is.na(PA.NUS.FCRF), exchange_EMU, PA.NUS.FCRF)) %>% 
  select(-c(exchange_EMU)) %>% 
  rename(date = year)


#### function Turner et al. 2019 ####
fun_turner <- function(country_1_iso3, country_2_iso3, yr_start, yr_end, value_tradable, value_non_tradable, PPP, method){
  x <- usd_ppp %>%
    filter(date %in% c(yr_start:yr_end) & iso3c == country_1_iso3 | date %in% c(yr_start:yr_end) & iso3c == country_2_iso3) %>% 
    select(iso3c, date, PA.NUS.PPP) %>% rename(iso3c_ppp = iso3c)
  y <- wb_cpi %>%
    filter(date %in% c(yr_start:yr_end) & iso3c == country_1_iso3 | date %in% c(yr_start:yr_end) & iso3c == country_2_iso3) %>% 
    select(iso3c, date, FP.CPI.TOTL) %>% rename(iso3c_cpi = iso3c)
  z <- usd_exchange %>%
    filter(date %in% c(yr_start:yr_end) & iso3c == country_1_iso3 | date %in% c(yr_start:yr_end) & iso3c == country_2_iso3) %>% 
    select(iso3c, date, PA.NUS.FCRF) %>% rename(iso3c_ex = iso3c)
  
  v1 <- c("ppp_tradable", "ppp_non_tradable", "ppp_mix")
  v2 <- c("ex_tradable", "ex_non_tradable", "ex_mix")
  
  ppp_tradable <- ((value_tradable / x$PA.NUS.PPP[x$iso3c_ppp == country_1_iso3 & x$date == yr_start]) * x$PA.NUS.PPP[x$iso3c_ppp == country_2_iso3 & x$date == yr_start])*
    (y$FP.CPI.TOTL[y$date == yr_end & y$iso3c_cpi == country_2_iso3]/y$FP.CPI.TOTL[y$date == yr_start & y$iso3c_cpi == country_2_iso3])
  
  ppp_non_tradable <- ((value_non_tradable * (y$FP.CPI.TOTL[y$date == yr_end & y$iso3c_cpi == country_1_iso3] / y$FP.CPI.TOTL[y$date == yr_start & y$iso3c_cpi == country_1_iso3])) /
                         x$PA.NUS.PPP[x$iso3c_ppp == country_1_iso3 & x$date == yr_end]) * x$PA.NUS.PPP[x$iso3c_ppp == country_2_iso3 & x$date == yr_end]
  
  ex_tradable <- ((value_tradable / z$PA.NUS.FCRF[x$iso3c_ppp == country_1_iso3 & x$date == yr_start]) * z$PA.NUS.FCRF[x$iso3c_ppp == country_2_iso3 & x$date == yr_start])*
    (y$FP.CPI.TOTL[y$date == yr_end & y$iso3c_cpi == country_2_iso3]/y$FP.CPI.TOTL[y$date == yr_start & y$iso3c_cpi == country_2_iso3])
  
  ex_non_tradable <- ((value_non_tradable * (y$FP.CPI.TOTL[y$date == yr_end & y$iso3c_cpi == country_1_iso3] / y$FP.CPI.TOTL[y$date == yr_start & y$iso3c_cpi == country_1_iso3])) /
                        z$PA.NUS.FCRF[x$iso3c_ppp == country_1_iso3 & x$date == yr_end]) * z$PA.NUS.FCRF[x$iso3c_ppp == country_2_iso3 & x$date == yr_end]
  
  data <- cbind(ppp_tradable, ppp_non_tradable, ex_tradable, ex_non_tradable) %>% as_tibble() %>%
    mutate(ppp_mix = sum(ppp_tradable, ppp_non_tradable),
           ex_mix = sum(ex_tradable, ex_non_tradable))
  if(PPP == 1){
    result <- data %>% select(contains("ppp"))
    result[[1, v1[method]]]
  } else {
    result <- data %>% select(contains("ex"))
    result[[1, v2[method]]]
  }
}

#fun_turner("GBR", "NLD", 2019, 2020, 100, 50, 0, 3)
#fun_turner("USA", "NLD", 2018, 2014, 0, 4.4612e+09, 1, 2)






