################################################################################################################
library(Hyperinflation)
library(ggplot2)
library(purrr)
library(fredr)
library(DataEditR)
################################################################################################################
# Covid
################################################################################################################
################################################################################################################
# Data
################################################################################################################
# Testing
api.key <- 'd62b9d8d4ce53e56ea04049dc463ac51'  # substitute ... with your API key
fredr_set_key(api.key)
startdate <- '2000-01-01'
################################################################################################################
tsquery <- list(
mb3=c(USA=c('MABMM301USM189S'),JPN=c('MABMM301JPM189S'),EUZ=c('MABMM301EZM189S'),GBR=c('MABMM301GBM189S'),CAN=c('MABMM301CAM189S'),NOR=c('MABMM301NOM189S'),DEN='MABMM301DKM189N',SWE='MABMM301SEM189N',SKE='MABMM301KRM189S'),
cpi=c(USA=c('CPIAUCSL'),JPN=c('JPNCPIALLMINMEI'),EUZ=c('CP0000EZ19M086NEST'),GBR=c('CPALCY01CAM661N'),CAN=c('CPALCY01CAM661N'),NOR=c('NORCPIALLMINMEI'),SWE=c('SWECPIALLMINMEI'),SKE=c('KORCPIALLMINMEI')),
une=c(USA=c('UNRATE'),JPN=c('LRHUTTTTJPM156S'),EUZ=c('LRHUTTTTEZM156S'),GBR=c('AURUKM'),CAN=c('LRUNTTTTCAM156S'),NOR=c('LMUNRRTTNOM156S'),SWE=c('LRHUTTTTSEM156S'),SKE=c('LRUNTTTTKRM156S')),
mba=c(USA=c('BOGMBASE')),
int=c(USA='WTB3MS',NOR='IR3TIB01NOM156N',JPN='IR3TIB01JPM156N',EUR='IR3TIB01EZM156N',USA='IR3TIB01SEM156N'),
gdp=c(USA='GDPC1',NOR='CLVMNACSCAB1GQNO',JPN='JPNRGDPEXP',EUZ='NAEXKP01EZQ652S')
)

olv <- seq(1,length(tsquery))
outl <- lapply(olv, function(x){
  ilv <- seq(1,length(tsquery[[x]]))
  lapply(ilv, function(y){
  sna <- names(tsquery)[x]
  cna <- names(tsquery[[x]][y])
  idp <- tsquery[[x]][[y]]
  # Make own function
  print(idp)
  bai <- fredr::fredr_series(idp)[c('title','notes')] 
  dfo <- fredr::fredr_series_observations(idp) %>%
	  base::cbind(bai) %>%
    dplyr::mutate(country=cna,id=sna) %>%
    dplyr::filter(date>startdate) %>%
    dplyr::mutate(lvalue=dplyr::lag(value,n=12)) %>% 
    dplyr::mutate(growth=round(value/lvalue-1, 6)*100) 
  })
})
all_mb2 <- do.call(rbind, unlist(outl, recursive = FALSE)) 
fred_ts <- list(tsid=tsquery,tsdf=all_mb2)
usethis::use_data(fred_ts, overwrite = TRUE)
devtools::document()
Hyperinflation::bm()

#=c(USA=c('MABMM301USM189S'),JPN=c('MABMM301JPM189S'),EUZ=c('MABMM301EZM189S'),GBR=c('MABMM301GBM189S'),CAN=c('MABMM301CAM189S'),NOR=c('MABMM301NOM189S'),DEN='MABMM301DKM189N',SWE='MABMM301SEM189N',SKE='MABMM301KRM189S'),
#MABMM301USM189S,MABMM301EZM189S,MABMM301NOM189S

