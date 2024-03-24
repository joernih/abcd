usedvw <- gretlReadWrite::read.gdt("gdt/usedvw.gdt")
newcars <- gretlReadWrite::read.gdt("gdt/newcars.gdt")
#icecream <- gretlReadWrite::read.gdt("ex1_linear/icecream.gdt")
HousePrice <- gretlReadWrite::read.gdt("ex1_linear/HousePrice.gdt")
icecream <- read.csv("ex1_linear/icecream.gdt", row.names = NULL)
adverticing <- gretlReadWrite::read.gdt("ex2_nonlinear/advertising.gdt")
tssales <- gretlReadWrite::read.gdt("ex3_timeseries/TSsales1.gdt")
usethis::use_data(usedvw,overwrite=TRUE)
usethis::use_data(HousePrice,overwrite=TRUE)
usethis::use_data(newcars,overwrite=TRUE)
usethis::use_data(icecream,overwrite=TRUE)
usethis::use_data(adverticing,overwrite=TRUE)
usethis::use_data(tssales,overwrite=TRUE)
###############################################################################################################
library(Hyperinflation)
library(ggplot2)
library(purrr)
library(fredr)
library(DataEditR)
library(gretlR)
usedvw <- gretlReadWrite::read.gdt("gdt/usedvw.gdt")
newcars <- gretlReadWrite::read.gdt("gdt/newcars.gdt")
usethis::use_data(usedvw, overwrite = TRUE)
usethis::use_data(newcars, overwrite = TRUE)
################################################################################################################
## Book ##
################################################################################################################
csvabc <- c("abc_out.data", "biler.gdt", "elbiler.csv", "luftkvalitet.csv", "pizza.csv", "toyota.csv", "vareAB.gdt", "abc_out.
js.metadata", "boliger.csv", "elbiler.gdt", "luftkvalitet.gdt", "pizza.gdt", "toyota.gdt", "biler.csv", "boliger.gdt
", "feriereiser.gdt", "luftkvalitet.gdt~", "tidsserier.gdt", "vareAB.csv")

abc <- read.csv(paste0("csv/",csvabc[4]))
usethis::use_data(abc, overwrite = TRUE)
# usethis::use_data(bjki_ts, overwrite = TRUE)
open_gdt(fpath, mkstruct = TRUE, info = TRUE)
################################################################################################################
## Fred ##
################################################################################################################
# Testing
api.key <- 'd62b9d8d4ce53e56ea04049dc463ac51'  # substitute ... with your API key
fredr_set_key(api.key)
startdate <- '2000-01-01'
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
#=c(USA=c('MABMM301USM189S'),JPN=c('MABMM301JPM189S'),EUZ=c('MABMM301EZM189S'),GBR=c('MABMM301GBM189S'),CAN=c('MABMM301CAM189S'),NOR=c('MABMM301NOM189S'),DEN='MABMM301DKM189N',SWE='MABMM301SEM189N',SKE='MABMM301KRM189S'),
#MABMM301USM189S,MABMM301EZM189S,MABMM301NOM189S
################################################################################################################
## Norges Bank ##
nbjson <- function(fre="B",dint=c("2016-10-17","2024-01-01"),vexr=c("USD","EUR","SEK")){
  frekh <- list("A"="year","M"="month","B"="day")[[fre]]
  url <- paste0(
		"https://data.norges-bank.no/api/data/EXR/"
		,fre,
		".",
                paste(vexr, collapse = '+'),
		".NOK.SP?format=sdmx-json&startPeriod="
		,dint[1],
		"&endPeriod=",
		dint[2],
		"&locale=no"
  )
  gjd <- httr::GET(url)
  con <- httr::content(gjd, as = "text")
  jda <- jsonlite::fromJSON(con)
  lex <- sapply(c(1:length(vexr)),function(x){
        t(as.numeric(as.matrix(jda$data$dataSets$series[[x]]$observations)))
  })
  nlexl <- as.data.frame(lex) %>% data.table::setnames(new=vexr) %>%
	  dplyr::mutate(dato=seq.Date(as.Date(dint[1]),by=frekh,length.out=dim(lex)[1])) %>%
          dplyr::select(dato, everything()) %>%
          tidyr::pivot_longer(
            cols = -dato,
            names_to = "rate",
            values_to = "value") %>%
	  dplyr::arrange(rate,dato) %>% dplyr::group_by(rate) %>%
          dplyr::mutate(raten = value/value[1]) %>%
          dplyr::mutate(percn = raten/raten[1]) %>%
          dplyr::ungroup()

  nlexw <- tidyr::pivot_wider(nlexl,names_from=rate,values_from=c(value,raten,percn))
  return(list(nlexw,nlexl))
}
nb_ts <- list(kurs=nbjson())
usethis::use_data(nb_ts, overwrite = TRUE)

