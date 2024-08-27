#' @export
durasjon_df <- function(r=0.0153,r_m=0.0175,n=2,T=10,M=1000){
	dur <- data.frame(per=seq(1,T*n-1)) %>%
		dplyr::mutate(kont=(M*r_m)/n) %>%
		dplyr::mutate(disk=round((1/(1+r/n))^per,digits=6)) %>%
		dplyr::mutate(NV=round(kont*disk,digits=6)) %>%
		dplyr::mutate(wNV=round(per*NV,digits=6))
}

#' @export
sd.p=function(x){sd(x)*sqrt((length(x)-1)/length(x))}

#' @export
kob <- function(A0=620,I=680,rf=0.01,theta=1.25,n=0.85){
  le1 <- 1/(1+rf)
  q <- (1+rf-n)/(theta-n)
  k_t <- max(0,theta*A0-I)
  k_n <- max(0,n*A0-I)
  qm1 <- 1-q
  kob <- le1*(q*k_t+(1-q)*k_n)
}

#' @export
abc <- function(){
	print("say hello!")
}


