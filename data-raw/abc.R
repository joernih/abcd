library(homepageJIH)
library(dplyr)
data1 <- iris
usethis::use_data(data1,overwrite=T)
#ℹ Did you accidentally source a file rather than using `load_all()`?
#  Run `rm(list = c("abc"))` to remove the conflicts. 

df_eks_2_1 <- data.frame(tilstand=c(1,2,3),
                         prob=c(0.2,0.5,0.3),
                         avk_a=c(0.16,0.12,0.06),
                         avk_b=c(0.05,0.20,0.40)
                         )
usethis::use_data(df_eks_2_1,overwrite=T)


# Øvelse I
df <- df_eks_2_1
w <- seq(0,1,0.01)
v <- as.vector(df[,(2)])
m <- as.matrix(df[,(3:4)])
covall <- cov.wt(m,v,method='ML')
avk <- as.vector(covall$center)
kov <- covall$cov[lower.tri(covall$cov)] 
var <- as.vector(diag(as.matrix(covall$cov)))
##
plotwf <- data.frame(kov=kov,w1=1-w, w2=w) %>%
	dplyr::mutate(forvavk=w1*avk[1]+w2*avk[2]) %>%
	dplyr::mutate(varians=w1^2*var[1]+w2^2*var[2]+2*w1*w2*kov) %>%
	dplyr::mutate(stdavk=sqrt(varians))
gg <- ggplot2::ggplot(data=plotwf,ggplot2::aes(x=stdavk,y=forvavk)) + ggplot2::geom_point() 


df <- df_eks_2_1
w <- seq(0,1,0.01)
v <- as.vector(df[,(2)])
m <- as.matrix(df[,(3:4)])
covall <- cov.wt(m,v,method='ML')
avk <- as.vector(covall$center)
kov <- covall$cov[lower.tri(covall$cov)] 
var <- as.vector(diag(as.matrix(covall$cov)))

plotwf <- data.frame(kov=kov,w1=1-w, w2=w) %>%
	dplyr::mutate(forvavk=w1*avk[1]+w2*avk[2]) %>%
	dplyr::mutate(varians=w1^2*var[1]+w2^2*var[2]+2*w1*w2*kov) %>%
	dplyr::mutate(stdavk=sqrt(varians))
gg <- ggplot2::ggplot(data=plotwf,ggplot2::aes(x=stdavk,y=forvavk)) + ggplot2::geom_point() 


