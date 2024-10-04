### R code from vignette source 'Empezando-con-Rcmdr-es.Rnw'

###################################################
### code chunk number 1: Empezando-con-Rcmdr-es.Rnw:25-29
###################################################
Sys.setenv(LANGUAGE='es')
Sys.setLanguage("es")
file.copy(from = 'Rcmdr-menus', to = 'Rcmdr-menus-es', overwrite = TRUE)
Sweave('Rcmdr-menus-es')


