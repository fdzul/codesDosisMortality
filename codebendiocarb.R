########################################################################################
########################################################################################
############  Análisis de dosis-respuesta de Ae. aegypti a Bendiocarb ##################
###### feb 2015 Unidad de Bioensayos, Acapulco, Guerrero  ##############################
########### Datos de la localidad de Puerto Vallarta  ##################################
########################################################################################
install.packages("drc", dependencies= TRUE); install.packages("car", dependencies= TRUE)
install.packages("gtools", dependencies= TRUE); install.packages("MASS", dependencies= TRUE)
install.packages("magic", dependencies= TRUE); install.packages("abind", dependencies= TRUE)
install.packages("plotrix", dependencies= TRUE); install.packages("stats", dependencies= TRUE)
install.packages("repmis", dependencies = TRUE)
library(repmis)
FinURL <- paste0("https://www.dropbox.com/s/kmhexsyttjj4eo7/bendiocarb.csv?dl=0")
bendiocarb <- source_data(FinURL, sep = ",", header = TRUE)
library(drc)
head(bendiocarb)
modelbendio <- drm(death/exp~dose, loc, weights=exp, data=bendiocarb, fct=LL.2(), type="binomial")
summary(modelbendio)
plot(modelbendio, bp= .5, conName="control", broken=TRUE, legend=TRUE, col=c("red", "blue"))
ED(modelbendio, c(50, 90, 99), interval="delta")
ED(modelbendio, 50, interval="delta")
plot(modelbendio, bp= .5, legend=FALSE, col=c("red", "blue"))
library(lattice)
xyplot(death / exp ~ log(dose+0.1) | loc, modelbendio)
xyplot(death / exp ~ dose | loc, modelbendio)
plot(modelbendio, bp= .5, legend=TRUE, legendPos= c(2, 1), col=c("red", "blue"), xtsty= "standard", xlim= c(0,15))
SI(modelbendio, c(50,50), reverse= TRUE)
SI(modelbendio, c(90,90))