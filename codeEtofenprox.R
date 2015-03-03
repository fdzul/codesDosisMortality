#######################################################################################
########################################################################################
############  Análisis de dosis-respuesta de Ae. aegypti a Etofenprox ##################
###### feb 2015 Unidad de Bioensayos, Acapulco, Guerrero  ##############################
########### Datos de la localidad de Puerto Vallarta  ##################################
########################################################################################
install.packages("drc", dependencies= TRUE); install.packages("car", dependencies= TRUE)
install.packages("gtools", dependencies= TRUE); install.packages("MASS", dependencies= TRUE)
install.packages("magic", dependencies= TRUE); install.packages("abind", dependencies= TRUE)
install.packages("plotrix", dependencies= TRUE); install.packages("stats", dependencies= TRUE)
install.packages("repmis")
library(repmis)
FinURL <- paste0("https://www.dropbox.com/s/pldelfcb4kxan9y/eto.csv?dl=0")
eto <- source_data(FinURL, sep = ",", header = TRUE)
library(drc)
head(eto)
modeleto <- drm(death/exp~dose, loc, weights=exp, data=eto, fct=LL.2(), type="binomial")
summary(modeleto)
plot(modeleto, bp= .5, conName="control", broken=TRUE, legend=TRUE, col=c("red", "blue"))
ED(modeleto, c(50, 90, 99), interval="delta")
ED(modeleto, 50, interval= "delta")
plot(modeleto, bp= .5, legend=FALSE, col=c("red", "blue"))
library(lattice)
xyplot(death/exp~log(dose+0.1)|loc, modeleto)
xyplot(death/exp~dose|loc, modeleto)
plot(modeleto, bp= .5, legend=TRUE, legendPos= c(2, 1), col=c("red", "blue"), xtsty= "standard", xlim= c(0,15))
SI(modeleto, c(50,50), reverse= TRUE)
SI(modeleto, c(90,90), reverse= TRUE)
