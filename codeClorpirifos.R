#######################################################################################
########################################################################################
############  Análisis de dosis-respuesta de Ae. aegypti a Clorpirifos ##################
###### feb 2015 Unidad de Bioensayos, Acapulco, Guerrero  ##############################
########### Datos de la localidad de Puerto Vallarta  ##################################
########################################################################################
install.packages("drc", dependencies= TRUE); install.packages("car", dependencies= TRUE)
install.packages("gtools", dependencies= TRUE); install.packages("MASS", dependencies= TRUE)
install.packages("magic", dependencies= TRUE); install.packages("abind", dependencies= TRUE)
install.packages("plotrix", dependencies= TRUE); install.packages("stats", dependencies= TRUE)
install.packages("repmis")
library(repmis)
FinURL <- paste0("https://www.dropbox.com/s/yadz10gcpj4nvln/clorpirifos.csv?dl=0")
clor <- source_data(FinURL, sep = ",", header = TRUE)
library(drc)
head(clor)
modelclor <- drm(death/exp~dose, loc, weights=exp, data=clor, fct=LL.2(), type="binomial")
summary(modelclor)
plot(modelclor, bp= .5, conName="control", broken=TRUE, legend=TRUE, col=c("red", "blue"))
ED(modelclor, c(50, 90, 99), interval="delta")
ED(modelclor, 50, interval="delta")
plot(modelclor, bp= .5, legend=FALSE, col=c("red", "blue"))
library(lattice)
xyplot(death/exp~log(dose+0.1)|loc, modelclor)
xyplot(death/exp~dose|loc, modelclor)
plot(modelclor, bp= .5, legend=TRUE, legendPos= c(5, 1), col=c("red", "blue"), xtsty= "standard", xlim= c(1,85))
SI(modelclor, c(50,50), reverse= TRUE)
SI(modelclor, c(90,90))