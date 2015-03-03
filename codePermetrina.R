#######################################################################################
########################################################################################
############  Análisis de dosis-respuesta de Ae. aegypti a Permetrina ##################
###### feb 2015 Unidad de Bioensayos, Acapulco, Guerrero  ##############################
########### Datos de la localidad de Puerto Vallarta  ##################################
########################################################################################
install.packages("drc", dependencies= TRUE); install.packages("car", dependencies= TRUE)
install.packages("gtools", dependencies= TRUE); install.packages("MASS", dependencies= TRUE)
install.packages("magic", dependencies= TRUE); install.packages("abind", dependencies= TRUE)
install.packages("plotrix", dependencies= TRUE); install.packages("stats", dependencies= TRUE)
install.packages("repmis")
library(repmis)
FinURL <- paste0("https://www.dropbox.com/s/6ggleb5u4ccyb05/permetrina.csv?dl=0")
perm <- source_data(FinURL, sep = ",", header = TRUE)
library(drc)
head(perm)
modelper <- drm(death/exp~dose, loc, weights=exp, data=permetrina, fct=LL.2(), type="binomial")
summary(modelper)
plot(modelper, bp= .5, conName="control", broken=TRUE, legend=TRUE, col=c("red", "blue"))
ED(modelper, 50, interval="delta")
ED(modelper, 90, interval="delta")
ED(modelper, 99, interval="delta")
ED(modelper, c(50, 90, 99), interval="delta")
plot(modelper, bp= .5, legend=FALSE, col=c("red", "blue"))
library(lattice)
xyplot(death/exp~log(dose+0.1)|loc, data= perm)
xyplot(death/exp~dose|loc, data= perm)
plot(modelper, bp= .5, legend=TRUE, legendPos= c(2, 1), col=c("red", "blue"), xtsty= "standard", xlim= c(0,15))
SI(modelper, c(50,50), reverse=TRUE)
