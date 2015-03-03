########################################################################################
########################################################################################
############  Análisis de dosis-respuesta de Ae. aegypti a Lambdacialotrina ############
###### feb 2015 Unidad de Bioensayos, Acapulco, Guerrero  ##############################
########### Datos de la localidad de Puerto Vallarta  ##################################
########################################################################################
install.packages("drc", dependencies= TRUE); install.packages("car", dependencies= TRUE)
install.packages("gtools", dependencies= TRUE); install.packages("MASS", dependencies= TRUE)
install.packages("magic", dependencies= TRUE); install.packages("abind", dependencies= TRUE)
install.packages("plotrix", dependencies= TRUE); install.packages("stats", dependencies= TRUE)
install.packages("repmis")
library(drc)
library(repmis)
FinURL <- paste0("https://www.dropbox.com/s/ic1ajjpvqddih5h/Lambda.csv?dl=0")
lambda <- source_data(FinURL, sep = ",", header = TRUE)
head(lambda)
modelambda <- drm(death/exp~dose, loc, weights=exp, data=lambda, fct=LL.2(), type="binomial")
summary(modelambda)
plot(modelambda, bp= .5, conName="control", broken=TRUE, legend=TRUE, col=c("red", "blue"))
ED(modelambda, c(50, 90, 99), interval="delta")
ED(modelambda, 50, interval="delta")
plot(modelambda, bp= .5, legend=FALSE, col=c("red", "blue"))
library(lattice)
xyplot(death/exp~log(dose+0.1)|loc, modelambda)
xyplot(death/exp~dose|loc, modelambda)
plot(modelambda, bp= .5, legend=TRUE, legendPos= c(2, 1), col=c("red", "blue"), xtsty= "standard", xlim= c(0,15))
SI(modelambda, c(50,50), reverse= TRUE)
SI(modelambda, c(90,90))