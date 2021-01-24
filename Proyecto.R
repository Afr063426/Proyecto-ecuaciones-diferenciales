#Referencia
#Martín, B. (2019)

#Autores
#Joshua Cervantes Artavia
#Moisés Monges Cordonero
#Daniel Sabater Guzmán

#Paquetes a usar
library(tidyverse)
library(lubridate)
library(deSolve)
install.packages("plotly")
#---------------------------------
#Datos a emplear
datosp<-read.csv("/home/joshua/UCR/MA0455/Proyecto/datos.csv",sep=";")
datosp<-datosp%>%select(FECHA,positivos,nue_posi,fallecidos,RECUPERADOS)
datosp<-datosp%>%mutate("Acumulados"=positivos-fallecidos-RECUPERADOS)%>%mutate("FECHA"=as.Date(FECHA))
#Codigo para modelo
SIR<- function(time,state,parameters){
  par<- as.list(c(state, parameters))
  with(par, {
    dS<- -beta*I*S/N
    dI<- beta*I*S/N-gamma*I
    dR<-gamma*I
    list(c(dS,dI,dR))
  })
}
#subset(datosp, FECHA >= datosp$FECHA[1] & FECHA <= datosp$FECHA[323])
Infected <- datosp$Acumulados[1:(length(datosp$Acumulados)-1)]
Day<-1:(length(Infected))
#Datos iniciales
N<- 5000000
init<-c(
  S=N-Infected[1],
  I=Infected[1],
  R=0
)

#Minimizamos la suma de residuo al cuadrado,
#indicamos los valores iniciales de beta y gamma
RSS<-function(parameters){
  names(parameters)<-c("beta","gamma")
  out<-ode(y=init,times=Day,func=SIR,parms=parameters)
  fit<-out[,3]
  sum((Infected-fit)^2)
}
Opt<-optim(c(0.5,0.5),
           RSS,
           method="L-BFGS-B",
           lower=c(0,0),
           upper=c(1,1)
)
Opt$message
Opt_par<-setNames(Opt$par,c("beta","gamma"))
Opt_par

#Valores predecidos por el modelo
sir_start_date<-"2020-03-06"
t<-1:as.integer(ymd("2021-01-24")-ymd("2020-03-06"))
fitted_cumulative_incidence<-data.frame(
  ode(
    y=init,time=t,
    func = SIR,parms=Opt_par
  )
)
#Generamos el gráfico
fitted_cumulative_incidence<-fitted_cumulative_incidence%>%
  mutate(
    Date=ymd(sir_start_date)+days(t-1),
    Country="Costa Rica",
    cumulative_incident_cases=datosp$Acumulados
    
  )

fitted_cumulative_incidence%>%ggplot(aes(x=Date))+geom_line(aes(y=I),color="red")+geom_point(aes(y=cumulative_incident_cases),color="blue")+labs(
  y="Incidencia acumulada",
  subtitle="rojo=prediccion modelo SIR, azul=valores reales"
)+theme_minimal()
