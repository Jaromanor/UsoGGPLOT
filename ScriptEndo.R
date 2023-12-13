
head(bdd1)
str(bdd1)

bdd2 <- bdd1[,-1]
summary(bdd2)
dim(bdd2)

bdd3 <- lapply(bdd2[c(1:7)], as.factor)
bdd3 <- data.frame(bdd3)
head(bdd3)
dim(bdd3)

bdd4 <- bdd2[c(8:50)]
final <- data.frame(bdd3,bdd4)
head(final)
summary(final)


    ##ANALIZAR LA CANTIDAD DE ENCUESTADOS POR ESCUELA##

library(ggplot2)
TemaTitulo = element_text(family = "Comic Sans MS",
                          face = "bold", #si se pone "bold" queda en negrilla
                          size = rel(2), #tamaño letra relativo a las demás
                          colour = "blue",
                          vjust = 0, #separar del gráfico
                          hjust = 0.5, #creo que centrar
                          angle = 0, #inclinación de la letra
                          lineheight=1, #separación entre líneas
                          debug = FALSE #como un fondo rectangular
                          )

#también se pueden manipular los títulos de los ejes
ejesTitulo = element_text(family = "Comic Sans MS",
                          face = "bold", #si se pone "bold" queda en negrilla
                          size = rel(1), #tamaño letra relativo a las demás
                          colour = "red",
                          vjust = 0, #separar del gráfico
                          hjust = 0.5, #creo que centrar
                          angle = 0, #inclinación de la letra
                          lineheight=1, #separación entre líneas
                          debug = FALSE #como un fondo rectangular
)

grafico1 <- ggplot(final,aes(x=COLEGIO,fill=COLEGIO))+
         geom_bar(width = 0.5)+
  xlab("Colegio") + ylab("Número")+
  ggtitle("Encuestados \n Por \n Colegios")+
  theme (text = element_text(size=8))+ #tamaño letra por defecto
  theme(plot.title = TemaTitulo)+
  theme(axis.title = ejesTitulo)

grafico1

#Agregar texto al interior del gráfico

grafico1+
  geom_text(data = NULL,
            x=1.5, y=10,
            label="Barras")

#Añadir cantidad a cada barra

  #En este caso es mejor crear un dataframe basado en una tabla
  # de frecuencia
#Nota: no vamos a detenernos en configurar ejes, tamaño letras, etc.
#Simplemente agregar cantidad a las barras

esc <- table(final$COLEGIO)
esc
esc1 <- data.frame(esc)
names(esc1)=c("COLEGIO","CANTIDAD")

grafico2 <- ggplot(esc1,aes(x=COLEGIO,y=CANTIDAD,fill=COLEGIO))+
  geom_bar(stat = "identity", width = 0.5) #stat para que asuma la barra el valor e "y"
grafico2
grafico2+
  geom_text(aes(y = CANTIDAD, ymax = CANTIDAD, label = CANTIDAD))

grafico2+
  geom_text(aes(y = CANTIDAD, ymax = CANTIDAD, label = CANTIDAD),
            position = position_dodge(width = 1), # crea un espacio a ambos lados del gráfico
            size=3, vjust=-1, hjust=0.5 ,col="black")
#La posición de la cantidad se debe arreglar
#se debe ajustar un valor ya que se está saliendo del gráfico
# se ajusta "vjust" más cerca (más positivo)
grafico2+
  geom_text(aes(y = CANTIDAD, ymax = CANTIDAD, label = CANTIDAD),
            position = position_dodge(width = 1), # crea un espacio a ambos lados del gráfico
            size=3, vjust=-0.5, hjust=0.5 ,col="black")


#Añadir porcentaje
  #Es mejor un datafram, anteriormente tenemos "esc1",
  #se le debe agregar a ese dataframe los porcentajes

prop.table(esc)
Porcentaje <- paste(round(prop.table(esc)*100, 2),"%",sep="")

esc2 <- data.frame(esc1,Porcentaje)
esc2


grafico3 <- ggplot(esc2,aes(x=COLEGIO,y=CANTIDAD,fill=COLEGIO))+
  geom_bar(stat = "identity", width = 0.5)+
  xlab("Col")+ylab("Cant")+
  ggtitle("Colegios")+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_text(aes(label=Porcentaje),vjust=-0.5,col="red",size=2)



        ##ANALIZAR POR SEXO##

summary(final)
#ojo tenemos un NA, se puede borrar todo el registro o ignorar el NA
#para el análisis univariado conviene ignorar el NA

#vamos a realizar un gráfico con cantidad y por porcentaje en barra
#Es necesario un nuevo dataframe con variables: SEXO, CANTIDAD, PORC

frec1 <- table(final$SEXO)
Porcsex <- paste(round(prop.table(frec1)*100,2),"%",sep = "")
sex <- data.frame(frec1,Porcsex)
names(sex)=c("SEXO","CANTIDAD","PORCSEX")
sex

graf4 <- ggplot(sex,aes(x=SEXO,y=CANTIDAD,fill=SEXO))+
  geom_bar(stat = "identity",width = 0.5)+
  xlab("Género")+ylab("Cantidad")+
  ggtitle("SEXO \n De Los \n Encuestados")+
  theme(plot.title=element_text(hjust = 0.5))

  #agregamos cantidad a las barras

graf4 + geom_text(aes(label=CANTIDAD),vjust=-0.5,size=3)

  #agregamos porcentaje a las barras

graf4 + geom_text(aes(label=PORCSEX),vjust=-0.5,size=3,col="blue")



    ##ANALIZAR SEXO DE LOS ENCUESTADOS AGRUPADOS POR COLEGIO##
#se crea un dataframe con las dos variables más la frecuencia
#relativa y su cantidad
dd <- sapply(split(final$SEXO,final$COLEGIO), table)
xx <- sapply(dd, as.vector) #mejor xy<-as.vector(dd)
xy<- as.vector(dd)
ee <- apply(dd, 2, prop.table)
yy <- sapply(ee, as.vector)
yy1 <- paste(round(yy*100,2),"%",sep="")
f11 <- rep(levels(final$COLEGIO),each=2)
f22 <- rep(levels(final$SEXO),2)
colsex2 <- data.frame(f11,f22,xx,yy1)
names(colsex2) <- c("COLEGIO","SEXO","CANTIDAD","PORCENTAJE")
colsex2

##gráfico base para barras apiladas
graf5 <- ggplot(colsex2,aes(x=COLEGIO,y=CANTIDAD,fill=SEXO))+
  geom_bar(stat="identity",width=0.5)


##agregamos cantidad a las barras (apiladas)
graf5+geom_text(aes(label=CANTIDAD),
                position = position_stack(), #importante este argumento
                vjust=-0.5,size=3)

##agregamos porcentaje a las barras (apiladas)
graf5+geom_text(aes(label=colsex2$PORCENTAJE),
                position = position_stack(), #importante este argumento
                vjust=-0.5,size=3)


##gráfico de bloques por colegio (cada bloque es de un colegio con
#dos barras una para hombre y otra para mujer)

graf6 <- ggplot(colsex2,aes(x=COLEGIO,y=CANTIDAD,fill=SEXO))+
  geom_bar(stat="identity", position=position_dodge())

##agregamos cantidad a las barras
graf6+geom_text(aes(label=CANTIDAD),
                position = position_dodge(width = 0.9),
                vjust=-0.5,hjust=0.5,size=3)

##agregamos porcentaje a las barras
graf6+geom_text(aes(label=colsex2$PORCENTAJE),
                position = position_dodge(width = 0.9),
                vjust=-0.5,hjust=0.5,size=3)


##Gráficos por panel

graf7 <- ggplot(colsex2,aes(x=SEXO,y=CANTIDAD,fill=SEXO))+
  geom_bar(stat="identity",width = 0.5)+
  facet_wrap(~COLEGIO)

##agregamos cantidad a las barras
graf7+geom_text(aes(label=CANTIDAD),
                position = position_dodge(width = 0.9),
                vjust=-0.5,hjust=0.5,size=3)
##agregamos porcentaje a las barras
graf7+geom_text(aes(label=colsex2$PORCENTAJE),
                position = position_dodge(width = 0.9),
                vjust=-0.5,hjust=0.5,size=3)



                 ##ANÁLISIS DE LA ESCALA LIKERT##

#aislamos de la bdd solamente las variables con escala likert
#la idea es mirar los cuantiles para tener un punto de partida de
#análisis general. Nota: nos interesa solamente un cuestionario
#(solamente las preguntas que empiezan por MI.PREG)

head(final)
dim(final)
likert <- final[c(8:29)]
head(likert)
summary(likert)

#Mirar el alfa, la base es policórica, entonces se calcula el alfa
#policórico
library(psych)
genpoli <- polychoric(likert)
psych::alpha(genpoli$rho)
describe(likert)




                  #####Mirar la percepción general

likert1 <- as.matrix(likert)
likert2 <- sapply(likert1, as.vector)
summary(likert2)

#Los datos mustran que el primer cuantil es 4 por lo tanto la percepción 
#de los encuestados es positiva frente al endomarketing

frecuencia <- table(likert2)
proporcion <- paste(round(prop.table(frecuencia)*100,2),"%",sep = "")
clasificación <- as.factor(c(0:6))

general <- data.frame(clasificación,frecuencia,proporcion)
general1 <- general[-2]

graf8 <- ggplot(general1,aes(x=clasificación,y=Freq,fill=clasificación))+
  geom_bar(stat="identity",width=0.5)

graf8+xlab("Escala Likert")+ylab("Frecuencia")+
  ggtitle("Percepción General \n del \n Endomarketing")+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_text(aes(label=proporcion),size=3,vjust=-0.5)

#De acuerdo con el gráfico #8 se evidencia bastante sensación positiva del
#endomarketing en los encuestados

#Ahora por medio de histogramas quiero analizar los resultados de cada
#pregunta y analizar puntualmente una pregunta con comportamiento particular

par(mfrow=c(4,6))
sapply(likert, hist)

#las preguntas 8,14 y 14 tienen un comportamiento distinto.
#analicemos el gráfico de cada uno

atipicas <- likert[,c(8,10,14)]
names(atipicas) <- c("P8","P10","P14")

frecuencia1 <- table(atipicas$P8)
porcentaje1 <- paste(round(prop.table(frecuencia1)*100,2),"%",sep = "")
clasificación1 <- as.factor(c(1:6))
at8 <- data.frame(clasificación1,frecuencia1,porcentaje1)
at8 <- at8[-2]

pre8 <- ggplot(at8,aes(x=clasificación1,y=Freq,fill=clasificación1))+
  geom_bar(stat = "identity")

#para no repetir comnados creamos una función para las siguientes
#variables, por ejemplo si fueran las 22 variables tendría que escribir
# y escribir el código, si hago una función pues es más breve

tablas <- function(bdd,c1){
  frec <- table(c1)
  porc <- paste(round(prop.table(frec)*100,2),"%",sep = "")
  clas <- as.factor(c(1:6)) #tenemos que mirar a veces ponerle desde cero
  at <- data.frame(clas,frec,porc)
  at[-2]
}

at10 <- tablas(atipicas,atipicas$P10)
at14 <- tablas(atipicas,atipicas$P14)

pre10 <- ggplot(at10,aes(x=clasificación1,y=Freq,fill=clasificación1))+
  geom_bar(stat = "identity")
pre14 <- ggplot(at14,aes(x=clasificación1,y=Freq,fill=clasificación1))+
  geom_bar(stat = "identity")
library(gridExtra)
grid.arrange(pre8,pre10,pre14,ncol=2)

#efectivamente la pregunta 8 referida a que si la organización
#da oportunidades de pagos extras presenta una especie de 
#distribución uniforme, eso quiere que por distintas razones en 
#algunos colegios no se motivan las horas extras

#respecto a la pregunta 10 sobre si se puede cambiar de función
#es evidente que no es permitido en la gran mayoría de los casos

#respecto a la pregunta 14 en primera intancia está mal formulada la
#pregunta, ya que es de carácter negativo, de todas maneras existen
#instituciones que efectivamente muestran sus resultados


#Sería interesante segmentar las preguntas 8, 10 y 14 por colegio
#vamos a analizar la pregunta 8 con base en el colegio

head(final)
extcol <- final[,c(1,15)]
names(extcol)[2]<-"P8"
extcol$COLEGIO <- as.factor(extcol$COLEGIO)
summary(extcol)

jj<-tapply(extcol$P8, extcol$COLEGIO, table)
jj$AL
freP8 <- as.vector(jj$AL)
freP8 <- c(freP8,0,0)
kk <- paste(round(prop.table(jj$AL)*100,2),"%",sep = "")
kk <- c(kk,"0%","0%")
clas <- as.factor(c(1:6))

extcolP8 <- data.frame(clas,freP8,kk)
graf9 <- ggplot(extcolP8,aes(x=clas,y=freP8,fill=clas))+
  geom_bar(stat = "identity")


jj$GD
freP81 <- as.vector(jj$GD)
kk1 <- paste(round(prop.table(jj$GD)*100,2),"%",sep = "")
clas1 <- as.factor(c(1:6))

extcolP81 <- data.frame(clas1,freP81,kk1)
graf10 <- ggplot(extcolP81,aes(x=clas1,y=freP81,fill=clas1))+
  geom_bar(stat = "identity")


jj$JA
freP82 <- as.vector(jj$JA)
kk2 <- paste(round(prop.table(jj$JA)*100,2),"%",sep = "")
clas2 <- as.factor(c(1,3:6))

extcolP82 <- data.frame(clas2,freP82,kk2)
graf11 <- ggplot(extcolP82,aes(x=clas2,y=freP82,fill=clas2))+
  geom_bar(stat = "identity")



jj$SP
freP83 <- as.vector(jj$SP)
kk3 <- paste(round(prop.table(jj$SP)*100,2),"%",sep = "")
clas3 <- as.factor(c(1:4,6))

extcolP83 <- data.frame(clas3,freP83,kk3)
graf12 <- ggplot(extcolP83,aes(x=clas3,y=freP83,fill=clas3))+
  geom_bar(stat = "identity")

grid.arrange(graf9,graf10,graf11,graf12,ncol=2)

prueba5 <- grid.arrange(graf9,graf10,graf11,graf12,ncol=2)



    ##Analizar estado civil por colegios

#lo primero es crear un data.frame con las variables: colegio, estado civil,
#cantidad y porcentaje

agruparcol <- sapply(split(final$ESTADO.CIVIL,final$COLEGIO), table)
agruparcol1 <- as.vector(agruparcol)
agrupacolporc <- apply(agruparcol, 2, prop.table)
agrupacolporc1 <- as.vector(agrupacolporc)
agrupacolporc2 <- paste(round(agrupacolporc1,2)*100,"%",sep = "")
colegio1 <- rep(levels(final$COLEGIO),each=3)
estciv <- rep(levels(final$ESTADO.CIVIL),4)
colestciv <- data.frame(colegio1,estciv,agruparcol1,agrupacolporc2)

names(colestciv) <- c("COLEGIO","ESTADO_CIVIL","CANTIDAD","PORCENTAJE")

#visualizar los datos por panel

graf13 <- ggplot(colestciv,aes(x=ESTADO_CIVIL,y=CANTIDAD,fill=ESTADO_CIVIL))+
  geom_bar(stat = "identity",width = 0.5)+facet_grid(~COLEGIO)


#vamos a analizar la pregunta 8 de acuerdo con el estado civil
head(final)
extESTCIV <- final[,c(4,15)]
names(extESTCIV) <- c("ESTCIV","P8")
summary(extESTCIV)
extESTCIV1 <- na.omit(extESTCIV)
summary(extESTCIV1)

ww <- tapply(extESTCIV1$P8, extESTCIV1$ESTCIV, table)

#analicemos casados
ww1 <- as.vector(ww$CASADO)
vv <- paste(round(prop.table(ww$CASADO)*100,2),"%",sep = "")
clas <- as.factor(c(1:6))

casadoP8 <- data.frame(clas,ww1,vv)
graf14 <- ggplot(casadoP8,aes(x=clas,y=ww1,fill=clas))+
  geom_bar(stat = "identity")









