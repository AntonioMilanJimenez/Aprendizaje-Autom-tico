
set.seed(3)

#--------------------------------------------------------------------------------------------------------------------------
#ACTIVIDAD 1
#--------------------------------------------------------------------------------------------------------------------------

# por defecto genera 2 puntos entre [0,1] de 2 dimensiones 

simula_unif = function (N=2,dims=2, rango = c(0,1)){
  m = matrix(runif(N*dims, min=rango[1], max=rango[2]),
             nrow = N, ncol=dims, byrow=T)
  m
}

## -----------------------------------------------------------------------

# función simula_gaus(N, dim, sigma) que genera un
# conjunto de longitud N de vectores de dimensión dim, conteniendo números 
# aleatorios gaussianos de media 0 y varianzas dadas por el vector sigma.
# por defecto genera 2 puntos de 2 dimensiones 

simula_gaus = function(N=2,dim=2,sigma){
  
  if (missing(sigma)) stop("Debe dar un vector de varianzas")
  sigma = sqrt(sigma)  # para la generación se usa sd, y no la varianza
  if(dim != length(sigma)) stop ("El numero de varianzas es distinto de la dimensión")
  
  simula_gauss1 = function() rnorm(dim, sd = sigma) # genera 1 muestra, con las desviaciones especificadas
  m = t(replicate(N,simula_gauss1())) # repite N veces, simula_gauss1 y se hace la traspuesta
  m
}

## ------------------------------------------------------------------------
#  simula_recta(intervalo) una funcion que calcula los parámetros
#  de una recta aleatoria, y = ax + b, que corte al cuadrado [-50,50]x[-50,50]
#  (Para calcular la recta se simulan las coordenadas de 2 ptos dentro del 
#  cuadrado y se calcula la recta que pasa por ellos), 
#  se pinta o no segun el valor de parametro visible

simula_recta = function (intervalo = c(-1,1), visible=F){
  
  ptos = simula_unif(2,2,intervalo) # se generan 2 puntos
  a = (ptos[1,2] - ptos[2,2]) / (ptos[1,1]-ptos[2,1]) # calculo de la pendiente
  b = ptos[1,2]-a*ptos[1,1]  # calculo del punto de corte
  
  if (visible) {  # pinta la recta y los 2 puntos
    if (dev.cur()==1) # no esta abierto el dispositivo lo abre con plot
      plot(1, type="n", xlim=intervalo, ylim=intervalo)
    points(ptos,col=3)  #pinta en verde los puntos
    abline(b,a,col=3)   # y la recta
  }
  c(a,b) # devuelve el par pendiente y punto de corte
}

#Apartado 1a
datos_unif=simula_unif(50,2,c(-50,50))
plot(datos_unif,xlab="datos_unif X", ylab="datos_unif Y")

scan()

#Apartado 1b
datos_gaus=simula_gaus(50,2,c(5,7))
plot(datos_gaus,xlab="datos_gaus X", ylab="datos_gaus Y")

scan()

#Apartado 2a
#Hemos creado una función generarEtiquetasMatriz con la que a partir de unos datos y una recta, 
#asigna a cada punto una etiqueta en función del signo respecto a la recta mediante f(x,y) = y-ax-b

generarEtiqueta<-function(datos,a,b){
  
  sign(datos[2]-a*datos[1]-b)
  
}

generarEtiquetasMatriz<-function(matriz,a,b){
  apply(matriz,1,generarEtiqueta,a,b)  
}

recta=simula_recta(c(-50,50))
datos01=simula_unif(50,2,c(-50,50))
etiquetas=generarEtiquetasMatriz(datos01,recta[1],recta[2])
plot(datos01,col=etiquetas+10,xlab="datos_01 X", ylab="datos_01 Y")
abline(recta[2],recta[1])

scan()

#Apartado 2b

#Para crear ruido, hemos creado la función modificarEtiquetas que altera en un porcentaje las 
#etiquetas positivas a negativas y viceversa. Dado que separamos en un inicio las etiquetas positivas 
#y negativas en dos conjuntos, hace que la misma etiqueta que se ha hecho positiva no pueda hacerse 
#de nuevo negativa:

modificarEtiquetas<-function(etiquetas,porcentaje){
  etiqP = c()
  etiqN = c()
  n = length(etiquetas)
  for(i in 1:n){
    if(etiquetas[i]==1)
      etiqP = c(etiqP,i)
    else
      etiqN = c(etiqN,i)
  }
  
  etiqP = sample(etiqP)
  etiqN = sample(etiqN)
  
  nP = length(etiqP)*porcentaje/100
  nN = length(etiqN)*porcentaje/100
  
  for(i in 1:nP){
    t=etiqP[i]
    etiquetas[t] = -1
  } ##¿Mas eficiente que tapply donde tendriamos una nueva lista y tener que ir metiendola en etiquetas?
  
  for(i in 1:nN){
    t=etiqN[i]
    etiquetas[t] = 1
  }
  etiquetas
  
}

etiquetas_ruido=modificarEtiquetas(etiquetas,10)
plot(datos01,col=etiquetas_ruido+10,xlab="datos_01 X", ylab="datos_01 Y")
abline(recta[2],recta[1])

scan()

#Apartado 3

#Para este apartado hemos utilizado la función pintar_frontera modficado para que nos dibuje 
#también la nube de puntos etiquetada. También hemos creado las 4 funciones que se nos pedían:


f1<-function(x,y){
  ((x-10)^2 + (y-20)^2 -400)
}

f2<-function(x,y){
  (0.5*(x+10)^2 + (y-20)^2 -400)
}

f3<-function(x,y){
  (0.5*(x-10)^2 - (y+20)^2 -400)
}

f4<-function(x,y){
  (y-20*x^2-5*x+3)
}

pintar_frontera = function(f,datos,etiquetas_ruido,rango=c(-50,50)) {
  x=y=seq(rango[1],rango[2],length.out = 100)
  z = outer(x,y,FUN=f)
  if (dev.cur()==1) # no esta abierto el dispositivo lo abre con plot
    plot(1, type="n", xlim=rango, ylim=rango)
  contour(x,y,z, levels = 0, drawlabels = FALSE,xlim =rango, ylim=rango, xlab = "x", ylab = "y")
  points(datos[,1],datos[,2],col=etiquetas_ruido+5)
}

pintar_frontera(f1,datos01,etiquetas_ruido)

scan()

pintar_frontera(f2,datos01,etiquetas_ruido)

scan()

pintar_frontera(f3,datos01,etiquetas_ruido)

scan()

pintar_frontera(f4,datos01,etiquetas_ruido)

scan()

# Ejemplo de llamada a una funcion f1_xy que hemos de definir
# pintar_frontera(f1_xy) 



#--------------------------------------------------------------------------------------------------------------------------
#ACTIVIDAD 2
#--------------------------------------------------------------------------------------------------------------------------

#Apartado 1

#Esta es la función que hemos creado para ajusta_PLA. En ella, recorremos nuestra matriz de datos 
#hasta que el signo de uno de las datos no se corresponde con la etiqueta que debería tener, es decir, 
#está mal clasificado. Entonces se reajusta el vector w en función de este dato tal y como se hace en 
#el PLA y se empieza de nuevo a revisar los datos. Estas revisiones se realizan en un máximo de
#"max_iter". Si llegamos a este máximo sin haber encontrado la solución devolvemos -1 como número de 
#iteraciones. Si por el contrario, en una de las revisiones/iteraciones todos los puntos están bien 
#clasificados, termina la función devolviendo la w y las iteraciones necesarias para haberla encontrado:

ajusta_PLA <-function(datos,label,max_iter,vini){
  iteraciones = 0
  
  for(i in 1:max_iter){
    iteraciones=0
    for(j in 1:nrow(datos)){
      if(sign(datos[j,1]*vini[1]+datos[j,2]*vini[2]+datos[j,3]*vini[3]) != sign(label[j])){
        vini[1] = vini[1] + label[j]*datos[j,1]
        vini[2] = vini[2] + label[j]*datos[j,2]
        vini[3] = vini[3] + label[j]*datos[j,3]
        iteraciones=-1
        break()
      }  
    }
    if(iteraciones==0){
      iteraciones=i
      break()
    }
  }
  resultado = c(vini,iteraciones)
}

#Apartado 2a

#Hemos creado la función calcularAB donde a partir del vector w nos devuelve las componentes 
#"a" y "b" de la recta: 
calcularAB<-function(w1,w2,w3){
  a=-w1/w2
  b=-w3/w2
  resultado=c(a,b)
}


cadena=replicate(nrow(datos01),0)
datos01_pla=cbind(datos01,cadena)
w_ini=c(0,0,0)
pla=ajusta_PLA(datos01_pla,etiquetas,10000,w_ini)
w=c(pla[1],pla[2],pla[3])
iteraciones_0=pla[4]

recta_pla=calcularAB(w[1],w[2],w[3])
plot(datos01,col=etiquetas+5,xlab="datos_01 X", ylab="datos_01 Y")
abline(recta_pla[2],recta_pla[1])

scan()

#Apartado 2b

#Ahora vamos a repetir el apartado anterior pero como w inicial tomaremos 10 w iniciales con valores 
#aleatorios entre 0 y 1. Hemos creado la función **pla_random** que se encarga de llamar a ajusta_PLA
#10 veces con diferentes w_iniciales como acabamos de comentar. Esta función nos devolvera la media 
#de iteraciones necesarias para encontrar la solución del apartado anterior en los casos que ha tenido 
#éxito junto con las w_iniciales que ha utilizado para ello. Además, nos devuelve las veces que no ha 
#conseguido encontrar la solución y nos pintará una de ellas.

pla_random<-function(datos01_pla,etiquetas){
  contador_no_encontrado=0
  contador_encontrado=0
  iteraciones=0
  w_correcta=0
  w_no_encontrada=0
  w_iniciales=c()
  for(i in 1:10){
    w_ini=runif(3,0,1)
    pla=ajusta_PLA(datos01_pla,etiquetas,10000,w_ini)
    if(pla[4]==-1){
      contador_no_encontrado=contador_no_encontrado+1
      w_no_encontrada=c(pla[1],pla[2],pla[3])
    }else{
      iteraciones=iteraciones+pla[4]
      contador_encontrado=contador_encontrado+1
      w_correcta=c(pla[1],pla[2],pla[3])
      w_iniciales=c(w_iniciales,w_ini)
    }
  }
 
  iteraciones=iteraciones/contador_encontrado
  if(contador_no_encontrado>0){
    plot(datos01_pla,col=etiquetas+5,xlab="datos_pla X", ylab="datos_pla Y")
    recta_pla_r=calcularAB(w_no_encontrada[1],w_no_encontrada[2],w_no_encontrada[3])
    abline(recta_pla_r[2],recta_pla_r[1])
  }
  resultado=c(iteraciones,contador_no_encontrado,w_iniciales)
  resultado
}  


resultado=pla_random(datos01_pla,etiquetas)
media_iteraciones=resultado[1]
w_no_encontradas=resultado[2]
x=((10-w_no_encontradas)*3)+2
w_iniciales=matrix(resultado[3:x],ncol=3)

media_iteraciones
w_no_encontradas
w_iniciales

scan()

#Apartado 3a


plot(datos01,col=etiquetas_ruido+5,xlab="datos_01 X", ylab="datos_01 Y")
w_ini=c(0,0,0)
pla=ajusta_PLA(datos01_pla,etiquetas_ruido,10000,w_ini)
w=c(pla[1],pla[2],pla[3])
iteraciones_0=pla[4]

recta_pla=calcularAB(w[1],w[2],w[3])
abline(recta_pla[2],recta_pla[1])
iteraciones_0

scan()

#Apartado 3b
resultado=pla_random(datos01_pla,etiquetas_ruido)
media_iteraciones=resultado[1]
w_no_encontradas=resultado[2]
x=((10-w_no_encontradas)*3)+2
w_iniciales=matrix(resultado[3:x],ncol=3)
w_no_encontradas

scan()

#---------------------------------------------------------------------------------------------------------------------------------------
#ACTIVIDAD 3
#-------------------------------------------------------------------------------------------------------------------------------------

#Apartado 1

#Utilizamos el código proporcionado en Decsai para leer zip.train y zip.test, y obtener los array 
#"grises" y "grises_test" con los números 1 y 5. Hemos creado dos funciones obtenerImagenes y 
#obtenerImagenes_test que se encargan de transformar los arrays grises y grises_test en 
#listas de las imagenes de 1 y 5 rotadas.

## lectura de los digitos de train

digit.train <- read.table("datos/zip.train",quote="\"", comment.char="", stringsAsFactors=FALSE)
digitos15.train = digit.train[digit.train$V1==1 | digit.train$V1==5,]
digitos_train = digitos15.train[,1]  # etiquetas
ndigitos_train = nrow(digitos15.train)
grises = array(unlist(subset(digitos15.train,select=-V1)),c(ndigitos_train,16,16))

# ------------------------------------------------------------------------

## lectura de los digitos de test

digit.test <- read.table("datos/zip.test",quote="\"", comment.char="", stringsAsFactors=FALSE)
digitos15.test = digit.test[digit.test$V1==1 | digit.test$V1==5,]
digitos_test = digitos15.test[,1]  # etiquetas
ndigitos_test = nrow(digitos15.test)
grises_test = array(unlist(subset(digitos15.test,select=-V1)),c(ndigitos_test,16,16))

obtenerImagenes<-function(){
  imagenes = list()
  for(i in 1:ndigitos_train){
    imagenes[[i]] = grises[i,,16:1]
  }
  imagenes
}

obtenerImagenes_test<-function(){
  imagenes = list()
  for(i in 1:ndigitos_test){
    imagenes[[i]] = grises_test[i,,16:1]
  }
  imagenes
}

imagenes_train=obtenerImagenes()
imagenes_test=obtenerImagenes_test()

#Apartado 2

#A continuación vamos a extraer la intensidad promedio y la simetría de cada una de estas imagenes que 
#hemos generado. Para ello utilizamos la función fsimetria que nos da la simetría de una imagen, calcularMedia 
#que nos da la intensidad promedio de la imagen sumando todos sus valores y haciendo la media, obtenerMatrizSimetriaMedia 
#que recorre las imagenes que hemos creado y le calcula a cada una su simetría y media, devolviendo una matriz con 
#todos estos valores, y finalmente una función cambiar_digitos_etiquetas que transforma el vector "digitos" de 1 y 5 
#en etiquetas de 1 y -1 (cambiando 5 por -1) para poder tratarlo como etiquetas positivas y negativas.

fsimetria <- function(A){
  A = abs(A-A[,ncol(A):1])
  -sum(A)
}

calcularMedia<-function(Matriz){
  media=sum(Matriz)/256
}

calculaMediaySimetria<-function(A){
  n1=calcularMedia(A)
  n2=fsimetria(A)
  resultado = c(n1,n2)
}

obtenerMatrizSimetriaMedia<-function(imagenes){
  matriz = c()
  n=length(imagenes)
  for(i in 1:n){
    r = calculaMediaySimetria(imagenes[[i]])
    matriz = rbind(matriz,r)
  }
  matriz
}

cambiar_digitos_etiquetas<-function(digs,tam){
  for(i in 1:tam){
    if(digs[i]==5)
      digs[i]=-1
  }
  digs
}

m_train=obtenerMatrizSimetriaMedia(imagenes_train)
m_test=obtenerMatrizSimetriaMedia(imagenes_test)

etiquetas_train=cambiar_digitos_etiquetas(digitos_train,599)
plot(m_train,col=etiquetas_train+5,xlab="Intensidad Promedio",ylab="Simetria")

scan()


#Apartado 3

#La transformación SVD para ajustar la regresión lineal la hemos implementado mediante las funciones Regress_Lin
#y pseudo_inversa. Regress_Lin nos devuelve la w para construir nuestra recta a partir de los datos de intensidad 
#y promedio calculados, y las etiquetas asignadas. Para ello multiplica la pseudo_inversa de la matriz de datos 
#(calculada con pseudo_inversa) por las etiquetas:

#Además, hemos creado la función calcularE que nos calcula el error en función del porcentaje de puntos mal 
#clasificados de nuestro modelo respecto de las etiquetas:

pseudo_inversa<-function(X){
  aux=t(X)%*%X + diag(ncol(X))*lambda
  X_svd=svd(aux)
  pseudo=X_svd$u%*%diag(1/X_svd$d)%*%t(X_svd$v)
  pseudo_X=pseudo%*%t(X)
  pseudo_X

}

calcularE<-function(w,datos,etiquetas,tam){
  cadena=replicate(tam,1)
  datos=cbind(datos,cadena)
  media=0
  for(i in 1:tam){
    if(sign(datos[i,1]*w[1]+datos[i,2]*w[2]+datos[i,3]*w[3])!=sign(etiquetas[i]))
      media=media+1
  }
  media=media*100/tam
  media
}

Regress_Lin<-function(datos,label){
  cadena=replicate(nrow(datos),1)
  datos=cbind(datos,cadena)
  pm=pseudo_inversa(datos)
  cad1=pm[1,]*label
  cad2=pm[2,]*label
  cad3=pm[3,]*label
  w1=sum(cad1)
  w2=sum(cad2)
  w3=sum(cad3)
  w=c(w1,w2,w3)
}


w=Regress_Lin(m_train,etiquetas_train)
recta_train=calcularAB(w[1],w[2],w[3])
abline(recta_train[2],recta_train[1])

scan()

etiquetas_test=cambiar_digitos_etiquetas(digitos_test,49)
plot(m_test,col=etiquetas_test+5,xlab="Intensidad Promedio",ylab="Simetria")
abline(recta_train[2],recta_train[1])

scan()

error_train=calcularE(w,m_train,etiquetas_train,599)
error_test=calcularE(w,m_test,etiquetas_test,49)
error_train
error_test

scan()

#Apartado 4

#Experimento 1
# A
datos_exp1=simula_unif(1000,2,c(-1,1))
plot(datos_exp1,col=5,xlab="datos_exp1 X",ylab="datos_exp1 Y")

scan()

#B

generarEtiquetaExp1<-function(datos){

    sign((datos[1]+0.2)^2 + datos[2]^2 - 0.6)
}

etiquetas_exp1=apply(datos_exp1,1,generarEtiquetaExp1)
etiquetas_exp1=modificarEtiquetas(etiquetas_exp1,10)
plot(datos_exp1,col=etiquetas_exp1+5,xlab="datos_exp1 X",ylab="datos_exp1 Y")

scan()

#C

Regress_Lin_exp1<-function(datos,label){
  pm=pseudo_inversa(datos)
  cad1=pm[1,]*label
  cad2=pm[2,]*label
  cad3=pm[3,]*label
  w1=sum(cad1)
  w2=sum(cad2)
  w3=sum(cad3)
  w=c(w1,w2,w3)
}

calcularE_exp1<-function(w,datos,etiquetas,tam){
  media=0
  for(i in 1:tam){
    if(sign(datos[i,1]*w[1]+datos[i,2]*w[2]+datos[i,3]*w[3])!=sign(etiquetas[i]))
      media=media+1
  }
  media=media*100/tam
  media
}

cadena=replicate(1000,1)
datos_exp1=cbind(cadena,datos_exp1)

w=Regress_Lin_exp1(datos_exp1,etiquetas_exp1)
recta_exp1=calcularAB(w[1],w[2],w[3])
abline(recta_exp1[2],recta_exp1[1])

error_train_exp1=calcularE_exp1(w,datos_exp1,etiquetas_exp1,1000)
error_train_exp1

scan()

#D

errorIn= c()
errorOut= c()

for(i in 1:1000){
  
  datos_exp1=simula_unif(1000,2,c(-1,1))
  etiquetas_exp1=apply(datos_exp1,1,generarEtiquetaExp1)
  etiquetas_exp1=modificarEtiquetas(etiquetas_exp1,10)
  datos_exp1_test=simula_unif(1000,2,c(-1,1))
  etiquetas_exp1_test=apply(datos_exp1_test,1,generarEtiquetaExp1)
  etiquetas_exp1_test=modificarEtiquetas(etiquetas_exp1_test,10)


  cadena=replicate(1000,1)
  datos_exp1=cbind(cadena,datos_exp1)
  datos_exp1_test=cbind(cadena,datos_exp1_test)
  
  w=Regress_Lin_exp1(datos_exp1,etiquetas_exp1)
  
  error_train_exp1=calcularE_exp1(w,datos_exp1,etiquetas_exp1,1000)
  error_test_exp1=calcularE_exp1(w,datos_exp1_test,etiquetas_exp1_test,1000)
  errorIn=errorIn + error_train_exp1
  errorOut=errorOut + error_test_exp1
   
}

errorIn=errorIn/1000
errorOut=errorOut/1000
errorIn
errorOut

#E

errorIn
errorOut

scan()


#Experimento 2

#Para cambiar el vector de caracteristicas hemos creado la función **modificarVector** que modifica 
#la matriz de x,y a 1,x,y,xy,x^2 ,y^2:

# A
modificarVector<-function(datos){
  cadena=replicate(1000,1)
  datos=cbind(cadena,datos)
  cadena=datos[,2]*datos[,3]
  datos=cbind(datos,cadena)
  cadena=datos[,2]*datos[,2]
  datos=cbind(datos,cadena)
  cadena=datos[,3]*datos[,3]
  datos=cbind(datos,cadena)
  datos
}

Regress_Lin_exp2<-function(datos,label){
  pm=pseudo_inversa(datos)
  cad1=pm[1,]*label
  cad2=pm[2,]*label
  cad3=pm[3,]*label
  cad4=pm[4,]*label
  cad5=pm[5,]*label
  cad6=pm[6,]*label
  w1=sum(cad1)
  w2=sum(cad2)
  w3=sum(cad3)
  w4=sum(cad4)
  w5=sum(cad5)
  w6=sum(cad6)
  
  w=c(w1,w2,w3,w4,w5,w6)
}

calcularE_exp2<-function(w,datos,etiquetas,tam){
  media=0
  for(i in 1:tam){
    if(sign(datos[i,1]*w[1]+datos[i,2]*w[2]+datos[i,3]*w[3]+datos[i,4]*w[4]+datos[i,5]*w[5]+datos[i,6]*w[6])!=sign(etiquetas[i]))
      media=media+1
  }
  media=media*100/tam
  media
}

datos_exp2=simula_unif(1000,2,c(-1,1))
etiquetas_exp2=apply(datos_exp2,1,generarEtiquetaExp1)
etiquetas_exp2=modificarEtiquetas(etiquetas_exp2,10)
plot(datos_exp2,col=etiquetas_exp2+5,xlab="datos_exp2 X",ylab="datos_exp2 Y")

datos_exp2=modificarVector(datos_exp2)

w=Regress_Lin_exp2(datos_exp2,etiquetas_exp2)

error_train_exp2=calcularE_exp2(w,datos_exp2,etiquetas_exp2,1000)
error_train_exp2

scan()

#B

errorIn= c()
errorOut= c()

for(i in 1:1000){
  
  datos_exp2=simula_unif(1000,2,c(-1,1))
  etiquetas_exp2=apply(datos_exp2,1,generarEtiquetaExp1)
  etiquetas_exp2=modificarEtiquetas(etiquetas_exp2,10)
  datos_exp2_test=simula_unif(1000,2,c(-1,1))
  etiquetas_exp2_test=apply(datos_exp2_test,1,generarEtiquetaExp1)
  etiquetas_exp2_test=modificarEtiquetas(etiquetas_exp2_test,10)
  
  
  
  datos_exp2=modificarVector(datos_exp2)
  datos_exp2_test=modificarVector(datos_exp2_test)
  
  w=Regress_Lin_exp2(datos_exp2,etiquetas_exp2)
  
  error_train_exp2=calcularE_exp2(w,datos_exp2,etiquetas_exp2,1000)
  error_test_exp2=calcularE_exp2(w,datos_exp2_test,etiquetas_exp2_test,1000)
  errorIn=errorIn + error_train_exp2
  errorOut=errorOut + error_test_exp2
  
}

errorIn=errorIn/1000
errorOut=errorOut/1000
errorIn
errorOut

#C

errorIn
errorOut

scan()


## Bonus

#Apartado A

media_error=0
for(i in 1:1000){
  datos_bonus = simula_unif(100,2, c(-10,10))
  recta_bonus = simula_recta(c(-10,10))
  etiquetas_bonus = generarEtiquetasMatriz(datos_bonus,recta_bonus[1],recta_bonus[2])
  w=Regress_Lin(datos_bonus,etiquetas_bonus)
  Ebonus = calcularE(w,datos_bonus,etiquetas_bonus,100)
  media_error=media_error+Ebonus
}
media_error=media_error/1000
media_error

scan()

#Apartado B

media_errorOut=0
for(i in 1:1000){
  datos_bonus = simula_unif(1000,2, c(-10,10))
  recta_bonus = simula_recta(c(-10,10))
  etiquetas_bonus = generarEtiquetasMatriz(datos_bonus,recta_bonus[1],recta_bonus[2])
  w=Regress_Lin(datos_bonus,etiquetas_bonus)
  Ebonus = calcularE(w,datos_bonus,etiquetas_bonus,1000)
  media_errorOut=media_errorOut+Ebonus
}
media_errorOut=media_errorOut/1000
media_errorOut

scan()


#Apartado C
#De forma similar a los apartados anteriores con 10 puntos obtenemos el vector de pesos con regresión lineal 
#y lo utilizamos como vector inicial en ajusta_PLA.  Se realiza 1000 veces y calculamos la media del número 
#de iteraciones necesarias para que converja el PLA. Se ha hecho una modificación sobre ajusta_PLA para que 
#en cada iteración empiece desde un punto aleatorio.

ajusta_PLA_mod <-function(datos,label,vini){
  iteraciones = 0
  n=nrow(datos)
  cadena=replicate(n,1)
  datos=cbind(datos,cadena)
  i=0
  while(i!=-1){
    iteraciones=0
    r=sample(n,1)
    j=r
    while(j!=r-1){
      if(sign(datos[j,1]*vini[1]+datos[j,2]*vini[2]+datos[j,3]*vini[3]) != sign(label[j])){
        vini[1] = vini[1] + label[j]*datos[j,1]
        vini[2] = vini[2] + label[j]*datos[j,2]
        vini[3] = vini[3] + label[j]*datos[j,3]
        iteraciones=-1
        break()
      }
      j=(j %% (n+1))+1
      if(j==n+1 && r==1){
        j=0
      }else if(j==n+1){
        j=1
      }  
    }
    if(iteraciones==0){
      iteraciones=i
      break()
    }
    i=i+1
  }
  resultado = c(vini,iteraciones)
}

iteraciones=0
for(i in 1:1000){
  datos_bonus = simula_unif(10,2, c(-10,10))
  recta_bonus = simula_recta(c(-10,10))
  etiquetas_bonus = generarEtiquetasMatriz(datos_bonus,recta_bonus[1],recta_bonus[2])
  w=Regress_Lin(datos_bonus,etiquetas_bonus)
  iter=ajusta_PLA_mod(datos_bonus,etiquetas_bonus,w)
  iteraciones=iteraciones+iter[4]
}

iteraciones=iteraciones/1000
iteraciones




