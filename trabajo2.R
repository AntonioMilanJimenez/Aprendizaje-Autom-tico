
set.seed(10)

#-------------------------------------------------------------------------------------------------------------------------
#EJERCICIO 1
#--------------------------------------------------------------------------------------------------------------------------

# Ejercicio 1a

#En este apartado tenemos que utilizar el gradiente descendiente para encontrar el minimo de la función del ejercicio 1.a.
#Para ello hemos derivado esta función respecto a u: (4*exp(-2*u))*((u^2) * exp(u+v) -2*(v^2))*(u*exp(u+v)+(v^2))   y respecto a v:
#(2*exp(-2*u))*((u^2) * exp(u+v) -4*v)*((u^2)*exp(u+v)-2*(v^2)). Estas dos derivadas estan implementadas en las funciones "derivada_u" y "derivada_v" respectivamente.
#Así, el gradiente recibe una w, realiza las derivadas con esta w y devuelve los dos resultados.
#Ya en el algoritmo del gradiente descendiente vamos calculando el gradiente, que lo utilizamos junto con una tasa para actualizar nuestro vector w.
#En el momento en el que para nuestra w el error es menor a 10^-4, termina el algoritmo.


derivada_u = function(u,v){
  
  result = (4*exp(-2*u))*((u^2) * exp(u+v) -2*(v^2))*(u*exp(u+v)+(v^2))
  result
  
}

derivada_v = function(u,v){
  
  result = (2*exp(-2*u))*((u^2) * exp(u+v) -4*v)*((u^2)*exp(u+v)-2*(v^2))
  result
  
}

error=function(w){
  e=(((w[1]^2)*exp(w[2]) -2*(w[2]^2)*exp(-w[1]))^2)
  e
}


gradiente = function(w){
  a= derivada_u(w[1],w[2])
  b= derivada_v(w[1],w[2])
  cad = c(a,b)
  cad
}

gradiente_descendiente = function(maximo,tasa){
  
  grad=c()
  mi_vector = c()
  i=0
  iteracion=-1
  w=c(1,1)  
  while(i<maximo){
    grad = gradiente(w)
    mi_vector = -grad
    w= w+ tasa*mi_vector
    if(error(w) < 10^(-4)){
      iteracion=i
      i=maximo
    }
    i=i+1
  }
  c(iteracion,w)
}

resultado = gradiente_descendiente(9999999,0.1)
#iteraciones necesarias
iteraciones=resultado[1]
iteraciones

scan()

#w obtenida
w=resultado[2:3]
w

scan()



# Ejercicio 1b

#Para este apartado se nos proponía utilizar el gradiente descendente con una nueva función: (x-2)(x-2) + 2(y-2)(y-2) + 2sin(2pix)sin(2piy), cambiando la tasa
#de aprendizaje y también el punto de inicio.

derivada_x = function(x,y){
  r=2*(2*pi*cos(2*pi*x)*sin(2*pi*y)+x-2)
  r
}

derivada_y = function(x,y){
  r=4*(pi*sin(2*pi*x)*cos(2*pi*y)+y-2)
  r
}

mi_funcion = function(x,y){
  r=(x-2)*(x-2) + 2*(y-2)*(y-2) + 2*sin(2*pi*x)*sin(2*pi*y)
  r
}

gradiente2= function(w){
  a= derivada_x(w[1],w[2])
  b= derivada_y(w[1],w[2])
  cad = c(a,b)
  cad
}

gradiente_descendienteb1 = function(maximo,tasa,w){
  
  
  matriz=matrix(ncol=2)
  grad=c()
  i=0
  w_iteracion=c()  
  while(i<maximo){
    grad = gradiente2(w)
    mi_vector = -grad
    w= w+ tasa*mi_vector
    auxiliar=c(i+1,mi_funcion(w[1],w[2]))
    matriz=rbind(matriz,auxiliar)
    i=i+1
  }
  matriz=matriz[2:51,1:2]
  matriz
}

# Ejercicio 1b1
matriz = gradiente_descendienteb1(50,0.01,c(1,1))
plot(matriz,xlab="iteraciones",ylab="valor función",main="Grad Desc tasa 0.01")
scan()
matriz = gradiente_descendienteb1(50,0.1,c(1,1))
plot(matriz,xlab="iteraciones",ylab="valor función",main="Grad Desc tasa 0.1")
scan()

#Ejercicio 1b2

#Tambien hemos probado a variar el punto de inicio para ver si obtenemos diferentes resultados. Esta es la tabla con los diferentes resultados con el valor mínimo 
#obtenido y las "x" e "y" que lo han obtenido:

gradiente_descendienteb2 = function(maximo,tasa,w){
  
  w_ini = w
  grad=c()
  i=0
  w_iteracion=c()  
  while(i<maximo){
    grad = gradiente2(w)
    mi_vector = -grad
    w= w+ tasa*mi_vector
    i=i+1
  }
  r=c(w_ini[1], w_ini[2], w[1], w[2], mi_funcion(w[1],w[2]))
  
}

matriz=matrix(ncol=5)
matriz = rbind(matriz,gradiente_descendienteb2(50,0.01,c(2.1,2.1)))
matriz = rbind(matriz,gradiente_descendienteb2(50,0.01,c(3,3)))
matriz = rbind(matriz,gradiente_descendienteb2(50,0.01,c(1.5,1.5)))
matriz = rbind(matriz,gradiente_descendienteb2(50,0.01,c(1,1)))
matriz=matriz[2:5,1:5]
colnames(matriz) = c("x_ini","y_ini","x_min","y_min","minimo")
matriz
scan()


#-------------------------------------------------------------------------------------------------------------------------
#EJERCICIO 2
#--------------------------------------------------------------------------------------------------------------------------

#Para implementar la regresión logística hemos utlizado algunas funciones que vimos en la anterior práctica como "simula_unif" o "simula_recta". También hemos 
#cogido de la anterior práctica la función que a partir de una recta y unos datos, generaba etiquetas a esos datos, y la función que a partir de la w, los datos y 
#las etiquetas, calculaba el error en la clasificación. El algoritmo de la regresión logística en cada época recorre todos los datos y sobre cada uno calcula el 
#gradiente estocástico con el que actualiza la w, tal y como hacíamos en el ejercicio 1. Además, antes de cada época se realiza un "sample" para que en cada época 
#se recorran los datos de forma aleatoria. Al final de cada época, se calcula la distancia entre la w anterior y la nueva w que acabamos de obtener. El algoritmo 
#termina cuando esta distancia es menor a 0.01. 

simula_unif = function (N=2,dims=2, rango = c(0,1)){
  m = matrix(runif(N*dims, min=rango[1], max=rango[2]),
             nrow = N, ncol=dims, byrow=T)
  m
}

simula_recta = function (intervalo = c(-1,1), ptos, visible=F){
  
  
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

generarEtiqueta<-function(datos,a,b){
  
  sign(datos[2]-a*datos[1]-b)
  
}

generarEtiquetasMatriz<-function(matriz,a,b){
  apply(matriz,1,generarEtiqueta,a,b)  
}

gradienteEstocastico = function(etiquetay, vectorCaractx,w){
  r=-(etiquetay*vectorCaractx)/(1 + exp(etiquetay%*%(w%*%vectorCaractx)))
  r
}

calcularAB<-function(w1,w2,w3){
  a=-w1/w2
  b=-w3/w2
  resultado=c(a,b)
}

calcularE<-function(w,datos,etiquetas,tam){
  if(ncol(datos)==3){
    datos=cbind(datos[,1],datos[,2])
  }
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

regresionLog = function(tasa,w,datos,etiquetas){
  
  
  matriz=matrix(ncol=2)
  grad=c()
  distancia=1
  while(distancia > 0.01){
    w_anterior=w
    
    t=sample(100,100)
    for(i in 1:100){
      grad = gradienteEstocastico(etiquetas[t[i]],datos[t[i],],w)
      mi_vector = -grad
      w= w+ tasa*mi_vector
    }
    distan=rbind(w,w_anterior)
    distancia=dist(distan,method="euclidean")
    
  }
  w
  
}

misDatos = simula_unif(100,2,c(0,2))
r=sample(100,2)
puntos = rbind(misDatos[r[1],],misDatos[r[2],])
recta = simula_recta(c(0,2),puntos)
etiquetas = generarEtiquetasMatriz(misDatos,recta[1],recta[2])
plot(misDatos,col=etiquetas+5,xlab="x",ylab="y",main="Ejercicio 2 train")
scan()

cadena=replicate(nrow(misDatos),0)
misDatos=cbind(misDatos,cadena)
w_ini = c(0,0,0)

w = regresionLog(0.01,w_ini,misDatos,etiquetas)
miRecta = calcularAB(w[1],w[2],w[3])
abline(miRecta[2],miRecta[1])
scan()

errorIn = calcularE(w,misDatos,etiquetas,100)
errorIn
scan()

#Apartado b

#Ahora vamos a utilizar un conjunto de datos de 1000 puntos que lo trataremos como el conjunto de test, para ver el comportamiento de la regresión logística 
#con unos datos que no se han trabajado sobre ellos.

misDatosO = simula_unif(1000,2,c(0,2))
etiquetasO = generarEtiquetasMatriz(misDatosO,recta[1],recta[2])
plot(misDatosO,col=etiquetasO+5,xlab="x",ylab="y",main="Ejercicio 2 test")
abline(miRecta[2],miRecta[1])
scan()

errorOut = calcularE(w,misDatosO,etiquetasO,1000)
errorOut
scan()


#-------------------------------------------------------------------------------------------------------------------------
#EJERCICIO 3
#--------------------------------------------------------------------------------------------------------------------------

#En este ejercicio vamos a clasificar los dígitos en 4 y 8. Para ello, vamos a necesitar las matrices con los datos de media y simetría de los diferentes digitos,
#además de las etiquetas de cada digitos de si es un 4 o un 8 para saber si estamos haciendo bien la clasificación y calcular los errores. Una vez con los datos de 
#simetría y media, aplicaremos Regresión lineal y PLA-Pocket para obtener un vector de w que construirá una recta para clasificar futuros dígitos en 4 u 8.

#En este ejercicio de clasificación de datos hemos utilizado funciones utilizadas en la anterior práctica para tratar las muestras de los dígitos 4 y 8, 
#tales como obtener las imagenes a partir de los datos o la de calcular la matriz de media y simetría. 

## lectura de los digitos de train

digit.train <- read.table("datos/zip.train",quote="\"", comment.char="", stringsAsFactors=FALSE)
digitos48.train = digit.train[digit.train$V1==4 | digit.train$V1==8,]
digitos_train = digitos48.train[,1]  # etiquetas
ndigitos_train = nrow(digitos48.train)
grises = array(unlist(subset(digitos48.train,select=-V1)),c(ndigitos_train,16,16))

# ------------------------------------------------------------------------

## lectura de los digitos de test

digit.test <- read.table("datos/zip.test",quote="\"", comment.char="", stringsAsFactors=FALSE)
digitos48.test = digit.test[digit.test$V1==4 | digit.test$V1==8,]
digitos_test = digitos48.test[,1]  # etiquetas
ndigitos_test = nrow(digitos48.test)
grises_test = array(unlist(subset(digitos48.test,select=-V1)),c(ndigitos_test,16,16))

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
    if(digs[i]==4)
      digs[i]=1
    else
      digs[i]=-1
  }
  digs
}

m_train=obtenerMatrizSimetriaMedia(imagenes_train)
m_test=obtenerMatrizSimetriaMedia(imagenes_test)

etiquetas_train=cambiar_digitos_etiquetas(digitos_train,432)
plot(m_train,col=etiquetas_train+5,xlab="Intensidad Promedio",ylab="Simetria",main="Ejercicio 3 train")
scan()

#Primero utilizamos la regresión lineal sobre los datos para obtener un vector w inicial sobre el que empezar el PLA-Pocket. La regresión lineal es tal y como 
#se hizo en la anterior práctica donde a partir de la pseudo_inversa de los datos y de las etiquetas, construimos un vector de pesos w

pseudo_inversa<-function(X){
  aux=t(X)%*%X
  X_svd=svd(aux)
  pseudo=X_svd$u%*%diag(1/X_svd$d)%*%t(X_svd$v)
  pseudo_X=pseudo%*%t(X)
  pseudo_X
  
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

#PLA-Pocket

ajusta_PLA <-function(datos,label,max_iter,vini){
  iteraciones = 0
  mejor_w=vini
  mejor=calcularE(vini,datos,label,nrow(datos))
  
  for(i in 1:max_iter){
    iteraciones=0
    for(j in 1:nrow(datos)){
      if(sign(datos[j,1]*vini[1]+datos[j,2]*vini[2]+datos[j,3]*vini[3]) != sign(label[j])){
        vini[1] = vini[1] + label[j]*datos[j,1]
        vini[2] = vini[2] + label[j]*datos[j,2]
        vini[3] = vini[3] + label[j]*datos[j,3]
        iteraciones=-1
        if(calcularE(vini,datos,label,nrow(datos)) < mejor){
          mejor=calcularE(vini,datos,label,nrow(datos))
          mejor_w=vini
        }
        break()
      }  
    }
    if(iteraciones==0){
      iteraciones=i
      break()
    }
  }
  mejor_w
}



w=Regress_Lin(m_train,etiquetas_train)
errorIn=calcularE(w,m_train,etiquetas_train,nrow(m_train))
errorIn
scan()

cadena=replicate(nrow(m_train),0)
m_train=cbind(m_train,cadena)

#Continuando con el ejercicio vamos a aplicar sobre esta w el PLA-Pocket como mejora y valorar los resultados. El algoritmo PLA-Pocket funciona igual que el PLA 
#de la anterior práctica salvo que ahora se encarga de guardar cúal ha sido la mejor w (en función del error obtenido) hasta ahora. Así, a diferencia del PLA que 
#nos devolvía siempre la última w calculada, en el PLA-Pocket se nos devolverá la mejor w que se haya encontrado, independientemente de la iteración

w=ajusta_PLA(m_train, etiquetas_train,10000,w)
errorIn=calcularE(w,m_train,etiquetas_train,nrow(m_train))
errorIn
scan()

plot(m_train,col=etiquetas_train+5,xlab="Intensidad Promedio",ylab="Simetria",main="Ejercicio 3 train")
recta_train=calcularAB(w[1],w[2],w[3])
abline(recta_train[2],recta_train[1])
scan()

#TEST

#Ahora vamos a utilizar los datos de test para comprobar como de bien se comporta la recta calculada con unos datos fuera de la muestra

etiquetas_test=cambiar_digitos_etiquetas(digitos_test,51)
errorTest=calcularE(w,m_test,etiquetas_test,nrow(m_test))
errorTest
scan()

plot(m_test,col=etiquetas_test+5,xlab="Intensidad Promedio",ylab="Simetria",main="Ejercicio 3 test")
recta_test=calcularAB(w[1],w[2],w[3])
abline(recta_test[2],recta_test[1])
scan()



#Cotas
#Cotas sobre Ein
#Tomando Ein como cota para Eout tenemos la ecuacion: eOut(g) <= eIn(g) + sqrt(8/N * ln((4*((2N)^3+1))/0.05))
#Si sustituimos y despejamos tenemos que eOut(g) <= eIn(g) + 0.67 como cota superior
cotaO = errorIn + 0.67
cotaO
scan()


#Cotas sobre Etest
#Tomando P(|eTest(g) - eOut(g)| > exilon) como 0.05, podemos despejar el exilon de la ecuación 0.05 <= 2*exp(-2N*exilon^2). Despejandolo obtenemos que
#exilon=0.19. Con este dato, utilizariamos eTest como cota superior para eOut, eTest(g) + exilon > eOut:
  
cotaO = errorTest + 0.19
cotaO
scan()




#-------------------------------------------------------------------------------------------------------------------------
#EJERCICIO 4
#--------------------------------------------------------------------------------------------------------------------------

#Para este ejercicio creamos unos datos y unas etiquetas a partir de "rnorm" y de la función simula_gauss que vimos en la anterior práctica. Sobre los datos de 
#tamaño 113, creamos 10 particiones sobre las que haremos validación cruzada. Así, iremos tomando una partición como los datos de test, y las otras 9 como los 
#datos de training. A continuación, obtendremos la w_reg.
  
#La función de la pseudo_inversa es la misma que utilizábamos en la anterior práctica salvo que ahora sumamos una matriz identidad y multiplicamos por una lambda. 
  
#Una vez que tenemos la w_reg, tenemos que calcular el error a partir de dicha w para la partición que habíamos dejado como test. El calculo del error se hace 
#sobre un bucle que itera sobre los datos y etiquetas de test.
  
#En el ejercicio se nos pedia que concretamente obtuviesemos el error para la primera partición, para la segunda y la media de todos los errores de las 
#particiones, Ecv. Este proceso se realiza 1000 veces, generando datos nuevos para cada iteración y haciendo la media de los resultados. 

simula_gaus = function(N=2,dim=2,sigma){
  
  if (missing(sigma)) stop("Debe dar un vector de varianzas")
  sigma = sqrt(sigma)  # para la generación se usa sd, y no la varianza
  if(dim != length(sigma)) stop ("El numero de varianzas es distinto de la dimensión")
  
  simula_gauss1 = function() rnorm(dim, sd = sigma) # genera 1 muestra, con las desviaciones especificadas
  m = t(replicate(N,simula_gauss1())) # repite N veces, simula_gauss1 y se hace la traspuesta
  m
}

pseudo_inversa<-function(X,lambda){
  aux=t(X)%*%X + diag(ncol(X))*lambda
  X_svd=svd(aux)
  pseudo=X_svd$u%*%diag(1/X_svd$d)%*%t(X_svd$v)
  pseudo
  
  
}

calcular_error<-function(w,datos,etiquetas){
  aux=0
  for(i in 1:nrow(datos)){
    aux=aux + (w%*%datos[i,]-etiquetas[i])^2
  }  
  error=aux/nrow(datos)
  error
}


errores_1=0
errores_2=0
errores_acumulados=0

for(i in 1:1000){
  
  w_f=rnorm(4,0,1)
  w_f[4] = w_f[4] + 1
  datos=simula_gaus(113,3,c(1,1,1))
  datos=cbind(datos,1)
  
  ruidos=rnorm(113,0,1)
  etiquetas=0
  for(j in 1:113){
    etiquetas[j]=t(w_f)*datos[j]+0.5*ruidos[j]
  } 
  
  
  fin=13
  for(t in 1:11){
    
    ini=fin+1
    if(t==1)
      ini=1
    
    fin=13+(t-1)*10  
     
    datos_train=datos[-c(ini:fin),]
    datos_test=datos[c(ini:fin),]
    etiquetas_train=etiquetas[-c(ini:fin)]
    etiquetas_test=etiquetas[c(ini:fin)]
    
    error1=0
    error2=0
    error_acumulado=0
    
    w_reg=pseudo_inversa(datos_train,0.05/113)%*%t(datos_train)%*%etiquetas_train
    w_reg=as.vector(w_reg)
    
    if(t==1){
      error1=calcular_error(w_reg,datos_test,etiquetas_test)
      error_acumulado=error_acumulado+error1
    }else if(t==2){
      error2=calcular_error(w_reg,datos_test,etiquetas_test)
      error_acumulado=error_acumulado+error2
    }else{
      errork=calcular_error(w_reg,datos_test,etiquetas_test)
      error_acumulado=error_acumulado+errork
    }
      
   
    
    
    error_acumulado=error_acumulado/11
    errores_1=errores_1+error1
    errores_2=errores_2+error2
    errores_acumulados=errores_acumulados+error_acumulado
    
    
  }  


}


errores_1=errores_1/1000
errores_2=errores_2/1000
errores_acumulados=errores_acumulados/1000

errores_1
errores_2
errores_acumulados
scan()



#-------------------------------------------------------------------------------------------------------------------------
#BONUS
#--------------------------------------------------------------------------------------------------------------------------

#-------------------------------------------------------------------------------------------------------------------------
#EJERCICIO 1
#--------------------------------------------------------------------------------------------------------------------------

#Para este ejercicio utilizamos la misma función del ejercicio 1.1a y una variante del gradiente descendente que hemos utilizado ya que, ahora primero solo 
#actualizamos la w con la u, y a continuación actualizamos la w solo con la v. De esta forma, la w se mueve sólo en una dirección en cada paso.

#En el ejercicio se nos pide que mostremos el error que se alcanza en la iteración 15 con una tasa de aprendizaje de 0.1. Sin embargo, rapidamente aumenta el 
#error hasta que en la tercera iteración el error es tan grande que se hace indeterminado. No obstante, si cambiamos la tasa a una mucho más pequeña, 0.001, 
#vemos que el error sí alcanza valores más lógicos.

derivada_u = function(u,v){
  
  result = (4*exp(-2*u))*((u*u) * exp(u+v) -2*(v*v))*(u*exp(u+v)+(v*v))
  result
  
}

derivada_v = function(u,v){
  
  result = (2*exp(-2*u))*((u*u) * exp(u+v) -4*v)*((u*u)*exp(u+v)-2*(v*v))
  result
  
}

error=function(w){
  e=(((w[1]*w[1])*exp(w[2]) -2*(w[2]*w[2])*exp(-w[1]))^2)
  e
}


gradiente_u = function(w){
  a= derivada_u(w[1],w[2])
  a
}

gradiente_v = function(w){
  
  b= derivada_v(w[1],w[2])
  b
}

coordenada_descendente = function(maximo,tasa){
  
  grad=c()
  mi_vector = c()
  i=0
  iteracion=-1
  w_iteracion=c()  
  w=c(1,1)  
  while(i<maximo){
    
    anterior_w = w
    grad = gradiente_u(w)
    nueva_u = -grad
    w[1]= w[1]+ tasa*nueva_u
    
    grad = gradiente_v(w)
    nueva_v = -grad
    w[2]= w[2]+ tasa*nueva_v
    
    
    i=i+1
  }
  error(w)
}

errorCoord = coordenada_descendente(15,0.1)
errorCoord
scan()

errorCoord = coordenada_descendente(15,0.001)
errorCoord
scan()

#Se nos pide que hagamos una comparación para el gradiente descendente asi que aquí tenemos los errores alcanzados por el gradiente descendente en la iteración 15 
#para una tasa de 0.1 y 0.001 respectivamente:

errorGrad = gradiente_descendiente(15,0.1)
errorGrad[2]
scan()

errorGrad = gradiente_descendiente(15,0.001)
errorGrad[2]
scan()


#-------------------------------------------------------------------------------------------------------------------------
#EJERCICIO 2
#--------------------------------------------------------------------------------------------------------------------------

#De forma similar al gradiente descendente salvo que ahora tenemos que calcular el incremento de w de otra forma. Este incremento de w se calcula multiplicando 
#la inversa de la matriz jacobiana por el gradiente de w (tal y como se hacía en el ejercicio 1). Para esta matriz jacobiana se ha necesitado calcular: doble derivada 
#respecto a "x", derivada respecto a "x" y a continuación derivada respecto a "y", derivada respecto a "y" y a continuación derivada respecto a "x", y doble derivada 
#respecto a "y". Así, se irá actualizando la w a lo largo de las iteraciones que marquemos.

#En el ejercicio se nos pide que comparemos la gráfica del valor de la función con la obtenida en el gradiente descendente del ejercicio 1.b. Además, como veremos que 
#dependen los valores obtenidos en función del punto inicial escogido, haremos una comparación entre el método de Newton y el gradiente descendente para el mismo punto 
#de inicio:

derivada_xf1 = function(x,y){
  r=2-8*pi*pi*sin(2*pi*x)*sin(2*pi*y)
  r
}

derivada_yf1 = function(x,y){
  r=8*pi*pi*cos(2*pi*x)*cos(2*pi*y)
  r
}

derivada_yf2 = function(x,y){
  r=4-8*pi*pi*sin(2*pi*x)*sin(2*pi*y)
  r
}

derivada_xf2 = function(x,y){
  r=8*pi*pi*cos(2*pi*x)*cos(2*pi*y)
  r
}

derivada_x = function(x,y){
  r=2*(2*pi*cos(2*pi*x)*sin(2*pi*y)+x-2)
  r
}

derivada_y = function(x,y){
  r=4*(pi*sin(2*pi*x)*cos(2*pi*y)+y-2)
  r
}

mi_funcion = function(x,y){
  r=(x-2)*(x-2) + 2*(y-2)*(y-2) + 2*sin(2*pi*x)*sin(2*pi*y)
  r
}

gradiente2= function(w){
  a= derivada_x(w[1],w[2])
  b= derivada_y(w[1],w[2])
  cad = c(a,b)
  cad
}

calcularMatrizNewton = function(x,y){
  a=derivada_xf1(x,y)
  b=derivada_yf1(x,y)
  c=derivada_xf2(x,y)
  d=derivada_yf2(x,y)
  matriz=matrix(c(a,b,c,d),ncol=2)
  matriz
}

newtonMethod = function(maximo,tasa,w){
  
  
  matriz=matrix(ncol=2)
  grad=c()
  i=0
  w_iteracion=c()  
  while(i<maximo){
    
   
    miMatriz=calcularMatrizNewton(w[1],w[2])
    incremento_w=-solve(miMatriz)%*%gradiente2(w)
    w= w+ tasa*incremento_w
    auxiliar=c(i+1,mi_funcion(w[1],w[2]))
    matriz=rbind(matriz,auxiliar)
    i=i+1
  }
  matriz=matriz[2:maximo+1,1:2]
  matriz
}


misDatos=newtonMethod(50,0.1,c(1,1))
plot(misDatos,ylab="valor función",main="Gradiente 1,1")
scan()

matriz = gradiente_descendienteb1(50,0.1,c(1,1))
plot(matriz,ylab="valor función",main="Gradiente 1,1")
scan()

misDatos=newtonMethod(50,0.1,c(2.1,2.1))
plot(misDatos,ylab="valor función",main="Gradiente 2.1,2.1")
scan()

matriz = gradiente_descendienteb1(50,0.1,c(2.1,2.1))
plot(matriz,ylab="valor función",main="Gradiente 2.1,2.1")
scan()

misDatos=newtonMethod(50,0.1,c(3,3))
plot(misDatos,ylab="valor función",main="Gradiente 3,3")
scan()

matriz = gradiente_descendienteb1(50,0.1,c(3,3))
plot(matriz,ylab="valor función",main="Gradiente 3,3")
scan()

misDatos=newtonMethod(50,0.1,c(1.5,1.5))
plot(misDatos,ylab="valor función",main="Gradiente 1.5,1.5")
scan()

matriz = gradiente_descendienteb1(50,0.1,c(1.5,1.5))
plot(matriz,ylab="valor función",main="Gradiente 1.5,1.5")
scan()


