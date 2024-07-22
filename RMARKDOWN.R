---
  title: "Validacion de la Ecuacion Mincer"
author: "Enriquez Acuña Joel - Martinez Hilario Jose - Sullcaray Paucar Joan"
date: "2024-07-21"
output: word_document
---
  
  ```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

**INTRODUCCION**
  
  En este trabajo, aplicaremos la ecuación de Mincer utilizando datos de la Encuesta Nacional de Hogares (ENAHO) del Instituto Nacional de Estadística e Informática (INEI) de Perú. La ENAHO es una fuente rica y detallada de información socioeconómica y demográfica que nos permitirá realizar un análisis exhaustivo de los determinantes del ingreso en el contexto peruano.
El objetivo principal de este estudio es aplicar los conceptos y técnicas aprendidas en clase, específicamente en el ámbito de la econometría aplicada, para estimar y interpretar modelos de regresión que expliquen el ingreso de los individuos. A través de este análisis, se buscará:
  
  
  Estimación de la ecuación de Mincer: Utilizar la ENAHO para estimar la ecuación de Mincer, cuantificando la relación entre educación, experiencia laboral y el ingreso.

Análisis e interpretación económica: Interpretar los coeficientes obtenidos en términos económicos, evaluando cómo la educación y la experiencia afectan los salarios en Perú.

A lo largo del trabajo, se emplearán técnicas econométricas asegurar  los resultados. Además, se discutirá la relevancia de los resultados en el contexto de economico, considerando cómo los hallazgos pueden informar distintoa contextos que llega a tener la ecuacion de minces.


**MARCO DE REFERENCIA**
  
  
  **LA ECUACION DE MINCER**
  
  A quien no le gustaría medir la rentabilidad que tendrá los años y el tiempo invertido en la
educación
Jacob Mincer, después de haber observado este hecho en estados unidos, en su libro,
escolarización, experiencia e ingresos, da a conocer un modelos de determinación, de los
ingresos, si bien esta controversia generó mucha bibliografía en el siglo 20, la que mas se
utiliza en la ecuación de Mincer y ha permitido la comparación de la eficiencia de la producción
en educación. Parte de la teoría del capital humano que nos dice que existe un relación
positiva entre el numero de años de escolaridad y sus ingresos a futuro.
La ecuación de Mincer estima un modelo semilogarítmico teniendo como variable
dependiente el logaritmo de ingresos y a la variable independiente los años de educción, la
experiencia laboral y el cuadrado de la experiencia laboral.
A continuación se presenta el modelo de la ecuacion contruida a partir de las variables a tratar:

ln(WS) = β0 + β1*EDUC + β2*EXP + β3*EXP2 + α1*SEXO + α2*ZONA + α3*REGION + α4*INFORM α5*GEDUC+ ϵ

Donde:

WS: Representa los ingresos del individuo

Educ: Es el número de años de educación formal completada

Exp: Años de experiencia laboral

Sexo: Si las personas es un hombre o mujer

Zona:La persona reside en una zona urbana o rural

Region: La personas en que geografia se encuentra

Inform: La persona se encuentra en un trabajo formal o informal

Geduc: Que nivel de educacion alcanzo la persona

𝜀: es el termino de perturbación aleatoria de media cero.




**LA LIBERTAD**

***Ubicación Geográfica*****

El departamento de La Libertad, situado en la costa nor-occidental del país, cubre una superficie de 25 500 Km2 (superficie continental más insular) que representa el 2,0 por ciento del territorio nacional. Limita por el norte con los departamentos de Lambayeque, Cajamarca y Amazonas; por el este, con San Martín; por el sur, con Huánuco y Ancash; y por el oeste, con el Océano Pacífico. Políticamente está dividido en 12 provincias y 83 distritos y su capital es la ciudad de Trujillo.
El territorio comprende las tres regiones naturales; sin embargo, el 80 por ciento de su área superficial es esencialmente andina. La Libertad presenta una altitud que oscila entre los 3 m.s.n.m. (Distrito de Salaverry - Provincia Trujillo) y los 4 008 m.s.n.m. (Distrito de Quiruvilca - Provincia de Santiago de Chuco).

***Relieve y Clima****

Las zonas costera y andina de esta región tienen en simultáneo estaciones climáticas opuestas. Mientras que en la primera es cálido y soleado la mayor parte del año, cuya temperatura oscila entre los 20 °C y 21 °C, aunque en verano del 2017 superó los 30 °C; en invierno, las pequeñas garúas humedecen la campiña de la costa. En tanto, en la sierra andina, a partir de los 3.000 m.s.n.m., tiene un clima seco y templado durante el día y frío en la noche; durante los meses de enero a marzo, época de invierno, hay intensas lluvias.
Además, en la costa, el clima es semitropical con una temperatura promedio de 18°C y precipitaciones inferiores a 50 o 20 mm anuales. En la zona andina existe un clima variado, cuyas temperaturas, lluvias y vegetación cambian a medida que se asciende en altura; así, la temperatura fluctúa entre 14°C y 2°C en invierno, mientras que en verano varía entre 24°C y 13° C. Sin embargo, en Trujillo, el clima es más húmedo y frío durante gran parte del año. En invierno y otoño, las garúas y neblina se dan a diario.

****Hidrografía**

El sistema hidrográfico está conformado principalmente por seis ríos, los que se forman en la vertiente occidental de los Andes y riegan los valles costeros como Jequetepeque, Chicama, Moche, Virú y Chao, cuyos caudales son variables debido a que se alimentan de precipitaciones pluviales que estacionalmente se registran en verano. En la vertiente amazónica se ubica el río Marañón, que dispone de agua todo el año. También tiene la presencia de lagunas como Sausacocha y Grande.

***Historia****

Diario el comercio, (2017) Nos mencionan la historia del departamento La Libertad fue territorio de varias civilizaciones importantes del antiguo Perú. Por el siglo I a.C. surgió en la costa la cultura Moche, famosa por su arte cerámico, y en la Sierra, la cultura Cajamarca. El departamento conoció la influencia Huari y en el Intermedio Tardío, la expansión del Imperio Chimú por toda la costa norte, así como su influencia en las zonas altas del reino de Huamachuco.
Posteriormente, esta región fue anexada al Imperio incaico y luego a la corona española. Durante la colonización de España se fundó la ciudad de Trujillo, que ejerció gran influencia en el norte del país como sede administrativa de importancia política y eclesiástica.
En la República, la costa liberteña fue sede de los más importantes ingenios azucareros del país, destruidos e incendiados por la Expedición Lynch. También la Guerra del Pacífico concluyó en La Libertad con la Batalla de Huamachuco en 1883.

***Población Total******
Según el XII Censo Nacional de Población, realizado en el año 2017 por el Instituto Nacional de Estadística e Informática (INEI), el departamento de La Libertad posee una población de 1 778 080 habitantes (6,1 por ciento del total nacional), situándolo como el tercero más poblado del país, después de Lima (35,7 por ciento) y Piura (6,3 por ciento). Las cifras reflejan una alta concentración en la ciudad capital (Trujillo), al albergar al 54,6 por ciento de la población departamental. El 51,2 por ciento de la población es femenina y el 48,8 por ciento masculina. Según ámbito geográfico, el 78,9 por ciento de la población es urbana y el 21,1 por ciento rural. En cuanto a la transición de la estructura demográfica según grandes grupos de edad, se observa mayor participación de la población en edad productiva (entre 15 y 64 años) que pasó de representar el 58,5 por ciento en el año 1993, a 63,6 por ciento en 2017. En tanto, la participación de la población entre 0 y 14 años pasó de 36,6 por ciento en 1993 a 27,9 por ciento en 2017. En cuanto a la población adulta (más de 65 años), su participación aumentó de 4,9 a 8,5 por ciento, para los mismos años de referencia. De otro lado, entre los años 2007 y 2017, la población censada del departamento creció a un ritmo promedio anual de 1,0 por ciento, superior al promedio nacional (0,7 por ciento).

***Economía**


Según información del INEI, La Libertad es el tercer departamento más importante del país, ya que contribuye con el 4,2 por ciento del Valor Agregado Bruto nacional y se ubica después de Lima (43,8 por ciento) y Arequipa (5,5 por ciento). La importancia relativa del departamento a nivel nacional es mayor en algunos sectores tales como agropecuario (13,1 por ciento), telecomunicaciones (5,3 por ciento), pesca y acuicultura (5,2 por ciento), manufactura (5,0 por ciento), transporte (4,8 por ciento), entre otros.
caza y silvicultura (17,9 por ciento), manufactura (15,2 por ciento), comercio (10,7 por ciento), extracción de petróleo, gas y minerales (7,1 por ciento) y construcción (6,7 por ciento), entre otros. En la última década (2013-2022), la economía de La Libertad registró un crecimiento promedio anual de 2,5 por ciento, por debajo del promedio nacional (2,8 por ciento). Entre los sectores más dinámicos destacan: Telecomunicaciones (8,0 por ciento); electricidad, gas y agua (5,1 por ciento); agropecuario (4,8 por ciento); administración pública y defensa (4,0 por ciento); y construcción (3,8 por ciento). De otro lado, según el INEI, la Población Económicamente Activa (PEA) del departamento ascendió a 1,1 millones personas, de las cuales, el 93,5 por ciento está ocupada, mientras que el 6,5 por ciento, desocupada. De la PEA ocupada (1,0 millones personas), el 28,0 por ciento labora en el sector servicios; el 24,0 por ciento, en el sector agropecuario y pesca; el 20,0 por ciento, en el sector comercio; el 10,8 por ciento, en manufactura; el 7,8 por ciento en transporte y comunicaciones, el 7,3 por ciento en construcción y el 2,1 en minería. La estructura empresarial de La Libertad está en su mayoría conformada por la micro y pequeña empresa (MYPE). Según el Ministerio de la Producción (2021), existen en el departamento 111,6 mil unidades productivas formales, de las cuales el 99,5 por ciento son micro y pequeñas empresas. El departamento es el tercero a nivel nacional en cuanto a número de empresas formales (111,6 mil empresas), después de Lima (960,7 mil empresas) y Arequipa (126,1 mil empresas).


***Agropecuario***

Arándano: La Libertad es el primer productor de arándano del país, con una producción de 158,3 mil toneladas (2022), y una participación del 54,1 por ciento de la producción nacional (292,6 mil toneladas). 

Palta:  La Libertad es el primer productor de palta a nivel nacional, con una producción de 254,9 mil toneladas en 2022 y una participación del 29,6 por ciento del total nacional. En el periodo 2013-2022, la producción registró un crecimiento promedio anual de 14,2 por ciento, como resultado del aumento en las hectáreas cultivadas (plantaciones),
particularmente de la palta Hass en los valles de Virú y Chao. 

Espárrago:  La Libertad es el segundo productor de espárrago del Perú y es cultivado en los valles de Virú, Chao, Santa Catalina y Chicama. Debido a la creciente demanda externa y las favorables ventajas comparativas, reflejadas en suelo y clima apropiado, se obtienen dos cosechas al año (contra - estación al mercado americano y mexicano). 

Arroz: El arroz es uno de los principales cultivos con mayor superficie cosechada, ya que se destinan 30 mil hectáreas para su producción. En la campaña agrícola 2021-2022 (de
agosto a julio) se cultivaron 28,7 mil hectáreas y su producción aumentó 4,3 por ciento en términos interanuales al sumar 309,3 mil toneladas. 


**Pesca**

En el año 2022, representó el 0,4 por ciento del VAB departamental. La actividad
pesquera se caracteriza por estar condicionada a efectos climáticos (Fenómenos de “El
Niño” y “La Niña”), así como por las vedas que se establecen para favorecer el ciclo
reproductivo de especies como la anchoveta, principal recurso marino, el cual se destina
para la elaboración de harina y aceite de pescado, cuyo principal mercado son los países de los continentes de Asia y Europa. La actividad pesquera se desarrolla a nivel industrial y artesanal. 

**Educación**

Instituciones Educativas: La Libertad cuenta con diversas universidades, siendo la Universidad Nacional de Trujillo la más destacada.
Niveles de Educación Alcanzados: Alta tasa de alfabetización y un número creciente de graduados universitarios.


***Vivienda y Servicios Básicos***


Acceso a Agua Potable: 85% de la población tiene acceso a agua potable.
Electricidad: 90% de cobertura eléctrica.
Saneamiento: 80% de la población tiene acceso a servicios de saneamiento.

**TABLA DE DATOS**

El archivo limpio y compacto se encuentra almacenado en una repositorio publico:

https://github.com/Joaprincs/Validacion-de-modelo/blob/main/TABLAREME.csv

\

**TABLA DE DATOS**

CATEGORIA SEXO
```{r, echo=FALSE,warning=FALSE, message=FALSE}

library(haven)
library(readr)

url <- "https://raw.githubusercontent.com/Joaprincs/Validacion-de-modelo/main/TABLAREME.csv"
TABLAREME <- read_csv(url)
head(TABLAREME)

library(car)
```
\

```{r, echo=FALSE,warning=FALSE, message=FALSE}

library(tseries)

```


\

CATEGORIA SEXO
```{r, echo=FALSE,warning=FALSE, message=FALSE}

library(dplyr)
model <- lm(LWS~SEXO, data=TABLAREME)
summary(model)$coef
TABLAREME$SEXO <- factor(TABLAREME$SEXO)
contrasts(TABLAREME$SEXO)

TABLAREME <- TABLAREME%>%mutate(SEXO=relevel(SEXO,ref="MASCULINO"))
model1 <-lm(LWS~SEXO, data=TABLAREME)
summary(model1)$coef

```



\

CATEGORIA ZONA
```{r, echo=FALSE,warning=FALSE, message=FALSE}

model2 <- lm(LWS~ZONA, data=TABLAREME)
summary(model2)$coef
TABLAREME$ZONA <- factor(TABLAREME$ZONA)
contrasts(TABLAREME$ZONA)

TABLAREME <- TABLAREME%>%mutate(ZONA=relevel(ZONA,ref="URBANA"))
model3 <-lm(LWS~ZONA, data=TABLAREME)
summary(model3)$coef

```



\

CATEGORIA REGION
```{r, echo=FALSE,warning=FALSE, message=FALSE}

model4 <- lm(LWS~REGION, data=TABLAREME)
summary(model4)$coef
TABLAREME$REGION <- factor(TABLAREME$REGION)
contrasts(TABLAREME$REGION)

TABLAREME <- TABLAREME%>%mutate(REGION=relevel(REGION,ref="COSTA"))
model5 <-lm(LWS~REGION, data=TABLAREME)
summary(model5)$coef

```



\

CATEGORIA INFORM
```{r, echo=FALSE,warning=FALSE, message=FALSE}

model6 <- lm(LWS~INFORM, data=TABLAREME)
summary(model6)$coef
TABLAREME$INFORM <- factor(TABLAREME$INFORM)
contrasts(TABLAREME$INFORM)

TABLAREME <- TABLAREME%>%mutate(INFORM=relevel(INFORM,ref="INFORMAL"))
model7 <-lm(LWS~INFORM, data=TABLAREME)
summary(model7)$coef

```



\

CATEGORIA GEDUC
```{r, echo=FALSE,warning=FALSE, message=FALSE}

res <- model.matrix(~GEDUC, data= TABLAREME)

head(res[,-1])

model8 <- lm(LWS~AEDUC+EXP+EXP2+SEXO+GEDUC, data = TABLAREME)
Anova(model8)


```



\

**VALIDACION DEL MODELO MINCER**

```{r, echo=FALSE,warning=FALSE, message=FALSE}

modelF <- lm(LWS~AEDUC+EXP+EXP2+SEXO+ZONA+REGION+INFORM+GEDUC, data = TABLAREME)

modelMIN <- lm(LWS~AEDUC+EXP+EXP2+SEXO+ZONA+REGION+INFORM+GEDUC, data = TABLAREME)
library(stargazer)
```



\

**PRUEBA DE NORMALIDAD**

Las hipotesis para la prueba son las siguientes:
  
Ho: los residuales del modelo siguen una distribucion normal

H1: los residuales del modelo no siguen una distribucion normal


```{r, echo=FALSE,warning=FALSE, message=FALSE}


modelF <- lm(LWS~AEDUC+EXP+EXP2+SEXO+ZONA+REGION+INFORM+GEDUC, data = TABLAREME)

jarque.bera.test(modelF$residuals)

```

Como jb <  0.05 se rechaza la hipotesis Ho. Hay evidencia que los residuales no tienen una distribucion normal.


\
```{r, echo=FALSE,warning=FALSE, message=FALSE}


qqnorm(modelF$residuals)

qqline(modelF$residuals, col="green")

```
\

**CORRECCION DE LA NO NORMALIDAD**

```{r, echo=FALSE,warning=FALSE, message=FALSE}


library(sandwich)


```
\

```{r, echo=FALSE,warning=FALSE, message=FALSE}

library(lmtest)
modelCO <- vcovHC(modelF, type = "HC1")
coeftest(modelF, vcov. = modelCO)


robust.se1<-sqrt(diag(modelCO))

jarque.bera.test(robust.se1)

```

Como jb >  0.05 se acepta la hipotesis Ho. Hay evidencia que los residuales tienen una distribucion normal.

\
**ANALISIS DE HETEROCEDASTICIDAD**

Hipotesis:
  
Ho: Hay evidencia de que la varianza de los residuos es Homocedastica

H1: Hay evidencia de que la varianza de los residuos es Heterocedastica

```{r, echo=FALSE,warning=FALSE, message=FALSE}

library(lmtest)

dwtest(modelF, alternative = "two.sided", iterations = 1000)

gqtest(modelF)

```

DW = 1.9545: Este valor está bastante cerca de 2, lo que sugiere que no hay autocorrelación 
significativa de primer orden en los residuos del modelo. Un valor de 2 indica ausencia de autocorrelación,
mientras que valores menores a 2 sugieren autocorrelación positiva y valores mayores a 2
sugieren autocorrelación negativa.

p-value = 0.4003: Este p-valor indica la probabilidad de observar una
estadística de Durbin-Watson tan extrema como la que has calculado, bajo la 
hipótesis nula de que no hay autocorrelación. Un p-valor alto (como 0.4003) 
sugiere que no hay evidencia suficiente para rechazar la hipótesis nula de que la autocorrelación es 0.

\

**ANALISIS DE MULTICOLINEALIDAD**

Calcular la matriz de normalidad

```{r, echo=FALSE,warning=FALSE, message=FALSE}

library(car)

X_mat <- model.matrix(modelF)
stargazer(head(X_mat,n=6), type = "text")

vif(modelMIN)

```

\
```{r , echo=FALSE,warning=FALSE, message=FALSE}

XX_matrix<-t(X_mat)%*%X_mat
stargazer(XX_matrix,type = "text")

```
\

```{r, echo=FALSE,warning=FALSE, message=FALSE}

options(scipen = 99999)
Sn<-solve(diag(sqrt(diag(XX_matrix))))
stargazer(Sn,type = "text")

```
\

```{r, echo=FALSE,warning=FALSE, message=FALSE}

XX_norm<-(Sn%*%XX_matrix)%*%Sn
stargazer(XX_norm,type = "text",digits = 4)

```
\

```{r, echo=FALSE,warning=FALSE, message=FALSE}

lambdas<-eigen(XX_norm,symmetric = TRUE)
stargazer(lambdas$values,type = "text")

```
\

```{r, echo=FALSE,warning=FALSE, message=FALSE}

K<-sqrt(max(lambdas$values)/min(lambdas$values))
print(K)

```

Un indice de condición con un valor por encima de 20, sugiere la presencia de multicolinealidad mediana
en los regresores del modelo, para nuestro caso el valor de este es de 17.5 con lo cual podemos decir 
que para nuestro modelo la multicolinealidad no es considerada un problema.

\
**ANALISIS DE EXOGENEIDAD**

Hipotesis:
  
Ho: Las variables dependientes no tienen correlacion con el termino de error

H1: Las variables dependientes tienen correlacion con el termino de error

```{r, echo=FALSE,warning=FALSE, message=FALSE}

library(AER)
library(ivreg)

modelMIN <- lm(LWS~AEDUC+EXP+EXP2+SEXO+SEXO*AEDUC+ZONA+ZONA*AEDUC+
                 REGION+REGION*AEDUC+INFORM+INFORM*AEDUC+GEDUC+GEDUC*AEDUC, data = TABLAREME)


modelMIN2 <- ivreg(LWS ~ AEDUC + EXP + EXP2 + SEXO + SEXO * AEDUC + 
                     ZONA + ZONA * AEDUC + REGION + REGION * AEDUC + INFORM + INFORM * AEDUC + GEDUC + GEDUC * AEDUC | AEDUC + 
                     EXP + EXP2 + SEXO + SEXO * AEDUC + 
      ZONA + ZONA * AEDUC + REGION + REGION * AEDUC + INFORM + INFORM * AEDUC + GEDUC + GEDUC * AEDUC, data = TABLAREME)

```






\

**ANALISIS DE EXOGENEIDAD - TEST DE HAUSMAN**

```{r, echo=FALSE,warning=FALSE, message=FALSE}

hausman_test <- function(modelMIN, modelMIN2) {
  b_ols <- coef(modelMIN)
  b_iv <- coef(modelMIN2)
  
  V_ols <- vcov(modelMIN)
  V_iv <- vcov(modelMIN2)
  
  diff <- b_iv - b_ols
  cov_diff <- V_iv - V_ols
  
  chisq_stat <- t(diff) %*% solve(cov_diff) %*% diff
  p_value <- pchisq(chisq_stat, df = length(b_ols), lower.tail = FALSE)
  
  return(list(statistic = chisq_stat, p.value = p_value))
}

hausman_result <- hausman_test(modelMIN, modelMIN2)
print(hausman_result)

```

Si el p-valor es 1, la prueba sugiere fuertemente que las variables dependientes son exógenas
\

**CONTRASTE DE HIPOTESIS**

```{r, echo=FALSE,warning=FALSE, message=FALSE}
summary_modelF <- summary(modelF)
F_Anova <- summary_modelF$fstatistic[1]
gl_num <- summary_modelF$fstatistic[2]
gl_den <- summary_modelF$fstatistic[3]
p_value <- pf(F_Anova, gl_num, gl_den, lower.tail = FALSE)


```

\
```{r, echo=FALSE,warning=FALSE, message=FALSE}

cat("Estadístico F:", F_Anova, "\n")
cat("Grados de libertad del numerador:", gl_num, "\n")
cat("Grados de libertad del denominador:", gl_den, "\n")
cat("P-valor:", p_value, "\n")


```

\
```{r, echo=FALSE,warning=FALSE, message=FALSE}

if (p_value < 0.05) {
  cat("Rechazamos la hipótesis nula: Hay evidencia de una relación lineal significativa entre LWS y las variables independientes.\n")
} else {
  cat("No rechazamos la hipótesis nula: No hay evidencia suficiente de una relación lineal significativa entre LWS y las variables independientes.\n")
}


```

\
**RGRESIONES EN CONJUNTO**

**MODELO 1**

```{r, echo=FALSE,warning=FALSE, message=FALSE}

modelo1 <-lm(LWS~AEDUC+EXP+EXP2+SEXO+ZONA+REGION+INFORM+GEDUC, data = TABLAREME)

summary(modelo1)
```

\

**MODELO 2**

```{r, echo=FALSE,warning=FALSE, message=FALSE}

modelo2 <- lm(LWS ~ AEDUC + EXP + EXP2+SEXO +AEDUC*SEXO ,data = TABLAREME)

stargazer(modelo2, type = "text", title = "Resultados del Modelo 2", align = TRUE)

```

 Cada año adicional de educación se asocia con un aumento promedio del 9.6% en el salario.Cada año adicional de experiencia se asocia con un aumento promedio del 3.1% en el salario, las mujeres ganan en promedio un 66.2% menos que los hombres.La interacción entre los años de educación y el género femenino tiene un coeficiente positivo de 0.016, aunque no es estadísticamente significativo. Esto sugiere que el retorno de la educación es ligeramente mayor para las mujeres, pero esta diferencia no es significativa desde el punto de vista estadístico.


\

**MODELO 3**

```{r, echo=FALSE,warning=FALSE, message=FALSE}

modelo3 <- lm(LWS ~ AEDUC + EXP + EXP2 +ZONA+AEDUC*ZONA, data = TABLAREME)

stargazer(modelo3, type = "text", title = "Resultados del Modelo 3", align = TRUE)

```

Cada año adicional de educación se asocia con un aumento promedio del 10.5% en el salario. Cada año adicional de experiencia se asocia con un aumento promedio del 3.3% en el salario, esidir en una zona rural se asocia con una disminución promedio del 1.2% en el salario. La interacción entre los años de educación y residir en una zona rural tiene un coeficiente negativo de -0.052, lo que sugiere que el retorno de la educación es menor en las zonas rurales. Específicamente, por cada año adicional de educación, el incremento en el salario es 5.2% menor en las zonas rurales en comparación con las zonas urbanas


\

**MODELO 4**

```{r, echo=FALSE,warning=FALSE, message=FALSE}

modelo4 <- lm(LWS ~ AEDUC + EXP + EXP2+REGION +AEDUC*REGION ,data = TABLAREME)

stargazer(modelo4, type = "text", title = "Resultados del Modelo 4", align = TRUE)

```

Cada año adicional de educación se asocia con un aumento promedio del 9.7% en el salario, Cada año adicional de experiencia se asocia con un aumento promedio del 3.6% en el salario, residir en la región Sierra se asocia con una disminución promedio del 37.1% en el salario en comparación con la región de referencia costa. Esto sugiere que el retorno de la educación no es significativamente diferente en la región Sierra en comparación con la región de referencia.


\

**MODELO 5**

```{r, echo=FALSE,warning=FALSE, message=FALSE}

modelo5 <- lm(LWS ~ AEDUC + EXP + EXP2+INFORM +AEDUC*INFORM ,data = TABLAREME)

stargazer(modelo5, type = "text", title = "Resultados del Modelo 5", align = TRUE)

```

Cada año adicional de educación se asocia con un aumento promedio del 5.8% en el salario. Cada año adicional de experiencia se asocia con un aumento promedio del 1.3% en el salario, trabajar en un empleo formal se asocia con un aumento promedio del 98.9% en el salario en comparación con trabajar en un empleo informal. Esto sugiere que el retorno de la educación no es significativamente diferente entre empleos formales e informales. 
\

**MODELO 6**

```{r, echo=FALSE,warning=FALSE, message=FALSE}

modelo6 <- lm(LWS ~ AEDUC + EXP + EXP2+GEDUC +AEDUC*GEDUC ,data = TABLAREME)

stargazer(modelo6, type = "text", title = "Resultados del Modelo 6", align = TRUE)

```

Esto indica que, en promedio, cada año adicional de educación está asociado con un aumento del 7.7% en el salario. Esto sugiere que cada año adicional de experiencia incrementa el salario semanal en un 2.9%, 
tener educación secundaria no tiene un impacto significativo en el salario semanal. Este coeficiente positivo indica que los rendimientos de cada año adicional de educación son mayores para aquellos con educación universitaria en comparación con la categoría base.

El aspecto grado de educacion universitaria siendo positivo y significativo sugiere mayores retornos a la educación para individuos con educación universitaria en comparación con aquellos con niveles de educación inferiores.

\

**MODELO 7**

```{r, echo=FALSE,warning=FALSE, message=FALSE}

modelo7 <- lm(LWS ~ AEDUC + EXP + EXP2+ZONA+SEXO +AEDUC*ZONA+AEDUC*SEXO,data = TABLAREME)

stargazer(modelo7, type = "text", title = "Resultados del Modelo 7", align = TRUE)

```

Esto indica que, en promedio, cada año adicional de educación está asociado con un aumento del 9.5% en el salario. Esto sugiere que cada año adicional de experiencia incrementa el salario semanal en un 3.1%. Esto indica que, en promedio, las mujeres ganan aproximadamente un 70.9% menos que los hombres. Cada año adicional de educación está asociado con una disminución del 6% en los retornos a la educación para las personas que viven en zonas rurales. 

\

**MODELO 8**

```{r, echo=FALSE,warning=FALSE, message=FALSE}

modelo8 <- lm(LWS ~ AEDUC + EXP + EXP2+ZONA+REGION +AEDUC*ZONA+AEDUC*REGION,data = TABLAREME)

stargazer(modelo8, type = "text", title = "Resultados del Modelo 8", align = TRUE)
```


La interacción entre años de educación y vivir en una zona rural tiene un efecto negativo y significativo en el salario. Esto sugiere que el impacto de la educación en el salario es menor para aquellos que viven en zonas rurales, mas exactamente es menor en un 6.2%.
La interacción entre años de educación y vivir en la región de la Sierra no es estadísticamente significativa (p-value = 0.412877), lo que indica que el efecto de la educación en el salario no varía significativamente entre las regiones de la Sierra y la Costa.


\

**MODELO 9**

```{r , echo=FALSE,warning=FALSE, message=FALSE}

modelo9 <- lm(LWS ~ AEDUC + EXP + EXP2+ZONA+INFORM +AEDUC*ZONA+AEDUC*INFORM,data = TABLAREME)

stargazer(modelo9, type = "text", title = "Resultados del Modelo 9", align = TRUE)

```

La interacción entre años de educación y vivir en una zona rural tiene un efecto negativo y marginalmente significativo en el salario (p-value = 0.0823). Esto sugiere que el impacto de la educación en el salario es menor para aquellos que viven en zonas rurales comparado con zonas urbanas, mas exactos en un 3% menos.
La interacción entre años de educación y tener un empleo formal no es estadísticamente significativa (p-value = 0.8703), lo que indica que el efecto de la educación en el salario no varía significativamente entre empleos formales e informales.


\
**MODELO 10**

```{r , echo=FALSE,warning=FALSE, message=FALSE}

modelo10 <- lm(LWS ~ AEDUC + EXP + EXP2+ZONA+GEDUC +AEDUC*ZONA+AEDUC*GEDUC,data = TABLAREME)

stargazer(modelo10, type = "text", title = "Resultados del Modelo 10", align = TRUE)

```


La interacción entre años de educación y vivir en una zona rural tiene un efecto negativo y marginalmente significativo en el logaritmo del salario (p-value = 0.093438). Esto sugiere que el impacto de la educación en el salario es menor para aquellos que viven en zonas rurales comparado con zonas urbanas.

PARA SECUNDARIA
La interacción entre años de educación y tener educación secundaria no es estadísticamente significativa (p-value = 0.711866), lo que indica que el efecto de la educación en el salario no varía significativamente para aquellos con educación secundaria, con respecto a los de primeria.

PARA SUP. NO UNI.
La interacción entre años de educación y tener educación superior no universitaria no es estadísticamente significativa (p-value = 0.815342).

PARA SUP. NO UNI.
La interacción entre años de educación y tener educación superior no universitaria no es estadísticamente significativa (p-value = 0.815342).

\

**MODELO 11**

```{r , echo=FALSE,warning=FALSE, message=FALSE}

modelo11 <- lm(LWS ~ AEDUC + EXP + EXP2+ZONA+SEXO+REGION +AEDUC*ZONA+AEDUC*SEXO+AEDUC*REGION,data = TABLAREME)

stargazer(modelo11, type = "text", title = "Resultados del Modelo 11", align = TRUE)


```


La interacción entre años de educación y vivir en una zona rural tiene un efecto negativo y significativo en el salario (p-value = 0.000185). Esto sugiere que el impacto de la educación en el salario es menor para aquellos que viven en zonas rurales comparado con zonas urbanas.
La interacción entre años de educación y ser mujer no es estadísticamente significativa (p value = 0.173776), lo que indica que el efecto de la educación en el salario no varía significativamente entre hombres y mujeres.
La interacción entre años de educación y vivir en la región de la sierra no es estadísticamente significativa (p-value = 0.550085) lo que quiere decir que vivir en estas dos distintas regiones, no impacta en los retorno educacio.



\

**MODELO 12**

```{r, echo=FALSE,warning=FALSE, message=FALSE}

modelo12 <- lm(LWS ~ AEDUC + EXP + EXP2+ZONA+SEXO+INFORM +AEDUC*ZONA+AEDUC*SEXO+AEDUC*INFORM,data = TABLAREME)

stargazer(modelo12, type = "text", title = "Resultados del Modelo 12", align = TRUE)

```

La interacción entre años de educación y vivir en una zona rural tiene un efecto negativo y significativo en elsalario (p-value = 0.031069). Esto sugiere que el impacto de la educación en el salario es menor para aquellos que viven en zonas rurales comparado con zonas urbanas.
La interacción entre años de educación y ser mujer es marginalmente significativa (p-value = 0.054885), lo que indica que el efecto de la educación en el salario podría variar ligeramente entre hombres y mujeres.
La interacción entre años de educación y tener un empleo formal no es estadísticamente significativa (p-value = 0.539750), lo que sugiere que el efecto de la educación en el salario no varía significativamente entre empleos formales e informales.

\

**MODELO 13**

```{r, echo=FALSE,warning=FALSE, message=FALSE}

modelo13 <- lm(LWS ~ AEDUC + EXP + EXP2+ZONA+SEXO+GEDUC +AEDUC*ZONA+AEDUC*SEXO+AEDUC*GEDUC,data = TABLAREME)

stargazer(modelo13, type = "text", title = "Resultados del Modelo 13", align = TRUE)

```


La interacción entre años de educación y vivir en una zona rural tiene un efecto negativo y marginalmente significativo en el logaritmo del salario (p-value = 0.093438). Esto sugiere que el impacto de la educación en el salario es menor para aquellos que viven en zonas rurales comparado con zonas urbanas.

PARA SECUNDARIA
La interacción entre años de educación y tener educación secundaria no es estadísticamente significativa (p-value = 0.711866), lo que indica que el efecto de la educación en el salario no varía significativamente para aquellos con educación secundaria, con respecto a los de primeria.

PARA SUP. NO UNI.
La interacción entre años de educación y tener educación superior no universitaria no es estadísticamente significativa (p-value = 0.815342).

PARA SUP UNI
La interacción entre años de educación y tener educación superior universitaria es significativa y positiva (p-value = 0.000996). Esto sugiere que el impacto de la educación en el salario es mayor para aquellos con educación superior universitaria, con respecto a los de primaria.


\

**MODELO 14**

```{r , echo=FALSE,warning=FALSE, message=FALSE}

modelo14 <- lm(LWS ~ AEDUC + EXP + EXP2+ZONA+INFORM+GEDUC +AEDUC*ZONA+AEDUC*INFORM+AEDUC*GEDUC,data = TABLAREME)

stargazer(modelo14, type = "text")


```


La interacción entre años de educación y vivir en una zona rural no es significativa, lo que sugiere que el efecto de la educación en el salario no varía mucho entre zonas rurales y urbanas.
La interacción entre años de educación y tener empleo formal no es significativa, indicando que el retorno de la educación no difiere significativamente entre empleos formales e informales.
Sin embargo, la interacción entre años de educación y tener educación superior universitaria es significativa y positiva, sugiriendo que el retorno de la educación es mayor para aquellos con educación superior universitaria



\

**MODELO 15**

```{r , echo=FALSE,warning=FALSE, message=FALSE}

modelo15 <- lm(LWS ~ AEDUC + EXP + EXP2+ZONA+REGION+GEDUC +AEDUC*ZONA+AEDUC*REGION+AEDUC*GEDUC,data = TABLAREME)

stargazer(modelo15, type = "text", title = "Resultados del Modelo 15", align = TRUE)

```


La interacción entre años de educación y vivir en una zona rural no es significativa, lo que sugiere que el efecto de la educación en el salario no varía mucho entre zonas rurales y urbanas.
La interacción entre años de educación y tener empleo formal no es significativa, indicando que el retorno de la educación no difiere significativamente entre empleos formales e informales.
Sin embargo, la interacción entre años de educación y tener educación superior universitaria es significativa y positiva, sugiriendo que el retorno de la educación es mayor para aquellos con educación superior universitaria


\

**MODELO 16**

```{r , echo=FALSE,warning=FALSE, message=FALSE}

modelo16 <- lm(LWS ~ AEDUC + EXP + EXP2+ZONA+REGION+INFORM +AEDUC*ZONA+AEDUC*REGION+AEDUC*INFORM,data = TABLAREME)

stargazer(modelo16, type = "text", title = "Resultados del Modelo 16", align = TRUE)

```

La interacción entre años de educación y vivir en una zona rural es negativa y significativa, indicando que el impacto de la educación en el salario es menor para quienes viven en zonas rurales.
Las interacciones entre años de educación y vivir en la región de la Sierra, así como entre años de educación y tener empleo formal, no son significativas, lo que sugiere que estos factores no modifican significativamente el retorno de la educación.


\

**MODELO 17**

```{r , echo=FALSE,warning=FALSE, message=FALSE}

modelo17 <- lm(LWS ~ AEDUC + EXP + EXP2+SEXO+REGION+INFORM +AEDUC*SEXO+AEDUC*REGION+AEDUC*INFORM,data = TABLAREME)

stargazer(modelo17, type = "text", title = "Resultados del Modelo 17", align = TRUE)

```

La interacción entre años de educación y ser mujer es marginalmente significativa y positiva, lo que sugiere que el retorno de la educación podría ser ligeramente mayor para mujeres.
La interacción entre años de educación y vivir en una zona rural es negativa y significativa, indicando un menor retorno de la educación para quienes viven en zonas rurales.
Las interacciones entre años de educación y vivir en la región de la Sierra, así como entre años de educación y tener empleo formal, no son significativas, sugiriendo que estos factores no afectan significativamente el retorno de la educación.

\

**MODELO 18**

```{r, echo=FALSE,warning=FALSE, message=FALSE}

modelo18 <- lm(LWS ~ AEDUC + EXP + EXP2+SEXO+REGION+GEDUC +AEDUC*ZONA+AEDUC*REGION+AEDUC*GEDUC,data = TABLAREME)

stargazer(modelo18, type = "text", title = "Resultados del Modelo 18", align = TRUE)

```

La interacción entre años de educación y ser mujer no es significativa, sugiriendo que el retorno de la educación no varía significativamente entre géneros.
La interacción entre años de educación y vivir en una zona rural no es significativa, indicando que el retorno de la educación no varía significativamente entre zonas rurales y urbanas.
La interacción entre años de educación y tener educación superior universitaria es significativa y positiva, lo que indica un mayor retorno de la educación para aquellos con educación superior universitaria.

\

**MODELO 19**

```{r , echo=FALSE,warning=FALSE, message=FALSE}

modelo19 <- lm(LWS ~ AEDUC + EXP + EXP2+SEXO+INFORM+GEDUC +AEDUC*ZONA+AEDUC*INFORM+AEDUC*GEDUC,data = TABLAREME)

stargazer(modelo19, type = "text", title = "Resultados del Modelo 19", align = TRUE)

```

La interacción entre años de educación y ser mujer es marginalmente significativa y positiva, sugiriendo que el retorno de la educación podría ser ligeramente mayor para mujeres.
La interacción entre años de educación y vivir en una zona rural es negativa y significativa, indicando un menor retorno de la educación para quienes viven en zonas rurales.
Las interacciones entre años de educación y vivir en la región de la Sierra, así como entre años de educación y tener empleo formal, no son significativas, sugiriendo que estos factores no afectan significativamente el retorno de la educación.
Conclusiones Generales
La educación tiene un impacto positivo en los salarios, pero este impacto varía según factores como el género, la zona geográfica, y el tipo de educación.
Las mujeres podrían experimentar un retorno ligeramente mayor de la educación en comparación con los hombres.
Vivir en zonas rurales tiende a reducir el retorno de la educación.
Tener educación superior universitaria incrementa significativamente el retorno de la educación.
Estos hallazgos son importantes para la formulación de políticas educativas y de desarrollo regional, destacando la necesidad de enfoques específicos para maximizar el impacto de la educación en diferentes grupos y regiones.

\
**MODELO20**

```{r , echo=FALSE,warning=FALSE, message=FALSE}

modelo20 <- lm(data = subset(TABLAREME, SEXO=="MASCULINO"& ZONA=="RURAL" & REGION=="COSTA"), LWS ~ AEDUC + EXP + EXP2 +
              + INFORM +GEDUC) 

stargazer(modelo20, type = "text", title = "Resultados del Modelo 20", align = TRUE)

```

Tener un empleo formal está asociado con un aumento de los salarios en relacion a un empelo informal
Tener educación secundaria, en comparación con tener educACION PRIMARIA, está asociado con una reducción de los salarios
El coeficiente no es significativo, sugiriendo que tener educación superior no universitaria no tiene un impacto claro en el retorno del salario, tener educación superior universitaria, en comparación a tener educacion primaria, refleja una alta brecha en el retorno del salario

\

**DISCUSION DE RESULTADOS**

Durante los análisis, se encontraron complicaciones al verificar la normalidad de los residuos. La normalidad de los residuos es un supuesto importante en la regresión lineal clásica, ya que afecta la validez de las pruebas de hipótesis y los intervalos de confianza.

Pruebas de Normalidad: Las pruebas como el test de Shapiro-Wilk y los gráficos qq mostraron desviaciones significativas de la normalidad, lo que sugiere que los residuos no seguían una distribución normal.

Análisis Robustos: Para mitigar los problemas de normalidad y heterocedasticidad, se aplicaron métodos robustos como la regresión robusta y los errores estándar robustos. Estos métodos son menos sensibles a las violaciones de los supuestos clásicos y proporcionan estimaciones más fiables en presencia de residuos no normales y heterocedasticidad.

El analisis de las disparidades de ingresos entre diferentes grupos demográficos (por ejemplo, género, zona rural/urbana) permite diseñar intervenciones específicas para reducir la desigualdad. 


**COCLUSIONES**
  
  
  En este trabajo se pretendía validar un modelo que nos permitiera predecir, interpretar y explicar aspectos de 
discriminación salarial y retornos de la educación según características sociodemográficas de la población de La
Libertad. Primero, es conveniente indicar lo que se obtuvo al realizar la validación del modelo presentado en este 
trabajo, ya que en primer lugar se planteó un modelo con el que principalmente buscamos analizar la diferencia salarial 
en La Libertad con respecto a la región (Costa y Sierra), la zona (Rural, Urbana), tipo de trabajo (Formal e Informal),
sexo (Varón y Mujer), grado de educación (primaria, secundaria, superior no universitaria y superior universitaria) y
también se buscó obtener los niveles de retorno de la educación para cada una de las variables anteriormente 
mencionadas.

La validación del modelo nos mostró que, después de hacer los análisis de normalidad de los residuos en los que se 
aplicó el test de Jarque-Bera y la teoría de MCO asintóticos, se concluyó que estos siguen una distribución normal. 
Con respecto a la homocedasticidad, después de haber aplicado el test de Durbin-Watson, se vio que los residuos del 
modelo tienen una varianza constante. En lo que respecta a la colinealidad, se aplicó el factor de inflación de la 
varianza (VIF), el cual mostró que las variables tienen un VIF menor que 10, indicando que la colinealidad no es un 
problema en el modelo. Respecto a la exogeneidad, se utilizó el test de Hausman, donde se observó que no hay problema
de endogeneidad. Finalmente, se realizaron contrastes de hipótesis en los que se comprobó la significatividad de las 
variables, variando de manera clara con respecto a cada regresión que se realizó. Además de estos análisis, se tomaron 
en cuenta teoría econométrica adicional, como la de MCO asintóticos y la teoría del límite central. Con base en estos
procedimientos, se puede confiar en la validez de los resultados y conclusiones obtenidas, ya que se reducen los 
posibles sesgos y errores en las estimaciones, garantizando la eficiencia y consistencia de los estimadores.

Por otro lado, esto también nos dice que el modelo se puede utilizar para realizar análisis de pruebas de hipótesis
e inferencias en La Libertad. De acuerdo a las regresiones generales realizadas, se pudo demostrar que en la mayoría 
de los casos los varones tienen mayor retorno de la educación que las mujeres, mostrando la existencia de 
discriminación salarial con respecto a las mujeres. En cuanto a las regiones, se pudo ver en la mayoría de los
casos que los de la Costa tienen mayor retorno de la educación que los de la Sierra, pero en algunos casos este 
coeficiente no es significativo, lo que quiere decir que los retornos de la educación son similares por región.
De manera similar, con respecto al tipo de trabajo, los retornos de la educación son casi similares en algunos 
casos, pero también hay más casos en los que los que tienen trabajo formal tienen mayor retorno de la educación
con respecto a los que tienen trabajo informal. En lo que respecta a la zona, los individuos de las zonas urbanas
tienen mayor retorno de la educación que los de las zonas rurales. Finalmente, se observó que no hay tanta diferencia
de los retornos de la educación de los de secundaria con respecto a los de primaria; en algunos casos, incluso se 
muestra que los de primaria tienen mayor retorno de la educación que los de superior no universitaria. Sin embargo,
donde sí se muestra una diferencia significativa de los retornos de la educación es en los que tienen grado de 
educación universitaria, quienes tienen un mayor retorno con respecto a los de primaria.

Finalmente, los resultados muestran que las conclusiones empíricas que se pueden sacar con respecto a la 
discriminación y los retornos de la educación de una región no se cumplen exactamente como se mencionaba en
el párrafo anterior con respecto a La Libertad. Por ello, es muy importante realizar un modelo basado en una 
teoría que lo respalde y añadir las variables que uno crea conveniente según sea el fin de la investigación, 
yo claro, teniendo en cuenta la validación del modelo.


**REFERNCIAS BIBLIOGRAFICAS**
  
  Martínez, P. (2021). Desarrollo económico y social en La Libertad: políticas de inversión pública. Editorial Desarrollo Regional

Freire, J & Tejeiro, M. (2010). La ecuacion de mincer y las tasas de rendimiento de la educación en Galicia. https://2010.economicsofeducation.com/user/pdfsesiones/095.pdf


**ANEXOS**
  
  R Markdown





























