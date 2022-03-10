---
title: "TRABAJO FINAL: INTERPRETACIÓN DE DATOS DE UNA ENCUESTA SOBRE MIOPÍA"
author: "Oier Azurmendi, Maite Telletxea, Sergio Peñas"
date: "9/3/2022"
output: html_document
---
Para este trabajo se parte de dos dataframes (X_train y Y_train). Poseen 49 y 4 variables, respectivamente. 

Se trata de realizar un programa en el cual a partir de los datos de X_train (incomes) y de Y_train (outcomes) se puedan realizar predecicciones sobre los outputs. Es decir, hay que modelar las cuatro categorías del Y_train. Se preparará una función la cual recibirá un valor x con variables predictoras y un valor y. Finalmente, a partir de un X_test (lo que se debe predecir) se comprobará la robustez de los modelos con un Y_test.

*Cargar librerías y paquetes necesarios:*

*Cargar datos:* Se cargan los dos dataframes con los que se va a trabajar.

*EDA: análisis exploratorio de los datos*

Sirve para echar un primer vistazo a los datos, del tipo de variables que poseen los dataframes, y de comprobar la existencia de datos faltantes (NAs). En este punto, se realizará por tanto un primer tratamiento de los datos, se corregirán los NAs y se normalizarán y escalarán las variables necesarias.

Primero se va a analizar el dataframe Y_train (variables dependientes), que consta de 4 variables:

  - M: si el encuestado es miope o no (SÍ/NO)
  - MM: si el encuestado es miope magno o no (SÍ/NO)
  - Combo: si el encuestado es miope, miope magno o control (M, MM, C)
  - DCombo: si el encuestado es miope (dos tipos), miope magno o control (M1, M2, MM, C)

*MODELOS*

En primer lugar, tratamos de determinar el método que se va a usar para elegir el valor óptimo de k. Para ello se usa el comando 'trainControl'. En este caso, se ha fijado validación cruzada en 3 partes. Se dividen los datos en 3 submuestras del mismo tamaño.

*NEURAL NETWORKS*

Primero, se utiliza el método de redes neuronales (nnet), apliacado para cada variable respuesta. Hemos decidido usar la métrica ROC como referencia para evaluar el rendimiento del modelo. La curva ROC muestra la relación entre la tasa de verdaderos positivos (TPR) y la tasa de falsos positivos (FPR) del modelo.

*RANDOM FOREST*

Hemos decidido aplicar también otros métodos, como el randomForest. Este algoritmo trabaja creando múltiples árboles de decisión y después combinando el outcome generado en cada uno de esos árboles. Al final, el árbol de decisión es un modelo de clasificación que obtiene información en cada nodo.

*KNN*

Por último, para cada variable respuesta también aplicamos el método de clasificación knn. Este método trata de identificar los vecinos más cercanos con respecto al k, calculando la distancia euclidea, donde k puede ser un número especificado por el usuario. Normalmente, por defecto suele ser la raíz cuadrada del número de observaciones,que se redondea a un número impar para evitar el empate de categorías. En este caso, la fase de entrenamiento simplemente sirve para guardar la información de una forma estructurada, en lugar de crear un modelo. Después, tras almacenar los datos estructurados, se obtiene un vector con la predicción de categorías que hemos hecho, y que después compararemos con las categorías reales.

*FUNCIÓN DE PREDICCIÓN*

En este apartado, encontramos la función de predicción (predict_function), que será la que se deberá utilizar para predecir las 4 variables respuesta. Su input será un data frame con variables predictoras y su output será un data frame con las 4 variables respuesta predecidas (4 columnas).
Tras la función, hay dos comandos comentados, que deberéis descomentar:

  - El primero leerá el test data (introducir el nombre del archivo) y creará un dataframe llamado 'dataset' para ese test data.
  - El segundo aplicará la función 'predict function' a 'dataset' y creará un nuevo dataframe, llamado 'prediccion_final'.

...