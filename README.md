![BDML-Banner](views/BDML.jpg)


# TALLER 3: Prediccion de la pobreza en Colombia


### Resumen del trabajo
Este trabajo busca construir un modelo predictivo que permita clasificar los hogares en condición de pobreza a partir de la información recolectada por el DANE, en el marco de la Misión para el Empalme de las Series de Empleo, Pobreza y Desigualdad (MESEP) . El interés principal de este estudio surge de la necesidad de identificar aquellos hogares que se encuentran por debajo de la línea de pobreza para enfocar las políticas públicas que buscan reducir esta problemática, tales como la focalización y distribución de subsidios a poblaciones vulnerables.

Se parte de una base que consolida datos relevantes a nivel de hogar, considerando características del jefe del hogar (sexo, edad, nivel educativo, tipo de trabajo, entre otras), del grupo familiar (número de integrantes, porcentaje de ocupación, promedio de años de ocupación, entre otras), variables de ingreso (ingresos laborales, primas, subsidios, entre otras) y características de la vivienda (número de habitaciones, condición de tenencia, entre otras). Para identificar el modelo predictivo con mejor ajuste se definieron dos aproximaciones, por un lado, se plantearon modelos de clasificación tomando como variable independiente la dummy que indica si un hogar este clasificado como pobre (1) o no (0). Por otro lado, se plantearon modelos de predicción del ingreso de los hogares, para posteriormente identificar si el hogar es pobre o no con respecto a la línea de pobreza. Como resultado, se identifica que ambas aproximaciones resultan en una precisión (Accuracy) entre 78% y 86%, donde el modelo que presenta el mejor ajuste corresponde al modelo de clasificación a través de regresión logística con regularización por elastic-net (modelo 4) con una precisión del 86%. 

Como principal conclusión, no se evidencian mayores diferencias en la capacidad predictiva de los diferentes modelos desarrollados, por lo que las mejoras de ajuste se dan por la inclusión y/o exclusión de variables de interés, en donde las principales variables para la predicción son el porcentaje de ocupados, la edad, el tipo de trabajo e interacciones con el género del jefe del hogar. Finalmente, resulta relevante para la selección del modelo la menor cantidad de predictores y el menor costo computacional del modelo seleccionado.

### Estructura del repositorio

Los archivos en este repositorio están contenidos en las siguientes carpetas:

- `document`: Contiene los documentos revisados para el desarrollo del trabajo, incluyendo el [documento final](document/Taller_3.pdf) del trabajo.
- `scripts`: Contiene todos los scripts en R para el desarrollo del trabajo.
- `stores`: Contiene los data sets descargados de la pagina web de la competencia en [Kaggle](https://www.kaggle.com/competitions/uniandes-bdml-202313-ps31) y el dataset final una vez realizado el procesamiento de datos descrito en el anexo 1.
- `views`: Contiene todas las imágenes y tablas generadas, las cuales se incluyen en el  [documento final](document/Taller_3.pdf)  del trabajo.


### Integrantes

Jose Nicolás Barragán Mendez - Cod. est. 20133204 

Andrea Yeraldine Clavijo Mora- Cod. est. 202213310 

Jazmine Roxana Galdos Garreta - Cod. est. 202120623 

Sergio Alejandro Jimenez Oviedo - Cod. est. 202214467 

David Stiven Peralta Mendieta - Cod. est. 201718234 
