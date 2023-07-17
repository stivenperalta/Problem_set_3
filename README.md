![BDML-Banner](views/BDML.jpg)


# TALLER 2: Prediccion de precios de inmuebles en Chapinero

Este repositorio contiene el segundo taller desarrollado en el marco del curso de Big Data y Machine Learning de la Facultad de Economia de la Universidad de los Andes durante el periodo intersemestral de 2023, realizado siguiendo las indicaciones definidas en el   [problem set](document/Problem_Set_2.pdf) del curso.

### Resumen del trabajo

Este trabajo busca construir un modelo predictivo para pronosticar precios de vivienda. La principal motivación para construir estos modelos se basa en los postulados de la teoría de los precios hedónicos en la que se incluyen atributos inherentes a las características de la vivienda para determinar su valor comercial. Particularmente, en este problem set se busca predecir los precios de los inmuebles en Chapinero a partir de sus características estructurales (área, tipo de inmueble, número de habitaciones, baños, amenities, entre otros), así como características del entorno rescatadas del análisis geoespacial (estrato socioeconómico, tasas de criminalidad, índice de espacio público efectivo, densidad poblacional, cercanía a parques, colegios, centros comerciales, sitios turísticos, transporte público, entre otros).
Con este objetivo en mente, se elaboraron diferentes modelos predictivos, con metodologías como lasso, ridge, elastic net, random forest, entre otros, de los cuales se profundiza en los 5 mejores modelos predictivos con mejor desempeño, medidos por el criterio del error absoluto medio (MAE). Como resultado, se obtiene que Boosting con validación cruzada por barrio presenta el mejor desempeño en la predicción de precios de vivienda en la localidad de Chapinero, con un MAE fuera de muestra de COP 214.332.282. No obstante, los modelos dentro de muestra tienen a sobreajustarse aun cuando se valoran métodos de validación cruzada que corrigen correlación espacial.

### Estructura del repositorio

Los archivos en este repositorio están contenidos en las siguientes carpetas:

- `document`: Contiene los documentos revisados para el desarrollo del trabajo, incluyendo el [documento final](document/Taller_2.pdf) del trabajo.
- `scripts`: Contiene todos los scripts en R para el desarrollo del trabajo.
- `stores`: Contiene los data sets descargados de la pagina web de la competencia en [Kaggle](https://www.kaggle.com/competitions/uniandes-bdml-202313-ps2) y el dataset final unavez realizado el procesamiento de datos descrito en el  [anexo 1](document/Anexo_1.pdf). Los datos de fuentes externas tienen un peso muy alto para este repositorio, pero se pueden descargar en este [enlace](https://drive.google.com/file/d/1pvOZqn-tUOfV-cS8v01EGpBHa63mAj9E/view) de acuerdo con lo descrito en el [anexo 1](document/Anexo_1.pdf).
- `views`: Contiene todas las imágenes y tablas generadas, las cuales se incluyen en el  [documento final](document/Taller_2.pdf)  del trabajo.


### Integrantes

Jose Nicolás Barragán Mendez - Cod. est. 20133204 

Andrea Yeraldine Clavijo Mora- Cod. est. 202213310 

Jazmine Roxana Galdos Garreta - Cod. est. 202120623 

Sergio Alejandro Jimenez Oviedo - Cod. est. 202214467 

David Stiven Peralta Mendieta - Cod. est. 201718234 
