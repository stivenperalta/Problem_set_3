![BDML-Banner](https://github.com/stivenperalta/Problem_set_1/assets/137315725/09325f91-f3df-45b0-9934-0816cb760b89)

# TALLER 1: PREDICCIÓN DEL INGRESO GEIH

Este repositorio contiene el primer taller desarrollado en el marco del curso de Big Data y Machine Learning de la Facultad de Economia de la Universidad de los Andes durante el periodo intersemestral de 2023, realizado siguiendo las indicaciones definidas en el   [problem set](document/Problem_Set_1%20%28preguntas%29.pdf) del curso.

Este trabajo busca analizar diferentes variables que afectan el comportamiento del salario *(edad de los trabajadores, sexo, nivel educativo, estrato socioeconomico, entre otras)*, y posteriormente, desarrollar un modelo predictivo del salario que pueda identificar *outliers* en los reportes de salario asi como características relevantes a considerar para la  identificación de individuos vulnerables, lo aterior con base en la información recogida por la Gran Encuesta Integrada de Hogares (GEIH) realizada por el Departamento Administrativo Nacional de Estadistica (DANE) en 2018.

### Resumen del trabajo

La subdeclaración de ingresos es un fenómeno que se presenta como una forma de reducir la carga fiscal de los contribuyentes: empresas y personas. Se estima que Colombia pierde anualmente cerca del 0,7% del PIB por este concepto en personas naturales. Comparado con America Latina, se encuentra por arriba de sus pares (Concha et al., 2017). Las disparidades del sistema tributario colombiano y vacíos en la norma han posibilitado que este fenómeno se mantenga. Se estima que la subdeclaración es superior al 50% en lo relacionado con el impuesto a la renta de personas naturales. A lo largo de este set Problem se desarrolla un modelo de predicción basado en características individuales usando datos de la Gran encuesta integrada de hogares (GEIH) del Departamento Administrativo Nacional de Estadística (DANE) para realizar una estimación de los ingresos. De este modo, se espera conocer, por esta vía, que variables determinan el ingreso. Esto podría, a futuro, servir como insumo para una mejor categorización de las personas que deben declarar como también poder establecer mejores mecanismos de recaudo. 

Entre los principales resultados se encontró en general que, variables como la experiencia, la educación, la edad y el estrato socioeconómico sirven para explicar el salario con un alto nivel de confianza. Respecto a la edad, se observó un comportamiento de orden cuadrático, con limite en una edad promedio de 57 años. Así mismo, se confirmó la existencia de una brecha salarial por género; en promedio, las mujeres devengan un salario 0.9% menor que los hombres, controlando por características generales de los individuos y del empleo. Finalmente, se abordaron modelos predictivos para el salario, en donde se identificó que la capacidad predictiva aumenta en la medida en que se incluyen más variables en el modelo de entrenamiento, llegando hasta tener una predicción con un error estándar distribuido normalmente y con un valor promedio de 0.34. Al analizar la distribución de esta predicción no se identificaron características atípicas que dieran cuenta de valores extremos a revisar en el marco de una política de control fiscal. Finalmente, se realizó la revisión de la bondad de ajuste de los modelos, en donde se identificó que el modelo predictivo que incluye condiciones como la experiencia, la educación, la edad y el estrato socioeconómico resulta consistente tanto a través del método de validación cruzada, como a través del método LOOCV. 

### Estructura del repositorio

Los archivos en este repositorio están contenidos en las siguientes carpetas:

- `document`: Contiene los documentos revisados para el desarrollo del trabajo, incluyendo el [documento final](document/Taller_1_problem_set_1.pdf) del trabajo.
- `scripts`: Contiene todos los scripts en R para el desarrollo del trabajo.
- `stores`: Contiene los data sets usados para el trabajo, corresponde a información de la GEIH-2018 extraída por web scrapping.
- `views`: Contiene todas las imágenes y tablas generadas, las cuales se incluyen en el [documento final](document/Taller_1_problem_set_1.pdf) del trabajo.


### Integrantes

Jose Nicolás Barragán Mendez - Cod. est. 20133204 

Andrea Yeraldine Clavijo Mora- Cod. est. 202213310 

Jazmine Roxana Galdos Garreta - Cod. est. 202120623 

Sergio Alejandro Jimenez Oviedo - Cod. est. 202214467 

David Stiven Peralta Mendieta - Cod. est. 201718234 

