setwd("C:/Users/yanni/Desktop/MASTER/CUATRI 1/Estadística y R/Act2EyR")

library(ggplot2)
library(stats)
library(car)
library(gtsummary)
library(dplyr)
library(nortest) # Contiene el test de Shapiro

dataframe1 <- read.csv("C:/Users/yanni/Desktop/MASTER/CUATRI 1/Estadística y R/Act2EyR/MUBioinfo_dataset_genes_oct2025.csv")
View(dataframe1)

########################## 1) Comprobar la normalidad de los genes y realizar una interpretación de los resultados obtenidos.

genes <- dataframe1 %>% select(starts_with("AQ")) # Filtro solo los las variables que empiezan por AQ (genes) mediante %>% select() del paquete dyplr y starts_with() del paquete gtsummary

View(genes)

normalidad <- sapply(genes, function(x){ # sapply(genes, function(x){...}) recorre cada columna de genes. En cada iteración, aplica la funcion x a todos los genes
  x <- x[!is.na(x)]  # 2) eliminar valores NA para no romper el test
  if(length(x) > 30){ # 3) Anderson-Darling si n>30
    p <- ad.test(x)$p.value 
  } else { # 4) Si n<30 Shapiro
    p <- shapiro.test(x)$p.value
  }
  return(p >= 0.05)   # 6) regla de decisión: TRUE = normalidad
})

normalidad # Observo los resultados

# Estadisticos importantes para gtsummary

stat_list <- lapply(normalidad, function(is_normal){ # La función lapply aplica una función a una lista o a un vector y devuelve una lista de la misma longitud que el objeto de entrada.
  if(is_normal) "{mean} ({sd})" # Si distribucion es normal, devuelve la media y sd
  else          "{median} ({p25}–{p75})" # Si distribucion es no normal, devuelve la mediana y cuartiles
})
names(stat_list) <- names(normalidad) # names() le pone nombre o recibe el nombre a o de un objeto

stat_list # Observo los resultados

# TABLA DE NORMALIDAD CORREGIDA

tabla_modelo1 <- data.frame(
  Variable = names(genes), # Nombres de cada uno de los genes
  Test_utilizado = ifelse(nrow(genes) > 30, # # Definir test utilizado según tamaño de muestra
                          "Anderson-Darling", 
                          "Shapiro-Wilk"),
  
  Valor_p = sapply(names(genes), function(g){ # Calcular p-value del test de normalidad
    x <- genes[[g]]
    x <- x[!is.na(x)]
    if(length(x) > 30) {ad.test(x)$p.value} 
    else {shapiro.test(x)$p.value}
    }),
  
  Interpretación = ifelse(normalidad, "Compatible con normalidad", "No normal")
)

tabla_modelo1

# TABLA GTSUMMARY + ESTADISTICOS

tabla_gtsummary <- genes %>%
  tbl_summary(
    statistic = stat_list,  # Usar la lista de estadísticos que gtsummary utilizara como plantilla
    digits = all_continuous() ~ 3) %>% # Mostrar 3 decimales
  modify_header(label ~ "Gen") %>%  # Cambiar nombre de la columna de variables (de Characteristics a Gen)
  modify_caption("Resumen descriptivo de los genes según normalidad")%>%

# Hasta aqui tengo una tabla gtsummary basica, que contiene la lista de genes con los estadisticos {median} ({p25}–{p75})
# Le voy a añadir el test utilizado y los ep-valores que estan almacenados en tabla_modelo1
  
  modify_table_body(~ .x %>% # .x es la tabla interna table_body de gtsummary
      # Unir los resultados de normalidad de tabla_modelo1 con la tabla gtsummary
      left_join( # left_join() añade datos correspondientes a las observaciones (es uno de los cuatro tipos de joins)
        tabla_modelo1 %>% 
        rename(label = Variable),  # renombrar 'Variable' de tabla_modelo1 por 'label' para unir correctamente. Para poder hacer un join (unir tablas), necesitamos que los nombres de las columnas coincidan.
        by = "label") %>% # El argumento by indica por qué columna se va a unir la tabla.
        mutate(p_value = formatC(Valor_p, format = "e", digits = 2)) # Crear columna 'p_value' con formato científico para mostrar claramente los valores muy pequeños
      ) %>% # OJO: acuerdate de poner el ultimo parentesis y la ultima pipe en una linea a parte para no liarte!
  
  # Cambiar nombres de columnas para que se vean bien en la tabla final
  modify_header(Test_utilizado ~ "Test utilizado") %>%
  modify_header(p_value ~ "p-valor")
  



tabla_gtsummary # Observo la tabla final



########################## 2) Calcular y analizar estadísticas descriptivas de los valores sin transformar de las variables bioquímicas, síntomas, sociodemográficas y de los genes

##Estadísticos de las variables continuas: bioquimicas
# Primero: Comprobar la normalidad de las var bioquimicas y realizar una interpretación de los resultados obtenidos.

bioquimicas <- dataframe1 %>% select(glucosa, leucocitos, linfocitos, neutrofilos, chol, hdl, ldl, trigliceridos, pcr, hierro, transferrina, cpk, iga, ige, igg, ign)

normalidad2 <- sapply(bioquimicas, function(x){ # sapply(genes, function(x){...}) recorre cada columna de genes. En cada iteración, aplica la funcion x a todos los genes
  x <- x[!is.na(x)]  # Eliminar valores NA para no romper el test
  if(length(x) > 30){ # Anderson-Darling si n>30
    p <- ad.test(x)$p.value 
  } else { # 4) Si n<30 Shapiro
    p <- shapiro.test(x)$p.value
  }
  return(p >= 0.05)   # Eegla de decisión: TRUE = normalidad
})

# Tabla de normalidad
tabla_modelo2 <- data.frame(
  Variable = names(bioquimicas), # Nombres de cada uno de los genes
  N = sapply(bioquimicas, 
             function(x) sum(!is.na(x))),  # Tamaño muestral por variable
  Test_utilizado = ifelse(nrow(bioquimicas) > 30, # Definir test utilizado según tamaño de muestra
                          "Anderson-Darling", 
                          "Shapiro-Wilk"),
  Valor_p = sapply(names(bioquimicas), function(g){ # Calcular p-value del test de normalidad
    x <- bioquimicas[[g]]
    x <- x[!is.na(x)]
    if(length(x) > 30) {ad.test(x)$p.value} 
    else {shapiro.test(x)$p.value}
  }),
  Interpretación = sapply(bioquimicas, function(x) {
    x <- x[!is.na(x)]
    p <- if (length(x) > 30) ad.test(x)$p.value else shapiro.test(x)$p.value
    ifelse(p >= 0.05, "Compatible con normalidad", "No normal")
  })
)

tabla_modelo2


# Estadisticos importantes para gtsummary (plantilla)
stat_list2 <- lapply(normalidad2, function(is_normal){ # La función lapply aplica una función a una lista o a un vector y devuelve una lista de la misma longitud que el objeto de entrada.
  if(is_normal) "{mean} ({sd})" # Si distribucion es normal, devuelve la media y sd
  else          "{median} ({p25}–{p75})" # Si distribucion es no normal, devuelve la mediana y cuartiles
})
names(stat_list2) <- names(normalidad2) # names() le pone nombre o recibe el nombre a o de un objeto

stat_list2 # Observo los resultados


# Observo los resultados con gtummary
resultados_bioq <- bioquimicas %>% 
  tbl_summary(
  statistic = stat_list2, # OJO: stat_list2 es la plantilla con la que gtsummary va a calcular los estadisticos
)%>%
modify_header(label ~ "Variable bioquimica") %>%  # Cambiar nombre de la columna de variables
  modify_caption("Estadisticos de las variables bioquimicas")

resultados_bioq # Tabla resultados



##Estadísticos de las variables categóricas: sintomas, sociodemograficas. Como no son ocontinuas, calculare las observaciones y proporcion de cada observacion (si/no, localizado, metastasico...)

sint_socio <-  dataframe1 %>% # Creo un dataframe solo con las variables categóricas
  select(where(is.factor) | where(is.character)) # where(is.factor) selecciona todas las columnas que son factores (factor).
                                                # where(is.character) selecciona todas las columnas que son de tipo carácter (character).

View(sint_socio) # Veo que se ha creado correctamente

resultados_categoricas <- sint_socio %>% 
  tbl_summary(
    statistic = list(all_categorical() ~ "{n} ({p}%)"),  # n es frecuencia absoluta y {p}% es la frecuencia relativa 
    digits = all_categorical() ~ c(0, 1)  # 0 decimales para n, 1 para %
  ) %>%
  modify_header(label ~ "Variable categorica") %>%
  modify_caption("Estadísticos de las variables categóricas")

resultados_categoricas





##2.1. Crear una tabla descriptiva utilizando el segundo modelo en la que se reflejen las variables de los genes en función del tipo de tratamiento y de la extensión tumoral.
# El primer estrato (tratamiento) va a dividir en dos: placebo/farmaco El segundo estrato (extension) va a dividir en dos: metastasico/localizado

genes2 <- dataframe1 %>% 
  select(starts_with("AQ"), extension, tratamiento) # Filtro solo los las variables que empiezan por AQ (genes), tratamiento y extension

tabla_gtsummary2 <- genes2 %>% # Selecciono el data frame genes2
  select(tratamiento, extension, all_of(starts_with("AQ"))) %>%
  tbl_strata(
    strata = tratamiento,# Creo el primer estrato: tratamiento
    .tbl_fun = ~ .x %>% # Función que define cómo construir cada tabla dentro de cada estrato.
      tbl_summary(
        by = extension,
        statistic = stat_list,
        digits = all_continuous() ~ 2 # Añado dos decimales porque son valores pequeños
      ) %>%
      add_p(
        test = all_continuous() ~ "kruskal.test",
        pvalue_fun = ~ style_pvalue(.x, digits = 3)
      )
  )

tabla_gtsummary2



##2.2 Crear una nueva variable que divida la edad en dos categorías, empleando como punto de corte la mediana: (1) < percentil 50 para la edad; (2) >= percentil 50 para la edad. 
##    Posteriormente, crear una nueva tabla descriptiva únicamente con las variables genéticas a partir de la siguiente referencia.

# Primero: Crear variable categórica de edad basada en la mediana
mediana_edad <- median(dataframe1$edad) # Calculo la mediana de la variable edad en el df original

dataframe1 <- dataframe1 %>%
  mutate(grupo_edad = factor( # mutate() modifica/crea columnas, en este caso va a crear la columna grupo_edad, las almacena como un factor
    ifelse(edad < mediana_edad, # ifelse() es una función que evalúa una condición y devuelve un valor u otro dependiendo del resultado.
           #if
           "< Percentil 50", # Si la edad es menor a la mediana calculada antes se asigna al grupo < Percentil 50
           #else
           ">= Percentil 50"), # Por el contrario, si es mayor se asigna al grupo >= PErcentil 50
    levels = c("< Percentil 50", ">= Percentil 50")
  ))

table(dataframe1$grupo_edad) # Compruebo la columna grupo_edad creada con mutate(grupo_edad = factor)


# Segundo: Recalcular stat_list con los nombres correctos de los genes, igual que en el ejercicio 1
genes_mediana <- dataframe1 %>% select(starts_with("AQ"))

stat_list_genes <- lapply(normalidad, 
                          function(is_normal){
  if(is_normal) "{mean} ({sd})" # Si es normal media y sd
  else          "{median} ({p25}–{p75})" # Si es no normal mediana y rangos intercuartilicos
})
names(stat_list_genes) <- names(genes_mediana)  # Asegurar que los nombres coincidan


# Tercero:  Tabla descriptiva de genes estratificada por los dos grupos de edad calculados en el priemr paso, que estan almacenados en dataframe1 gracias a mutate()
tabla_genes_edad <- dataframe1 %>%
  select(grupo_edad, starts_with("AQ")) %>% # Selecciono la columna de edad (donde se encuentran los dos grupos de edad) y la columna de genes (empiezan por AQ)
  tbl_summary(
    by = grupo_edad,
    statistic = stat_list_genes,  # Usar stat_list_genes como plantilla para calcular mis estadisticos
    digits = all_continuous() ~ 3 # Tres decimales
  ) %>%
  add_p(test = all_continuous() ~ "wilcox.test") %>% # Uso el test de wiloxon
  modify_header(label ~ "Gen") %>% # Cambio titulo de Characteristics a Gen
  modify_caption("Expresión génica teniendo en cuenta la mediana como divisor")

tabla_genes_edad # Observo la tabla
