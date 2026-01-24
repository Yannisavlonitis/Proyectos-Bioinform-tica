setwd("C:/Users/yanni/Desktop/MASTER/CUATRI 1/Estadística y R/Act1EyR")
df2 <- read_csv("C:/Users/yanni/Desktop/MASTER/CUATRI 1/Estadística y R/Act1EyR/MUBioinfo_dataset_genes_oct2025.csv")

# Cargo bibliotecas
library(readr)
library(ggplot2)
install.packages("ggplot2") #para instalar el paquete 
library(pheatmap)
library(ComplexHeatmap)
library(patchwork)
install.packages("patchwork")
library(dendextend)

################################################################################1

# Tengo estos genes: "AQ_ESR1", "AQ_HER2", "AQ_BRCA1", "AQ_BRCA2", "AQ_MKI67", "AQ_GATA3", "AQ_FOXA1", "AQ_CCND1", "AQ_CDH1", "AQ_TP53"
# Voy a crear dos cajas para cada gen: caja trata y caja trataB

#AQ_ESR1
g0 <- ggplot(data = df2, aes(x = tratamiento, y = AQ_esr1, fill = tratamiento)) + #Añado g0 para asignar cada boxplot a un valor g, luego lo usaré para unir todos los graficos en patchwork
  geom_boxplot()+
  labs(title = "Expresión de ESR1", # Definir los nombres de ejes y título (no el estilo)
       x = "Tratamiento", 
       y = "Valor AQ")+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5, 
                                  size = 10,
                                  face = "bold"),
        axis.title.x = element_text(size = 7, face = "bold"), # Eje X en negrita
        axis.title.y = element_text(size = 7, face = "bold"), # Eje Y en negrita
        axis.text.x = element_text(size = 7),                  # Texto de ticks eje X
        axis.text.y = element_text(size = 7) ) +  # Estilo de ejes y título, 
  scale_fill_manual(values = c("violet", "lightgreen"))
  
#AQ_HER2
g1 <- ggplot(data = df2, aes(x = tratamiento, y = AQ_her2, fill = tratamiento)) +
  geom_boxplot()+
  labs(title = "Expresión de HER2", 
       x = "Tratamiento", 
       y = "Valor AQ")+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5, 
                                  size = 10,
                                  face = "bold"),
        axis.title.x = element_text(size = 7, face = "bold"), 
        axis.title.y = element_text(size = 7, face = "bold"), 
        axis.text.x = element_text(size = 7),                  
        axis.text.y = element_text(size = 7) ) +   
  scale_fill_manual(values = c("violet", "lightgreen"))
  
#AQ_BRCA1
g2 <- ggplot(data = df2, aes(x = tratamiento, y = AQ_brca1, fill = tratamiento))+
  geom_boxplot()+
  labs(title = "Expresión de BRCA1", 
       x = "Tratamiento", 
       y = "Valor AQ")+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5, 
                                  size = 10,
                                  face = "bold"),
        axis.title.x = element_text(size = 7, face = "bold"), 
        axis.title.y = element_text(size = 7, face = "bold"), 
        axis.text.x = element_text(size = 7),                  
        axis.text.y = element_text(size = 7) ) +  
  scale_fill_manual(values = c("violet", "lightgreen"))

#AQ_BRCA2
g3 <- ggplot(data = df2, aes(x = tratamiento, y = AQ_brca2, fill = tratamiento))+
  geom_boxplot()+
  labs(title = "Expresión de BRCA2",
       x = "Tratamiento", 
       y = "Valor AQ")+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5, 
                                  size = 10,
                                  face = "bold"),
        axis.title.x = element_text(size = 7, face = "bold"), 
        axis.title.y = element_text(size = 7, face = "bold"), 
        axis.text.x = element_text(size = 7),                  
        axis.text.y = element_text(size = 7) ) +  
  scale_fill_manual(values = c("violet", "lightgreen"))

#AQ_MKI67
g4 <- ggplot(data = df2, aes(x = tratamiento, y = AQ_mki67, fill = tratamiento))+
  geom_boxplot()+
  labs(title = "Expresión de MKI67", 
       x = "Tratamiento", 
       y = "Valor AQ")+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5, 
                                  size = 10,
                                  face = "bold"),
        axis.title.x = element_text(size = 7, face = "bold"), 
        axis.title.y = element_text(size = 7, face = "bold"), 
        axis.text.x = element_text(size = 7),                  
        axis.text.y = element_text(size = 7) ) +  
  scale_fill_manual(values = c("violet", "lightgreen"))

#AQ_GATA3
g5 <- ggplot(data = df2, aes(x = tratamiento, y = AQ_gata3, fill = tratamiento))+
  geom_boxplot()+
  labs(title = "Expresión de AQ_GATA3", 
       x = "Tratamiento", 
       y = "Valor AQ")+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5, 
                                  size = 10,
                                  face = "bold"),
        axis.title.x = element_text(size = 7, face = "bold"), 
        axis.title.y = element_text(size = 7, face = "bold"), 
        axis.text.x = element_text(size = 7),                  
        axis.text.y = element_text(size = 7) ) +  
  scale_fill_manual(values = c("violet", "lightgreen"))

#AQ_FOXA1
g6 <- ggplot(data = df2, aes(x = tratamiento, y = AQ_foxa1, fill = tratamiento))+
  geom_boxplot()+
  labs(title = "Expresión de FOXA1", 
       x = "Tratamiento", 
       y = "Valor AQ")+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5, 
                                  size = 10,
                                  face = "bold"),
        axis.title.x = element_text(size = 7, face = "bold"), 
        axis.title.y = element_text(size = 7, face = "bold"), 
        axis.text.x = element_text(size = 7),                  
        axis.text.y = element_text(size = 7) ) +  
  scale_fill_manual(values = c("violet", "lightgreen"))

#AQ_ccnd1
g7 <- ggplot(data = df2, aes(x = tratamiento, y = AQ_ccnd1, fill = tratamiento))+
  geom_boxplot()+
  labs(title = "Expresión de CCND1  ", 
       x = "Tratamiento", 
       y = "Valor AQ")+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5, 
                                  size = 10,
                                  face = "bold"),
        axis.title.x = element_text(size = 7, face = "bold"), 
        axis.title.y = element_text(size = 7, face = "bold"), 
        axis.text.x = element_text(size = 7),                  
        axis.text.y = element_text(size = 7) ) +  
  scale_fill_manual(values = c("violet", "lightgreen"))

#AQ_cdh1
g8 <- ggplot(data = df2, aes(x = tratamiento, y = AQ_cdh1, fill = tratamiento))+
  geom_boxplot()+
  labs(title = "Expresión de CDH1", 
       x = "Tratamiento", 
       y = "Valor AQ")+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5, 
                                  size = 10,
                                  face = "bold"),
        axis.title.x = element_text(size = 7, face = "bold"), 
        axis.title.y = element_text(size = 7, face = "bold"), 
        axis.text.x = element_text(size = 7),                  
        axis.text.y = element_text(size = 7) ) +  
  scale_fill_manual(values = c("violet", "lightgreen"))

#AQ_TP53
g9 <- ggplot(data = df2, aes(x = tratamiento, y = AQ_tp53, fill = tratamiento))+
  geom_boxplot()+
  labs(title = "Expresión de TP53", 
       x = "Tratamiento", 
       y = "Valor AQ")+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5, 
                                  size = 10,
                                  face = "bold"),
        axis.title.x = element_text(size = 7, face = "bold"), 
        axis.title.y = element_text(size = 7, face = "bold"), 
        axis.text.x = element_text(size = 7),                  
        axis.text.y = element_text(size = 7) ) +  
  scale_fill_manual(values = c("violet", "lightgreen"))

# Ahora que tengo cada boxplot, los uno con la libreria "patchwork"
((g0 | g1 | g2 ) /
    (g4 | g5 | g6) /
    (g7 | g8 | g9)) +
  plot_layout(guides = "collect") # Para poner solo una leyenda (guides)


################################################################################2
# Tengo los siguientes valores bioquímicos: "glucosa", "leucocitos", "linfocitos", "neutrofilos", "chol", "hdl", "hierro", "igA", "igE", "igG", "igN", "ldl", "pcr", "transferrina", "trigliceridos", "cpk"
# Creo un histograma para cada variable, donde se muestre su frecuencia. Con 30 bins

# Glucosa
h1 <- ggplot(df2, aes(x = glucosa))+ # En los histogramas solo hay variable x en aes()
  geom_histogram(aes(y = ..density..), fill = "skyblue", color = "darkblue", bins = 35)+ # Personalización, bins es el número de barras, aes(y = ..density..) para la curva de densidad
  labs(title = "Glucosa", 
      x = "Distribución", 
      y = "Frecuencia")+
  geom_density(color = "red", # geom_density() calcula y dibuja la curva suavizada basada en tus datos
               size = 1,
               lwd = 1.5)+ 
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5, # Cetrar el titulo
                                  size = 10, # Tamaño título
                                  face = "bold"),# Titulo en negrita
        axis.title.x = element_text(size = 7, face = "bold"), 
        axis.title.y = element_text(size = 7, face = "bold"), 
        axis.text.x = element_text(size = 7),                  
        axis.text.y = element_text(size = 7) ) 

# Leucocitos
h2 <- ggplot(df2, aes(x = leucocitos))+
  geom_histogram(aes(y = ..density..), fill = "skyblue", color = "darkblue", bins = 35)+ 
  labs(title = "Leucocitos", 
       x = "Distribución", 
       y = "Frecuencia")+
  geom_density(color = "red", 
               size = 1,
               lwd = 1.5)+ 
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5, 
                                  size = 10,
                                  face = "bold"),
        axis.title.x = element_text(size = 7, face = "bold"), 
        axis.title.y = element_text(size = 7, face = "bold"), 
        axis.text.x = element_text(size = 7),                  
        axis.text.y = element_text(size = 7) )

# Linfocitos
h3 <- ggplot(df2, aes(x = linfocitos))+
  geom_histogram(aes(y = ..density..), fill = "skyblue", color = "darkblue", bins = 35)+ 
  labs(title = "Linfocitos", 
       x = "Distribución", 
       y = "Frecuencia")+
  geom_density(color = "red", 
               size = 1,
               lwd = 1.5)+ 
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5, 
                                  size = 10,
                                  face = "bold"),
        axis.title.x = element_text(size = 7, face = "bold"), 
        axis.title.y = element_text(size = 7, face = "bold"), 
        axis.text.x = element_text(size = 7),                  
        axis.text.y = element_text(size = 7) )

# Neutrófilos
h4 <- ggplot(df2, aes(x = neutrofilos))+
  geom_histogram(aes(y = ..density..), fill = "skyblue", color = "darkblue", bins = 35)+ 
  labs(title = "Neutrofilos", 
       x = "Distribución", 
       y = "Frecuencia")+
  geom_density(color = "red", 
               size = 1,
               lwd = 1.5)+ 
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5, 
                                  size = 10,
                                  face = "bold"),
        axis.title.x = element_text(size = 7, face = "bold"), 
        axis.title.y = element_text(size = 7, face = "bold"), 
        axis.text.x = element_text(size = 7),                  
        axis.text.y = element_text(size = 7) )

# Colesterol
h5 <- ggplot(df2, aes(x = chol))+
  geom_histogram(aes(y = ..density..), fill = "skyblue", color = "darkblue", bins = 35)+ 
  labs(title = "Colesterol", 
       x = "Distribución", 
       y = "Frecuencia")+
  geom_density(color = "red", 
               size = 1,
               lwd = 1.5)+ 
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5, 
                                  size = 10,
                                  face = "bold"),
        axis.title.x = element_text(size = 7, face = "bold"), 
        axis.title.y = element_text(size = 7, face = "bold"), 
        axis.text.x = element_text(size = 7),                  
        axis.text.y = element_text(size = 7) )

# HDL
h6 <- ggplot(df2, aes(x = hdl))+
  geom_histogram(aes(y = ..density..), fill = "skyblue", color = "darkblue", bins = 35)+ 
  labs(title = "HDL", 
       x = "Distribución", 
       y = "Frecuencia")+
  geom_density(color = "red", 
               size = 1,
               lwd = 1.5)+ 
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5, 
                                  size = 10,
                                  face = "bold"),
        axis.title.x = element_text(size = 7, face = "bold"), 
        axis.title.y = element_text(size = 7, face = "bold"), 
        axis.text.x = element_text(size = 7),                  
        axis.text.y = element_text(size = 7) )

# Hierro
h7 <- ggplot(df2, aes(x = hierro))+
  geom_histogram(aes(y = ..density..), fill = "skyblue", color = "darkblue", bins = 35)+ 
  labs(title = "Hierro", 
       x = "Distribución", 
       y = "Frecuencia")+
  geom_density(color = "red", 
               size = 1,
               lwd = 1.5)+ 
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5, 
                                  size = 10,
                                  face = "bold"),
        axis.title.x = element_text(size = 7, face = "bold"), 
        axis.title.y = element_text(size = 7, face = "bold"), 
        axis.text.x = element_text(size = 7),                  
        axis.text.y = element_text(size = 7) )

# igA
h8 <- ggplot(df2, aes(x = iga))+
  geom_histogram(aes(y = ..density..), fill = "skyblue", color = "darkblue", bins = 35)+ 
  labs(title = "IgA", 
       x = "Distribución", 
       y = "Frecuencia")+
  geom_density(color = "red", 
               size = 1,
               lwd = 1.5)+ 
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5, 
                                  size = 10,
                                  face = "bold"),
        axis.title.x = element_text(size = 7, face = "bold"), 
        axis.title.y = element_text(size = 7, face = "bold"), 
        axis.text.x = element_text(size = 7),                  
        axis.text.y = element_text(size = 7) )

# IgE
h9 <- ggplot(df2, aes(x = ige))+
  geom_histogram(aes(y = ..density..), fill = "skyblue", color = "darkblue", bins = 35)+ 
  labs(title = "IgE", 
       x = "Distribución", 
       y = "Frecuencia")+
  geom_density(color = "red", 
               size = 1,
               lwd = 1.5)+ 
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5, 
                                  size = 10,
                                  face = "bold"),
        axis.title.x = element_text(size = 7, face = "bold"), 
        axis.title.y = element_text(size = 7, face = "bold"), 
        axis.text.x = element_text(size = 7),                  
        axis.text.y = element_text(size = 7) )

# IgG
h10 <- ggplot(df2, aes(x = igg))+
  geom_histogram(aes(y = ..density..), fill = "skyblue", color = "darkblue", bins = 35)+ 
  labs(title = "igG", 
       x = "Distribución", 
       y = "Frecuencia")+
  geom_density(color = "red", 
               size = 1,
               lwd = 1.5)+ 
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5, 
                                  size = 10,
                                  face = "bold"),
        axis.title.x = element_text(size = 7, face = "bold"), 
        axis.title.y = element_text(size = 7, face = "bold"), 
        axis.text.x = element_text(size = 7),                  
        axis.text.y = element_text(size = 7) )

# IgN
h11 <-ggplot(df2, aes(x = ign))+
  geom_histogram(aes(y = ..density..), fill = "skyblue", color = "darkblue", bins = 35)+ 
  labs(title = "IgN", 
       x = "Distribución", 
       y = "Frecuencia")+
  geom_density(color = "red", 
               size = 1,
               lwd = 1.5)+ 
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5, 
                                  size = 10,
                                  face = "bold"),
        axis.title.x = element_text(size = 7, face = "bold"), 
        axis.title.y = element_text(size = 7, face = "bold"), 
        axis.text.x = element_text(size = 7),                  
        axis.text.y = element_text(size = 7) )

# LDL
h12 <- ggplot(df2, aes(x = ldl))+
  geom_histogram(aes(y = ..density..), fill = "skyblue", color = "darkblue", bins = 35)+ 
  labs(title = "LDL", 
       x = "Distribución", 
       y = "Frecuencia")+
  geom_density(color = "red", 
               size = 1,
               lwd = 1.5)+ 
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5, 
                                  size = 10,
                                  face = "bold"),
        axis.title.x = element_text(size = 7, face = "bold"), 
        axis.title.y = element_text(size = 7, face = "bold"), 
        axis.text.x = element_text(size = 7),                  
        axis.text.y = element_text(size = 7) )

# PCR
h13 <- ggplot(df2, aes(x = pcr))+
  geom_histogram(aes(y = ..density..), fill = "skyblue", color = "darkblue", bins = 35)+ 
  labs(title = "PCR", 
       x = "Distribución", 
       y = "Frecuencia")+
  geom_density(color = "red", 
               size = 1,
               lwd = 1.5)+ 
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5, 
                                  size = 10,
                                  face = "bold"),
        axis.title.x = element_text(size = 7, face = "bold"), 
        axis.title.y = element_text(size = 7, face = "bold"), 
        axis.text.x = element_text(size = 7),                  
        axis.text.y = element_text(size = 7) )

# Transferrina
h14 <- ggplot(df2, aes(x = transferrina))+
  geom_histogram(aes(y = ..density..), fill = "skyblue", color = "darkblue", bins = 35)+ 
  labs(title = "Transferrina", 
       x = "Distribución", 
       y = "Frecuencia")+
  geom_density(color = "red", 
               size = 1,
               lwd = 1.5)+ 
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5, 
                                  size = 10,
                                  face = "bold"),
        axis.title.x = element_text(size = 7, face = "bold"), 
        axis.title.y = element_text(size = 7, face = "bold"), 
        axis.text.x = element_text(size = 7),                  
        axis.text.y = element_text(size = 7) )

# Triglicéridos
h15 <- ggplot(df2, aes(x = trigliceridos))+
  geom_histogram(aes(y = ..density..), fill = "skyblue", color = "darkblue", bins = 35)+ 
  labs(title = "Triglicéridos", 
       x = "Distribución", 
       y = "Frecuencia")+
  geom_density(color = "red", 
               size = 1,
               lwd = 1.5)+ 
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5, 
                                  size = 10,
                                  face = "bold"),
        axis.title.x = element_text(size = 7, face = "bold"), 
        axis.title.y = element_text(size = 7, face = "bold"), 
        axis.text.x = element_text(size = 7),                  
        axis.text.y = element_text(size = 7) )

# CPK
h16 <- ggplot(df2, aes(x = cpk))+
  geom_histogram(aes(y = ..density..), fill = "skyblue", color = "darkblue", bins = 35)+ 
  labs(title = "CPK", 
       x = "Distribución", 
       y = "Frecuencia")+
  geom_density(color = "red", 
               size = 1,
               lwd = 1.5)+ 
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5, 
                                  size = 10,
                                  face = "bold"),
        axis.title.x = element_text(size = 7, face = "bold"), 
        axis.title.y = element_text(size = 7, face = "bold"), 
        axis.text.x = element_text(size = 7),                  
        axis.text.y = element_text(size = 7) )

# Ahora combino todos los histogramas mediante patchwork
(h1 | h2 | h3 | h4) /
    (h5 | h6 | h7 | h8) /
    (h9 | h10 | h11 | h12) /
    (h13 | h14 | h15 | h16)


################################################################################3
# Hay que mapear todos los valores de expresión de genes para poder visualizar posibles patrones
# Hago un heatmap en el que visualize los datos crudos de todas las variables AQ

set.seed(1995) # Semilla de aleatorización predeterminada

df2_scaled <- scale(df2[sapply(df2, is.numeric)]) # scale() normaliza las variables centrándolas alrededor de una media de 0 y una desviación estándar de 1, asegurando que todas las variables contribuyan equitativamente al análisis
                                                  # sapply(df2, is.numeric) identifica las columnas numéricas

aq_df <- df2_scaled[, grepl("^AQ", colnames(df2_scaled))] # Para seleccionar columnas por nombre (solo queremos las columnas de genes), lo guardo en una nueva variable
aq_mat <- t(aq_df) # Poner AQ en el eje Y y los pacientes en el eje X, t() es para transponer, uso la variable anterior, que tiene los datos ya normalizados

# Clustering y dendrograma
hclust_gen <- hclust(dist(aq_mat), method = "complete") # hclust() aplica clustering jerárquico usando el método de enlace completo 

library(dendextend)

plot(as.dendrogram(hclust_gen), # plot(as.dendrogram()) para visualizar el dendrograma
     horiz = TRUE) # Dibuja el dendrograma de forma horizontal

# Anotación por clúster
my_gene_col <- cutree(as.dendrogram(hclust_gen), k = 3) # Corta el dendrograma en 3 grupos (k) y los guarda en un vector (my_gene_col) que asigna a cada gen un número de clúster (1-3)
my_gene_col <- data.frame(cluster = factor(my_gene_col)) # Convierte el vector en un data frame
                                                          # Este objeto (my_gene_col) se puede usar como annotation_row en pheatmap() para mostrar los clústeres como una barra lateral de colores.

# Heatmap con pheatmap (sin títulos por eje)
pheatmap(aq_mat,
         cutree_rows = 3,
         cutree_cols = 8,
         name = "Expresión", # Equivalente a labs() de ggplot2
         annotation_row = my_gene_col, # Añade anotaciones, my_gene_col es un data frame que contiene la asignación de cada gen a un clúster
         main = "Pacientes",
         fontsize_row = 7)

# Heatmap con ComplexHeatmap (más control)
Heatmap(aq_mat,
        name = "Expresión",
        row_km = 4,
        column_km = 10,
        row_title = "Genes",
        column_title = "Pacientes")


