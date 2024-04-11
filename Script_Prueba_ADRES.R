
#Cargar archivo de Municipios  y vacios como NA. 

install.packages("readxl")
library(readxl)
municipios <- read_excel("Municipios.xlsx", na ="NA")

#Esta base claramentente tiene varios errores de caracteres, Usare dos formas diferentes para solucionar estos problemas, mediante lista y mediante join. 

# Extraer la lista de https://www.dian.gov.co/atencionciudadano/formulariosinstructivos/Formularios/2012/departamentos_2012.pdf: 
lista_depa <- list(
  "Antioquia" = "05",
  "Atlántico" = "08",
  "Barranquilla D.E" = "09",
  "Santa Fe de Bogotá D.C." = "11",
  "Bolívar" = "13",
  "Cartagena D.E." = "14",
  "Boyaca" = "15",
  "Caldas" = "17",
  "Caquetá" = "18",
  "Cauca" = "19",
  "Cesar" = "20",
  "Córdoba" = "23",
  "Cundinamarca" = "25",
  "Chocó" = "27",
  "Huila" = "41",
  "La Guajira" = "44",
  "Magdalena" = "47",
  "Santamarta D.E" = "48",
  "Meta" = "50",
  "Nariño" = "52",
  "Norte de Santander" = "54",
  "Quindio" = "63",
  "Risaralda" = "66",
  "Santander" = "68",
  "Sucre" = "70",
  "Tolima" = "73",
  "Valle" = "76",
  "Arauca" = "81",
  "Casanare" = "85",
  "Putumayo" = "86",
  "San Andrés" = "88",
  "Amazonas" = "91",
  "Guainía" = "94",
  "Guaviare" = "95",
  "Vaupés" = "97",
  "Vichada" = "99"
)

# Mostrar la lista creada
print(lista_depa)

#Reemplazo de la lista de los Departamentos
library(plyr)
mun1 <- municipios %>%
  mutate(Departamento = mapvalues(Dep, from = lista_depa, to = names(lista_depa)))

#Tomar la base de municipios, tomada de DIVIPOLA https://www.datos.gov.co/Mapas-Nacionales/DIVIPOLA-C-digos-municipios/gdxc-w37w/data_preview: 


library(readr)
 pola <- read_csv("DIVIPOLA-_C_digos_municipios_20240405.csv")
 
#Extraer las columnas necesarias.
 
 library(stringr)
 polamun <- data.frame(
   
   Nombre.Municipio =  str_to_title(pola$"Nombre Municipio"),
   Código.Municipio =  pola$"Código Municipio")
 
 # Hacer el merge para reemplazar todos los municipios mal escritos 
 
mun2 <- merge(mun1, polamun, by.x = "Depmun", by.y = "Código.Municipio", all.x = TRUE)
 
#Eliminar la columna de municipio 
mun2 <- subset(mun2, select = -Municipio)

# Renombrar la columna 
names(mun2)[names(mun2) == "Nombre.Municipio"] <- "municipio"

#Finalmente subsanar Mapiripana que tiene un faltante 

#Subsanar el de Mapiripana municipio y listo 

mun2 <- mutate(mun2,
                        municipio = ifelse(Depmun == 94663, "Mapiripana", municipio)
)

mun2

# Ahora exportar la nueva base para subirla a SQLite3,  Para tener noción visual del proceso utilice DB Browser for SQLite, luego llamar desde R la base y trabajar acá. 

write.csv(mun2, "municipios.csv")



#Cargar archivo de Prestadores y vacios como NA. 

install.packages("readxl")
library(readxl)
prestadores <- read_excel("Prestadores.xlsx", na ="NA")



#Contamos con 60.946 observaciones y 30 variables 

#Comprobar variables que no tienen completamente datos para depurar. 
summary(prestadores$tido_codigo)
summary(prestadores$gerente_codigo)
summary(prestadores$fecha_cierre)

# Armar un df depurado de 27 variables. Normalizando criterio de variables de primera letra mayúscula y resto minúscula. 

#Cargar el paquete stringr para poder usar la función str_to_title

library(stringr)
pres1 <- data.frame(
  depa_nombre = str_to_title(prestadores$depa_nombre),
  muni_nombre = str_to_title(prestadores$muni_nombre),
  nombre_prestador = str_to_title(prestadores$nombre_prestador),
  razon_social = str_to_title(prestadores$razon_social),
  clpr_nombre = str_to_title(prestadores$clpr_nombre),
  ese = str_to_title(prestadores$ese),
  direccion = str_to_title(prestadores$direccion),
  habilitado = str_to_title(prestadores$habilitado),
  clase_persona = str_to_title(prestadores$clase_persona),
  naju_nombre = str_to_title(prestadores$naju_nombre),
  rep_legal = str_to_title(prestadores$rep_legal),
  caracter = str_to_title(prestadores$caracter),
  codigo_habilitacion = prestadores$codigo_habilitacion,
  nits_nit = prestadores$nits_nit,
  clpr_codigo= prestadores$clpr_codigo,
  telefono=prestadores$telefono,
  fax= prestadores$fax,
  email = prestadores$email,
  nivel= prestadores$nivel,
  fecha_radicacion = prestadores$fecha_radicacion,
  fecha_vencimiento = prestadores$fecha_vencimiento,
  dv = prestadores$dv,
  naju_codigo = prestadores$naju_codigo,
  numero_sede_principal = prestadores$numero_sede_principal,
  telefono_adicional = prestadores$telefono_adicional,
  email_adicional = prestadores$email_adicional,
  fecha_corte_REPS = prestadores$fecha_corte_REPS
)

View(pres1)

# Se observan varias inconsistencias en los datos, proceder a limpiarlos. 

# Revisar  números de celular en observaciones de dirección 
regex <- "^[0-9]{10}$"
filas_con_celular <- grep(regex, pres1$direccion)
print (filas_con_celular)

# Hacer el cambio de estas  3 observaciones por NA 
pres1$direccion[filas_con_celular] <- NA

# Convertir variable telefono, fax y telefono adicional a variable númerica para corregir varios de los errores existentes en estas ( palabras, caracteres extraños )

library(shiny)
pres1$telefono <- as.numeric(pres1$telefono)
pres1$fax <- as.numeric(pres1$fax)
pres1$telefono_adicional <- as.numeric(pres1$telefono_adicional)

#Allí se reemplazan por NA al realizar este cambio. 

#Al existir valores negativos, usamos valor absoluto para parchar este tema. 
pres1$telefono <- abs(pres1$telefono)
pres1$fax <- abs(pres1$fax)
pres1$telefono_adicional <- abs(pres1$telefono_adicional)


# Realizar  comprobación de variable númerica  y que corresponda con el número de observaciones

es_numerico <- sapply(pres1$telefono, is.numeric)
conteo_true <- sum(es_numerico == TRUE)
print(conteo_true)

# Concuerda, seguimos. 


# Se observan casos de caracteres xxxx entre las observaciones.

#Quitar las xxx y dejar el espacio para correción del nombre de los  rep legal que estaban mal

pres1$rep_legal <- gsub("xxx", "", pres1$rep_legal)

#Mismo caso de un email con XXX 

pres1$email <- gsub("XXX", "", pres1$email)

# Ahora arreglas fechas que queden en formato correcto.

pres1$fecha_radicacion <- as.Date(as.character(pres1$fecha_radicacion), format = "%Y%m%d")
pres1$fecha_vencimiento <- as.Date(as.character(prestadores$fecha_vencimiento), format = "%Y%m%d")

View(pres1)


# Ahora en la última variable eliminar el "Fecha corte REPS :" para dejar limpia la variable

pres1$fecha_corte_REPS <- gsub("^.*Fecha corte REPS: ", "", pres1$fecha_corte_REPS)

# 
summary(pres1)


# Ahora exportar la nueva base para subirla a SQLite3,  Para tener noción visual del proceso utilice DB Browser for SQLite, luego llamar desde R la base y trabajar acá. 

write.csv(pres1, "prestadores.csv")


### Comenzare con la segunda base, realizaré los mismos pasos para llegar al archivo que cargare en sqlite3. 







# Ahora trabajaremos con las bases cargadas en SQL

install.packages("DBI")
install.packages("RSQLite")
library(RSQLite)

# Conectandose a la ruta de la base
rutabase <- "C:/sqlite/Bases/Municipios.db"
conectarbase <- dbConnect(SQLite(), dbname = rutabase)

# Tablas en SQLite
tablas <- dbListTables(conectarbase)
print(tablas)


tablafrecuencia1 <- dbGetQuery (conectarbase, "SELECT region,
    COUNT(*) AS frecuencia,
    ROUND((COUNT(*) * 100.0 / SUM(COUNT(*)) OVER ()), 2) AS porcentaje
FROM municipios
GROUP BY region
ORDER BY frecuencia DESC;
" )

print(tablafrecuencia1)

tablafrecuencia2 <- dbGetQuery (conectarbase, "SELECT departamento, COUNT(*) AS frecuencia
FROM municipios
GROUP BY departamento
ORDER BY frecuencia DESC ;
")

superficie <-dbGetQuery (conectarbase," SELECT departamento, SUM(superficie) AS total_superficie
FROM municipios
GROUP BY departamento
ORDER BY total_superficie DESC
limit 10 ; ")

poblacion <- dbGetQuery(conectarbase, "SELECT municipio, departamento, superficie, poblacion
FROM municipios
WHERE superficie != 'NA'
ORDER BY superficie DESC, poblacion DESC
LIMIT 10;")

tablapobl.sup.den <- dbGetQuery(conectarbase, "SELECT municipio, departamento, poblacion, superficie, poblacion / superficie AS densidad
FROM municipios;
")

munimayordensidad <- dbGetQuery(conectarbase, "SELECT municipio, departamento, poblacion, superficie, poblacion / superficie AS densidad
FROM municipios
ORDER BY densidad DESC
LIMIT 5;
")

munimenordensidad <- dbGetQuery(conectarbase,"SELECT municipio, departamento, poblacion, superficie, poblacion / superficie AS densidad
FROM municipios
WHERE densidad IS NOT NULL
ORDER BY densidad ASC
LIMIT 5;")

regionmayorpoblacion <- dbGetQuery(conectarbase,"SELECT region, SUM(poblacion) AS total_poblacion
FROM municipios
GROUP BY region
ORDER BY total_poblacion DESC ;")

regionmayorsuperficie <- dbGetQuery(conectarbase,"SELECT region, SUM(superficie) AS total_superficie
FROM municipios
GROUP BY region
ORDER BY total_superficie DESC ;")


# Querys de la segunda base cargada, Prestadores. 

prestadorespormunicipio <- dbGetQuery(conectarbase,"SELECT depa_nombre, COUNT(DISTINCT nombre_prestador) AS cantidad_prestadores
FROM prestadores
GROUP BY depa_nombre
ORDER BY cantidad_prestadores DESC;")


prestadores5pormunicipio <- dbGetQuery(conectarbase,"SELECT muni_nombre, COUNT(DISTINCT nombre_prestador) AS cantidad_prestadores
FROM prestadores
GROUP BY muni_nombre
ORDER BY cantidad_prestadores DESC
LIMIT 5;")


prestadores5menpormunicipio <- dbGetQuery(conectarbase,"SELECT muni_nombre, COUNT(DISTINCT nombre_prestador) AS cantidad_prestadores
FROM prestadores
GROUP BY muni_nombre
ORDER BY cantidad_prestadores ASC
LIMIT 5;")

cantidad_prestadores5mayores <- dbGetQuery(conectarbase,"SELECT muni_nombre, COUNT(DISTINCT nombre_prestador) AS cantidad_prestadores,
       COUNT(DISTINCT CASE WHEN clpr_nombre IS NOT NULL THEN nombre_prestador END) AS cantidad_prestadores_con_clpr
FROM prestadores
WHERE muni_nombre IN ('Bogotá', 'Medellín','Cali','Barranquilla','Cartagena')
GROUP BY muni_nombre;")

tipoprestadoresbog <- dbGetQuery(conectarbase,"SELECT clpr_nombre, COUNT(DISTINCT nombre_prestador) AS cantidad_prestadores
FROM prestadores
WHERE muni_nombre IN ('Bogotá')
AND clpr_nombre IS NOT NULL
GROUP BY clpr_nombre
ORDER BY cantidad_prestadores DESC;
")

tipoprestadoresmed <- dbGetQuery(conectarbase,"SELECT clpr_nombre, COUNT(DISTINCT nombre_prestador) AS cantidad_prestadores
FROM prestadores
WHERE muni_nombre IN ('Medellín')
AND clpr_nombre IS NOT NULL
GROUP BY clpr_nombre
ORDER BY cantidad_prestadores DESC;
")


tipoprestadoresach <- dbGetQuery(conectarbase,"SELECT clpr_nombre, COUNT(DISTINCT nombre_prestador) AS cantidad_prestadores
FROM prestadores
WHERE muni_nombre IN ('Achí')
AND clpr_nombre IS NOT NULL
GROUP BY clpr_nombre
ORDER BY cantidad_prestadores DESC;
")

tipoprestadoresita <- dbGetQuery(conectarbase,"SELECT clpr_nombre, COUNT(DISTINCT nombre_prestador) AS cantidad_prestadores
FROM prestadores
WHERE muni_nombre IN ('Itagui')
AND clpr_nombre IS NOT NULL
GROUP BY clpr_nombre
ORDER BY cantidad_prestadores DESC;
")

tipoprestadoresagua <- dbGetQuery(conectarbase,"SELECT clpr_nombre, COUNT(DISTINCT nombre_prestador) AS cantidad_prestadores
FROM prestadores
WHERE muni_nombre IN ('Aguada')
AND clpr_nombre IS NOT NULL
GROUP BY clpr_nombre
ORDER BY cantidad_prestadores DESC;
")


itaprestpor <- dbGetQuery(conectarbase,"SELECT clpr_nombre,
       COUNT(DISTINCT nombre_prestador) AS cantidad_prestadores,
       ROUND((COUNT(*) * 100.0 / SUM(COUNT(*)) OVER ()), 2) AS porcentaje
FROM prestadores
WHERE muni_nombre IN ('Itagui')
AND clpr_nombre IS NOT NULL
GROUP BY clpr_nombre
ORDER BY cantidad_prestadores DESC;;
")

solprestpor <- dbGetQuery(conectarbase,"SELECT clpr_nombre,
       COUNT(DISTINCT nombre_prestador) AS cantidad_prestadores,
       ROUND((COUNT(*) * 100.0 / SUM(COUNT(*)) OVER ()), 2) AS porcentaje
FROM prestadores
WHERE muni_nombre IN ('Soledad')
AND clpr_nombre IS NOT NULL
GROUP BY clpr_nombre
ORDER BY cantidad_prestadores DESC;
")


pobmax <- dbGetQuery(conectarbase,"SELECT municipio, departamento, superficie, poblacion
FROM municipios
ORDER BY poblacion DESC
LIMIT 5;
")


# Gráficos para soporte gráfico en Word 


#Gráfico  Municipios con mayor número de prestadores dada la query. 

library(ggplot2)

ggplot(data = prestadores5pormunicipio, aes(x = muni_nombre, y = cantidad_prestadores)) +
  geom_bar(stat = "identity", fill = "steelblue") +  
  labs(title = "Municipios con mayor número de prestadores ",
       x = "Municipio",
       y = "Cantidad de Prestadores") +
  theme_minimal() + 
  theme(
    plot.title = element_text(hjust = 0.5),  
    axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.5)  
  ) +
  geom_text(aes(label = cantidad_prestadores), vjust = -0.5)  


library(ggplot2)

# Gráfico de torta Itagui
ggplot(itaprestpor, aes(x = "", y = porcentaje, fill = clpr_nombre)) +
  geom_bar(stat = "identity", width = 1) +
  geom_text(aes(label = paste0(porcentaje, "%")), position = position_stack(vjust = 0.5), size = 4, color = "white") +  # Agregar los porcentajes en cada sector
  coord_polar("y", start = 0) +
  labs(fill = "Categoría",
       y = "Porcentaje") +
  theme_void()


library(ggplot2)

# Gráfico de torta Soledad
ggplot(solprestpor, aes(x = "", y = porcentaje, fill = clpr_nombre)) +
  geom_bar(stat = "identity", width = 1) +
  geom_text(aes(label = paste0(porcentaje, "%")), position = position_stack(vjust = 0.5), size = 4, color = "white") +  # Agregar los porcentajes en cada sector
  coord_polar("y", start = 0) +
  labs(fill = "Categoría",
       y = "Porcentaje") +
  theme_void()
