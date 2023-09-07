# Tarea - ENAHO BCRP 
# Juan Eduardo Castañeda Cornejo


#install.packages("data.table")
#install.packages("reshape2")
library(haven)
library(data.table)
library(dplyr)
library(reshape2)
set.seed(5056)

# Especifica la ruta al directorio que contiene tus bases de datos
directorio <- "C:/Users/Juan Eduardo/OneDrive - UDEP/Laptop/databcrp"

# Crear un objeto para almacenar los datos finales
datos_finales <- NULL

# Bucle para realizar el append de las bases de datos
for (año in 2011:2022) {
  # Construir el nombre del archivo correspondiente al año actual
  archivo <- file.path(directorio, paste0("sumaria-", año, ".dta"))
  
  # Leer el archivo .dta
  datos <- read_dta(archivo)
  
  # Hacer el append
  if (is.null(datos_finales)) {
    datos_finales <- datos
  } else {
    datos_finales <- rbindlist(list(datos_finales, datos), fill = TRUE)
  }
}

# Guardar los datos finales en un nuevo archivo .dta
write_dta(datos_finales, "datos_combinados.dta")

print(names(datos_finales))

print(unique(datos_finales$aÑo)) # Verificamos que

datos_finales <- datos_finales %>%
  rename(year = aÑo)


print(unique(datos_finales$year)) # Variable year ya está creada y cambiada


datos_finales <- datos_finales %>%
  select(year, pobreza)

print(unique(datos_finales$pobreza)) # Qué valores toma pobreza


# Crear las variables "poverty" y "poverty_e" en RStudio
datos_finales <- datos_finales %>%
  group_by(year) %>%
  mutate(poverty = sum(pobreza == 1) * 100 / n(),
         poverty_e = sum(pobreza == 2) * 100 / n()) %>%
  ungroup()

# Verificar los cambios realizados
print(datos_finales)
tail(datos_finales, 10)


###################### Hacer las tablas###############################
# collapse (sum) povert*, by(year)
datos_agrupados <- datos_finales %>%
  group_by(year) %>%
  summarise(total_poverty = sum(poverty),
            total_poverty_e = sum(poverty_e))

print(datos_agrupados)

# table year, stat(total poverty)
tabla_poverty <- table(datos_finales$year, datos_finales$poverty)
total_poverty <- rowSums(tabla_poverty)
tabla_final <- cbind(year = unique(datos_finales$year), total_poverty)
print(tabla_final)

# table year, stat(total poverty_e)
tabla_poverty_e <- table(datos_finales$year, datos_finales$poverty_e)
total_poverty_e <- rowSums(tabla_poverty_e)
tabla_final_e <- cbind(year = unique(datos_finales$year), total_poverty_e)
print(tabla_final_e)

############## Hacer el test estadístico: 

# Filtrar los datos para los años mayores o iguales a 2021
datos_filtrados <- subset(datos_finales, year >= 2021)

# Calcular la variable de pobreza
datos_filtrados$poverty <- ifelse(datos_filtrados$pobreza == 1, 1, 0)

# Realizar el t-test para la variable de pobreza
resultado_pobreza <- t.test(datos_filtrados$poverty ~ datos_filtrados$year)

# Calcular la diferencia porcentual
diff_pobreza <- 100 * (mean(subset(datos_filtrados, year == max(year))$poverty) - 
                         mean(subset(datos_filtrados, year == min(year))$poverty)) / 
  mean(subset(datos_filtrados, year == min(year))$poverty)

# Imprimir los resultados
print(paste("p value:", format.pval(resultado_pobreza$p.value)))
print(paste("Diferencia porcentual:", format(diff_pobreza, digits = 5, nsmall = 1)))

# Calcular la variable de pobreza_e
datos_filtrados$poverty_e <- ifelse(datos_filtrados$pobreza == 2, 1, 0)

# Realizar el t-test para la variable de pobreza_e
resultado_pobreza_e <- t.test(datos_filtrados$poverty_e ~ datos_filtrados$year)

# Calcular la diferencia porcentual
diff_pobreza_e <- 100 * (mean(subset(datos_filtrados, year == max(year))$poverty_e) - 
                           mean(subset(datos_filtrados, year == min(year))$poverty_e)) / 
  mean(subset(datos_filtrados, year == min(year))$poverty_e)

# Imprimir los resultados
print(paste("p value:", format.pval(resultado_pobreza_e$p.value)))
print(paste("Diferencia porcentual:", format(diff_pobreza_e, digits = 5, nsmall = 1)))

