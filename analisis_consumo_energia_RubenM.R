energia <- c(rep("Renovable", 10), rep("No Renovable", 10))
consumo <- c(120, 130, 110, 150, NA, 140, 125, 135, NA, 145, 200, 210, NA, 190, 205, 195, NA, 185, 200, 215)
costo_kwh <- c(rep(0.10, 10), rep(0.15, 10))

library(dplyr)
mediana_renovable <- median(consumo[energia == "Renovable"], na.rm = TRUE)
mediana_no_renovable <- median(consumo[energia == "No Renovable"], na.rm = TRUE)
consumo[is.na(consumo) & energia == "Renovable"] <- mediana_renovable
consumo[is.na(consumo) & energia == "No Renovable"] <- mediana_no_renovable

df_consumo <- data.frame(
  Energia = energia,
  Consumo = consumo,
  Costo_kWh = costo_kwh
)

df_consumo$Costo_Total <- df_consumo$Consumo * df_consumo$Costo_kWh
total_consumo <- tapply(df_consumo$Consumo, df_consumo$Energia, sum)
costo_total <- tapply(df_consumo$Costo_Total, df_consumo$Energia, sum)
media_consumo <- tapply(df_consumo$Consumo, df_consumo$Energia, mean)
df_consumo$Ganancia <- df_consumo$Costo_Total * 1.1

df_ordenado <- df_consumo[order(-df_consumo$Costo_Total), ]
top_3_costos <- head(df_ordenado, 3)

resumen_energia <- list(
  Dataframe_Ordenado = df_ordenado,
  Total_Consumo = total_consumo,
  Costo_Total = costo_total,
  Top_3_Costos = top_3_costos
)

print(resumen_energia)
