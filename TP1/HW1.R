library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(janitor)
library(lubridate)
library(sf)
library(fastDummies)
library(stargazer)
library(spdep)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
terrenos <- read.csv("Terrenos-en-venta-2019.csv")
summary(terrenos)
#ver si son NA, vacíos o valores negativos
colSums(is.na(terrenos))
colSums(terrenos == "", na.rm = TRUE)
colSums(terrenos < 0, na.rm = TRUE)
#borro los negativos y los = 0
df <- df %>%
  filter(M2TOTAL > 0, PRECIOUSD > 0)
# Calculo los log
terrenos <- terrenos %>%
  mutate(
    ln_preciousd = log(PRECIOUSD),
    ln_m2total   = log(M2TOTAL)
  )

reg1 <- lm(ln_preciousd ~ ln_m2total + BARRIO, 
           data = terrenos)
summary(reg1)

stargazer(reg1, type = "latex",
          title = "Modelo de Precios de Terrenos",
          label = "MODELO",
          dep.var.labels = "Log(Precio USD)",
          covariate.labels = c("Log(M2)", levels(terrenos$barrio)[-1]),
          omit.stat = c("f", "ser"),
          no.space = TRUE)
#descargo la base

write_csv(terrenos, "terrenos_limpio.csv")
terrenos <- terrenos %>%
  mutate(residuos_mod1 = residuals(reg1))
write_csv(terrenos, "terrenos_limpio_res.csv")

#abro el DF con las  varaibles que creamos en qgis
df <- read.csv("/Users/ninadicostanzopereira/Downloads/base_completa.csv")
df <- df %>% mutate(ln_preciousd = log(PRECIOUSD),
                    ln_m2total   = log(M2TOTAL))
df <- df %>%
  filter(M2TOTAL > 0, PRECIOUSD > 0)

# Calcular estadísticas descriptivas
stats <- df %>%
  st_drop_geometry() %>%
  summarise(across(c(count_NUMPOINTS, Matrix_distancia_f_Distance, Matrix_.distancia_S_Distance, Matriz_distancia_O_Distance),
                   list(
                     media = ~ mean(.x, na.rm = TRUE),
                     mediana = ~ median(.x, na.rm = TRUE),
                     sd = ~ sd(.x, na.rm = TRUE),
                     minimo = ~ min(.x, na.rm = TRUE),
                     maximo = ~ max(.x, na.rm = TRUE)
                   ),
                   .names = "{.col}_{.fn}"))

# Calcular correlaciones con la variable dependiente
correls <- df %>%
  st_drop_geometry() %>%
  summarise(across(c(count_NUMPOINTS, Matrix_distancia_f_Distance, Matrix_.distancia_S_Distance, Matriz_distancia_O_Distance),
                   ~ cor(.x, ln_preciousd, use = "complete.obs"),
                   .names = "{.col}_correl"))

# Unir en una sola tabla para exportar
tabla_final <- bind_cols(stats, correls)

# Ver tabla
print(tabla_final)


reg2 <- lm(ln_preciousd ~ ln_m2total + BARRIO + count_NUMPOINTS + Matrix_distancia_f_Distance + Matrix_.distancia_S_Distance + Matriz_distancia_O_Distance, 
           data = df) 
summary(reg2)

stargazer(reg2, type = "latex",
          title = "Modelo de Precios de Terrenos",
          label = "MODELO",
          dep.var.labels = "Log(Precio USD)",
          covariate.labels = c("Log(M2)", levels(terrenos$barrio)[-1]),
          omit.stat = c("f", "ser"),
          no.space = TRUE)



