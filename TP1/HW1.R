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

# Seteo de directorio y abro la bases

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
terrenos <- read.csv("Terrenos-en-venta-2019.csv")

# Chequeo de NAs, vacíos o valores negativos
summary(terrenos)
colSums(is.na(terrenos))
colSums(terrenos == "", na.rm = TRUE)
colSums(terrenos < 0, na.rm = TRUE)

# Limpieza: elimino negativos y ceros
terrenos <- terrenos %>%
  filter(M2TOTAL > 0, PRECIOUSD > 0)

# Calculo los log
terrenos <- terrenos %>%
  mutate(
    ln_preciousd = log(PRECIOUSD),
    ln_m2total   = log(M2TOTAL)
  )

# Regresión base
reg_base <- lm(ln_preciousd ~ ln_m2total + BARRIO, 
           data = terrenos)
summary(reg_base)

# Exportar modelo a LaTeX
stargazer(reg_base, type = "latex",
          title = "Modelo de Precios de Terrenos",
          label = "MODELO",
          dep.var.labels = "Log(Precio USD)",
          covariate.labels = c("Log(M2)", levels(terrenos$barrio)[-1]),
          omit.stat = c("f", "ser"),
          no.space = TRUE)

# Descargo la base limpia con y sin residuos.
write_csv(terrenos, "terrenos_limpio.csv")

terrenos <- terrenos %>%
  mutate(residuos_reg_base = residuals(reg_base))

write_csv(terrenos, "terrenos_limpio_res.csv")

# Abro el DF con las  variables que creamos en QGIS y dropeo de nuevo.

df <- read.csv("/Users/ninadicostanzopereira/Downloads/base_completa.csv") %>%
  filter(M2TOTAL > 0, PRECIOUSD > 0) %>%
  mutate(
    ln_preciousd = log(PRECIOUSD),
    ln_m2total   = log(M2TOTAL)
  )

# Calcular estadísticas descriptivas
stats <- df %>%
  st_drop_geometry() %>%
  summarise(across(c(count_NUMPOINTS, 
                     Matrix_distancia_f_Distance, 
                     Matrix_.distancia_S_Distance, 
                     Matriz_distancia_O_Distance),
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
  summarise(across(c(count_NUMPOINTS, 
                     Matrix_distancia_f_Distance, 
                     Matrix_.distancia_S_Distance, 
                     Matriz_distancia_O_Distance),
                   ~ cor(.x, ln_preciousd, use = "complete.obs"),
                   .names = "{.col}_correl"))

# Unir en una sola tabla para exportar

tabla_final <- bind_cols(stats, correls)
print(tabla_final)


reg2 <- lm(ln_preciousd ~ ln_m2total + BARRIO + 
             count_NUMPOINTS + 
             Matrix_distancia_f_Distance + 
             Matrix_.distancia_S_Distance + 
             Matriz_distancia_O_Distance, 
           data = df) 
summary(reg2)

# Exportar segundo modelo a LaTeX

stargazer(reg, type = "latex",
          title = "Modelo de Precios de Terrenos",
          label = "MODELO",
          dep.var.labels = "Log(Precio USD)",
          covariate.labels = c("Log(M2)", levels(terrenos$barrio)[-1]),
          omit.stat = c("f", "ser"),
          no.space = TRUE)



base <- read.csv("base_completa.csv")
terrenos_res <- read_csv("terrenos_limpio_res.csv")
intersect(names(base), names(terrenos_res))

base_final <- base %>%
  left_join(
    dplyr::select(terrenos_res, POLY_ID, residuos_reg_base),
    by = "POLY_ID") %>%
      mutate(
        ln_preciousd = log(PRECIOUSD),
        ln_m2total   = log(M2TOTAL)
      )
  
write_csv(base_final, "base_completa_con_residuos.csv")
