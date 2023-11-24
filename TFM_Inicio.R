
# TFM - SERGIO ORTIGOSA

# Importante cargar librerías
# Importante cambiar las rutas de los archivos cuando sea necesario


# *****************************************************************************
# *****************      LIBRERÍAS NECESARIAS      ****************************
# *****************************************************************************

library(readxl)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(skimr)
library(DescTools)
library(openxlsx)


# *****************************************************************************
# *****************      LECTURA Y LIMPIEZA DE DATOS     **********************
# *****************************************************************************

# Importación del fichero
ivcr_dataset_bruto <- read_excel("./Dataset_Original.xlsx")

# Observar variables, tipo, y clases en la variable objetivo (Y)
skim(ivcr_dataset_bruto)
glimpse(ivcr_dataset_bruto)

ivcr_dataset_bruto |> 
  count(Y) |> 
  mutate(porc = 100*n/sum(n))

# Conversión de las variables a tipo factor (son todas categóricas)
ivcr_dataset <- 
  ivcr_dataset_bruto %>%
  mutate(across(where(is.character), as_factor))

ivcr_dataset <- 
  ivcr_dataset %>%
  mutate(across(where(is.numeric), as_factor))

# Comprobación de registros duplicadas
if (any(duplicated(ivcr_dataset))) {
  print("Hay filas duplicadas en el dataset")
} else {
  print("No hay filas duplicadas en el dataset")
}

# Eliminación de los registros duplicados
ivcr_dataset <- unique(ivcr_dataset, fromLast = TRUE)

# Valores ausentes (NAs) en el fichero de datos
na_counts <- colSums(is.na(ivcr_dataset))
na_percent <- colMeans(is.na(ivcr_dataset)) * 100
na_summary <- data.frame(NA_Counts = na_counts, NA_Percent = na_percent)
na_summary




# *****************************************************************************
# **********************      DEPURACIÓN      *********************************
# *****************************************************************************

# Car (eliminar)
ivcr_dataset <- ivcr_dataset |>  select(-car)

# Bar (imputación modal)
moda_bar <- names(which.max(table(ivcr_dataset$Bar)))
ivcr_dataset$Bar[is.na(ivcr_dataset$Bar)] <- moda_bar

# CoffeeHouse (imputación modal)
moda_CoffeeHouse <- names(which.max(table(ivcr_dataset$CoffeeHouse)))
ivcr_dataset$CoffeeHouse[is.na(ivcr_dataset$CoffeeHouse)] <- moda_CoffeeHouse

# CarryAway (imputación modal)
moda_CarryAway <- names(which.max(table(ivcr_dataset$CarryAway)))
ivcr_dataset$CarryAway[is.na(ivcr_dataset$CarryAway)] <- moda_CarryAway

# RestaurantLessThan20 (imputación modal)
moda_RestaurantLessThan20 <- names(which.max(table(ivcr_dataset$RestaurantLessThan20)))
ivcr_dataset$RestaurantLessThan20[is.na(ivcr_dataset$RestaurantLessThan20)] <- moda_RestaurantLessThan20

# Restaurant20To50 (imputación modal)
moda_Restaurant20To50 <- names(which.max(table(ivcr_dataset$Restaurant20To50)))
ivcr_dataset$Restaurant20To50[is.na(ivcr_dataset$Restaurant20To50)] <- moda_Restaurant20To50

# toCoupon_GEQ5 (eliminar)
ivcr_dataset <- ivcr_dataset |>  select(-toCoupon_GEQ5min)

# Creación de time_toCoupon
ivcr_dataset <- ivcr_dataset %>%
  mutate(time_toCoupon = case_when(
    toCoupon_GEQ15min == 0 ~ "Nearly",
    toCoupon_GEQ25min == 0 ~ "Medium",
    TRUE ~ "FarAway"
  )) %>%
  mutate(time_toCoupon = factor(time_toCoupon, levels = c("Nearly", "Medium", "FarAway")))

# toCoupon_GEQ15min (eliminar)
ivcr_dataset <- ivcr_dataset |>  select(-toCoupon_GEQ15min)

# toCoupon_GEQ25min (eliminar)
ivcr_dataset <- ivcr_dataset |>  select(-toCoupon_GEQ25min)

# Age (recodificación)
ivcr_dataset <- ivcr_dataset |> 
  mutate(age = recode(age,
                      "below21" = "<21",
                      "21" = "21to26",
                      "26" = "26to31",
                      "31" = "31to36",
                      "36" = "36to41",
                      "41" = "41to46",
                      "46" = "46to50",
                      "50plus" = ">50"))

# Temperature (a grados centígrados)
ivcr_dataset$temperature <- as.numeric(as.character(ivcr_dataset$temperature)) # Conversión a numérica para poder operar.
ivcr_dataset <- ivcr_dataset |> mutate(temperature = (temperature - 32)*5/9) # Fahrenhait a grados centígrados.
ivcr_dataset$temperature <- round(ivcr_dataset$temperature, 0) # Eliminar los decimales.
ivcr_dataset$temperature <- as.factor(ivcr_dataset$temperature) # Conversión a factor nuevamente.

# direction_opp (eliminar)
ivcr_dataset <- ivcr_dataset |>  select(-direction_opp)

# Comprobar que ya no hay valores ausentes
na_counts <- colSums(is.na(ivcr_dataset))
na_percent <- colMeans(is.na(ivcr_dataset)) * 100
na_summary <- data.frame(NA_Counts = na_counts, NA_Percent = na_percent)
na_summary

# Test de independencia (ver documento)
chisq <-
  tibble("variable" = ivcr_dataset %>% select(where(is.factor)) %>% names(),
         "p_value" = ivcr_dataset %>% select(where(is.factor)) %>%
           map_dbl(.f = function(x) { chisq.test(ivcr_dataset$Y, x)$p.value}))
chisq %>% arrange(desc(p_value))





# *****************************************************************************
# ******************      ANÁLISIS EXPLORATORIO      **************************
# *****************************************************************************

# GRÁFICOS CON GGPLOT2

# DESTINATION
ivcr_dataset %>%
  count(destination, Y, sort = TRUE) %>%
  group_by(destination) %>%
  mutate(porc = n / sum(n)) %>%
  ggplot(aes(x = destination, y = n, fill = Y)) +
  geom_bar(stat = "identity", position = "stack", width = 0.5) +
  scale_fill_manual(values = c("#DE4E37", "#3FAB44"), labels = c("Rechazado", "Aceptado")) +
  geom_text(aes(label = paste0(round(porc * 100), "%")),
            position = position_stack(vjust = 0.5),
            color = "white", size = 4, fontface = "bold") +
  coord_flip() +
  labs(x = "Destination", y = "Frequency", fill = "Y", title = "Frecuencia de la variable Destination") +
  theme_classic() +
  theme(
    text = element_text(size = 12),
    axis.title = element_text(size = 10),
    plot.title = element_text(size = 12, face = "bold"),
    plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),
    panel.spacing = unit(0.2, "cm"),
    axis.text.y = element_text(size = 12, face = "bold"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "topright"
  )

# PASSANGER
ivcr_dataset %>%
  count(passanger, Y, sort = TRUE) %>%
  group_by(passanger) %>%
  mutate(porc = n / sum(n)) %>%
  ggplot(aes(x = passanger, y = n, fill = Y)) +
  geom_bar(stat = "identity", position = "stack", width = 0.5) +
  scale_fill_manual(values = c("#DE4E37", "#3FAB44"), labels = c("Rechazado", "Aceptado")) +
  geom_text(aes(label = paste0(round(porc * 100), "%")),
            position = position_stack(vjust = 0.5),
            color = "white", size = 4, fontface = "bold") +
  coord_flip() +
  labs(x = "Passanger", y = "Frequency", fill = "Y", title = "Frecuencia de la variable Passanger") +
  theme_classic() +
  theme(
    text = element_text(size = 12),
    axis.title = element_text(size = 10),
    plot.title = element_text(size = 12, face = "bold"),
    plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),
    panel.spacing = unit(0.2, "cm"),
    axis.text.y = element_text(size = 12, face = "bold"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "topright"
  )

# WEATHER
ivcr_dataset %>%
  count(weather, Y, sort = TRUE) %>%
  group_by(weather) %>%
  mutate(porc = n / sum(n)) %>%
  ggplot(aes(x = weather, y = n, fill = Y)) +
  geom_bar(stat = "identity", position = "stack", width = 0.5) +
  scale_fill_manual(values = c("#DE4E37", "#3FAB44"), labels = c("Rechazado", "Aceptado")) +
  geom_text(aes(label = paste0(round(porc * 100), "%")),
            position = position_stack(vjust = 0.5),
            color = "white", size = 4, fontface = "bold") +
  coord_flip() +
  labs(x = "Weather", y = "Frequency", fill = "Y", title = "Frecuencia de la variable Weather") +
  theme_classic() +
  theme(
    text = element_text(size = 12),
    axis.title = element_text(size = 10),
    plot.title = element_text(size = 12, face = "bold"),
    plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),
    panel.spacing = unit(0.2, "cm"),
    axis.text.y = element_text(size = 12, face = "bold"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "topright"
  )

# TEMPERATURE
ivcr_dataset %>%
  count(temperature, Y, sort = TRUE) %>%
  group_by(temperature) %>%
  mutate(porc = n / sum(n)) %>%
  ggplot(aes(x = temperature, y = n, fill = Y)) +
  geom_bar(stat = "identity", position = "stack", width = 0.5) +
  scale_fill_manual(values = c("#DE4E37", "#3FAB44"), labels = c("Rechazado", "Aceptado")) +
  geom_text(aes(label = paste0(round(porc * 100), "%")),
            position = position_stack(vjust = 0.5),
            color = "white", size = 4, fontface = "bold") +
  coord_flip() +
  labs(x = "Temperature", y = "Frequency", fill = "Y", title = "Frequencia de la variable Temperature") +
  theme_classic() +
  theme(
    text = element_text(size = 12),
    axis.title = element_text(size = 10),
    plot.title = element_text(size = 12, face = "bold"),
    plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),
    panel.spacing = unit(0.2, "cm"),
    axis.text.y = element_text(size = 12, face = "bold"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "topright"
  )

# TIME
ivcr_dataset %>%
  count(time, Y, sort = TRUE) %>%
  group_by(time) %>%
  mutate(porc = n / sum(n)) %>%
  ggplot(aes(x = time, y = n, fill = Y)) +
  geom_bar(stat = "identity", position = "stack", width = 0.5) +
  scale_fill_manual(values = c("#DE4E37", "#3FAB44"), labels = c("Rechazado", "Aceptado")) +
  geom_text(aes(label = paste0(round(porc * 100), "%")),
            position = position_stack(vjust = 0.5),
            color = "white", size = 4, fontface = "bold") +
  coord_flip() +
  labs(x = "Time", y = "Frequency", fill = "Y", title = "Frequencia de la variable Time") +
  theme_classic() +
  theme(
    text = element_text(size = 12),
    axis.title = element_text(size = 10),
    plot.title = element_text(size = 12, face = "bold"),
    plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),
    panel.spacing = unit(0.2, "cm"),
    axis.text.y = element_text(size = 12, face = "bold"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "topright"
  )

# COUPON
ivcr_dataset %>%
  count(coupon, Y, sort = TRUE) %>%
  group_by(coupon) %>%
  mutate(porc = n / sum(n)) %>%
  ggplot(aes(x = coupon, y = n, fill = Y)) +
  geom_bar(stat = "identity", position = "stack", width = 0.5) +
  scale_fill_manual(values = c("#DE4E37", "#3FAB44"), labels = c("Rechazado", "Aceptado")) +
  geom_text(aes(label = paste0(round(porc * 100), "%")),
            position = position_stack(vjust = 0.5),
            color = "white", size = 4, fontface = "bold") +
  coord_flip() +
  labs(x = "Coupon", y = "Frequency", fill = "Y", title = "Frequencia de la variable Coupon") +
  theme_classic() +
  theme(
    text = element_text(size = 12),
    axis.title = element_text(size = 10),
    plot.title = element_text(size = 12, face = "bold"),
    plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),
    panel.spacing = unit(0.2, "cm"),
    axis.text.y = element_text(size = 12, face = "bold"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "topright"
  )

# EXPIRATION
ivcr_dataset %>%
  count(expiration, Y, sort = TRUE) %>%
  group_by(expiration) %>%
  mutate(porc = n / sum(n)) %>%
  ggplot(aes(x = expiration, y = n, fill = Y)) +
  geom_bar(stat = "identity", position = "stack", width = 0.5) +
  scale_fill_manual(values = c("#DE4E37", "#3FAB44"), labels = c("Rechazado", "Aceptado")) +
  geom_text(aes(label = paste0(round(porc * 100), "%")),
            position = position_stack(vjust = 0.5),
            color = "white", size = 4, fontface = "bold") +
  coord_flip() +
  labs(x = "Expiration", y = "Frequency", fill = "Y", title = "Frecuencia de la variable Expiration") +
  theme_classic() +
  theme(
    text = element_text(size = 12),
    axis.title = element_text(size = 10),
    plot.title = element_text(size = 12, face = "bold"),
    plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),
    panel.spacing = unit(0.2, "cm"),
    axis.text.y = element_text(size = 12, face = "bold"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "topright"
  )

# GENDER
ivcr_dataset %>%
  count(gender, Y, sort = TRUE) %>%
  group_by(gender) %>%
  mutate(porc = n / sum(n)) %>%
  ggplot(aes(x = gender, y = n, fill = Y)) +
  geom_bar(stat = "identity", position = "stack", width = 0.5) +
  scale_fill_manual(values = c("#DE4E37", "#3FAB44"), labels = c("Rechazado", "Aceptado")) +
  geom_text(aes(label = paste0(round(porc * 100), "%")),
            position = position_stack(vjust = 0.5),
            color = "white", size = 4, fontface = "bold") +
  coord_flip() +
  labs(x = "Gender", y = "Frequency", fill = "Y", title = "Frecuencia de la variable Gender") +
  theme_classic() +
  theme(
    text = element_text(size = 12),
    axis.title = element_text(size = 10),
    plot.title = element_text(size = 12, face = "bold"),
    plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),
    panel.spacing = unit(0.2, "cm"),
    axis.text.y = element_text(size = 12, face = "bold"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "topright"
  )

# AGE
ivcr_dataset %>%
  count(age, Y, sort = TRUE) %>%
  group_by(age) %>%
  mutate(porc = n / sum(n)) %>%
  ggplot(aes(x = age, y = n, fill = Y)) +
  geom_bar(stat = "identity", position = "stack", width = 0.5) +
  scale_fill_manual(values = c("#DE4E37", "#3FAB44"), labels = c("Rechazado", "Aceptado")) +
  geom_text(aes(label = paste0(round(porc * 100), "%")),
            position = position_stack(vjust = 0.5),
            color = "white", size = 4, fontface = "bold") +
  coord_flip() +
  labs(x = "Age", y = "Frequency", fill = "Y", title = "Frequencia de la variable Age") +
  theme_classic() +
  theme(
    text = element_text(size = 12),
    axis.title = element_text(size = 10),
    plot.title = element_text(size = 12, face = "bold"),
    plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),
    panel.spacing = unit(0.2, "cm"),
    axis.text.y = element_text(size = 12, face = "bold"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "topright"
  )

# MARITALSTATUS
ivcr_dataset %>%
  count(maritalStatus, Y, sort = TRUE) %>%
  group_by(maritalStatus) %>%
  mutate(porc = n / sum(n)) %>%
  ggplot(aes(x = maritalStatus, y = n, fill = Y)) +
  geom_bar(stat = "identity", position = "stack", width = 0.5) +
  scale_fill_manual(values = c("#DE4E37", "#3FAB44"), labels = c("Rechazado", "Aceptado")) +
  geom_text(aes(label = paste0(round(porc * 100), "%")),
            position = position_stack(vjust = 0.5),
            color = "white", size = 4, fontface = "bold") +
  coord_flip() +
  labs(x = "MaritalStatus", y = "Frequency", fill = "Y", title = "Frecuencia de la variable MaritalStatus") +
  theme_classic() +
  theme(
    text = element_text(size = 12),
    axis.title = element_text(size = 10),
    plot.title = element_text(size = 12, face = "bold"),
    plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),
    panel.spacing = unit(0.2, "cm"),
    axis.text.y = element_text(size = 12, face = "bold"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "topright"
  )

# HAS_CHILDREN
ivcr_dataset %>%
  count(has_children, Y, sort = TRUE) %>%
  group_by(has_children) %>%
  mutate(porc = n / sum(n)) %>%
  ggplot(aes(x = has_children, y = n, fill = Y)) +
  geom_bar(stat = "identity", position = "stack", width = 0.5) +
  scale_fill_manual(values = c("#DE4E37", "#3FAB44"), labels = c("Rechazado", "Aceptado")) +
  geom_text(aes(label = paste0(round(porc * 100), "%")),
            position = position_stack(vjust = 0.5),
            color = "white", size = 4, fontface = "bold") +
  coord_flip() +
  labs(x = "Has_Children", y = "Frequency", fill = "Y", title = "Frecuencia de la variable Has_Children") +
  theme_classic() +
  theme(
    text = element_text(size = 12),
    axis.title = element_text(size = 10),
    plot.title = element_text(size = 12, face = "bold"),
    plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),
    panel.spacing = unit(0.2, "cm"),
    axis.text.y = element_text(size = 12, face = "bold"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "topright"
  )

# EDUCATION
ivcr_dataset %>%
  count(education, Y, sort = TRUE) %>%
  group_by(education) %>%
  mutate(porc = n / sum(n)) %>%
  ggplot(aes(x = education, y = n, fill = Y)) +
  geom_bar(stat = "identity", position = "stack", width = 0.5) +
  scale_fill_manual(values = c("#DE4E37", "#3FAB44"), labels = c("Rechazado", "Aceptado")) +
  geom_text(aes(label = paste0(round(porc * 100), "%")),
            position = position_stack(vjust = 0.5),
            color = "white", size = 4, fontface = "bold") +
  coord_flip() +
  labs(x = "Education", y = "Frequency", fill = "Y", title = "Frecuencia de la variable Education") +
  theme_classic() +
  theme(
    text = element_text(size = 12),
    axis.title = element_text(size = 10),
    plot.title = element_text(size = 12, face = "bold"),
    plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),
    panel.spacing = unit(0.2, "cm"),
    axis.text.y = element_text(size = 12, face = "bold"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "topright"
  )

# OCCUPATION
ivcr_dataset %>%
  count(occupation, Y, sort = TRUE) %>%
  group_by(occupation) %>%
  mutate(porc = n / sum(n)) %>%
  ggplot(aes(x = occupation, y = n, fill = Y)) +
  geom_bar(stat = "identity", position = "stack", width = 0.5) +
  scale_fill_manual(values = c("#DE4E37", "#3FAB44"), labels = c("Rechazado", "Aceptado")) +
  geom_text(aes(label = paste0(round(porc * 100), "%")),
            position = position_stack(vjust = 0.5),
            color = "white", size = 4, fontface = "bold") +
  coord_flip() +
  labs(x = "Occupation", y = "Frequency", fill = "Y", title = "Frecuencia de la variable Occupation") +
  theme_classic() +
  theme(
    text = element_text(size = 12),
    axis.title = element_text(size = 10),
    plot.title = element_text(size = 12, face = "bold"),
    plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),
    panel.spacing = unit(0.2, "cm"),
    axis.text.y = element_text(size = 12, face = "bold"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "topright"
  )

# INCOME
ivcr_dataset %>%
  count(income, Y, sort = TRUE) %>%
  group_by(income) %>%
  mutate(porc = n / sum(n)) %>%
  ggplot(aes(x = income, y = n, fill = Y)) +
  geom_bar(stat = "identity", position = "stack", width = 0.5) +
  scale_fill_manual(values = c("#DE4E37", "#3FAB44"), labels = c("Rechazado", "Aceptado")) +
  geom_text(aes(label = paste0(round(porc * 100), "%")),
            position = position_stack(vjust = 0.5),
            color = "white", size = 4, fontface = "bold") +
  coord_flip() +
  labs(x = "Income", y = "Frequency", fill = "Y", title = "Frecuencia de la variable Income") +
  theme_classic() +
  theme(
    text = element_text(size = 12),
    axis.title = element_text(size = 10),
    plot.title = element_text(size = 12, face = "bold"),
    plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),
    panel.spacing = unit(0.2, "cm"),
    axis.text.y = element_text(size = 12, face = "bold"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "topright"
  )

# BAR
ivcr_dataset %>%
  count(Bar, Y, sort = TRUE) %>%
  group_by(Bar) %>%
  mutate(porc = n / sum(n)) %>%
  ggplot(aes(x = Bar, y = n, fill = Y)) +
  geom_bar(stat = "identity", position = "stack", width = 0.5) +
  scale_fill_manual(values = c("#DE4E37", "#3FAB44"), labels = c("Rechazado", "Aceptado")) +
  geom_text(aes(label = paste0(round(porc * 100), "%")),
            position = position_stack(vjust = 0.5),
            color = "white", size = 4, fontface = "bold") +
  coord_flip() +
  labs(x = "Bar", y = "Frequency", fill = "Y", title = "Frecuencia de la variable Bar") +
  theme_classic() +
  theme(
    text = element_text(size = 12),
    axis.title = element_text(size = 10),
    plot.title = element_text(size = 12, face = "bold"),
    plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),
    panel.spacing = unit(0.2, "cm"),
    axis.text.y = element_text(size = 12, face = "bold"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "topright"
  )

# COFFEEHOUSE
ivcr_dataset %>%
  count(CoffeeHouse, Y, sort = TRUE) %>%
  group_by(CoffeeHouse) %>%
  mutate(porc = n / sum(n)) %>%
  ggplot(aes(x = CoffeeHouse, y = n, fill = Y)) +
  geom_bar(stat = "identity", position = "stack", width = 0.5) +
  scale_fill_manual(values = c("#DE4E37", "#3FAB44"), labels = c("Rechazado", "Aceptado")) +
  geom_text(aes(label = paste0(round(porc * 100), "%")),
            position = position_stack(vjust = 0.5),
            color = "white", size = 4, fontface = "bold") +
  coord_flip() +
  labs(x = "CoffeeHouse", y = "Frequency", fill = "Y", title = "Frequencia de la variable CoffeeHouse") +
  theme_classic() +
  theme(
    text = element_text(size = 12),
    axis.title = element_text(size = 10),
    plot.title = element_text(size = 12, face = "bold"),
    plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),
    panel.spacing = unit(0.2, "cm"),
    axis.text.y = element_text(size = 12, face = "bold"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "topright"
  )

# CARRYAWAY
ivcr_dataset %>%
  count(CarryAway, Y, sort = TRUE) %>%
  group_by(CarryAway) %>%
  mutate(porc = n / sum(n)) %>%
  ggplot(aes(x = CarryAway, y = n, fill = Y)) +
  geom_bar(stat = "identity", position = "stack", width = 0.5) +
  scale_fill_manual(values = c("#DE4E37", "#3FAB44"), labels = c("Rechazado", "Aceptado")) +
  geom_text(aes(label = paste0(round(porc * 100), "%")),
            position = position_stack(vjust = 0.5),
            color = "white", size = 4, fontface = "bold") +
  coord_flip() +
  labs(x = "CarryAway", y = "Frequency", fill = "Y", title = "Frecuencia de la variable CarryAway") +
  theme_classic() +
  theme(
    text = element_text(size = 12),
    axis.title = element_text(size = 10),
    plot.title = element_text(size = 12, face = "bold"),
    plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),
    panel.spacing = unit(0.2, "cm"),
    axis.text.y = element_text(size = 12, face = "bold"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "topright"
  )

# RESTAURANTLESSTHAN20
ivcr_dataset %>%
  count(RestaurantLessThan20, Y, sort = TRUE) %>%
  group_by(RestaurantLessThan20) %>%
  mutate(porc = n / sum(n)) %>%
  ggplot(aes(x = RestaurantLessThan20, y = n, fill = Y)) +
  geom_bar(stat = "identity", position = "stack", width = 0.5) +
  scale_fill_manual(values = c("#DE4E37", "#3FAB44"), labels = c("Rechazado", "Aceptado")) +
  geom_text(aes(label = paste0(round(porc * 100), "%")),
            position = position_stack(vjust = 0.5),
            color = "white", size = 4, fontface = "bold") +
  coord_flip() +
  labs(x = "RestaurantLessThan20", y = "Frequency", fill = "Y", title = "Frequencia de la variable RestaurantLessThan20") +
  theme_classic() +
  theme(
    text = element_text(size = 12),
    axis.title = element_text(size = 10),
    plot.title = element_text(size = 12, face = "bold"),
    plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),
    panel.spacing = unit(0.2, "cm"),
    axis.text.y = element_text(size = 12, face = "bold"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "topright"
  )

# RESTAURANT20TO50
ivcr_dataset %>%
  count(Restaurant20To50, Y, sort = TRUE) %>%
  group_by(Restaurant20To50) %>%
  mutate(porc = n / sum(n)) %>%
  ggplot(aes(x = Restaurant20To50, y = n, fill = Y)) +
  geom_bar(stat = "identity", position = "stack", width = 0.5) +
  scale_fill_manual(values = c("#DE4E37", "#3FAB44"), labels = c("Rechazado", "Aceptado")) +
  geom_text(aes(label = paste0(round(porc * 100), "%")),
            position = position_stack(vjust = 0.5),
            color = "white", size = 4, fontface = "bold") +
  coord_flip() +
  labs(x = "Restaurant20To50", y = "Frequency", fill = "Y", title = "Frecuencia de la variable Restaurant20To50") +
  theme_classic() +
  theme(
    text = element_text(size = 12),
    axis.title = element_text(size = 10),
    plot.title = element_text(size = 12, face = "bold"),
    plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),
    panel.spacing = unit(0.2, "cm"),
    axis.text.y = element_text(size = 12, face = "bold"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "topright"
  )

# TIME_TOCOUPON
ivcr_dataset %>%
  count(time_toCoupon, Y, sort = TRUE) %>%
  group_by(time_toCoupon) %>%
  mutate(porc = n / sum(n)) %>%
  ggplot(aes(x = time_toCoupon, y = n, fill = Y)) +
  geom_bar(stat = "identity", position = "stack", width = 0.5) +
  scale_fill_manual(values = c("#DE4E37", "#3FAB44"), labels = c("Rechazado", "Aceptado")) +
  geom_text(aes(label = paste0(round(porc * 100), "%")),
            position = position_stack(vjust = 0.5),
            color = "white", size = 4, fontface = "bold") +
  coord_flip() +
  labs(x = "time_toCoupon", y = "Frequency", fill = "Y", title = "Frecuencia de la variable time_toCoupon") +
  theme_classic() +
  theme(
    text = element_text(size = 12),
    axis.title = element_text(size = 10),
    plot.title = element_text(size = 12, face = "bold"),
    plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),
    panel.spacing = unit(0.2, "cm"),
    axis.text.y = element_text(size = 12, face = "bold"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "topright"
  )

# DIRECTION_SAME
ivcr_dataset %>%
  count(direction_same, Y, sort = TRUE) %>%
  group_by(direction_same) %>%
  mutate(porc = n / sum(n)) %>%
  ggplot(aes(x = direction_same, y = n, fill = Y)) +
  geom_bar(stat = "identity", position = "stack", width = 0.5) +
  scale_fill_manual(values = c("#DE4E37", "#3FAB44"), labels = c("Rechazado", "Aceptado")) +
  geom_text(aes(label = paste0(round(porc * 100), "%")),
            position = position_stack(vjust = 0.5),
            color = "white", size = 4, fontface = "bold") +
  coord_flip() +
  labs(x = "Direction_same", y = "Frequency", fill = "Y", title = "Frequencia de la variable Direction_same") +
  theme_classic() +
  theme(
    text = element_text(size = 12),
    axis.title = element_text(size = 10),
    plot.title = element_text(size = 12, face = "bold"),
    plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),
    panel.spacing = unit(0.2, "cm"),
    axis.text.y = element_text(size = 12, face = "bold"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "topright"
  )



# TABLAS DE CADA VARIABLE CRUZADA CON LA VARIABLE OBJETIVO

# DESTINATION
ivcr_dataset %>%
  group_by(destination) %>% 
  count(Y) %>%
  mutate(porc = 100*n/sum(n)) %>% 
  ungroup()

# PASSANGER
ivcr_dataset %>%
  group_by(passanger) %>% 
  count(Y) %>%
  mutate(porc = 100*n/sum(n)) %>% 
  ungroup()

# WHEATHER
ivcr_dataset %>%
  group_by(weather) %>% 
  count(Y) %>%
  mutate(porc = 100*n/sum(n)) %>% 
  ungroup()

# TEMPERATURE
ivcr_dataset %>%
  group_by(temperature) %>% 
  count(Y) %>%
  mutate(porc = 100*n/sum(n)) %>% 
  ungroup()

# TIME
ivcr_dataset %>%
  group_by(time) %>% 
  count(Y) %>%
  mutate(porc = 100*n/sum(n)) %>% 
  ungroup()

# COUPON
ivcr_dataset %>%
  group_by(coupon) %>% 
  count(Y) %>%
  mutate(porc = 100*n/sum(n)) %>% 
  ungroup()

# EXPIRATION
ivcr_dataset %>%
  group_by(expiration) %>% 
  count(Y) %>%
  mutate(porc = 100*n/sum(n)) %>% 
  ungroup()

# GENDER
ivcr_dataset %>%
  group_by(gender) %>% 
  count(Y) %>%
  mutate(porc = 100*n/sum(n)) %>% 
  ungroup()

# AGE
ivcr_dataset %>%
  group_by(age) %>% 
  count(Y) %>%
  mutate(porc = 100*n/sum(n)) %>% 
  ungroup()

# MARITALSTATUS
ivcr_dataset %>%
  group_by(maritalStatus) %>% 
  count(Y) %>%
  mutate(porc = 100*n/sum(n)) %>% 
  ungroup()

# HAS_CHILDREN
ivcr_dataset %>%
  group_by(has_children) %>% 
  count(Y) %>%
  mutate(porc = 100*n/sum(n)) %>% 
  ungroup()

# EDUCATION
ivcr_dataset %>%
  group_by(education) %>% 
  count(Y) %>%
  mutate(porc = 100*n/sum(n)) %>% 
  ungroup()

# OCCUPATION
ivcr_dataset %>%
  group_by(occupation) %>% 
  count(Y) %>%
  mutate(porc = 100*n/sum(n)) %>% 
  ungroup()

# INCOME
ivcr_dataset %>%
  group_by(income) %>% 
  count(Y) %>%
  mutate(porc = 100*n/sum(n)) %>% 
  ungroup()

# BAR
ivcr_dataset %>%
  group_by(Bar) %>% 
  count(Y) %>%
  mutate(porc = 100*n/sum(n)) %>% 
  ungroup()

# COFFEEHOUSE
ivcr_dataset %>%
  group_by(CoffeeHouse) %>% 
  count(Y) %>%
  mutate(porc = 100*n/sum(n)) %>% 
  ungroup()

# CARRYAWAY
ivcr_dataset %>%
  group_by(CarryAway) %>% 
  count(Y) %>%
  mutate(porc = 100*n/sum(n)) %>% 
  ungroup()

# RESTUARANTLESSTHAN2O
ivcr_dataset %>%
  group_by(RestaurantLessThan20) %>% 
  count(Y) %>%
  mutate(porc = 100*n/sum(n)) %>% 
  ungroup()

# RESTAURANT20TO50
ivcr_dataset %>%
  group_by(Restaurant20To50) %>% 
  count(Y) %>%
  mutate(porc = 100*n/sum(n)) %>% 
  ungroup()

# TIME_TOCOUPON
ivcr_dataset %>%
  group_by(time_toCoupon) %>% 
  count(Y) %>%
  mutate(porc = 100*n/sum(n)) %>% 
  ungroup()

# DIRECTION_SAME
ivcr_dataset %>%
  group_by(direction_same) %>% 
  count(Y) %>%
  mutate(porc = 100*n/sum(n)) %>% 
  ungroup()




# *****************************************************************************
# *************      MODIFICACIONES EN LA BASE DE DATOS      ******************
# *****************************************************************************
# Ver documento

# Destination
ivcr_dataset <- ivcr_dataset |> 
  mutate(destination = recode(destination,
                              "Home" = "Urgent Place",
                              "Work" = "Urgent Place"))

# Passanger (sin modificaciones)

# Weather
ivcr_dataset <- ivcr_dataset |> 
  mutate(weather = recode(weather,
                          "Rainy" = "Adverse Climatology",
                          "Snowy" = "Adverse Climatology"))

# Temperature (sin modificaciones)

# Time (sin modificaciones)

# Coupon (sin modificaciones)

# Expiration (sin modificaciones)

# Gender (sin modificaciones)

# Age
ivcr_dataset <- ivcr_dataset |> 
  mutate(age = recode(age,
                      "<21" = "Teen",
                      "21to26" = "Youth",
                      "26to31" = "Youth",
                      "31to36" = "Youth",
                      "36to41" = "Adult",
                      "41to46" = "Adult",
                      "46to50" = "Adult",
                      ">50" = "Elderly"))

# MaritalStatus (sin modificaciones)

# Has_Children (sin modificaciones)

# Education (sin modificaciones)

# Occupation
ivcr_dataset <- ivcr_dataset |> 
  mutate(occupation = recode(occupation,
                             "Installation Maintenance & Repair" = "Employee",
                             "Transportation & Material Moving" = "Employee",
                             "Food Preparation & Serving Related" = "Employee",
                             "Building & Grounds Cleaning & Maintenance" = "Employee",
                             "Office & Administrative Support" = "Employee",
                             "Production Occupations" = "Employee",
                             "Farming Fishing & Forestry" = "Employee",
                             "Architecture & Engineering" = "Employee",
                             "Education&Training&Library" = "Employee",
                             "Healthcare Practitioners & Technical" = "Employee",
                             "Management" = "Employee",
                             "Arts Design Entertainment Sports & Media" = "Employee",
                             "Computer & Mathematical" = "Employee",
                             "Legal" = "Employee",
                             "Business & Financial" = "Employee",
                             "Sales & Related" = "Employee",
                             "Personal Care & Service" = "Employee",
                             "Protective Service" = "Employee",
                             "Life Physical Social Science" = "Employee",
                             "Healthcare Support" = "Employee",
                             "Community & Social Services" = "Employee",
                             "Construction & Extraction" = "Employee",
                             
                             "Student" = "Student",
                             
                             "Unemployed" = "Unemployed",
                             
                             "Retired" = "Retired"))

# Income (sin modificaciones)

# Car (eliminada)

# Bar (imputación modal realizada)

# CoffeeHouse (imputación modal realizada)

# CarryAway (imputación modal realizada)

# RestaurantLessThan20 (imputación modal realizada)

# Restaurant20To50 (imputación modal realizada)

# toCoupon_GEQ5min (eliminada)

# toCoupon_GEQ15min (eliminada)

# toCoupon_GEQ25min (eliminada)

# time_toCoupon (creada)

# direction_same (sin modificaciones)

# direction_opp (eliminada)




# *****************************************************************************
# **************      EXPORTACIÓN DEL FICHERO A PYTHON      *******************
# *****************************************************************************

write.xlsx(ivcr_dataset, "INTRODUCIR_RUTA_PARA_GUARDAR_ARCHIVO", sheetName = "Hoja1")

# Cuando se genere el archivo, asegurarse de que se llama "Dataset_Cod_Python.xlsx".

# Ahora, seguir trabajando en Python con el archivo "Codificación_numérica.ipynb"
#
#
