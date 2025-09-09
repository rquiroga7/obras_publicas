# Load required libraries
library(ggplot2)
library(dplyr)
library(tidyr)  # Added for pivot_longer

# Step 1: Read the dataset
data <- read.csv("/home/rquiroga/github/obras_publicas/dataset_mop.csv", stringsAsFactors = FALSE)

# Step 2: Filter for sectornombre == "AGUA Y CLOACA"
filtered_data <- data %>% filter(sectornombre == "AGUA Y CLOACA")

# Debug: Print number of rows after filtering
print(paste("Rows after filtering:", nrow(filtered_data)))

# Step 3: Prepare data for plotting
# Convert fechainicioanio to numeric and filter for years 2016-2025
filtered_data <- filtered_data %>%
  mutate(fechainicioanio = as.numeric(fechainicioanio)) %>%
  filter(fechainicioanio >= 2016 & fechainicioanio <= 2025)

# Debug: Print number of rows after year filtering
print(paste("Rows after year filtering:", nrow(filtered_data)))

# Calculate average avancefisico for En ejecución
en_ejecucion_data <- filtered_data %>% filter(etapaobra != "FINALIZADAS")
avg_avance <- mean(en_ejecucion_data$avancefisico, na.rm = TRUE)
print(paste("Average avancefisico for En ejecución:", avg_avance))

# Calculate average avancefisico per year for En ejecución
avg_data <- filtered_data %>%
  filter(etapaobra != "FINALIZADAS") %>%
  group_by(fechainicioanio) %>%
  summarise(avg_avance = mean(avancefisico, na.rm = TRUE))

# Summarize counts per year
summary_data <- filtered_data %>%
  group_by(fechainicioanio) %>%
  summarise(
    total = n(),
    finalized = sum(etapaobra == "FINALIZADAS", na.rm = TRUE)
  ) %>%
  mutate(en_ejecucion = total - finalized)

# Ensure all years 2016-2025 are included, even if empty
all_years <- data.frame(fechainicioanio = 2016:2025)
summary_data <- left_join(all_years, summary_data, by = "fechainicioanio") %>%
  mutate(
    total = ifelse(is.na(total), 0, total),
    finalized = ifelse(is.na(finalized), 0, finalized),
    en_ejecucion = total - finalized
  )

# Join average data
summary_data <- left_join(summary_data, avg_data, by = "fechainicioanio") %>%
  mutate(avg_avance = ifelse(is.na(avg_avance), 0, avg_avance))

# Debug: Print summary data
print("Summary data:")
print(summary_data)

# Reshape for plotting
plot_data <- summary_data %>%
  select(fechainicioanio, en_ejecucion, finalized) %>%
  pivot_longer(cols = c(en_ejecucion, finalized), names_to = "status", values_to = "count") %>%
  mutate(status = recode(status, "en_ejecucion" = "En ejecución", "finalized" = "Finalizadas")) %>%
  mutate(status = factor(status, levels = c("En ejecución", "Finalizadas")))  # Set factor levels for proper scaling

# Debug: Print plot data
print("Plot data:")
print(plot_data)

# Step 4: Create the stacked bar plot
# Calculate totals for labels
totals <- summary_data %>% select(fechainicioanio, total)

# Calculate data for the average text
lines_data <- summary_data %>%
  mutate(
    line_y = finalized + en_ejecucion / 2,  # Center of the orange bar
    x_num = as.numeric(factor(fechainicioanio))  # Numeric x for text
  ) %>%
  select(fechainicioanio, line_y, x_num, avg_avance)

plot_obras <- ggplot(plot_data, aes(x = factor(fechainicioanio), y = count, fill = status)) +
  geom_bar(stat = "identity", position = "stack") +  # Single geom_bar with full data for stacking
  geom_text(data = lines_data %>% filter(fechainicioanio >= 2020 & fechainicioanio <= 2023), aes(x = x_num, y = line_y, label = paste0(round(avg_avance, 0), "%")), color = "black", size = 4.2, inherit.aes = FALSE) +  # Black outline (larger)
  geom_text(data = totals, aes(x = factor(fechainicioanio), y = total, label = total), inherit.aes = FALSE, vjust = -0.5, size = 6) +  # Add labels on top of bars, no inheritance
  labs(
    title = "Obras Públicas Nacionales de \"Agua y Cloacas\" por Año de Inicio (2016-2025)",
    x = "Año de Inicio de Obra",
    y = "Cantidad de Obras",
    fill = "Obras",
    caption = "Gráfico: Rodrigo Quiroga. Datos: Secretaría de Obras Públicas, actualizado el 22/5/2025.\nSe incluye el porcentaje promedio de avance de obra para las obras en ejecución iniciadas en cada año\nver: https://mapainversiones.obraspublicas.gob.ar"
  ) +
  theme_light(base_size = 14) +  # Use light theme with larger base text size
  theme(aspect.ratio = 1, legend.position = "top") +  # Make plot square and position legend at the top
  scale_fill_manual(values = c("En ejecución" = "orange", "Finalizadas" = "gray40"))

# Save the plot
ggsave("/home/rquiroga/github/obras_publicas/plot_obras.png", plot = plot_obras, dpi = 300, width = 10, height = 10)
