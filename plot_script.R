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

plot_obras <- ggplot(plot_data, aes(x = factor(fechainicioanio), y = count, fill = status)) +
  geom_bar(stat = "identity", position = "stack") +  # Single geom_bar with full data for stacking
  geom_text(data = totals, aes(x = factor(fechainicioanio), y = total, label = total), inherit.aes = FALSE, vjust = -0.5, size = 4) +  # Add labels on top of bars, no inheritance
  labs(
    title = "Histograma de Obras por Año de Inicio (2016-2025)",
    x = "Año de Inicio",
    y = "Cantidad de Obras",
    fill = "Obras",
    caption = "Datos: Secretaría de Obras Públicas,\ndatos actualizados el 22/5/2025;\nver: https://mapainversiones.obraspublicas.gob.ar"  # Multiline footnote
  ) +
  theme_light(base_size = 14) +  # Use light theme with larger base text size
  theme(aspect.ratio = 1, legend.position = "top") +  # Make plot square and position legend at the top
  scale_fill_manual(values = c("En ejecución" = "orange", "Finalizadas" = "gray40"))

# Save the plot
ggsave("/home/rquiroga/github/obras_publicas/plot_obras.png", plot = plot_obras, dpi = 300, width = 10, height = 10)
