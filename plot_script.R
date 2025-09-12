# Load required libraries
library(ggplot2)
library(dplyr)
library(tidyr)  # Added for pivot_longer

# Step 1: Read the dataset (use relative path so it works on Windows)
data <- read.csv("dataset_mop.csv", stringsAsFactors = FALSE)

# Normalize sector names: trim whitespace (helps avoid duplicated facet labels)
data$sectornombre <- trimws(data$sectornombre)

# Debug: list unique sectors in the raw dataset
print("Unique sectornombre in original dataset:")
print(sort(unique(data$sectornombre)))
print("Counts per sectornombre (sample):")
print(utils::head(sort(table(data$sectornombre), decreasing = TRUE), 20))

# Step 2: Filter for sectornombre == "AGUA Y CLOACA" and years 2016-2025
# Use explicit dplyr::filter to avoid masking by other packages
data2 <- data %>%
  dplyr::filter(fechainicioanio >= 2016 & fechainicioanio <= 2025)

# Debug: show sectors remaining after the year filter
print("Unique sectornombre after year filter (2016-2025):")
print(sort(unique(data2$sectornombre)))
print("Counts per sectornombre after year filter (sample):")
print(utils::head(sort(table(data2$sectornombre), decreasing = TRUE), 20))

# Debug: Show basic info to help diagnose 'mutate' problems
print(paste("Class of 'data':", paste(class(data), collapse = ", ")))
print("Columns in 'data':")
print(names(data))
print(paste("Rows after filtering by year (2016-2025):", nrow(data2)))

# Step 3: Prepare data for plotting
# Convert fechainicioanio to numeric and ensure it's a data.frame
print(paste("Class of 'data2' before mutate:", paste(class(data2), collapse = ", ")))
data2 <- data2 %>%
  dplyr::mutate(fechainicioanio = as.numeric(fechainicioanio)) 

print(paste("Class of 'data2' after mutate:", paste(class(data2), collapse = ", ")))
print(paste("Rows after year filtering:", nrow(data2)))


# Calculate average avancefisico per year for En ejecución (sector)
avg_data <- data2 %>%
  dplyr::filter(etapaobra != "FINALIZADAS") %>%
  dplyr::group_by(fechainicioanio, sectornombre) %>%
  dplyr::summarise(avg_avance = mean(avancefisico, na.rm = TRUE))

# Summarize counts per year (sector)
summary_data <- data2 %>%
  dplyr::group_by(fechainicioanio, sectornombre) %>%
  dplyr::summarise(
    total = n(),
    finalized = sum(etapaobra == "FINALIZADAS", na.rm = TRUE)
  ) %>%
  dplyr::mutate(en_ejecucion = total - finalized)

# Ensure all years 2016-2025 are present for every sector (fill missing with zeros)
# Use the full list of sectors observed in the filtered data so we don't lose any facet
# Build explicit sector list and full year×sector grid so missing years (e.g. 2025)
# are guaranteed to appear even when no rows exist in the source data.
all_sectors <- sort(unique(data2$sectornombre))

# Ungroup to avoid grouped-data behavior affecting completion
summary_data <- summary_data %>% dplyr::ungroup()

# Ensure year columns are integer for reliable joins
summary_data$fechainicioanio <- as.integer(summary_data$fechainicioanio)

# Create full grid and left-join the aggregated summary onto it
full_grid <- expand.grid(fechainicioanio = as.integer(2016:2025), sectornombre = all_sectors, stringsAsFactors = FALSE)
summary_data <- dplyr::left_join(full_grid, summary_data, by = c("fechainicioanio", "sectornombre")) %>%
  dplyr::mutate(
    total = ifelse(is.na(total), 0, total),
    finalized = ifelse(is.na(finalized), 0, finalized),
    en_ejecucion = ifelse(is.na(en_ejecucion), 0, en_ejecucion)
  )

# Join average data (per year+sector) and replace NA averages with 0
summary_data <- dplyr::left_join(summary_data, avg_data, by = c("fechainicioanio", "sectornombre")) %>%
  dplyr::mutate(avg_avance = ifelse(is.na(avg_avance), 0, avg_avance))

# Debug: Print summary data
print("Summary data:")
print(summary_data)

# Reshape for plotting

# Reshape for plotting
plot_data <- summary_data %>%
  dplyr::select(fechainicioanio, en_ejecucion, finalized, sectornombre) %>%
  tidyr::pivot_longer(cols = c(en_ejecucion, finalized), names_to = "status", values_to = "count") %>%
  dplyr::mutate(status = dplyr::recode(status, "en_ejecucion" = "En ejecución", "finalized" = "Finalizadas")) %>%
  dplyr::mutate(status = factor(status, levels = c("En ejecución", "Finalizadas")))  # Set factor levels for proper scaling

# Debug: Print plot data
print("Plot data:")
print(plot_data)

# Diagnostics: sectors present in plot_data
print("Unique sectornombre in plot_data:")
print(sort(unique(plot_data$sectornombre)))
print("Counts per sectornombre in plot_data (sample):")
print(utils::head(sort(table(plot_data$sectornombre), decreasing = TRUE), 20))

# Warn if only one sector remains (faceting will show a single panel)
n_sectors <- length(unique(plot_data$sectornombre))
if (n_sectors < 2) {
  warning(paste0("Only ", n_sectors, " sector(s) present in plot_data. Facet will show only one panel."))
}

# Ensure sectornombre is a factor with levels = all_sectors so facet panels are preserved
plot_data$sectornombre <- factor(plot_data$sectornombre, levels = all_sectors)
# Write debug CSVs so you can inspect the exact data ggplot receives
write.csv(plot_data, file = "plot_data_debug.csv", row.names = FALSE)
write.csv(summary_data, file = "summary_data_debug.csv", row.names = FALSE)
message("Wrote plot_data_debug.csv and summary_data_debug.csv")

# Step 4: Create the stacked bar plot
# Calculate totals for labels
totals <- summary_data %>% dplyr::select(fechainicioanio, total, sectornombre)

# Calculate data for the average text (vertical placement inside bars)
lines_data <- summary_data %>%
  dplyr::mutate(
    line_y = finalized + en_ejecucion / 2  # Center of the orange bar
  ) %>%
  dplyr::select(fechainicioanio, line_y, avg_avance, sectornombre)

# Ensure totals and lines_data have the same factor levels for sectornombre
totals$sectornombre <- factor(totals$sectornombre, levels = all_sectors)
lines_data$sectornombre <- factor(lines_data$sectornombre, levels = all_sectors)

# Compute per-sector ymax (10% higher than observed max total) and expand to all years
ymax_df <- totals %>%
  dplyr::group_by(sectornombre) %>%
  dplyr::summarise(ymax = max(total, na.rm = TRUE) * 1.1) %>%
  dplyr::ungroup()

ymax_grid <- tidyr::crossing(sectornombre = ymax_df$sectornombre, fechainicioanio = unique(summary_data$fechainicioanio)) %>%
  dplyr::left_join(ymax_df, by = "sectornombre")

plot_obras <- ggplot(plot_data, aes(x = factor(fechainicioanio), y = count, fill = status)) +
  geom_bar(stat = "identity", position = "stack") +  # Single geom_bar with full data for stacking
  # Add an invisible geom to enforce per-facet ymax = 110% of the max total for that sector
  geom_blank(data = ymax_grid, aes(x = factor(fechainicioanio), y = ymax), inherit.aes = FALSE) +
  geom_text(data = totals, aes(x = factor(fechainicioanio), y = total, label = total, color = "gray10"), inherit.aes = FALSE, vjust = -0.5, size = 4, fontface = "bold") +  # Add bold labels on top of bars with colors
  labs(
    title = "Obras Públicas Nacionales de \"Agua y Cloacas\" por Año de Inicio",
    x = "Año de Inicio de Obra",
    y = "Cantidad de Obras",
    fill = "Obras",
    caption = "Gráfico: Rodrigo Quiroga. Datos: Secretaría de Obras Públicas, datos 2016-2025, actualizado el 22/5/2025.\nSe incluye el porcentaje promedio de avance de obra para las obras en ejecución iniciadas en cada año.\nDatos: https://mapainversiones.obraspublicas.gob.ar, código: www.github.com/rquiroga7/obras_publicas"
  ) +
  theme_light(base_size = 18) +  # Use light theme with larger base text size
  theme(aspect.ratio = 1, legend.position = "top", axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +  # Rotate x labels 90 degrees
  scale_fill_manual(values = c("En ejecución" = "orange", "Finalizadas" = "gray40")) +
  scale_color_identity() +  # Use the color values as is
  facet_wrap(~sectornombre, scales = "free_y", drop = FALSE)  # Facet by sectornombre with independent y-scales

plot_obras
# Save the plot with error handling
tryCatch({
  ggsave("./plot_obras_facet.png", plot = plot_obras, dpi = 300, width = 12, height = 12)
  message("Saved ./plot_obras_facet.png")
}, error = function(e) {
  message("ggsave failed: ", e$message)
})

# Fallback: save one plot per sector (sanitized filenames) so you always have per-sector images
for (s in all_sectors) {
  safe_name <- gsub("[^A-Za-z0-9_-]", "_", s)
  file_name <- paste0("plot_obras_", safe_name, ".png")
  sector_plot_data <- plot_data %>% dplyr::filter(sectornombre == s)
  sector_totals <- totals %>% dplyr::filter(sectornombre == s)
  sector_lines <- lines_data %>% dplyr::filter(sectornombre == s)

  p <- ggplot(sector_plot_data, aes(x = factor(fechainicioanio), y = count, fill = status)) +
    geom_bar(stat = "identity", position = "stack") +
    geom_text(data = sector_lines %>% dplyr::filter(fechainicioanio >= 2021 & fechainicioanio <= 2023), aes(x = factor(fechainicioanio), y = line_y, label = paste0(round(avg_avance, 0), "% avance")), color = "black", size = 4.2, angle = 90, inherit.aes = FALSE) +
    geom_text(data = sector_totals, aes(x = factor(fechainicioanio), y = total, label = total, color = "gray10"), inherit.aes = FALSE, vjust = -0.5, size = 5, fontface = "bold") +
    labs(title = paste("Obras -", s), x = "Año de Inicio de Obra", y = "Cantidad de Obras", fill = "Obras") +
    theme_light(base_size = 14) +
    scale_fill_manual(values = c("En ejecución" = "orange", "Finalizadas" = "gray40")) +
    scale_color_identity()

  tryCatch({
    ggsave(file_name, plot = p, dpi = 300, width = 8, height = 6)
    message("Saved ", file_name)
  }, error = function(e) {
    message("Failed to save ", file_name, ": ", e$message)
  })
}
