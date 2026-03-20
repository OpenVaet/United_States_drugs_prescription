###############################################################################
# summarise_medicare_partd.R
#
# Reads the deduplicated long-form file produced by extract_medicare_partd.R
# and produces a one-row-per-drug summary for every drug that has data in 2023.
#
# Output columns:
#   brand_name, generic_name,
#   years_with_data      – count of years with non-NA dosage data
#   mean_beneficiaries   – mean across all available years
#   latest_beneficiaries – value in 2023
#   mean_dosage_unit     – mean across all available years
#   latest_dosage_unit   – value in 2023
###############################################################################

library(dplyr)

# ── Configuration ──────────────────────────────────────────────────────────────
input_file  <- "medicare_partd_dosage_units_2012_2023.csv"
output_file <- "medicare_partd_dosage_units_2023_stats.csv"

# ── Load ───────────────────────────────────────────────────────────────────────
raw <- read.csv(input_file, stringsAsFactors = FALSE)

cat("Loaded", nrow(raw), "rows,",
    length(unique(paste(raw$brand_name, raw$generic_name))), "unique drugs\n")

# ── Keep only drugs that have a non-NA row in 2023 ───────────────────────────
drugs_2023 <- raw |>
  filter(year == 2023, !is.na(total_dosage_units)) |>
  distinct(brand_name, generic_name)

cat("Drugs with 2023 data:", nrow(drugs_2023), "\n")

# ── Compute summary stats ────────────────────────────────────────────────────
stats <- raw |>
  semi_join(drugs_2023, by = c("brand_name", "generic_name")) |>
  group_by(brand_name, generic_name) |>
  summarise(
    years_with_data      = sum(!is.na(total_dosage_units)),
    mean_beneficiaries   = round(mean(total_beneficiaries, na.rm = TRUE), 1),
    latest_beneficiaries = total_beneficiaries[year == 2023],
    mean_dosage_unit     = round(mean(total_dosage_units, na.rm = TRUE), 1),
    latest_dosage_unit   = total_dosage_units[year == 2023],
    .groups = "drop"
  ) |>
  arrange(desc(latest_dosage_unit))

cat("Summary rows:", nrow(stats), "\n")
cat("Years-with-data range:", min(stats$years_with_data), "-",
    max(stats$years_with_data), "\n\n")

cat("Top 10 by latest dosage units:\n")
print(head(stats, 10))

# ── Save ───────────────────────────────────────────────────────────────────────
write.csv(stats, output_file, row.names = FALSE)
cat("\nSaved to:", output_file, "\n")
