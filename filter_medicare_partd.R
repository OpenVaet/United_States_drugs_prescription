###############################################################################
# filter_medicare_partd.R
#
# Loads the summary stats file and applies quality/relevance filters:
#   - at least 5 years of non-NA dosage data
#   - at least 1,000 beneficiaries in 2023 (latest_beneficiaries)
#   - at least 500 mean beneficiaries across all available years
#
# Saves the filtered set to medicare_partd_dosage_units_2023_filtered.csv
###############################################################################

library(dplyr)

# ── Configuration ──────────────────────────────────────────────────────────────
input_file  <- "medicare_partd_dosage_units_2023_stats.csv"
output_file <- "medicare_partd_dosage_units_2023_filtered.csv"

min_years       <- 6
min_latest_bene <- 50000
min_mean_bene   <- 10000

# ── Load ───────────────────────────────────────────────────────────────────────
stats <- read.csv(input_file, stringsAsFactors = FALSE)
cat("Loaded:", nrow(stats), "drugs\n")

# ── Filter ─────────────────────────────────────────────────────────────────────
filtered <- stats |>
  filter(
    years_with_data      >= min_years,
    latest_beneficiaries >= min_latest_bene,
    mean_beneficiaries   >= min_mean_bene
  )

cat("After filtering:\n")
cat("  years_with_data >=", min_years, ":",
    sum(stats$years_with_data >= min_years), "\n")
cat("  latest_beneficiaries >=", min_latest_bene, ":",
    sum(stats$latest_beneficiaries >= min_latest_bene, na.rm = TRUE), "\n")
cat("  mean_beneficiaries >=", min_mean_bene, ":",
    sum(stats$mean_beneficiaries >= min_mean_bene, na.rm = TRUE), "\n")
cat("  All three combined:", nrow(filtered), "drugs retained\n")

# ── Save ───────────────────────────────────────────────────────────────────────
write.csv(filtered, output_file, row.names = FALSE)
cat("\nSaved to:", output_file, "\n")
