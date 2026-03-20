###############################################################################
# extract_medicare_partd.R
#
# Extracts Brand Name, Generic Name, Total Dosage Units and Total Beneficiaries
# from CMS Medicare Part D Spending by Drug xlsx files (DYT2016–DYT2023).
#
# Each file covers 5 years of history (e.g. DYT2016 covers 2012–2016).
# The script deduplicates by drug/year, keeping the value from the most recent
# file (i.e. the file with the highest DYT year wins).
###############################################################################

library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)

# ── Configuration ──────────────────────────────────────────────────────────────
data_dir <- "data"  # folder containing the 8 sub-folders

# ── Discover files ─────────────────────────────────────────────────────────────
# Each sub-folder is named "Medicare Part D Spending by Drug DYT20XX"
sub_dirs <- list.dirs(data_dir, recursive = FALSE, full.names = TRUE)
sub_dirs <- sub_dirs[grepl("DYT20\\d{2}$", sub_dirs)]

if (length(sub_dirs) == 0) stop("No DYT sub-folders found in '", data_dir, "'")

cat("Found", length(sub_dirs), "sub-folders:\n")
cat(paste(" ", basename(sub_dirs), collapse = "\n"), "\n\n")

# ── Helper: parse one xlsx file ────────────────────────────────────────────────
parse_one_file <- function(dir_path) {

  # Identify the DYT year from the folder name
  dyt_year <- as.integer(str_extract(basename(dir_path), "\\d{4}$"))

  # Find the xlsx inside
  xlsx_file <- list.files(dir_path, pattern = "\\.xlsx$", full.names = TRUE)[1]
  if (is.na(xlsx_file)) {
    warning("No xlsx found in ", dir_path)
    return(NULL)
  }

  # Identify the target sheet (pattern: "Spending Utilization YTD 20XX")
  sheets <- excel_sheets(xlsx_file)
  target_sheet <- sheets[grepl("Spending.*Utilization.*YTD", sheets, ignore.case = TRUE)]
  if (length(target_sheet) == 0) {
    warning("No 'Spending Utilization YTD' sheet in ", xlsx_file)
    return(NULL)
  }
  target_sheet <- target_sheet[1]

  cat("  Reading:", basename(xlsx_file), " | sheet:", target_sheet, "\n")

  # ── Read row 3 (year headers) and row 4 (column names) ────────────────────
  # Row 3 contains merged cells like "Calendar Year 2012" at the start of each
  # year-block.  Row 4 has the repeated column names.
  # Data starts at row 5.

  # Read the raw header rows (rows 3-4) as character, no col names

  hdr <- read_excel(xlsx_file, sheet = target_sheet, range = "A3:BZ4",
                    col_names = FALSE, col_types = "text")

  year_row <- as.character(hdr[1, ])   # row 3
  col_row  <- as.character(hdr[2, ])   # row 4

  # Extract calendar years from row 3
  year_positions <- which(!is.na(year_row) & grepl("Calendar Year", year_row))
  years <- as.integer(str_extract(year_row[year_positions], "\\d{4}"))

  # Within each year-block the columns repeat:
  #   Total Spending | Total Dosage Units | Total Claims | Total Beneficiaries
  #   | Avg Spending Per Dosage Unit (Weighted) | Avg Spending Per Claim
  #   | Avg Spending Per Beneficiary
  # The block starts at the position found in year_positions.
  # "Total Dosage Units" is offset +1, "Total Beneficiaries" is offset +3

  # ── Read data rows ─────────────────────────────────────────────────────────
  # Suppress warnings about new names / NAs
  raw <- suppressMessages(
    read_excel(xlsx_file, sheet = target_sheet, skip = 3,
               col_names = FALSE, col_types = "text")
  )

  # First two columns are Brand Name / Generic Name
  brand_col   <- 1
  generic_col <- 2

  results <- list()

  for (i in seq_along(years)) {
    yr       <- years[i]
    base_col <- year_positions[i]           # 1-indexed position in the row
    dosage_col <- base_col + 1
    bene_col   <- base_col + 3

    df <- tibble(
      brand_name        = str_trim(raw[[brand_col]]),
      generic_name      = str_trim(raw[[generic_col]]),
      year              = yr,
      total_dosage_units = raw[[dosage_col]],
      total_beneficiaries = raw[[bene_col]],
      source_dyt        = dyt_year
    )

    # Clean: drop rows where brand_name is NA/empty, convert numerics
    df <- df |>
      filter(!is.na(brand_name), brand_name != "") |>
      mutate(
        total_dosage_units  = suppressWarnings(as.numeric(total_dosage_units)),
        total_beneficiaries = suppressWarnings(as.numeric(total_beneficiaries))
      )

    results[[length(results) + 1]] <- df
  }

  bind_rows(results)
}

# ── Process all files ──────────────────────────────────────────────────────────
cat("Parsing files...\n")
all_data <- map_dfr(sort(sub_dirs), parse_one_file)

cat("\nRaw rows extracted:", nrow(all_data), "\n")
cat("Drug/year combinations (before dedup):",
    nrow(distinct(all_data, brand_name, generic_name, year)), "\n")

# ── Deduplicate: keep the entry from the most recent DYT file ────────────────
# A drug+year pair may appear in multiple files (e.g. 2015 data appears in
# DYT2016 through DYT2019).  We keep the row with the highest source_dyt,
# which represents the most recently published figure.
deduped <- all_data |>
  group_by(brand_name, generic_name, year) |>
  slice_max(source_dyt, n = 1, with_ties = FALSE) |>
  ungroup() |>
  select(brand_name, generic_name, year, total_dosage_units, total_beneficiaries) |>
  arrange(brand_name, generic_name, year)

cat("Rows after deduplication:", nrow(deduped), "\n")
cat("Year range:", min(deduped$year), "-", max(deduped$year), "\n")
cat("Unique drugs:", nrow(distinct(deduped, brand_name, generic_name)), "\n\n")

# ── Quick summary ──────────────────────────────────────────────────────────────
cat("Records per year:\n")
print(deduped |> count(year, name = "n_drugs"))

# ── Save ───────────────────────────────────────────────────────────────────────
output_file <- "medicare_partd_dosage_units_2012_2023.csv"
write.csv(deduped, output_file, row.names = FALSE)
cat("\nSaved to:", output_file, "\n")
