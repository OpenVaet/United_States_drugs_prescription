###############################################################################
# plot_drug_consumption.R
#
# Plots Medicare Part D consumption (Total Dosage Units) for a given drug,
# fits a linear trend on the pre-2021 period, and projects 95% prediction
# intervals onto 2021-2023, annotating % deviation from the trend.
#
# All font sizes aggressively scaled for Twitter/X card readability.
# Projection labels placed BELOW data points to keep PI ribbon visible.
###############################################################################

library(dplyr)
library(ggplot2)
library(scales)
library(showtext)

# ── Configuration ──────────────────────────────────────────────────────────────
drug_generic   <- "Bosutinib"
csv_file       <- "medicare_partd_dosage_units_2012_2023.csv"
output_file    <- "bosutinib_consumption.png"

trend_end_year <- 2020
proj_years     <- 2021:2023

# ── Fonts ──────────────────────────────────────────────────────────────────────
font_add_google("Lora",        "lora")
font_add_google("Nunito Sans", "nunito")
showtext_auto()

f_title <- "lora"
f_body  <- "nunito"

# ── Load & filter ──────────────────────────────────────────────────────────────
raw <- read.csv(csv_file, stringsAsFactors = FALSE)

df <- raw |>
  filter(tolower(trimws(generic_name)) == tolower(drug_generic)) |>
  group_by(year) |>
  summarise(
    brand_name          = first(brand_name),
    total_dosage_units  = sum(total_dosage_units,  na.rm = TRUE),
    total_beneficiaries = sum(total_beneficiaries, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(year)

if (nrow(df) == 0) stop("No data found for generic_name = '", drug_generic, "'")

cat("Data for", drug_generic, ":\n")
print(df)

# ── Linear model on training period ───────────────────────────────────────────
train <- df |> filter(year <= trend_end_year)

fit <- lm(total_dosage_units ~ year, data = train)
cat("\n-- Linear trend (", min(train$year), "-", trend_end_year, ") --\n")
print(summary(fit))

# ── Predictions + 95% prediction interval for ALL years ──────────────────────
all_years <- tibble(year = seq(min(df$year), max(df$year)))

pred <- predict(fit, newdata = all_years, interval = "prediction", level = 0.95)
pred_df <- all_years |>
  bind_cols(as_tibble(pred)) |>
  rename(predicted = fit, pi_lo = lwr, pi_hi = upr)

# ── Merge with actuals & compute deviations ──────────────────────────────────
plot_df <- df |>
  left_join(pred_df, by = "year") |>
  mutate(
    deviation     = total_dosage_units - predicted,
    deviation_pct = deviation / predicted * 100,
    period        = ifelse(year <= trend_end_year, "trend", "projection")
  )

cat("\n-- Post-trend deviations --\n")
plot_df |>
  filter(year > trend_end_year) |>
  select(year, total_dosage_units, predicted, deviation_pct) |>
  mutate(across(c(predicted, deviation_pct), \(x) round(x, 1))) |>
  print()

# ── Helper: format large numbers as compact "7K", "526K" etc. ────────────────
fmt_k <- function(x) {
  ifelse(x >= 1e6, paste0(round(x / 1e6, 1), "M"),
         ifelse(x >= 1e3, paste0(round(x / 1e3, 0), "K"),
                as.character(round(x))))
}

# ── Label dataframes ─────────────────────────────────────────────────────────

# Training-period labels: just the raw value
train_labels <- plot_df |>
  filter(period == "trend") |>
  mutate(label = fmt_k(total_dosage_units))

# Projection labels: raw value + deviation %
proj_labels <- plot_df |>
  filter(period == "projection") |>
  mutate(
    dev_str = paste0(ifelse(deviation_pct >= 0, "+", ""),
                     sprintf("%.1f%%", deviation_pct)),
    label   = paste0(fmt_k(total_dosage_units), "\n", dev_str)
  )

# ── Palette ──────────────────────────────────────────────────────────────────
col_trend    <- "#2C5F7C"
col_proj     <- "#B44B3A"
col_ribbon   <- "#B44B3A18"
col_grid     <- "#E8E0D8"
col_bg       <- "#FAF7F2"
col_text     <- "#3A3226"
col_annot    <- "#8C5E3C"

# ── Build plot ────────────────────────────────────────────────────────────────
yr_min <- min(df$year)
yr_max <- max(df$year)
y_max  <- max(df$total_dosage_units)

p <- ggplot(plot_df, aes(x = year)) +
  
  # ── 95% PI ribbon ──
  geom_ribbon(
    data = pred_df |> filter(year >= trend_end_year),
    aes(x = year, ymin = pi_lo, ymax = pi_hi),
    fill = col_ribbon, colour = NA
  ) +
  
  # ── Vertical boundary ──
  geom_vline(xintercept = trend_end_year + 0.5,
             linetype = "longdash", colour = col_annot, linewidth = 0.6) +
  
  # ── Trend line: solid on training, dashed on projection ──
  geom_line(
    data = pred_df |> filter(year <= trend_end_year),
    aes(y = predicted),
    colour = col_trend, linewidth = 1.0, linetype = "solid"
  ) +
  geom_line(
    data = pred_df |> filter(year >= trend_end_year),
    aes(y = predicted),
    colour = col_trend, linewidth = 0.9, linetype = "dashed", alpha = 0.5
  ) +
  
  # ── Actual consumption line ──
  geom_line(aes(y = total_dosage_units), colour = col_trend,
            linewidth = 1.2, alpha = 0.3) +
  
  # ── Training-period points ──
  geom_point(
    data = plot_df |> filter(period == "trend"),
    aes(y = total_dosage_units),
    colour = col_trend, fill = col_bg, shape = 21,
    size = 6, stroke = 2
  ) +
  
  # ── Projection-period points ──
  geom_point(
    data = plot_df |> filter(period == "projection"),
    aes(y = total_dosage_units),
    colour = col_proj, fill = col_bg, shape = 21,
    size = 6, stroke = 2
  ) +
  
  # ── Deviation segments ──
  geom_segment(
    data = proj_labels,
    aes(x = year, xend = year, y = total_dosage_units, yend = predicted),
    colour = col_proj, linewidth = 0.7, linetype = "dotted"
  ) +
  
  # ── Training-period value labels (alternating above/below) ──
  geom_label(
    data = train_labels |> mutate(
      nudge = ifelse(row_number() %% 2 == 1, y_max * 0.07, -y_max * 0.07),
      y_lab = total_dosage_units + nudge
    ),
    aes(y = y_lab, label = label),
    family = f_body, fontface = "bold", size = 10,
    colour = col_trend, fill = col_bg,
    label.size = 0, label.padding = unit(0.2, "lines")
  ) +
  
  # ── Projection value + deviation labels (BELOW the points) ──
  geom_label(
    data = proj_labels,
    aes(y = total_dosage_units - y_max * 0.20, label = label),
    family = f_body, fontface = "bold", size = 11,
    colour = col_proj, fill = col_bg, lineheight = 0.85,
    label.size = 0.5, label.r = unit(0.25, "lines"),
    label.padding = unit(0.4, "lines")
  ) +
  
  # ── Zone annotations ──
  annotate("text",
           x = mean(c(yr_min, trend_end_year)), y = Inf,
           label = paste0("Linear trend ", yr_min, " \u2013 ", trend_end_year),
           vjust = 1.6, hjust = 0.5,
           family = f_body, size = 11, colour = col_annot) +
  annotate("text",
           x = mean(proj_years), y = Inf,
           label = "Projected (95% PI)",
           vjust = 1.6, hjust = 0.5,
           family = f_body, size = 11, colour = col_proj,
           fontface = "italic") +
  
  # ── Scales ──
  scale_x_continuous(breaks = yr_min:yr_max,
                     expand = expansion(mult = c(0.03, 0.04))) +
  scale_y_continuous(labels = label_comma(),
                     expand = expansion(mult = c(0.12, 0.16))) +
  
  # ── Labels ──
  labs(
    title    = paste0(df$brand_name[1], " (", drug_generic, ")"),
    subtitle = paste0("Medicare Part D \u2014 Total Dosage Units dispensed per year, ",
                      yr_min, " \u2013 ", yr_max),
    x        = NULL,
    y        = "Total Dosage Units",
    caption  = paste0(
      "Source: CMS Medicare Part D Spending by Drug  |  ",
      "Linear fit R\u00b2 = ", sprintf("%.3f", summary(fit)$r.squared), ",  ",
      "slope = +", format(round(coef(fit)["year"]), big.mark = ","),
      " units/year"
    )
  ) +
  
  # ── Theme ──
  theme_minimal(base_family = f_body, base_size = 32) +
  theme(
    plot.background    = element_rect(fill = col_bg, colour = NA),
    panel.background   = element_rect(fill = col_bg, colour = NA),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(colour = col_grid, linewidth = 0.3),
    panel.grid.minor   = element_blank(),
    axis.text.x        = element_text(colour = col_text, size = 28,
                                      face = "bold"),
    axis.text.y        = element_text(colour = col_text, size = 26),
    axis.title.y       = element_text(colour = col_text, size = 28,
                                      margin = margin(r = 14)),
    plot.title         = element_text(family = f_title, face = "bold",
                                      size = 48, colour = col_text,
                                      margin = margin(b = 6)),
    plot.subtitle      = element_text(family = f_body, size = 30,
                                      colour = col_annot,
                                      margin = margin(b = 20)),
    plot.caption       = element_text(family = f_body, size = 22,
                                      colour = col_annot, hjust = 0,
                                      margin = margin(t = 16)),
    plot.margin        = margin(26, 32, 20, 22)
  )

# ── Save ───────────────────────────────────────────────────────────────────────
ggsave(output_file, plot = p, width = 14, height = 8, dpi = 300, bg = col_bg)
cat("\nPlot saved to:", output_file, "\n")