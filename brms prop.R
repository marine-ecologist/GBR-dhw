library(brms)
library(tidybayes)
library(tidyverse)
library(sf)
library(patchwork)

fixed_dhw <- 8

lab_levels <- c("No", "Low-Moderate", "High", "Very High-Extreme")

cat_labs <- setNames(lab_levels, as.character(1:4))

interpcolors2 <- c(
  "No" = "#ABD9E9",
  "Low-Moderate" = "#FFFFBF",
  "High" = "#FDAE61",
  "Very High-Extreme" = "#F46D43"
)

target_years <- c(1998, 2002, 2016, 2017, 2020, 2022, 2024, 2025)

obs <- GBRMP_aerial_centroids |>
  st_drop_geometry() |>
  mutate(
    year_num = as.integer(as.character(year)),
    year_c = year_num - 2012,
    aerial = ordered(aerial, levels = c(1, 2, 3, 4))
  ) |>
  filter(
    year_num != 2025,
    is.finite(dhw),
    !is.na(aerial)
  )

ctrl <- list(adapt_delta = 0.95)

pr_b <- prior(normal(0, 2), class = "b")
#
# m_spl <- brm(
#   aerial ~ s(year_num, k = 3) + dhw,
#   data = obs,
#   family = cumulative("logit"),
#   prior = pr_b,
#   chains = 4,
#   cores = 4,
#   iter = 2000,
#   seed = 1,
#   control = ctrl,
#   backend = "rstan"
# )
#
# saveRDS(m_spl, "~/GBR-dhw/outputs/m_spl.rds")
m_spl <- readRDS("~/GBR-dhw/outputs/m_spl.rds")

nd <- tibble(
  year_num = seq(1998, 2024, length.out = 100),
  dhw = fixed_dhw
) |>
  mutate(year_c = year_num - 2012)

ed_df <- nd |>
  add_epred_draws(m_spl, category = "aerial") |>
  rename(cat = aerial) |>
  group_by(year_num, .draw) |>
  arrange(desc(as.integer(cat)), .by_group = TRUE) |>
  mutate(p_ge = cumsum(.epred)) |>
  ungroup() |>
  mutate(
    cat_lab = factor(cat_labs[as.character(cat)], levels = lab_levels)
  )

closest_years <- ed_df |>
  distinct(year_num) |>
  crossing(target_year = target_years) |>
  mutate(diff = abs(year_num - target_year)) |>
  slice_min(diff, by = target_year, n = 1, with_ties = FALSE) |>
  select(target_year, year_num)

mean_ed_df <- ed_df |>
  group_by(year_num, cat_lab) |>
  summarise(
    p_ge = median(p_ge),
    .groups = "drop"
  )

events <- closest_years |>
  filter(target_year <= 2024)

event_years <- c(1998, 2002, 2016, 2017, 2020, 2022, 2024)

brms_exceedance_plot <- mean_ed_df |>
  ggplot() +
  theme_bw() +
  geom_area(
    aes(year_num, p_ge, colour = cat_lab, fill = cat_lab),
    position = "identity",
    alpha = 1
  ) +
  geom_segment(
    data = tibble(year_num = event_years),
    aes(
      x = year_num,
      xend = year_num,
      y = 0,
      yend = 1
    ),
    linetype = "dashed",
    inherit.aes = FALSE
  ) +
  scale_colour_manual(values = interpcolors2, name = "≥ class") +
  scale_fill_manual(values = interpcolors2, name = "≥ class") +
  scale_x_continuous(
    limits = c(1998, 2026),
    breaks = seq(1998, 2026, 4)
  ) +
  scale_y_continuous(
    limits = c(0, 1),
    breaks = c(0, 0.5, 1),
    expand = expansion(mult = 0)
  ) +
  labs(
    x = NULL,
    y = paste0("P(Y ≥ class) @ ", fixed_dhw, " DHW")
  )

brms_exceedance_plot
