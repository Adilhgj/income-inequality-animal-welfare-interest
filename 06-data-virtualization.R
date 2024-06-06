library(here)
library(tidyverse)
library(lme4)
library(lmerTest)
library(performance)
library(broom.mixed)
library(ggplot2)
library(gridExtra)
library(ggeffects)
library(corrplot)
library(Hmisc)
library(ggrepel)
library(ggcorrplot)

# Load data set ------------------------------------------------------------

here("data", "2023-03-26-search-interest-year-state") |> 
  read_rds() |> 
  filter(
    keyword == 
      "vegan + vegetarian + dairy free + animal testing + cruelty free + grass fed + free range + cage free"
    # "vegan + vegetarian + dairy free"
    # "animal testing + cruelty free"
    # "grass fed + free range + cage free"
  ) |> 
  select(-keyword) ->
  search_interest_state_year

here("data", "2024-05-18-search-interest") |> 
  read_rds() |> 
  filter(
    keyword == 
      "vegan + vegetarian + dairy free + animal testing + cruelty free + grass fed + free range + cage free"
    # "vegan + vegetarian + dairy free"
    # "animal testing + cruelty free"
    # "grass fed + free range + cage free"
  ) |> 
  select(-keyword) |> 
  mutate(year = as.integer(year))->
  updated_data

search_interest_state_year_long <- bind_rows(search_interest_state_year, updated_data)

# Aggregate the data across samples
search_interest_state_year_long |>
  group_by(state, year) |>
  dplyr::summarize(search_interest = mean(search_interest), .groups = "drop") ->
  search_interest

gini_index <- read_rds(here("data", "2023-03-26-gini-index"))
covariate  <- read_rds(here("data", "2023-03-26-covariate"))

gini_index |> 
  select(-gini_se) |> 
  left_join(covariate, by = c("state", "year")) |> 
  left_join(search_interest, by = c("state", "year")) ->
  data

# Null model
null_mod <- lmer(search_interest ~ 1 + (1 | state) + (1 | year), data)
summary(null_mod)
# How much variance in search_interest do the levels within the state and year
# clusters explain?
performance::icc(null_mod, by_group = TRUE)

# Cluster-mean center (cwc) gini and the control variables by states and years
data |>  
  group_by(state) |> 
  mutate(
    gini_state = mean(gini),
    across(
      .cols    = c(gini, bachelors_or_higher_percent:population_density),
      .fns     = ~. - mean(.),
      .names   = "{.col}_cwc"
    )
  ) |> 
  group_by(year) |> 
  mutate(
    gini_year  = mean(gini),
    across(
      .cols    = ends_with("_cwc"),
      .fns     = ~. - mean(.)
    )
  ) |>
  ungroup() ->
  data

# Fit the linear mixed-effects model
lmerTest::lmer(
  search_interest ~ 
    gini_cwc +
    age_median_cwc +
    bachelors_or_higher_percent_cwc +
    male_percent_cwc +
    white_percent_cwc +
    income_mean_cwc +
    population_density_cwc +
    (1 | year) +
    (1 | state), 
  data
) ->
  model

summary(model)




# 01_Combined_PDPs_One_Graph_Standardized ------------------------------------
covariates <- c("gini_cwc", "age_median_cwc", "bachelors_or_higher_percent_cwc", 
                "male_percent_cwc", "white_percent_cwc", "income_mean_cwc", "population_density_cwc")
covariate_labels <- c("Income Inequality", "Age", "Education", "Gender", "Race", "Income", "Population Density")
collect_pdp_data <- function(covariate, covariate_label, model) {
  effect <- ggpredict(model, terms = covariate)
  min_val <- min(effect$x)
  max_val <- max(effect$x)
  effect$x_standardized <- 2 * (effect$x - min_val) / (max_val - min_val) - 1
  data.frame(covariate = covariate_label, effect)
}
pdp_data <- bind_rows(mapply(collect_pdp_data, covariates, covariate_labels, MoreArgs = list(model = model), SIMPLIFY = FALSE))
pdp_data$covariate <- factor(pdp_data$covariate, levels = covariate_labels)
label_positions <- pdp_data |> 
  group_by(covariate) |> 
  dplyr::summarize(x_standardized = max(x_standardized), predicted = last(predicted))
colors <- c("Income Inequality" = "#034638", "Age" = "#582C83", "Education" = "#C5B783", 
            "Gender" = "#0097A0", "Race" = "#108F74", "Income" = "#A6BB48", "Population Density" = "#63666A")
p <- ggplot(pdp_data, aes(x = x_standardized, y = predicted, color = covariate, group = covariate)) +
  geom_line(size = 1) +
  geom_text(data = label_positions, aes(label = covariate, x = x_standardized + 0.02, y = predicted), 
            size = 3, show.legend = FALSE, hjust = 0, fontface = "bold") + 
  coord_cartesian(clip = "off") +
  theme_minimal() +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    plot.title.position = "plot",
    text = element_text(size = 10, color = "black"),
    legend.position = "none",
    plot.margin = margin(t = 10, r = 120, b = 10, l = 10, unit = "pt")
  ) +
  labs(title = "Partial Dependence Plots for All Covariates",
       x = "Standardized Value",
       y = "Predicted Search Interest Index") +
  scale_color_manual(values = colors)
ggsave(filename = here("graphs", "01_Combined_PDPs_One_Graph_Standardized.png"), plot = p, width = 12, height = 6)

# 02_Trend_of_Search_Interest ----------------------------------------------
custom_colors <- c(
  "North Dakota" = "#034638",
  "Kansas" = "#582C83",
  "Minnesota" = "#C5B783",
  "New Hampshire" = "#0097A0",
  "Oregon" = "#A6BB48", 
  "Other" = "grey"
)
data_last <- data |> 
  group_by(state) |> 
  filter(year == max(year)) |> 
  ungroup()
data_last_filtered <- data_last |> 
  filter(state %in% c("North Dakota", "Kansas", "Minnesota", "New Hampshire", "Oregon"))
p1 <- ggplot(data, aes(x = year, y = search_interest)) +
  geom_point(aes(group = state), color = "grey", alpha = 0.2) +
  geom_line(aes(group = state), color = "grey", alpha = 0.2) +
  geom_point(data = subset(data, state %in% names(custom_colors)), 
             aes(x = year, y = search_interest, color = state), size = 1.5) +
  geom_line(data = subset(data, state %in% names(custom_colors)), 
            aes(x = year, y = search_interest, color = state), size = 1) +
  scale_color_manual(values = custom_colors) +
  coord_cartesian(clip = "off") +
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    plot.title.position = "plot",
    text = element_text(size = 10),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "#F5F5F5"),
    legend.position = "none"
  ) +
  geom_text(data = data_last_filtered, aes(label = state), 
            size = 2, color = "black", hjust = 0, nudge_x = 0.2, fontface = "bold") + 
  labs(title = "Search Interest in Animal Welfare Products Over Time",
       x = "Year",
       y = "Search Interest Index",
       caption = "Note: Data from 2020 are excluded due to the U.S. Census Bureau's temporary halt of the ACS that year.
    Source: Google Trends") +
  scale_x_continuous(breaks = seq(min(data$year), max(data$year), by = 1)) +
  scale_y_continuous(limits = c(0, 100))
ggsave(here("graphs", "02_General_Trend_of_Search_Interest.png"), plot = p1, width = 12, height = 6)

# 03_Heatmap_of_Search_Interest ---------------------------------------------
p2 <- ggplot(data, aes(x = year, y = reorder(state, search_interest, median), fill = search_interest)) +
  geom_tile() +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    legend.position = "none",
    text = element_text(size = 10)
  ) +
  labs(
    title = "Heatmap of Search Interest in Animal Welfare Products",
    caption = "Note: Data from 2020 are excluded due to the U.S. Census Bureau's temporary halt of the ACS that year.
    Source: Google Trends",
    fill = "Search Interest Index"
  ) +
  scale_fill_gradient(low = "green", high = "red") +
  scale_x_continuous(breaks = seq(min(data$year), max(data$year), by = 1))
ggsave(here("graphs", "03_Heatmap_of_Search_Interest.png"), plot = p2, width = 10, height = 6)

# 04_Coefficient_Plot_of_Covariates ------------------------------------------
fixed_effects <- tidy(model, effects = "fixed", conf.int = TRUE) |> 
  filter(term != "(Intercept)") |>
  mutate(term = recode(term,
                       "gini_cwc" = "Income Inequality",
                       "age_median_cwc" = "Age",
                       "bachelors_or_higher_percent_cwc" = "Education",
                       "male_percent_cwc" = "Gender",
                       "white_percent_cwc" = "Race",
                       "income_mean_cwc" = "Income",
                       "population_density_cwc" = "Population Density")) |>
  mutate(term = factor(term, levels = rev(c("Income Inequality", "Age", "Education", "Gender", "Race", "Income", "Population Density"))))
p3 <- ggplot(fixed_effects, aes(x = estimate, y = term)) +
  geom_point() +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2) +
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    plot.title.position = "plot",
    text = element_text(size = 10)
  ) +
  labs(title = "Effect of Covariates on Search Interest",
       x = "Coefficient Estimate",
       y = "Covariate") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red")
ggsave(here("graphs", "04_Coefficient_Plot_of_Covariates.png"), plot = p3, height = 3, width = 6)

# 05_Distribution_of_Gini_Index ----------------------------------------------
data_2022 <- data |> filter(year == 2022)
range_gini <- range(data_2022$gini, na.rm = TRUE)
bin_width <- 1
bin_edges <- seq(floor(range_gini[1]), ceiling(range_gini[2]), by = bin_width)
p4 <- ggplot(data_2022, aes(x = gini)) +
  geom_histogram(fill = "#034638", color = "black", binwidth = bin_width, boundary = bin_edges[1]) +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_blank(),
    # panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    # axis.text.y = element_blank(),
    plot.title.position = "plot",
    text = element_text(size = 10)
  ) +
  coord_cartesian(clip = "on") +
  labs(title = "Distribution of Gini Index",
       x = "Gini Index",
       y = "Frequency") +
  scale_x_continuous(breaks = bin_edges) +
  ylim(0, 15)
ggsave(here("graphs", "05_Distribution_of_Gini_Index.png"), plot = p4, height = 3, width = 5)

# 06_Search_Interest_vs_Income_Inequality -----------------------------------
model <- lm(search_interest ~ gini_cwc, data = data)
intercept <- coef(model)[1]
slope <- coef(model)[2]
formula_text <- paste("y =", round(slope, 2), "x", "+",round(intercept, 2))
p5 <- ggplot(data, aes(x = gini_cwc, y = search_interest)) +
  geom_point() +
  geom_smooth(method = "lm") +
  annotate("text", x = max(data$gini_cwc) * 0.8, y = predict(model, newdata = data.frame(gini_cwc = max(data$gini_cwc) * 0.8)), 
           label = formula_text, hjust = 0.5, vjust = -2, size = 4, color = "blue") +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    plot.title.position = "plot",
    text = element_text(size = 10)
  ) +
  labs(title = "Search Interest vs. Income Inequality",
       x = "Gini Index",
       y = "Search Interest Index")
ggsave(here("graphs", "06_Search_Interest_vs_Income_Inequality.png"), plot = p5)

# 07_Correlation_Matrix (END)------------------------------------------------------
data |> 
  select(
    "Search Interest" = search_interest,
    "Income Inequality" = gini_cwc,
    "Age" = age_median_cwc,
    "Education" = bachelors_or_higher_percent_cwc,
    "Gender" = male_percent_cwc,
    "Race" = white_percent_cwc,
    "Income" = income_mean_cwc,
    "Population Density" = population_density_cwc
  ) ->
  cor_data
rcorr_results <- rcorr(as.matrix(cor_data))
output_file <- here("graphs", "07_Correlation_Matrix_of_Variables.png")
png(filename = output_file, width = 800, height = 600, res = 150)
corrplot(rcorr_results$r, method = "color", 
         col = colorRampPalette(c("#582C83", "white", "#034638"))(200),
         tl.cex = 0.75, tl.col = "black", 
         type = "lower", diag = FALSE, cl.pos = "r", outline = FALSE,
         tl.pos = "lt", cl.cex = 0.75
)
dev.off()

# 08_random_intercepts_slopes_plot ----------------------------------------

# Extract random effects from the model
random_effects <- ranef(model, condVar = TRUE)

# Random intercepts for states
state_intercepts <- data.frame(
  state = rownames(random_effects$state),
  intercept = random_effects$state[,"(Intercept)"]
)

# Merge random intercepts with original data
data_with_intercepts <- data %>%
  left_join(state_intercepts, by = "state")

# Create a variable to highlight specific states
highlight_states <- c("North Dakota", "Kansas", "Minnesota", "New Hampshire", "Oregon")
data_with_intercepts <- data_with_intercepts %>%
  mutate(highlight = ifelse(state %in% highlight_states, state, "Other"))

# Fit LME model for random intercept and slope for demonstration
lme_model <- lmer(search_interest ~ gini_cwc + (gini_cwc | state), data = data)

# Extract fixed effects and random effects
fixed_effects <- fixef(lme_model)
random_effects <- ranef(lme_model)

# Prepare data for plotting
state_intercepts <- random_effects$state[, "(Intercept)"]
state_slopes <- random_effects$state[, "gini_cwc"]
states <- rownames(random_effects$state)

lfits <- data.frame(
  state = factor(states),
  intercept = fixed_effects["(Intercept)"] + state_intercepts,
  slope = fixed_effects["gini_cwc"] + state_slopes,
  highlight = ifelse(states %in% highlight_states, states, "Other")
)

# Define custom colors
custom_colors <- c(
  "North Dakota" = "#034638",
  "Kansas" = "#582C83",
  "Minnesota" = "#C5B783",
  "New Hampshire" = "#0097A0",
  "Oregon" = "#A6BB48", 
  "Other" = "grey"
)

# Visualize the fit
plot <- ggplot(data, aes(x = gini_cwc, y = search_interest, color = ifelse(state %in% highlight_states, state, "Other"))) + 
  geom_point(data = subset(data_with_intercepts, highlight == "Other"), alpha = 1/3) + 
  geom_point(data = subset(data_with_intercepts, highlight != "Other")) + 
  geom_abline(data = subset(lfits, highlight == "Other"), aes(intercept = intercept, slope = slope), color = "grey", alpha = 1/3) +
  geom_abline(data = subset(lfits, highlight != "Other"), aes(intercept = intercept, slope = slope, color = highlight)) +
  geom_abline(intercept = fixed_effects["(Intercept)"], slope = fixed_effects["gini_cwc"], size = 2, alpha = 0.5) +
  labs(
    x = "Gini Index",
    y = "Search Interest",
    caption = bquote(Y == (.(format(round(fixed_effects["(Intercept)"], 1), nsmall = 1)) + N(0, .(format(round(sqrt(181.07), 1), nsmall = 1)))[state]) + 
                       (.(format(round(fixed_effects["gini_cwc"], 1), nsmall = 1)) + N(0, .(format(round(sqrt(15.72), 1), nsmall = 1)))[state]) * X + N(0, .(format(round(sqrt(46.01), 1), nsmall = 1))))) +
  scale_color_manual(values = custom_colors, guide = "none") +  
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    plot.title = element_blank(),
    text = element_text(size = 10),
    panel.grid.minor.y = element_blank(),
    panel.grid.minor.x = element_blank()
  ) +
  coord_cartesian(ylim = c(20, 96.3)) +
  geom_text(data = lfits %>% 
              filter(highlight != "Other") %>%
              group_by(state) %>%
              summarise(intercept = last(intercept), slope = last(slope)),
            aes(x = pmin(3, max(data$gini_cwc)), 
                y = case_when(
                  state == "North Dakota" ~ pmin(intercept + slope * 3, 100) - 2.5,
                  TRUE ~ pmin(intercept + slope * 3, 100)
                ), 
                label = state), 
            nudge_x = 0.2, 
            color = "black", size = 3, hjust = 0.55, vjust = -0.5)

ggsave(here("graphs", "08_Random_Intercepts_Slopes_Plot.png"), plot = plot, width = 12, height = 6)

