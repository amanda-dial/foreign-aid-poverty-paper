library(modelsummary)
library(countrycode)
library(tidyverse)
library(gridExtra)
library(ggthemes)
library(naniar)
library(fixest)
library(giscoR)
library(haven)
library(broom)
library(glue)
library(gt)
library(sf)

# Carnegie & Marinov (2017) Replication: CIRI (Human Rights) ------

load("data/Final_Main.RData")

# Data frame for CIRI regressions

dat_CIRI <- x |>
  mutate(rev_year = case_when(
    year >= 1987 ~ year,
    .default = NA
  )) |>
  select(rev_year, ccode, EV, l2CPcol2, new_empinxavg, polity2avg, starts_with("cov")) |>
  filter(!is.na(new_empinxavg))

# First-stage for CIRI regression

first_stage_CIRI <- feols(EV ~ l2CPcol2 | ccode + rev_year, cluster = ~ ccode + rev_year, ssc = ssc(fixef.K = "full"), data = dat_CIRI)

# Two-stage least squared estimate of Logged Foreign Aid on human rights (No covariates)

two_stage_CIRI <- feols(new_empinxavg ~ 1 | ccode + rev_year | EV ~ l2CPcol2, cluster = ~ ccode + rev_year, data = dat_CIRI)

# Two-stage least squared estimate of Logged Foreign Aid on human rights (With covariates)

covs <- str_subset(colnames(dat_CIRI), "cov") 

two_stage_CIRI_form <- glue("new_empinxavg ~ {str_c(covs, collapse = ' + ')} | ccode + rev_year | EV ~ l2CPcol2")

two_stage_CIRI_2 <- feols(as.formula(two_stage_CIRI_form), cluster = ~ ccode + rev_year, ssc = ssc(fixef.K = "nested"), data = dat_CIRI)

# Carnegie & Marinov (2017) Replication: Polity IV (Democracy) ------

# Data frame for Polity IV Score regressions

dat_Polity <- x |>
  mutate(rev_year = case_when(
    year >= 1987 ~ year,
    .default = NA
  )) |>
  select(rev_year, ccode, EV, l2CPcol2, new_empinxavg, polity2avg, starts_with("cov")) |>
  filter(!is.na(polity2avg))

# First-stage for Polity IV regression

first_stage_Polity <- feols(EV ~ l2CPcol2 | ccode + rev_year, cluster = ~ ccode + rev_year, ssc = ssc(fixef.K = "full"), data = dat_Polity)

# Two-stage least squared estimate of Logged Foreign Aid on human rights (No covariates)

two_stage_Polity <- feols(polity2avg ~ 1 | ccode + rev_year | EV ~ l2CPcol2, cluster = ~ ccode + rev_year, data = dat_Polity)

# Two-stage least squared estimate of Logged Foreign Aid on human rights (With covariates)

two_stage_Polity_form <- glue("polity2avg ~ {str_c(covs, collapse = ' + ')} | ccode + rev_year | EV ~ l2CPcol2")

two_stage_Polity_2 <- feols(as.formula(two_stage_Polity_form), cluster = ~ ccode + rev_year, data = dat_Polity)

# Carnegie & Marinov (2017) Replication: Table 1 ------

model_spec <- data.frame(
  first_column = c("Countries", "Years", "Covariates", "Year Fixed Effects", "Country Fixed Effects", "N"),
  second_column = c("115", "20", "No", "Yes", "Yes", "1,792"),
  third_column = c("115", "20", "Yes", "Yes", "Yes", "1,792"),
  fourth_column = c("95", "20", "No", "Yes", "Yes", "1,818"),
  fifth_column = c("95", "20", "Yes", "Yes", "Yes", "1,818")
)

cm_table1 <- modelsummary(
  list(two_stage_CIRI, two_stage_CIRI_2, two_stage_Polity, two_stage_Polity_2),
  coef_map = c(fit_EV = "Effect of Aid"),
  gof_map = "",
  add_rows = model_spec,
  stars = c("**" = 0.05),
  output = "gt"
) |>
  tab_spanner(label = "CIRI Human Empowerment Index", columns = c("(1)", "(2)")) |>
  tab_spanner(label = "Polity IV Combined Score", columns = c("(3)", "(4)")) |>
  tab_footnote("Note: Standard errors clustered by country and year are reported in parentheses.") |>
  opt_table_font(stack = "old-style") |>
  tab_options(column_labels.font.weight = "bolder",
              table.font.size = 12,
              table.font.weight = "bold")

gtsave(cm_table1, "paper/figures/replication_table_1.png")

# Carnegie & Marinov (2017) Replication: Figure 1 ------

load("data/figuredata.RData")

# Create dataset for Figure 1 replication

dat_fig <- x |>
  mutate(rev_year = case_when(
    year >= 1987 ~ year,
    .default = NA
  )) |> 
  mutate(
    new_empinx_1 = lead(new_empinx, 1),
    new_empinx_2 = lead(new_empinx, 2),
    new_empinx_3 = lead(new_empinx, 3),
    new_empinx_4 = lead(new_empinx, 4),
    new_empinx_5 = lead(new_empinx, 5),
    polity2_1 = lead(polity2, 1),
    polity2_2 = lead(polity2, 2),
    polity2_3 = lead(polity2, 3),
    polity2_4 = lead(polity2, 4),
    polity2_5 = lead(polity2, 5)
  )

# Estimates for effect of aid on CIRI score in years t through t + 5

CIRI_t <- feols(new_empinx ~ 1 | ccode + rev_year | EV ~ l2CPcol2, cluster = ~ ccode + rev_year, data = dat_fig) |>
  tidy()
CIRI_t1 <- feols(new_empinx_1 ~ 1 | ccode + rev_year | EV ~ l2CPcol2, cluster = ~ ccode + rev_year, data = dat_fig) |>
  tidy()
CIRI_t2 <- feols(new_empinx_2 ~ 1 | ccode + rev_year | EV ~ l2CPcol2, cluster = ~ ccode + rev_year, data = dat_fig) |>
  tidy()
CIRI_t3 <- feols(new_empinx_3 ~ 1 | ccode + rev_year | EV ~ l2CPcol2, cluster = ~ ccode + rev_year, data = dat_fig) |>
  tidy()
CIRI_t4 <- feols(new_empinx_4 ~ 1 | ccode + rev_year | EV ~ l2CPcol2, cluster = ~ ccode + rev_year, data = dat_fig) |>
  tidy()
CIRI_t5 <- feols(new_empinx_5 ~ 1 | ccode + rev_year | EV ~ l2CPcol2, cluster = ~ ccode + rev_year, data = dat_fig) |>
  tidy()

CIRI_trend <- rbind(CIRI_t, CIRI_t1, CIRI_t2, CIRI_t3, CIRI_t4, CIRI_t5) |> # CIRI time trend estimates and standard errors
  mutate(time = 0:5, var = "CIRI Human Empowerment Index") 

# Estimates for effect of aid on Polity score in years t through t + 5 

Polity_t <- feols(polity2 ~ 1 | ccode + rev_year | EV ~ l2CPcol2, cluster = ~ ccode + rev_year, data = dat_fig) |>
  tidy()
Polity_t1 <- feols(polity2_1 ~ 1 | ccode + rev_year | EV ~ l2CPcol2, cluster = ~ ccode + rev_year, data = dat_fig) |>
  tidy()
Polity_t2 <- feols(polity2_2 ~ 1 | ccode + rev_year | EV ~ l2CPcol2, cluster = ~ ccode + rev_year, data = dat_fig) |>
  tidy()
Polity_t3 <- feols(polity2_3 ~ 1 | ccode + rev_year | EV ~ l2CPcol2, cluster = ~ ccode + rev_year, data = dat_fig) |>
  tidy()
Polity_t4 <- feols(polity2_4 ~ 1 | ccode + rev_year | EV ~ l2CPcol2, cluster = ~ ccode + rev_year, data = dat_fig) |>
  tidy()
Polity_t5 <- feols(polity2_5 ~ 1 | ccode + rev_year | EV ~ l2CPcol2, cluster = ~ ccode + rev_year, data = dat_fig) |>
  tidy()

Polity_trend <- rbind(Polity_t, Polity_t1, Polity_t2, Polity_t3, Polity_t4, Polity_t5) |> # Polity IV time trend estimates and standard errors
  mutate(time = 0:5, var = "Polity IV Score") 

# Replicate Carnegie & Marinov Figure 1 

dat_trend_rep <- rbind(CIRI_trend, Polity_trend)

cm_figure1 <- ggplot(dat_trend_rep, aes(x = time, y = estimate)) +
  geom_point(size = 2, shape = "square") +
  geom_errorbar(aes(ymin = estimate - 2*std.error, ymax = estimate + 2*std.error), width = 0) +
  geom_hline(yintercept = 0, linetype = 2) +
  facet_wrap(vars(var)) +
  labs(
    x = "Years Since Aid Receipt",
    y = "Effect of Foreign Aid"
  ) +
  theme_bw() +
  theme(text = element_text(family = "Iowan Old Style", face = "bold"))

ggsave("figures/replication_figure_1.png", plot = cm_figure1, height = 6, width = 12)

# Extension: Main Dataset for Extension Analysis ------

# Prepare Poverty and Inequality Platform dataset (Poverty dependent variables)

dat_pip <- read.csv("data/pip.csv") |>
  select(country_name, reporting_year, poverty_line, headcount, poverty_gap, poverty_severity)

dat_pip$ccode <- countrycode(dat_pip$country_name, "country.name", "cown")

# Prepare V-Dem dataset (Additional controls for democracy, corruption, institutions, and economic conditions)

dat_vdem <- read.csv("data/VDem_Full.csv") |>
  select(COWcode, country_name, country_text_id, year, v2x_polyarchy, v2x_egaldem, v2x_corr, v2x_rule) |>
  filter(year >= 1987)

# Prepare World Development Indicators dataset (Mortality dependent variable and additional controls for economic conditions)

dat_wdi <- read.csv("data/wdi_data.csv") |>
  replace_with_na_all(condition = ~.x == "..") |>
  select(-("Time.Code"))

dat_wdi <- dat_wdi[-c(5426:5430), ]

colnames(dat_wdi) <- c("year", "country_name", "country_abb", "mortality_rate", "wdi_gdp", "wdi_gdppc", 
                       "wdi_pop", "wdi_trade", "wdi_govexp", "wdi_agric", "wdi_industry", "wdi_infl") 

dat_wdi$ccode <- countrycode(dat_wdi$country_name, "country.name", "cown")

dat_wdi <- dat_wdi |>
  mutate_at(c("year", "mortality_rate", "wdi_gdp", "wdi_gdppc", "wdi_pop", "wdi_trade", "wdi_govexp", 
              "wdi_agric","wdi_industry", "wdi_infl"), as.numeric) |>
  distinct(ccode, year, .keep_all = TRUE)

# Prepare Carnegie & Marinov dataset

load("data/Final_Main.RData")

dat_cm <- x |>
  select(year, ccode, EV, l2CPcol2, new_empinxavg, polity2avg, new_empinx, polity2, starts_with("cov")) 

# Merge datasets (main cleaned dataset for analysis)

dat_main <- dat_cm |>
  left_join(dat_pip, by = c("year" = "reporting_year", "ccode" = "ccode")) |>
  distinct(ccode, year, .keep_all = TRUE) |>
  left_join(dat_vdem, by = c("year" = "year", "ccode" = "COWcode")) |>
  left_join(dat_wdi, by = c("year" = "year", "ccode" = "ccode")) |>
  select(-c(country_name.x, country_name.y, country_text_id, country_name, country_abb)) |>
  mutate(across(c(wdi_gdp, wdi_gdppc, wdi_pop, wdi_govexp:wdi_industry), ~log(.))) |>
  mutate(rev_year = case_when(year >= 1987 ~ year, .default = NA))

# Extension: Figure 1 (Map of Mortality Rate, Poverty Gap, and Democracy) ------

dat_fig2 <- dat_main |> # Create dataframe for Figure 1
  group_by(ccode) |>
  filter(year >= 1987 & year <= 2011) |>
  mutate(
    mean_poly = mean(v2x_polyarchy),
    mean_mort = mean(mortality_rate),
    mean_povgap = mean(poverty_gap, na.rm = TRUE)
  ) |>
  select(year, ccode, v2x_polyarchy, mean_poly, mortality_rate, mean_mort, poverty_gap, mean_povgap) |>
  mutate(
    democracy = case_when(
      mean_poly >= 0.5 ~ "High Democracy",
      mean_poly < 0.5 ~ "Low Democracy"
    ),
    mortality = case_when(
      mean_mort < 39 ~ "Below Median",
      mean_mort >= 39 ~ "Above Median"
    ),
    poverty = case_when(
      mean_povgap < 0.36 ~ "Below Median",
      mean_povgap >= 0.36 ~ "Above Median"
    )
  ) |>
  mutate_at(c("democracy", "mortality", "poverty"), as.factor)

dat_fig2$democracy <- factor(dat_fig2$democracy, levels = c("High Democracy", "Low Democracy")) # Reorder factor levels
dat_fig2$mortality <- factor(dat_fig2$mortality, levels = c("Below Median", "Above Median"))
dat_fig2$poverty <- factor(dat_fig2$poverty, levels = c("Below Median", "Above Median"))

dat_fig2$country <- countrycode(dat_fig2$ccode, "cown", "country.name") # Obtain country names

world <- gisco_get_countries() # Get world shapefile

dat_fig2 <- dat_fig2 |>
  left_join(world, by = c("country" = "NAME_ENGL"))

# Create map for mortality, poverty, and democracy

gg_dem <- ggplot(world) +
  geom_sf() +
  geom_sf(data = dat_fig2, mapping = aes(geometry = geometry, fill = democracy), size = 0.2) +
  scale_fill_manual(
    name = "",
    values = c(`High Democracy` = "#03396c", `Low Democracy` = "#b3cde0")
  ) +
  ggtitle("Democracy") +
  theme_map() +
  theme(
    legend.position = "right",
    plot.title = element_text(hjust = 0.5, face = "bold", family = "Iowan Old Style"),
    legend.text = element_text(family = "Iowan Old Style")
  )


gg_mort <- ggplot(world) +
  geom_sf() +
  geom_sf(data = dat_fig2, mapping = aes(geometry = geometry, fill = mortality), size = 0.2) +
  scale_fill_manual(
    name = "",
    values = c(`Below Median` = "#03396c", `Above Median` = "#b3cde0")) +
  ggtitle("Infant Mortality") +
  theme_map() +
  theme(
    legend.position = "right",
    plot.title = element_text(hjust = 0.5, face = "bold", family = "Iowan Old Style"),
    legend.text = element_text(family = "Iowan Old Style")
  )

gg_pov <- ggplot(world) +
  geom_sf() +
  geom_sf(data = dat_fig2, mapping = aes(geometry = geometry, fill = poverty), size = 0.2) +
  scale_fill_manual(
    name = "",
    values = c(`Below Median` = "#03396c", `Above Median` = "#b3cde0")) +
  ggtitle("Poverty Gap") +
  theme_map() +
  theme(
    legend.position = "right",
    plot.title = element_text(hjust = 0.5, face = "bold", family = "Iowan Old Style"),
    legend.text = element_text(family = "Iowan Old Style")
  )

fig_1 <- grid.arrange(gg_dem, gg_mort, gg_pov)

ggsave(fig_1, filename = "figures/figure_1.png", width = 5)

# Extension: Appendix Table 1 (Descriptive Statistics) ------

means <- dat_main |> # Calculate the mean for each variable
  summarise(across(c(EV, new_empinx, polity2, mortality_rate, poverty_gap, v2x_polyarchy, v2x_egaldem, v2x_corr, 
                     v2x_rule, wdi_gdppc, wdi_gdp, wdi_pop, wdi_trade, wdi_govexp, wdi_agric, wdi_industry, wdi_infl), \(x) mean(x, na.rm = TRUE))) |>
  gather(key = "Variable", value = "Mean", everything())

sd <- dat_main |> # Calculate the standard deviation for each variable
  summarise(across(c(EV, new_empinx, polity2, mortality_rate, poverty_gap, v2x_polyarchy, v2x_egaldem, v2x_corr, 
                     v2x_rule, wdi_gdppc, wdi_gdp, wdi_pop, wdi_trade, wdi_govexp, wdi_agric, wdi_industry, wdi_infl), \(x) sd(x, na.rm = TRUE))) |>
  gather(key = "Variable", value = "Standard Deviation", everything())

obs <- dat_main |> # Count number of observations
  summarise(across(c(EV, new_empinx, polity2, mortality_rate, poverty_gap, v2x_polyarchy, v2x_egaldem, v2x_corr, 
                     v2x_rule, wdi_gdppc, wdi_gdp, wdi_pop, wdi_trade, wdi_govexp, wdi_agric, wdi_industry, wdi_infl), ~ sum(!is.na(.)))) |>
  gather(key = "Variable", value = "Observations", everything())

desc_stats <- means |> # Join into a single descriptive dataset
  left_join(sd, by = c("Variable")) |>
  left_join(obs, by = c("Variable")) |>
  mutate_if(is.numeric, round, 2)

desc_stats$Observations <- formatC(desc_stats$Observations, big.mark = ",") # Add commas

desc_stats <- desc_stats |> # Rename variables
  mutate(Variable = recode(
    Variable, 
    EV = "Logged Net EU Aid",
    new_empinx = "Human Rights (CIRI Index)",
    polity2 = "Polity IV Score",
    mortality_rate = "Infant Mortality Rate (per 1,000 births)",
    poverty_gap = "Poverty Gap (%)",
    v2x_polyarchy = "Electoral Democracy Index",
    v2x_egaldem = "Egalitarian Democracy Index",
    v2x_corr = "Political Corruption Index",
    v2x_rule = "Rule of Law Index",
    wdi_gdppc = "Log GDP per Capita",
    wdi_gdp = "Log GDP",
    wdi_pop = "Log Population",
    wdi_trade = "Trade (% GDP)",
    wdi_govexp = "Log Government Expenditure",
    wdi_agric = "Log Agriculture, Value Added",
    wdi_industry = "Log Industry, Value Added",
    wdi_infl = "Inflation Rate (%)"))

appx_table_1 <- gt(desc_stats) |>
  tab_row_group(
    label = "D. Economic Characteristics",
    rows = 10:17
  ) |>
  tab_row_group(
    label = "C. Human Rights, Democracy, & Institutions",
    rows = c(2:3, 6:9)
  ) |>
  tab_row_group(
    label = "B. Poverty",
    rows = c(4, 7, 5, 6)
  ) |>
  tab_row_group(
    label = "A. EU Foreign Aid",
    rows = 1
  ) |>
  opt_table_font(stack = "old-style") |>
  tab_options(column_labels.font.weight = "bolder",
              table.font.size = 12,
              table.font.weight = "bold")

gtsave(appx_table_1, "figures/appendix_table_1.png")

# Extension: First Stage Regressions ------

dat_povgap <- dat_main |> # Dataset for poverty gap regressions
  filter(!is.na(poverty_gap))

dat_mort <- dat_main |> # Dataset for mortality rate
  filter(!is.na(mortality_rate))

povgap_fs <- feols(EV ~ l2CPcol2 | ccode + rev_year, cluster = ~ ccode + rev_year, data = dat_povgap)
 
mortality_fs <- feols(EV ~ l2CPcol2 | ccode + rev_year, cluster = ~ ccode + rev_year, data = dat_mort)

# Extension: Appendix Table 2 (First Stage Regression Results) ------

model_spec1 <- data.frame(
  first_column = c("Countries", "Years", "Year Fixed Effects", "Country Fixed Effects", "N"),
  second_column = c("89", "22", "Yes", "Yes", "458"),
  third_column = c("89", "22", "Yes", "Yes", "458")
)

appx_table_2 <- modelsummary(
  list(mortality_fs, povgap_fs),
  coef_map = c(l2CPcol2 = "Colony"),
  gof_map = "",
  add_rows = model_spec1,
  stars = c("**" = 0.05),
  output = "gt"
) |>
  tab_spanner(label = "Mortality Rate Regression", columns = c("(1)")) |>
  tab_spanner(label = "Poverty Gap Regression", columns = c("(2)")) |>
  opt_table_font(stack = "old-style") |>
  tab_options(column_labels.font.weight = "bolder",
              table.font.size = 12,
              table.font.weight = "bold")

gtsave(appx_table_2, "figures/appendix_table_2.png")

# Extension: 2SLS Estimates (Mortality Rate) ------

# No covariates

mort_ts1 <- feols(mortality_rate ~ 1 | ccode + rev_year | EV ~ l2CPcol2, cluster = ~ ccode + rev_year, data = dat_mort)

# Electoral democracy and economic covariates

econ_covs <- str_subset(colnames(dat_mort), "^wdi_")
mort_ts2_form <- glue("mortality_rate ~ v2x_polyarchy + v2x_rule + v2x_corr + {str_c(econ_covs, collapse = ' + ')} | ccode + rev_year | EV ~ l2CPcol2")
mort_ts2 <- feols(as.formula(mort_ts2_form), cluster = ~ ccode + rev_year, data = dat_mort)

# Egalitarian democracy and economic covariates

mort_ts3_form <- glue("mortality_rate ~ v2x_egaldem + v2x_rule + v2x_corr + {str_c(econ_covs, collapse = ' + ')} | ccode + rev_year | EV ~ l2CPcol2")
mort_ts3 <- feols(as.formula(mort_ts3_form), cluster = ~ ccode + rev_year, data = dat_mort)

# Polity IV score and economic covariates

mort_ts4_form <- glue("mortality_rate ~ polity2 + v2x_rule + v2x_corr + {str_c(econ_covs, collapse = ' + ')} | ccode + rev_year | EV ~ l2CPcol2")
mort_ts4 <- feols(as.formula(mort_ts4_form), cluster = ~ ccode + rev_year, data = dat_mort)

# Rule of law, corruption, and economic covariates

mort_ts5_form <- glue("mortality_rate ~ v2x_rule + v2x_corr + {str_c(econ_covs, collapse = ' + ')} | ccode + rev_year | EV ~ l2CPcol2")
mort_ts5 <- feols(as.formula(mort_ts5_form), cluster = ~ ccode + rev_year, data = dat_mort)

# Only economic covariates

mort_ts6_form <- glue("mortality_rate ~ {str_c(econ_covs, collapse = ' + ')} | ccode + rev_year | EV ~ l2CPcol2")
mort_ts6 <- feols(as.formula(mort_ts6_form), cluster = ~ ccode + rev_year, data = dat_mort)

# Extension: 2SLS Estimates (Poverty Gap) ------

# No covariates

povgap_ts1 <- feols(poverty_gap ~ 1 | ccode + rev_year | EV ~ l2CPcol2, cluster = ~ ccode + rev_year, data = dat_povgap)

# Electoral democracy and economic covariates

econ_covs <- str_subset(colnames(dat_povgap), "^wdi_")
povgap_ts2_form <- glue("poverty_gap ~ v2x_polyarchy + v2x_rule + v2x_corr + {str_c(econ_covs, collapse = ' + ')} | ccode + rev_year | EV ~ l2CPcol2")
povgap_ts2 <- feols(as.formula(povgap_ts2_form), cluster = ~ ccode + rev_year, data = dat_povgap)

# Egalitarian democracy and economic covariates

povgap_ts3_form <- glue("poverty_gap ~ v2x_egaldem + v2x_rule + v2x_corr + {str_c(econ_covs, collapse = ' + ')} | ccode + rev_year | EV ~ l2CPcol2")
povgap_ts3 <- feols(as.formula(povgap_ts3_form), cluster = ~ ccode + rev_year, data = dat_povgap)

# Polity IV score and economic covariates

povgap_ts4_form <- glue("poverty_gap ~ polity2 + v2x_rule + v2x_corr + {str_c(econ_covs, collapse = ' + ')} | ccode + rev_year | EV ~ l2CPcol2")
povgap_ts4 <- feols(as.formula(povgap_ts4_form), cluster = ~ ccode + rev_year, data = dat_povgap)

# Rule of law, corruption, and economic covariates

povgap_ts5_form <- glue("poverty_gap ~ v2x_rule + v2x_corr + {str_c(econ_covs, collapse = ' + ')} | ccode + rev_year | EV ~ l2CPcol2")
povgap_ts5 <- feols(as.formula(povgap_ts5_form), cluster = ~ ccode + rev_year, data = dat_povgap)

# Only economic covariates

povgap_ts6_form <- glue("poverty_gap ~ {str_c(econ_covs, collapse = ' + ')} | ccode + rev_year | EV ~ l2CPcol2")
povgap_ts6 <- feols(as.formula(povgap_ts6_form), cluster = ~ ccode + rev_year, data = dat_povgap)

# Extension: Table 1 (Main Regression Table) ------

model_spec2 <- data.frame(
  first_column = c("Countries", "Years", "Fixed Effects", "Economic Covariates", "N"),
  second_column = c("89", "22", "Yes", "No", "458"),
  third_column = c("60", "22", "Yes", "Yes", "308"),
  fourth_column = c("60", "22", "Yes", "Yes", "308"),
  fifth_column = c("58", "22", "Yes", "Yes", "301"),
  sixth_column = c("60", "22", "Yes", "Yes", "308"),
  seventh_column = c("61", "22", "Yes", "Yes", "315"),
  eighth_column = c("114", "22", "Yes", "No", "2,483"),
  ninth_column = c("76", "22", "Yes", "Yes", "1,159"),
  tenth_column = c("76", "22", "Yes", "Yes", "1,159"),
  eleventh_column = c("72", "22", "Yes", "Yes", "1,114"),
  twelveth_column = c("76", "22", "Yes", "Yes", "1,159"),
  thirteenth_column = c("80", "22", "Yes", "Yes", "1,223")
)

table_1 <- modelsummary(
  list(
    povgap_ts1, povgap_ts2, povgap_ts3, povgap_ts4, povgap_ts5, povgap_ts6,
    mort_ts1, mort_ts2, mort_ts3, mort_ts4, mort_ts5, mort_ts6
  ),
  coef_map = c(
    fit_EV = "Effect of Aid",
    v2x_polyarchy = "Electoral Democracy",
    v2x_egaldem = "Egalitarian Democracy",
    polity2 = "Polity IV Score",
    v2x_rule = "Rule of Law",
    v2x_corr = "Corruption"
  ),
  gof_map = "",
  add_rows = model_spec2,
  stars = c("**" = 0.05),
  output = "gt"
) |>
  tab_spanner(label = "Poverty Gap", columns = c("(1)":"(6)")) |>
  tab_spanner(label = "Mortality Rate", columns = c("(7)":"(12)")) |>
  opt_table_font(stack = "old-style") |>
  tab_options(
    column_labels.font.weight = "bolder",
    table.font.size = 12,
    table.font.weight = "bold"
  ) |>
  tab_row_group(label = "", rows = c(1:12)) |>
  tab_footnote(footnote = "Note: Fixed effects for country and year are included in each model. 
                Standard errors are robust and clustered at the country and year levels. Economic 
                covariates not shown include: Log GDP per Capita, Log GDP, Trade Openness, Log 
                Government Expenditure, Log Total Population, Log Agriculture Value Added, Log 
                Industry Value Added, and Inflation Rate.")

gtsave(table_1, filename = "figures/table_1.png")

# Extension: Figure 2 (Aid & Poverty Time Trends) ------

dat_main <- dat_main |> # Create time trend variables
mutate(
  povgap_1 = lead(poverty_gap, 1),
  povgap_2 = lead(poverty_gap, 2),
  povgap_3 = lead(poverty_gap, 3),
  povgap_4 = lead(poverty_gap, 4),
  povgap_5 = lead(poverty_gap, 5),
  mort_1 = lead(mortality_rate, 1),
  mort_2 = lead(mortality_rate, 2),
  mort_3 = lead(mortality_rate, 3),
  mort_4 = lead(mortality_rate, 4),
  mort_5 = lead(mortality_rate, 5)
)

# Poverty gap time trend regressions

povgap_t <- feols(poverty_gap ~ 1 | ccode + rev_year | EV ~ l2CPcol2, cluster = ~ ccode + rev_year, data = dat_main) |>
  tidy()
povgap_t1 <- feols(povgap_1 ~ 1 | ccode + rev_year | EV ~ l2CPcol2, cluster = ~ ccode + rev_year, data = dat_main) |>
  tidy()
povgap_t2 <- feols(povgap_2 ~ 1 | ccode + rev_year | EV ~ l2CPcol2, cluster = ~ ccode + rev_year, data = dat_main) |>
  tidy()
povgap_t3 <- feols(povgap_3 ~ 1 | ccode + rev_year | EV ~ l2CPcol2, cluster = ~ ccode + rev_year, data = dat_main) |>
  tidy()
povgap_t4 <- feols(povgap_4 ~ 1 | ccode + rev_year | EV ~ l2CPcol2, cluster = ~ ccode + rev_year, data = dat_main) |>
  tidy()
povgap_t5 <- feols(povgap_5 ~ 1 | ccode + rev_year | EV ~ l2CPcol2, cluster = ~ ccode + rev_year, data = dat_main) |>
  tidy()

povgap_trend <- rbind(povgap_t, povgap_t1, povgap_t2, povgap_t3, povgap_t4, povgap_t5) |> # Poverty gap estimates and standard errors
  mutate(time = 0:5, var = "Poverty Gap") 

# Mortality rate time trend regressions

mort_t <- feols(mortality_rate ~ 1 | ccode + rev_year | EV ~ l2CPcol2, cluster = ~ ccode + rev_year, data = dat_main) |>
  tidy()
mort_t1 <- feols(mort_1 ~ 1 | ccode + rev_year | EV ~ l2CPcol2, cluster = ~ ccode + rev_year, data = dat_main) |>
  tidy()
mort_t2 <- feols(mort_2 ~ 1 | ccode + rev_year | EV ~ l2CPcol2, cluster = ~ ccode + rev_year, data = dat_main) |>
  tidy()
mort_t3 <- feols(mort_3 ~ 1 | ccode + rev_year | EV ~ l2CPcol2, cluster = ~ ccode + rev_year, data = dat_main) |>
  tidy()
mort_t4 <- feols(mort_4 ~ 1 | ccode + rev_year | EV ~ l2CPcol2, cluster = ~ ccode + rev_year, data = dat_main) |>
  tidy()
mort_t5 <- feols(mort_5 ~ 1 | ccode + rev_year | EV ~ l2CPcol2, cluster = ~ ccode + rev_year, data = dat_main) |>
  tidy()

mort_trend <- rbind(mort_t, mort_t1, mort_t2, mort_t3, mort_t4, mort_t5) |> # Mortality rate estimates and standard errors
  mutate(time = 0:5, var = "Mortality Rate") 

# Create mortality rate and poverty gap time trend plots

povgap_figure_2 <- ggplot(povgap_trend, aes(x = time, y = estimate)) +
  geom_point(size = 2, shape = "square") +
  geom_errorbar(aes(ymin = estimate - 2*std.error, ymax = estimate + 2*std.error), width = 0) +
  geom_hline(yintercept = 0, linetype = 2) +
  facet_wrap(vars(var)) +
  labs(
    x = "Years Since Aid Receipt",
    y = "Effect of Foreign Aid"
  ) +
  theme_bw() +
  theme(text = element_text(family = "Iowan Old Style", face = "bold", size = 18))
  
mort_figure_2 <- ggplot(mort_trend, aes(x = time, y = estimate)) +
  geom_point(size = 2, shape = "square") +
  geom_errorbar(aes(ymin = estimate - 2*std.error, ymax = estimate + 2*std.error), width = 0) +
  geom_hline(yintercept = 0, linetype = 2) +
  facet_wrap(vars(var)) +
  labs(
    x = "Years Since Aid Receipt",
    y = "Effect of Foreign Aid"
  ) +
  theme_bw() +
  theme(text = element_text(family = "Iowan Old Style", face = "bold", size = 18))

figure_2 <- grid.arrange(povgap_figure_2, mort_figure_2, nrow = 1)

ggsave("figures/figure_2.png", plot = figure_2, height = 6, width = 12)

