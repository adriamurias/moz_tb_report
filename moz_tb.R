# Packages
library(tidyverse)

#------------------------------------------------------------------------------
# Import World TB Data
#------------------------------------------------------------------------------
# Data obtained from WHO:
# https://www.who.int/teams/global-tuberculosis-programme/data#csv_files

# WHO Dictionary
dictionary_who_tb <- read_csv('Data/TB_data_dictionary_2022-03-22.csv')

# TB Burden
tb_burden <- read_csv('Data/TB_burden_countries_2022-03-22.csv')
# Variable labels and definitions
dictionary_tb_burden <- tibble(variable_name = names(tb_burden)) %>% 
  left_join(
    dictionary_who_tb[,c("variable_name","dataset","definition")],
    by = "variable_name"
  )

# TB Notifications
tb_notif <- read_csv('Data/TB_notifications_2022-03-23.csv')
# Variable labels and definitions
dictionary_tb_notif <- tibble(variable_name = names(tb_notif)) %>% 
  left_join(
    dictionary_who_tb[,c("variable_name","dataset","definition")],
    by = "variable_name"
  )

#------------------------------------------------------------------------------
# Tidy Data
#------------------------------------------------------------------------------

# Mozambique TB Data
moz_tb <- tb_notif %>% 
  # filter for Mozambique and for dates after year 2000
  filter(iso3 == "MOZ" & year >= 2000) %>% 
  select(year, c_newinc, ret_nrel) %>% 
  # add data from TB burden database
  left_join(
    tb_burden %>% 
      filter(iso3 == "MOZ") %>% 
      select(year, e_inc_100k, e_inc_100k_lo, e_inc_100k_hi,
             e_inc_tbhiv_100k, e_inc_tbhiv_100k_lo, e_inc_tbhiv_100k_hi,
             e_mort_exc_tbhiv_100k, e_mort_exc_tbhiv_100k_lo,
             e_mort_exc_tbhiv_100k_hi, e_mort_tbhiv_100k, e_mort_tbhiv_100k_lo,
             e_mort_tbhiv_100k_hi, e_mort_100k, e_mort_100k_lo, e_mort_100k_hi,
             e_pop_num),
    by = "year"
  )

# Variable labels and definitions
dictionary_moz_tb <- tibble(variable_name = names(moz_tb)) %>% 
  left_join(
    dictionary_who_tb[,c("variable_name","dataset","definition")],
    by = "variable_name"
  )

# Mozambique Data set to be used
moz_tb_modif <- moz_tb %>% 
  mutate(new = c_newinc, # new (unknown TB history) & relapse (ttmt completed)
         defaulters = ret_nrel, # previously treated but not completed ttmt
         total_cases = if_else(is.na(defaulters), new, new + defaulters),
         cases_100k = round(total_cases/e_pop_num * 100000)
  ) %>% 
  select(year,
         # cases detected
         new, defaulters, total_cases, cases_100k,
         # estimated population
         e_pop_num,
         # estimated incidence
         e_inc_100k, e_inc_100k_lo, e_inc_100k_hi,
         # estimated incidence of HIV+ TB cases
         e_inc_tbhiv_100k, e_inc_tbhiv_100k_lo, e_inc_tbhiv_100k_hi,
         # estimated mortality of tb cases living with HIV
         e_mort_tbhiv_100k, e_mort_tbhiv_100k_lo, e_mort_tbhiv_100k_hi,
         # estimated mortality of tb cases (all forms)
         e_mort_100k, e_mort_100k_lo, e_mort_100k_hi,)

saveRDS(moz_tb_modif, file = "data/moz_tb_modif.rds")

#------------------------------------------------------------------------------
# Plot
#------------------------------------------------------------------------------

# Notifications of new, relapse, and unknown previous treatment history cases
plot_noti <- moz_tb_modif %>% 
  ggplot(aes(x = year, y = new)) +
  geom_line(color = "#69b3a2", size = 1.4) +
  geom_point(shape=21, color="#69b3a2", fill="white", size=3) +
  theme_bw(base_size = 18) +
  theme(axis.title.y=element_blank()) +
  scale_x_continuous(breaks = seq(min(moz_tb_modif$year),
                                  max(moz_tb_modif$year), by = 2),
                     minor_breaks = seq(2000, 2020, 1)) +
  scale_y_continuous(breaks = seq(0, max(moz_tb_modif$new), by = 10000),
                     minor_breaks = seq(20000, 100000, 10000),
                     labels = function(x) format(x, big.mark = ",",
                                                 scientific = FALSE)) +
  xlab("Year")

ggsave("figures/plot_noti.png", plot_noti, width = 10, height = 5)

# Evolution of main epidemiologic variables
plot_tb_evol <- moz_tb_modif %>% 
  ggplot(aes(x = year)) +
  # Notifications
  geom_line(aes(y = e_inc_100k, color = "Inicidence"),
            size = 1.4) +
  geom_ribbon(aes(ymin = e_inc_100k_lo, ymax = e_inc_100k_hi),
                  fill = "#4daf4a",
              alpha = 0.1) +
  # Notifications
  geom_line(aes(y = cases_100k, color = "Notifications"),
            size = 1.4) +
  # HIV Positive Proportion
  geom_line(aes(y = e_inc_tbhiv_100k, color = "TB & HIV"),
            size = 1.4) +
  geom_ribbon(aes(ymin = e_inc_tbhiv_100k_lo, ymax = e_inc_tbhiv_100k_hi),
                  fill = "#e41a1c",
              alpha = 0.1) +
  # Mortality
  geom_line(aes(y = e_mort_tbhiv_100k, color = "Mortality"),
            size = 1.4) + 
  geom_ribbon(aes(ymin = e_mort_tbhiv_100k_lo, ymax = e_mort_tbhiv_100k_hi),
                  fill = "#984ea3",
              alpha = 0.1) +
  # Legend
  scale_colour_manual("", 
                      breaks = c(
                        "Inicidence", "Notifications", "TB & HIV", "Mortality"),
                      values = c(
                        "#4daf4a", "#377eb8", "#e41a1c", "#984ea3")) +
  # Aesthetics
  theme_bw(base_size = 14) +
  theme(legend.position="top") +
  scale_x_continuous(breaks = seq(min(moz_tb_modif$year),
                                  max(moz_tb_modif$year), by = 2),
                     minor_breaks = seq(2000, 2020, 1)) +
  scale_y_continuous(breaks = seq(0, max(moz_tb_modif$e_inc_100k_hi), by = 50),
                     minor_breaks = seq(20000, 100000, 10000),
                     labels = function(x) format(x, big.mark = ",",
                                                 scientific = FALSE)) +
  xlab("Year") +
  ylab("Rate per 100,000 population")

ggsave("figures/plot_tb_evol.png", plot_tb_evol, width = 8, height = 6)
