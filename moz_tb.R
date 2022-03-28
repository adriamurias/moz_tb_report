# Packages
library(tidyverse)
library(ggthemes)
library(ggthemr)

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

# # TB incidence disaggregated
# world_tb_disg <- read_csv('Data/TB_burden_age_sex_2022-03-22.csv')
# # Variable labels and definitions
# metadata_world_tb_disg <- tibble(variable_name = names(world_tb_disg)) %>% 
#   left_join(
#     dictionary_world_tb[,c("variable_name","dataset","definition")],
#     by = "variable_name"
#   )

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
             e_mort_exc_tbhiv_100k, e_mort_exc_tbhiv_100k_lo, e_mort_exc_tbhiv_100k_hi,
             e_mort_tbhiv_100k, e_mort_tbhiv_100k_lo, e_mort_tbhiv_100k_hi,
             e_mort_100k, e_mort_100k_lo, e_mort_100k_hi, e_pop_num),
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

#------------------------------------------------------------------------------
# Tidy Data
#------------------------------------------------------------------------------

# Notifications of new and relapse cases (with unknown previous ttmt history)
plot_noti <- moz_tb_modif %>% 
  ggplot(aes(x = year, y = new)) +
  geom_line(color = "#69b3a2", size = 1.4) +
  geom_point(shape=21, color="#69b3a2", fill="white", size=3) +
  theme_bw(base_size = 22) +
  theme(axis.title.y=element_blank()) +
  scale_x_continuous(breaks = seq(min(moz_tb_modif$year),
                                  max(moz_tb_modif$year), by = 2),
                     minor_breaks = seq(2000, 2020, 1)) +
  scale_y_continuous(breaks = seq(0, max(moz_tb_modif$new), by = 10000),
                     minor_breaks = seq(20000, 100000, 10000),
                     labels = function(x) format(x, big.mark = ",",
                                                 scientific = FALSE)) +
  xlab("Year")

ggsave("figures/plot_recruitment.png", plot_noti, width = 10)

# Evolution of Main Epidemiological Elements
ggplot(moz_tb_modif, aes(x = year)) +
  # Notifications
  # geom_line(aes(y = total_cases),
  #           color = "#beaed4", size = 0.8) +
  geom_line(aes(y = e_inc_100k, color = "Inicidence"),
            size = 0.8) +
  geom_ribbon(aes(ymin = e_inc_100k_lo, ymax = e_inc_100k_hi),
              alpha = 0.1) +
  # Treatment
  geom_line(aes(y = cases_100k, color = "Treatment"),
            size = 0.8) +
  # HIV Positive Proportion
  geom_line(aes(y = e_inc_tbhiv_100k, color = "TB & HIV +"),
            size = 0.8) +
  geom_ribbon(aes(ymin = e_inc_tbhiv_100k_lo, ymax = e_inc_tbhiv_100k_hi),
              alpha = 0.1) +
  # Mortality
  geom_line(aes(y = e_mort_tbhiv_100k, color = "Mortality"),
            size = 0.8) + 
  geom_ribbon(aes(ymin = e_mort_tbhiv_100k_lo, ymax = e_mort_tbhiv_100k_hi),
              alpha = 0.1) +
  # Legend
  scale_colour_manual("", 
                      breaks = c(
                        "Inicidence", "Treatment", "TB & HIV +", "Mortality"),
                      values = c(
                        "#7fc97f", "blue", "#d73027", "black")) +
  # Aesthetics
  theme_bw(base_size = 22) +
  theme(axis.title.y=element_blank()) +
  scale_x_continuous(breaks = seq(min(moz_tb_modif$year),
                                  max(moz_tb_modif$year), by = 2),
                     minor_breaks = seq(2000, 2020, 1)) +
  scale_y_continuous(breaks = seq(0, max(moz_tb_modif$new), by = 10000),
                     minor_breaks = seq(20000, 100000, 10000),
                     labels = function(x) format(x, big.mark = ",",
                                                 scientific = FALSE)) +
  xlab("Year")






#------------------------------------------------------------------------------
# Plots
#------------------------------------------------------------------------------

# Notifications
moz_tb %>% 
  ggplot(aes(x = year, y = c_newinc_100k)) +
  geom_line(color = "#69b3a2", size = 0.8) +
  geom_point(shape=21, color="black", fill="#69b3a2", size=3) +
  scale_x_continuous(breaks = seq(min(moz_tb$year),
                                  max(moz_tb$year), by = 1)) +
  scale_y_continuous(breaks = seq(0, max(moz_tb$c_newinc_100k), by =20))

# Incidence
moz_tb %>% 
  ggplot(aes(x = year, y = e_inc_100k)) +
  geom_line(color = "#69b3a2", size = 0.8) +
  geom_point(shape=21, color="black", fill="#69b3a2", size=3) +
  scale_x_continuous(breaks = seq(min(moz_tb$year),
                                  max(moz_tb$year), by = 1)) +
  scale_y_continuous(breaks = seq(0, 500, by = 5))

# Notifications of new and relapse cases (with unknown previous ttmt history)
plot_noti <- moz_tb_noti %>% 
  ggplot(aes(x = year, y = c_newinc)) +
  geom_line(color = "#69b3a2", size = 1.4) +
  geom_point(shape=21, color="#69b3a2", fill="white", size=3) +
  theme_bw(base_size = 22) +
  theme(axis.title.y=element_blank()) +
  scale_x_continuous(breaks = seq(min(moz_tb_noti$year),
                                  max(moz_tb_noti$year), by = 2),
                     minor_breaks = seq(2000, 2020, 1)) +
  scale_y_continuous(breaks = seq(0, max(moz_tb_noti$c_newinc), by = 10000),
                     minor_breaks = seq(20000, 100000, 10000),
                     labels = function(x) format(x, big.mark = ",",
                                                 scientific = FALSE)) +
  xlab("Year") #+
# ylab("Number of notifications")

ggsave("figures/plot_recruitment.png", plot_noti, width = 10)

# Notifications of new and relapse cases (with unknown previous ttmt history)
ggplot() +
  geom_line(data = moz_tb_noti,
            aes(x = year, y = notif_100k),
            color = "#beaed4", size = 0.8) +
  geom_line(data = moz_tb,
            aes(x = year, y = e_inc_100k),
            color = "#7fc97f", size = 0.8) +
  geom_line(data = moz_tb,
            aes(x = year, y = e_mort_100k),
            color = "#d73027", size = 0.8) +
  geom_line(data = moz_tb,
            aes(x = year, y = c_cdr),
            color = "black", size = 0.8) +
  scale_x_continuous(breaks = seq(min(moz_tb$year),
                                  max(moz_tb$year), by = 1)) +
  scale_y_continuous(breaks = seq(0, max(moz_tb$e_inc_100k), by = 20))
