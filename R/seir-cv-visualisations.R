#################################################################################
# Visualising results

# Purpose:  Visualise simulation results
# Inputs:   R/simulation-control.R (for libraries and global parameters)
#           Outputs/incidence_results_full_noNPI.Rda
#           Outputs/incidence_results_full_NPI.Rda
# Outputs:  Outputs/yyyy-mm-dd-vaccine-projections.png
#           Outputs/yyyy-mm-dd-incidence-projections.png
#           Outputs/yyyy-mm-dd-hospitalisation-projections.png

#################################################################################


# Todays date (for file names)
today <- Sys.Date()

# Facet labels
scenarioLabs1 <- c(
    'Scenario 1\n2,500 daily arrivals\nSlow rollout',
    'Scenario 2\n2,500 daily arrivals\nFast rollout',
    'Scenario 3\n13,000 daily arrivals\nSlow rollout',
    'Scenario 4\n13,000 daily arrivals\nFast rollout'
)

scenarioLabs2 <- c(
    'Scenario 5\n2,500 daily arrivals\nSlow rollout',
    'Scenario 6\n2,500 daily arrivals\nFast rollout',
    'Scenario 7\n13,000 daily arrivals\nSlow rollout',
    'Scenario 8\n13,000 daily arrivals\nFast rollout'
)


# Load the data
load(here::here('Outputs/incidence_results_full_noNPI.Rda'))

# Visualise the vaccine rollout
v1 <- incidence_results_full_noNPI %>%
    filter(lab %in% c('LowFastDec10_1', 'LowSlowDec10_1')) %>%
    ggplot(aes(x=date, y=vacAdults/sum(popBase[1:3]), group = lab, color = rollout)) +
    geom_line(size = 1.4) +
    scale_x_date('Date', date_labels = '%b', date_breaks = '1 months', limits = as.Date(c('2021-09-01', '2022-08-01'))) +
    scale_y_continuous('Vaccine coverage\n(two doses)', labels = scales::percent, breaks = seq(0, 1, .2), limits = c(0, 1)) +
    scale_color_discrete("") +
    theme(legend.position = 'right')

v2 <- incidence_results_full_noNPI %>%
    filter(lab %in% c('LowFastDec10_1', 'LowSlowDec10_1')) %>%
    ggplot(aes(x=date, y=vac/(POPAUS), group = lab, color = rollout)) +
    geom_line(size = 1.4) +
    scale_x_date('Date', date_labels = '%b', date_breaks = '1 months', limits = as.Date(c('2021-09-01', '2022-08-01'))) +
    scale_y_continuous('Vaccine coverage\n(two doses)', labels = scales::percent, breaks = seq(0, 1, .2), limits = c(0,1.0)) +
    scale_color_discrete("") +
    theme(legend.position = 'right')

ggarrange(v1, v2,
          ncol = 1,
          align = 'v',
          labels = 'AUTO',
          common.legend = TRUE,
          legend = 'right')

ggsave(filename = here::here(paste0('Outputs/', today, '-vaccine-projections.png')), width = 16, height = 11, units = 'cm')


# Summarise % vaccinated by Dec and Feb
incidence_results_full_noNPI %>%
    filter(lab %in% c('LowFastDec10_1', 'LowSlowDec10_1') & date %in% as.Date(c('2021-12-01', '2022-02-01'))) %>%
    mutate(pct1 = round(100*vac/(POPAUS-popBase[4])),
           pct2 = round(100*vac/(POPAUS))) %>%
    select(date, rollout, pct1, pct2)

# Check when 80% is achieved
# Total pop coverage
incidence_results_full_noNPI %>% filter(lab %in% c('LowFastDec10_1', 'LowSlowDec10_1')) %>% mutate(pctTot = abs(0.8 - vac/POPAUS)) %>% select(date, lab, pctTot) %>% filter(pctTot<0.01)
# Adult aged 15+ coverage
incidence_results_full_noNPI %>% filter(lab %in% c('LowFastDec10_1', 'LowSlowDec10_1')) %>% mutate(pctAdult = abs(0.8 - vacAdults/sum(popBase[1:3]))) %>% select(date, lab, pctAdult) %>% filter(pctAdult<0.01)


# First plot, R0 = 3.5 + no interventions
d1noNPI <- incidence_results_full_noNPI %>%
    filter(r0 == 'R0 = 3.5') %>%
    select(date, rollout, arrivals, scenario, iter, H, Hv) %>%
    pivot_longer(cols = c(H, Hv)) %>%
    mutate(group = str_c(iter, name),
           facet = factor(
               case_when(
                   scenario == 'LowSlowDec10' ~ '1',
                   scenario == 'LowFastDec10' ~ '2',
                   scenario == 'LowSlowDec50' ~ '3',
                   scenario == 'LowFastDec50' ~ '4'),
               labels = scenarioLabs1
           )
    )

d1bnoNPI <- d1noNPI %>% group_by(date, facet, name) %>% summarise(value = mean(value))

g1 <-  ggplot() +
    geom_line(data = d1noNPI, aes(x = date, y = value, color = name, linetype = name, group = group), size = 0.1) +
    geom_line(data = d1bnoNPI, aes(x = date, y = value, color = name, linetype = name), size = 0.5) +
    scale_x_date('Date', date_labels = '%b', date_breaks = '2 months', limits = as.Date(c('2021-12-01', '2023-01-01'))) +
    scale_y_continuous('Hospitalised patients', limits = c(0, 20)) +
    scale_color_manual(NULL, labels = c('Unvaccinated', 'Vaccinated'), values = brewer.pal(8, 'Dark2')[3:4]) +
    scale_linetype_manual(NULL, labels = c('Unvaccinated', 'Vaccinated'), values = c('solid', 'dashed')) +
    facet_wrap(~ facet, nrow = 1) +
    theme(text=element_text(size=8), legend.position = 'bottom', strip.text.x = element_text(angle = 0, hjust = 0)) +
    labs(title = 'R0 = 3.5 | Unobtrusive measures in place')


# Second plot, R0 = 7.0 + no interventions

d2noNPI <- incidence_results_full_noNPI %>%
    filter(r0 == 'R0 = 7.0') %>%
    select(date, rollout, arrivals, scenario, iter, H, Hv) %>%
    pivot_longer(cols = c(H, Hv)) %>%
    mutate(group = str_c(iter, name),
           facet = factor(
               case_when(
                   scenario == 'ModrSlowDec10' ~ '1',
                   scenario == 'ModrFastDec10' ~ '2',
                   scenario == 'ModrSlowDec50' ~ '3',
                   scenario == 'ModrFastDec50' ~ '4'),
               labels = scenarioLabs2
           )
    )

d2bnoNPI <- d2noNPI %>% group_by(date, facet, name) %>% summarise(value = mean(value))

g2 <-  ggplot() +
    geom_line(data = d2noNPI, aes(x = date, y = value, color = name, linetype = name, group = group), size = 0.1) +
    geom_line(data = d2bnoNPI, aes(x = date, y = value, color = name, linetype = name), size = 0.5) +
    scale_x_date('Date', date_labels = '%b', date_breaks = '2 months', limits = as.Date(c('2021-12-01', '2023-01-01'))) +
    scale_y_continuous('Hospitalised patients', labels = scales::comma, limits = c(0, NA)) +
    scale_color_manual(NULL, labels = c('Unvaccinated', 'Vaccinated'), values = brewer.pal(8, 'Dark2')[3:4]) +
    scale_linetype_manual(NULL, labels = c('Unvaccinated', 'Vaccinated'), values = c('solid', 'dashed')) +
    facet_wrap(~ facet, nrow = 1) +
    theme(text=element_text(size=8), legend.position = 'bottom', strip.text.x = element_text(angle = 0, hjust = 0)) +
    labs(title = 'R0 = 7.0 | Unobtrusive measures in place')

## Repeat for scenarios with interventions
load(here::here('Outputs/incidence_results_full_NPI.Rda'))

# Third plot, R0 = 3.5 + interventions
d1NPI <- incidence_results_full_NPI %>%
    filter(r0 == 'R0 = 3.5') %>%
    select(date, rollout, arrivals, scenario, iter, H, Hv, restrictionC, restrictionT) %>%
    pivot_longer(cols = c(H, Hv)) %>%
    mutate(group = str_c(iter, name),
           facet = factor(
               case_when(
                   scenario == 'LowSlowDec10' ~ '1',
                   scenario == 'LowFastDec10' ~ '2',
                   scenario == 'LowSlowDec50' ~ '3',
                   scenario == 'LowFastDec50' ~ '4'),
               labels = scenarioLabs1
           )
    )

d1bNPI <- d1NPI %>% group_by(date, facet, name) %>% summarise(value = mean(value))

g3 <-  ggplot() +
    geom_line(data = d1NPI, aes(x = date, y = value, color = name, linetype = name, group = group), size = 0.1) +
    geom_line(data = d1bNPI, aes(x = date, y = value, color = name, linetype = name), size = 0.5) +
    geom_ribbon(data = d1NPI, aes(x = date, ymin = 0, ymax = restrictionC*300, group = iter), fill = 'red', alpha = 0.04) +
    scale_x_date('Date', date_labels = '%b', date_breaks = '2 months', limits = as.Date(c('2021-12-01', '2023-01-01'))) +
    scale_y_continuous('Hospitalised patients', limits = c(0, 20)) +
    scale_color_manual(NULL, labels = c('Unvaccinated', 'Vaccinated'), values = brewer.pal(8, 'Dark2')[3:4]) +
    scale_linetype_manual(NULL, labels = c('Unvaccinated', 'Vaccinated'), values = c('solid', 'dashed')) +
    facet_wrap(~ facet, nrow = 1) +
    theme(text=element_text(size=8), legend.position = 'bottom', strip.text.x = element_text(angle = 0, hjust = 0)) +
    labs(title = 'R0 = 3.5 | Unobtrusive measures + contacts reduced by 70% if cases exceed 10,000')


# Fourth plot, R0 = 7.0 + interventions
d2NPI <- incidence_results_full_NPI %>%
    filter(r0 == 'R0 = 7.0') %>%
    select(date, rollout, arrivals, scenario, iter, H, Hv, restrictionC, restrictionT) %>%
    pivot_longer(cols = c(H, Hv)) %>%
    mutate(group = str_c(iter, name),
           facet = factor(
               case_when(
                   scenario == 'ModrSlowDec10' ~ '1',
                   scenario == 'ModrFastDec10' ~ '2',
                   scenario == 'ModrSlowDec50' ~ '3',
                   scenario == 'ModrFastDec50' ~ '4'),
               labels = scenarioLabs2
           )
    )

d2bNPI <- d2NPI %>% group_by(date, facet, name) %>% summarise(value = mean(value))

g4 <-  ggplot() +
    geom_line(data = d2NPI, aes(x = date, y = value, color = name, linetype = name, group = group), size = 0.1) +
    geom_line(data = d2bNPI, aes(x = date, y = value, color = name, linetype = name), size = 0.5) +
    geom_ribbon(data = d2NPI, aes(x = date, ymin = 0, ymax = restrictionC*210, group = iter), fill = 'red', alpha = 0.04) +
    scale_x_date('Date', date_labels = '%b', date_breaks = '2 months', limits = as.Date(c('2021-12-01', '2023-01-01'))) +
    scale_y_continuous('Hospitalised patients', labels = scales::comma, limits = c(0, NA)) +
    scale_color_manual(NULL, labels = c('Unvaccinated', 'Vaccinated'), values = brewer.pal(8, 'Dark2')[3:4]) +
    scale_linetype_manual(NULL, labels = c('Unvaccinated', 'Vaccinated'), values = c('solid', 'dashed')) +
    facet_wrap(~ facet, nrow = 1) +
    theme(text=element_text(size=8), legend.position = 'bottom', strip.text.x = element_text(angle = 0, hjust = 0)) +
    labs(title = 'R0 = 7.0 | Unobtrusive measures + contacts reduced by 70% if cases exceed 10,000')


# Combine the plots
ggarrange(g1, g2, g3, g4,
          ncol = 1,
          align = 'v',
          labels = 'AUTO',
          common.legend = TRUE,
          legend = 'bottom') +
    geom_text(data = data.frame(x = 0.375, y = 0.18, label = "Restrictions\napplied"),
              mapping = aes(x = x, y = y, label = label),
              colour = "red", size = 1.4, hjust = 'right', inherit.aes = FALSE)

ggsave(filename = here::here(paste0('Outputs/', today, '-hospitalisation-projections.png')), width = 6.5, height = 7.8, units = 'in')

# hi-res version
ggsave(filename = here::here(paste0('Outputs/', today, '-hospitalisation-projections-hi-res.png')), type = 'cairo',  bg = 'white', width = 6.5, height = 7.8, units = 'in', dpi = 600)


############################
### Repeat for incidence ###
############################

# Load the data
load(here::here('Outputs/incidence_results_full_noNPI.Rda'))

# First plot, R0 = 3.5 + no interventions
i1noNPI <- incidence_results_full_noNPI %>%
    filter(r0 == 'R0 = 3.5') %>%
    select(date, rollout, arrivals, scenario, iter, I, Iv) %>%
    pivot_longer(cols = c(I, Iv)) %>%
    mutate(group = str_c(iter, name),
           facet = factor(
               case_when(
                   scenario == 'LowSlowDec10' ~ '1',
                   scenario == 'LowFastDec10' ~ '2',
                   scenario == 'LowSlowDec50' ~ '3',
                   scenario == 'LowFastDec50' ~ '4'),
               labels = scenarioLabs1
           )
    )

i1bnoNPI <- i1noNPI %>% group_by(date, facet, name) %>% summarise(value = mean(value))

i1 <-  ggplot() +
    geom_line(data = i1noNPI, aes(x = date, y = value, color = name, linetype = name, group = group), size = 0.1) +
    geom_line(data = i1bnoNPI, aes(x = date, y = value, color = name, linetype = name), size = 0.5) +
    scale_x_date('Date', date_labels = '%b', date_breaks = '2 months', limits = as.Date(c('2021-12-01', '2023-01-01'))) +
    scale_y_continuous('Infected individuals', limits = c(0, 210)) +
    scale_color_manual(NULL, labels = c('Unvaccinated', 'Vaccinated'), values = brewer.pal(8, 'Dark2')[3:4]) +
    scale_linetype_manual(NULL, labels = c('Unvaccinated', 'Vaccinated'), values = c('solid', 'dashed')) +
    facet_wrap(~ facet, nrow = 1) +
    theme(text=element_text(size=8), legend.position = 'bottom', strip.text.x = element_text(angle = 0, hjust = 0)) +
    labs(title = 'R0 = 3.5 | Unobtrusive measures in place')



# Second plot, R0 = 7.0 + no interventions

i2noNPI <- incidence_results_full_noNPI %>%
    filter(r0 == 'R0 = 7.0') %>%
    select(date, rollout, arrivals, scenario, iter, I, Iv) %>%
    pivot_longer(cols = c(I, Iv)) %>%
    mutate(group = str_c(iter, name),
           facet = factor(
               case_when(
                   scenario == 'ModrSlowDec10' ~ '1',
                   scenario == 'ModrFastDec10' ~ '2',
                   scenario == 'ModrSlowDec50' ~ '3',
                   scenario == 'ModrFastDec50' ~ '4'),
               labels = scenarioLabs2
           )
    )

i2bnoNPI <- i2noNPI %>% group_by(date, facet, name) %>% summarise(value = mean(value))

i2 <-  ggplot() +
    geom_line(data = i2noNPI, aes(x = date, y = value, color = name, linetype = name, group = group), size = 0.1) +
    geom_line(data = i2bnoNPI, aes(x = date, y = value, color = name, linetype = name), size = 0.5) +
    scale_x_date('Date', date_labels = '%b', date_breaks = '2 months', limits = as.Date(c('2021-12-01', '2023-01-01'))) +
    scale_y_continuous('Infected individuals', labels = scales::comma, limits = c(0, NA)) +
    scale_color_manual(NULL, labels = c('Unvaccinated', 'Vaccinated'), values = brewer.pal(8, 'Dark2')[3:4]) +
    scale_linetype_manual(NULL, labels = c('Unvaccinated', 'Vaccinated'), values = c('solid', 'dashed')) +
    facet_wrap(~ facet, nrow = 1) +
    theme(text=element_text(size=8), legend.position = 'bottom', strip.text.x = element_text(angle = 0, hjust = 0)) +
    labs(title = 'R0 = 7.0 | Unobtrusive measures in place')

## Repeat for scenarios with interventions
load(here::here('Outputs/incidence_results_full_NPI.Rda'))

# Third plot, R0 = 3.5 + interventions
i1NPI <- incidence_results_full_NPI %>%
    filter(r0 == 'R0 = 3.5') %>%
    select(date, rollout, arrivals, scenario, iter, I, Iv, restrictionC, restrictionT) %>%
    pivot_longer(cols = c(I, Iv)) %>%
    mutate(group = str_c(iter, name),
           facet = factor(
               case_when(
                   scenario == 'LowSlowDec10' ~ '1',
                   scenario == 'LowFastDec10' ~ '2',
                   scenario == 'LowSlowDec50' ~ '3',
                   scenario == 'LowFastDec50' ~ '4'),
               labels = scenarioLabs1
           )
    )

i1bNPI <- i1NPI %>% group_by(date, facet, name) %>% summarise(value = mean(value))

i3 <-  ggplot() +
    geom_line(data = i1NPI, aes(x = date, y = value, color = name, linetype = name, group = group), size = 0.1) +
    geom_line(data = i1bNPI, aes(x = date, y = value, color = name, linetype = name), size = 0.5) +
    geom_ribbon(data = i1NPI, aes(x = date, ymin = 0, ymax = restrictionC*200, group = iter), fill = 'red', alpha = 0.04) +
    scale_x_date('Date', date_labels = '%b', date_breaks = '2 months', limits = as.Date(c('2021-12-01', '2023-01-01'))) +
    scale_y_continuous('Infected individuals', limits = c(0, 210)) +
    scale_color_manual(NULL, labels = c('Unvaccinated', 'Vaccinated'), values = brewer.pal(8, 'Dark2')[3:4]) +
    scale_linetype_manual(NULL, labels = c('Unvaccinated', 'Vaccinated'), values = c('solid', 'dashed')) +
    facet_wrap(~ facet, nrow = 1) +
    theme(text=element_text(size=8), legend.position = 'bottom', strip.text.x = element_text(angle = 0, hjust = 0)) +
    labs(title = 'R0 = 3.5 | Unobtrusive measures + contacts reduced by 70% if cases exceed 10,000')


# Fourth plot, R0 = 7.0 + interventions
i2NPI <- incidence_results_full_NPI %>%
    filter(r0 == 'R0 = 7.0') %>%
    select(date, rollout, arrivals, scenario, iter, I, Iv, restrictionC, restrictionT) %>%
    pivot_longer(cols = c(I, Iv)) %>%
    mutate(group = str_c(iter, name),
           facet = factor(
               case_when(
                   scenario == 'ModrSlowDec10' ~ '1',
                   scenario == 'ModrFastDec10' ~ '2',
                   scenario == 'ModrSlowDec50' ~ '3',
                   scenario == 'ModrFastDec50' ~ '4'),
               labels = scenarioLabs2
           )
    )

i2bNPI <- i2NPI %>% group_by(date, facet, name) %>% summarise(value = mean(value))

i4 <-  ggplot() +
    geom_line(data = i2NPI, aes(x = date, y = value, color = name, linetype = name, group = group), size = 0.1) +
    geom_line(data = i2bNPI, aes(x = date, y = value, color = name, linetype = name), size = 0.5) +
    geom_ribbon(data = d2NPI, aes(x = date, ymin = 0, ymax = restrictionC*10000, group = iter), fill = 'red', alpha = 0.04) +
    scale_x_date('Date', date_labels = '%b', date_breaks = '2 months', limits = as.Date(c('2021-12-01', '2023-01-01'))) +
    scale_y_continuous('Infected individuals', labels = scales::comma, limits = c(0, 10000)) +
    scale_color_manual(NULL, labels = c('Unvaccinated', 'Vaccinated'), values = brewer.pal(8, 'Dark2')[3:4]) +
    scale_linetype_manual(NULL, labels = c('Unvaccinated', 'Vaccinated'), values = c('solid', 'dashed')) +
    facet_wrap(~ facet, nrow = 1) +
    theme(text=element_text(size=8), legend.position = 'bottom', strip.text.x = element_text(angle = 0, hjust = 0)) +
    labs(title = 'R0 = 7.0 | Unobtrusive measures + contacts reduced by 70% if cases exceed 10,000')


# Combine the plots
ggarrange(i1, i2, i3, i4,
          ncol = 1,
          align = 'v',
          labels = 'AUTO',
          common.legend = TRUE,
          legend = 'bottom') +
    geom_text(data = data.frame(x = 0.375, y = 0.18, label = "Restrictions\napplied"),
              mapping = aes(x = x, y = y, label = label),
              colour = "red", size = 1.4, hjust = 'right', inherit.aes = FALSE)

ggsave(filename = here::here(paste0('Outputs/', today, '-infection-projections.png')), width = 6.5, height = 7.8, units = 'in')

# hi-res version
ggsave(filename = here::here(paste0('Outputs/', today, '-infection-projections-hi-res.png')), type = 'cairo', bg = 'white', width = 6.5, height = 7.8, units = 'in', dpi = 600)


# --- ### JAGO ### --- #
