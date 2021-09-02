# Aim: Estimate SEIR+ modelling varying:
# - Vaccine rollout speed (Fast/Slow)
# - Daily arrivals (2500/13000)
# - R0 (3.5/7.0)
# No interventions are implemented(!)

# Define simulation scenarios
simulations <- list(
    "LowFastDec10" = create_simulation("LowFastDec10",
                                       hesitancy=hesitancyPctFast,
                                       R0=3.5,
                                       nvac=nvacFast,
                                       date=openDate,
                                       arrivals=2500),
    "LowFastDec50" = create_simulation("LowFastDec50",
                                       hesitancy=hesitancyPctFast,
                                       R0=3.5,
                                       nvac=nvacFast,
                                       date=openDate,
                                       arrivals=13000),
    "LowSlowDec10" = create_simulation("LowSlowDec10",
                                       hesitancy=hesitancyPctSlow,
                                       R0=3.5,
                                       nvac=nvacSlow,
                                       date=openDate,
                                       arrivals=2500),
    "LowSlowDec50" = create_simulation("LowSlowDec50",
                                       hesitancy=hesitancyPctSlow,
                                       R0=3.5,
                                       nvac=nvacSlow,
                                       date=openDate,
                                       arrivals=13000),
    "ModrFastDec10" = create_simulation("ModrFastDec10",
                                       hesitancy=hesitancyPctFast,
                                       R0=7.0,
                                       nvac=nvacFast,
                                       date=openDate,
                                       arrivals=2500),
    "ModrFastDec50" = create_simulation("ModrFastDec50",
                                       hesitancy=hesitancyPctFast,
                                       R0=7.0,
                                       nvac=nvacFast,
                                       date=openDate,
                                       arrivals=13000),
    "ModrSlowDec10" = create_simulation("ModrSlowDec10",
                                       hesitancy=hesitancyPctSlow,
                                       R0=7.0,
                                       nvac=nvacSlow,
                                       date=openDate,
                                       arrivals=2500),
    "ModrSlowDec50" = create_simulation("ModrSlowDec50",
                                       hesitancy=hesitancyPctSlow,
                                       R0=7.0,
                                       nvac=nvacSlow,
                                       date=openDate,
                                       arrivals=13000)
)

# Simulate from each model times
num_sims <- length(simulations)
sim_ids <- names(simulations)
simulation_results_noNPI <- vector(mode="list",length=num_draws*num_sims)
names(simulation_results_noNPI) <- paste0(rep(sim_ids, each = num_draws), '_', 1:num_draws)


# Assume a low-level transmission intervention is always in place
intT <- covoid::reactive_intervention(threshold = 0,
                                      reduce = 0.7,
                                      state = reactive_state(length = 365, lowerbound = 0))

# Turn off interventions by setting the intervention threshold to an arbitrarily high number
intCoff <- covoid::reactive_intervention(threshold = 10E6,
                                         reduce = 1.0,
                                         state = reactive_state(length = 14, lowerbound = 5))

# Loop through list of defined simulations
for(i in 1:num_sims){

    id_i <- sim_ids[i]

    state_i <- setup_state(popBase,simulations[[id_i]]$hesitancy)
    param_i <- setup_param(simulations[[id_i]]$R0,
                           simulations[[id_i]]$nvac,
                           simulations[[id_i]]$date,
                           simulations[[id_i]]$arrivals)
    param_i$transmission_intervention <- intT
    param_i$contact_intervention <- intCoff

    for(j in 1:num_draws){

        id_i_n <-paste0(sim_ids[i], '_', j)

        simulation_results_noNPI[[id_i_n]] <- simulate_seir_cv(t = NDAYS,
                                                         state_t0 = state_i,
                                                         param = param_i)
    }
}

# Compile simulation results
incidence_results_noNPI <- imap(simulation_results_noNPI, compileIncidence2)

# Compile simulation results as a dataframe
incidence_results_full_noNPI <- do.call(rbind,incidence_results_noNPI) %>%
    mutate(
        scenario = stringr::str_split(rollout, "_", simplify = TRUE)[,1],
        iter = stringr::str_split(rollout, "_", simplify = TRUE)[,2],
        r0 = case_when(
            grepl("Low", lab) ~ 'R0 = 3.5',
            grepl("Modr", lab) ~ 'R0 = 7.0'
        ),
        rollout = factor(
            case_when(
                grepl("Slow", lab) ~ 'Slow rollout',
                grepl("Fast", lab) ~ 'Fast rollout'
            ),
            levels = c('Slow rollout', 'Fast rollout')
        ),
        opening = factor(
            case_when(
                grepl("Dec", lab) ~ '1'
            ),
            labels = list('1' = 'Dec 2021')),
        arrivals = factor(
            case_when(
                grepl("10", lab) ~ 'Daily arrivals = 2,500',
                grepl("50", lab) ~ 'Daily arrivals = 13,000'
            ),
            levels = c('Daily arrivals = 2,500', 'Daily arrivals = 13,000')
        )
    )

# Save the data
save(simulation_results_noNPI, file = here::here("Outputs/simulation_results_noNPI.Rda"))
save(incidence_results_noNPI, file = here::here("Outputs/incidence_results_noNPI.Rda"))
save(incidence_results_full_noNPI, file = here::here("Outputs/incidence_results_full_noNPI.Rda"))


# --- ### JAGO ### --- #

