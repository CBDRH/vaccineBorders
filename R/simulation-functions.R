# Define a simulation
create_simulation <- function(id,hesitancy,R0,nvac,date,arrivals) {
    list(
        id = id,
        hesitancy = hesitancy,
        R0 = R0,
        nvac=nvac,
        date = date,
        arrivals=arrivals
    )
}

# set up the number of imported cases
setup_nimp <- function(date,arrivals) {
    n_imp_cases_date <- function(t){
        datediff <- as.numeric(as.Date(date) - as.Date(DAY0))
        failure(t,
                arrival_params = list(
                    arrivals = c(rep(0, datediff), rep(arrivals, NDAYS-datediff)),
                    p = (3.1/1E5)/14, # Failure risk based on https://www.medrxiv.org/content/10.1101/2021.02.17.21251946v1.full.pdf
                    days = NDAYS
                ))
    }
    n_imp_cases_date
}

# set up the simulation parameters
setup_param <- function(R0,nvac,date,arrivals,param=paramBase) {
    param$R0 <- R0
    param$n_imp <- setup_nimp(date,arrivals)
    param$nvac <- nvac
    param
}

# set up the simulation initial state
setup_state <- function(popBase, hesitancy, p=startVac) {
    stopifnot(length(popBase) == length(hesitancy))

    S1 <- rep(popBase-startVac, each = 4)*hesitancy
    Sv <- rep(startVac, each = 4)*hesitancy
    covoid::seir_cv_state0(S = S1, E = E1Base, I = I1Base, H = H1Base, R = R1Base,
                           Sv = Sv, Ev = EvBase, Iv = IvBase, Hv = HvBase, Rv = RvBase)
}

# # Function to extract incidence data from seir output
# compileIncidence <- function(res, nm){
#     df <- tibble(date = res$epi$t + as.Date(DAY0),
#                  inc = res$epi$incidenceE + res$epi$incidenceEv,
#                  hosp = res$epi$H + res$epi$Hv,
#                  restriction = (res$epi$contact_intervention + res$epi$transmission_intervention)/2,
#                  vac = res$epi$Sv + res$epi$Ev + res$epi$Iv + res$epi$Hv + res$epi$Rv,
#                  lab=nm,
#                  rollout = nm)
#     return(df)
# }
#
# Second version that splits by vaccination status
compileIncidence2 <- function(res, nm){
    df <- tibble(date = res$epi$t + as.Date(DAY0) -1,
                 S = res$epi$S,
                 Sv = res$epi$Sv,
                 E = res$epi$E,
                 Ev = res$epi$Ev,
                 I = res$epi$I,
                 Iv = res$epi$Iv,
                 H = res$epi$H,
                 Hv = res$epi$Hv,
                 R = res$epi$R,
                 Rv = res$epi$Rv,
                 inc = res$epi$incidenceE,
                 incV = res$epi$incidenceEv,
                 restrictionC = res$epi$contact_intervention,
                 restrictionT = + res$epi$transmission_intervention,
                 vac = res$epi$Sv + res$epi$Ev + res$epi$Iv + res$epi$Hv + res$epi$Rv,
                 vacKids =  lag(res$epi$Sv13+res$epi$Sv14+res$epi$Sv15+res$epi$Sv16+
                            res$epi$Ev13+res$epi$Ev14+res$epi$Ev15+res$epi$Ev16+
                            res$epi$Iv13+res$epi$Iv14+res$epi$Iv15+res$epi$Iv16+
                            res$epi$Hv13+res$epi$Hv14+res$epi$Hv15+res$epi$Hv16+
                            res$epi$Rv13+res$epi$Rv14+res$epi$Rv15+res$epi$Rv16),
                 vacAdults = vac - vacKids,
                 lab=nm,
                 rollout = nm)
    return(df)
}

# Function to determine number of vaccine doses available at time t)
nvacBase <- function(t) {(t < 150)*(200000*(t/150)) + (t >= 150)*(200000)}
nvacFast <- function(t) {200000}
nvacSlow <- function(t) {80000}

# To test: tibble(day = seq(1:200), doses = nvacBase(day)) %>% ggplot(aes(x = day, y = doses)) + geom_line()



# Function to determine how available vaccinations are distributed across eligible groups at time t
# see ?covoid::vaccination_allocation_pp
random_vac_allocBase <- function(t, n, s) {
    covoid::vaccination_allocation_pp(t, n, s,
                                      vac_params = list(
                                          openDay = openDayBase,
                                          propensity = propensityBase)
    )
}


# Function to define hotel quarantine failures based on number of arrivals
failure <- function(t, arrival_params){

    with (arrival_params, {
        incoming <- tibble(
            date = seq(1, days, by = 1),
            land = arrivals,
            inQH = runner::sum_run(x = land, k = 14, idx = date),
            failure = rbinom(days, round(inQH), p)
        )

        return(incoming$failure[t])
    })
}

# Function to define new imported cases based on hotel quarantine failures
# Arrivals pre-covid varied from a low of 600-700k per month in May/June to a high of 1 million per month in December/February, with an average of 787k per month
# Arrivals post-covid have increased fairly linearly from 5,400 in April 2020 15,200 in April 2021 (~775 per month)

n_imp_casesBase <- function(t){

    failure(t,
            arrival_params = list(
                arrivals = c(rep(500, 161), rep(2500, 400-161)),
                p = (3.1/1E5)/14, # Failure risk based on https://www.medrxiv.org/content/10.1101/2021.02.17.21251946v1.full.pdf
                days = 400
            )
    )

}

# ------------------------------------------------------------------------------
