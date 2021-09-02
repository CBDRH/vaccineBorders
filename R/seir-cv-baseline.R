#########################################
### Defining a baseline SEIR-CV model ###
#########################################

######################################################
### 1. Define the population size and distribution ###
######################################################

# The population distribution by five-year age group
age_dist <- covoid::import_age_distribution('Australia')

# The approx Australian population
POPAUS <- 25.7E6

# The size of each five-year age group
age_distN <- age_dist*POPAUS

# The size of the aggregated age groups (60+, 40-59, 15-39, 0-14)
popBase <- round(c(sum(age_distN[13:16]),
                  sum(age_distN[9:12]),
                  sum(age_distN[4:8]),
                  sum(age_distN[1:3]))/1E6, digits = 1)*1E6

# The number of rows in the output matrix (4 age groups x 4 hesitancy groups = 16)
nJ <- length(popBase)*4


# Other global variables
DAY0 <- '2021-08-31' # Eve of simulation
NDAYS <- as.numeric(as.Date('2023-01-01') - as.Date(DAY0)) # Number of days to run for (e.g. until 31 Dec 2022)
openDate <- '2021-12-01' # Border opening
num_draws <- 5 # The number of draws from the model


# The population fully vaccinated at Time 0
# Ref: https://www.health.gov.au/sites/default/files/documents/2021/08/covid-19-vaccination-doses-by-age-and-sex_29.pdf
startVac = c(2E6, 3E6, 2E6, 0)


################################################
### 2. Define the vaccine rollout parameters ###
################################################

# The probability of being available in each hesitancy group
propensityBase <- c(.1, .05, .005, .0005)

# The day that each group becomes eligible to receive the vaccine
# 60+ and 40-59y are already eligible at the start of the simulation; 15-39y on Sep 1; 0-14y open on Oct 1
# *|* this is a bit arbitrary and might not make sense to have 0-14y being vacinated anytime soon?
openDayBase <- c(1, 1, 1, 154) # Aug 1, Aug 1, Sep 1, effectively no vaccines for under 15s until February To check: as.Date(DAY0) + openDayBase

# Proportion in each hesitancy group (Defintiely will, probably will, probably won't, definitely won't)
# Ref: https://csrm.cass.anu.edu.au/sites/default/files/docs/2021/5/Vaccine_willingness_and_concerns_in_Australia_-_August_2020_to_April_2021.pdf
hesitancyPctFast <- c(0.60, 0.24, 0.12, 0.04)
hesitancyPctSlow <- c(0.36, 0.28, 0.24, 0.12)
hesitancyPctBase <- c(0.60, 0.20, 0.15, 0.05) # used as a baseline place holder

####################################
### 3. Define the baseline state ###
####################################


# Numbers in susceptible unvaccinated group
S1Base <- rep(popBase-startVac, each = 4)*hesitancyPctBase

# Numbers in susceptible vaccinated group (assumes startVac% vaccinated)
SvBase <- rep(startVac, each = 4)*hesitancyPctBase

# Numbers in exposed unvaccinated and vaccinated group
E1Base <- EvBase <- rep(0, nJ)

# Numbers in infectious unvaccinated and vaccinated group
I1Base <- IvBase <- rep(0, nJ)

# Numbers in hospitalised vaccinated and unvaccinated group
H1Base <- HvBase <- rep(0, nJ)

# Numbers in recovered vaccinated and unvaccinated group
R1Base <- RvBase <- rep(0, nJ)

## dist (Distribution of age groups)
distBase <- S1Base/sum(S1Base)

# Define as a covoid state
stateBase <- covoid::seir_cv_state0(S = S1Base, E = E1Base, I = I1Base, H = H1Base, R = R1Base,
                                    Sv = SvBase, Ev = EvBase, Iv = IvBase, Hv = HvBase, Rv = RvBase)


###############################################
### 4. Define the baseline model parameters ###
###############################################

## R0 (R naught)
R0Base <- 2.5

## sigma (inverse of average latency period)
# *|* Needs a more recent reference. Different for delta?
sigmaBase <- 1/6

## gamma (inverse of average infectious period)
# *|* Needs a more recent reference. Different for delta?
gammaBase <- 1/10 # https://www.medrxiv.org/content/10.1101/2020.07.28.20163873v1.full.pdf

## Contact matrix
cm_oz <- covoid::import_contact_matrix("Australia","general")

# Aggregate the 5-year contact matrix to the four age bands
popSize <- POPAUS*age_dist
r1c1 <- sum((popSize*cm_oz)[13:16, 13:16])/sum(popSize[13:16])
r1c2 <- sum((popSize*cm_oz)[9:12, 13:16])/sum(popSize[13:16])
r1c3 <- sum((popSize*cm_oz)[4:8, 13:16])/sum(popSize[13:16])
r1c4 <- sum((popSize*cm_oz)[1:3, 13:16])/sum(popSize[13:16])

r2c1 <- sum((popSize*cm_oz)[13:16, 9:12])/sum(popSize[9:12])
r2c2 <- sum((popSize*cm_oz)[9:12, 9:12])/sum(popSize[9:12])
r2c3 <- sum((popSize*cm_oz)[4:8, 9:12])/sum(popSize[9:12])
r2c4 <- sum((popSize*cm_oz)[1:3, 9:12])/sum(popSize[9:12])

r3c1 <- sum((popSize*cm_oz)[13:16, 4:8])/sum(popSize[4:8])
r3c2 <- sum((popSize*cm_oz)[9:12, 4:8])/sum(popSize[4:8])
r3c3 <- sum((popSize*cm_oz)[4:8, 4:8])/sum(popSize[4:8])
r3c4 <- sum((popSize*cm_oz)[1:3, 4:8])/sum(popSize[4:8])

r4c1 <- sum((popSize*cm_oz)[13:16, 1:3])/sum(popSize[1:3])
r4c2 <- sum((popSize*cm_oz)[9:12, 1:3])/sum(popSize[1:3])
r4c3 <- sum((popSize*cm_oz)[4:8, 1:3])/sum(popSize[1:3])
r4c4 <- sum((popSize*cm_oz)[1:3, 1:3])/sum(popSize[1:3])

# Compile into a 4x4 contact matrix
cm <- matrix(c(r1c1, r1c2, r1c3, r1c4,
                r2c1, r2c2, r2c3, r2c4,
                r3c1, r3c2, r3c3, r3c4,
                r4c1, r4c2, r4c3, r4c4),
              nrow = 4, ncol=4,
              dimnames =list(c('60+', '40-59', '15-39', '0-14'), c('60+', '40-59', '15-39', '0-14')))

# Expand to a 16*16 contact matrix (4 age bands x 4 hesitancy levels)
cmBase <- matrix(ncol=nJ, nrow=nJ)
for (i in 1:nJ) {
    cmBase[i,] <- rep(cm[ceiling(i/4),], each=4)
}


## vaceff1 (Probability of infection in the vaccinated)
# Based on any vaccine for Delta
# See Table 1 in https://media.tghn.org/articles/Effectiveness_of_COVID-19_vaccines_against_hospital_admission_with_the_Delta_B._G6gnnqJ.pdf
vaceff1Base <- rep(0.80, nJ)

## vaceff2 (turned off)
vaceff2Base <- rep(1, nJ)

## vaceff3 (turned off)
vaceff3Base <- rep(1, nJ)


# Risk of hospitalisation for vaccinated and unvaccinated groups (60+, 40-59, 15-39, 0-14)

# Probability of hospitalisation in the unvaccinated by age group
# Base Table 1.1 in Grattan Institute Race to 80 technical document
# https://grattan.edu.au/wp-content/uploads/2021/07/Race-to-80-Grattan-technical-supplement.pdf
phospBase <- rep(c(0.36, 0.10, 0.04, 0.02) , each = nJ/length(hesitancyPctBase))

# Probability of hospitalisation by phase for vaccinated individuals
# See table 1 in https://media.tghn.org/articles/Effectiveness_of_COVID-19_vaccines_against_hospital_admission_with_the_Delta_B._G6gnnqJ.pdf
phospvBase <- 0.29*phospBase

thospBase <- 1/7 # average days in hospital


## contact_intervention
## transmission_intervention
# A reactive intervention that imposes a 70% reduction in activity when active infectious cases exceeds 100 and persists until <5
# intBase <- NULL
intBase <- covoid::reactive_intervention(threshold = 10000,
                                  reduce = 0.3,
                                  state = reactive_state(length = 14, lowerbound = 2000))

## Put it all together
paramBase <- seir_cv_param(R0 = R0Base,
                        sigma = sigmaBase,
                        gamma = gammaBase,
                        phosp = phospBase,
                        phospv = phospvBase,
                        thosp = thospBase,
                        cm = cmBase,
                        dist = distBase,
                        vaceff1 = vaceff1Base,
                        vaceff2 = vaceff2Base,
                        vaceff3 = vaceff3Base,
                        nvac = nvacBase,
                        vac_alloc = random_vac_allocBase,
                        n_imp = n_imp_casesBase,
                        transmission_intervention = intBase,
                        contact_intervention = intBase)

# ### JAGO ### #
