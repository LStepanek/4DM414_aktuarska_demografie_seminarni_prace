###############################################################################
###############################################################################
###############################################################################

## počítám některé další modely predikce mortality ----------------------------

###############################################################################

## Kannistö logistický model --------------------------------------------------

getKannistoSumOfSquares <- function(x){
    
    # '''
    # vrací součet vážených čtverců reziduí pro odhady intenzity úmrtnosti
    # dle modelu Kannistö
    # '''
    
    my_indices <- c(
        which(names(specific_mortality_rate[["2014"]]) == "60"):
        which(names(specific_mortality_rate[["2014"]]) == "99")
    )
    
    sum(
        (number_of_alive[["2014"]][my_indices] +
         number_of_alive[["2015"]][my_indices]) / (
            2 * specific_mortality_rate[["2014"]][my_indices] *
            (1 - specific_mortality_rate[["2014"]][my_indices])
        ) * (specific_mortality_rate[["2014"]][my_indices] -
             (
                  x[1] +
                  (x[2] * exp(x[3] * age[my_indices])) / (
                      1 - x[2] * exp(x[3] * age[my_indices])
                      )
             )) ^ 2
    )
    
}

x0 <- c(0.5, 0.01, -0.1)

fminsearch(
    getKannistoSumOfSquares,
    x0 = x0,
    minimize = TRUE,
    dfree = FALSE
)

a_kannisto <- fminsearch(
    getKannistoSumOfSquares,
    x0 = x0,
    minimize = TRUE,
    dfree = FALSE
)$xval[1]

b_kannisto <- fminsearch(
    getKannistoSumOfSquares,
    x0 = x0,
    minimize = TRUE,
    dfree = FALSE
)$xval[2]

c_kannisto <- fminsearch(
    getKannistoSumOfSquares,
    x0 = x0,
    minimize = TRUE,
    dfree = FALSE
)$xval[3]


getMyKannisto <- function(
    
    x,
    a = a_kannisto,
    b = b_kannisto,
    c = c_kannisto
    
    ){
    
    # '''
    # vrací odhad intenzity úmrtnosti podle Kannista
    # '''
    
    a + (b * exp(c * x)) / (1 - b * exp(c * x))
    
}

plot(getMyKannisto(0:110))


## ----------------------------------------------------------------------------

###############################################################################

## Thatcherův logistický model ------------------------------------------------

getThatcherSumOfSquares <- function(x){
    
    # '''
    # vrací součet vážených čtverců reziduí pro odhady intenzity úmrtnosti
    # dle modelu Thatcher
    # '''
        
    my_indices <- c(
        which(names(specific_mortality_rate[["2014"]]) == "60"):
        which(names(specific_mortality_rate[["2014"]]) == "99")
    )
    
    sum(
        (number_of_alive[["2014"]][my_indices] +
         number_of_alive[["2015"]][my_indices]) / (
            2 * specific_mortality_rate[["2014"]][my_indices] *
            (1 - specific_mortality_rate[["2014"]][my_indices])
        ) * (specific_mortality_rate[["2014"]][my_indices] -
             (
                  x[1] +
                  (x[2] * exp(x[3] * age[my_indices])) / (
                      1 + x[2] * exp(x[3] * age[my_indices])
                      )
             )) ^ 2
    )
    
}

x0 <- c(-0.01, -0.05, -0.01)

fminsearch(
    getThatcherSumOfSquares,
    x0 =  x0,
    minimize = TRUE,
    dfree = FALSE
)

alpha_thatcher <- fminsearch(
    getThatcherSumOfSquares,
    x0 = x0,
    minimize = TRUE,
    dfree = FALSE
)$xval[2]

beta_thatcher <- fminsearch(
    getThatcherSumOfSquares,
    x0 = x0,
    minimize = TRUE,
    dfree = FALSE
)$xval[3]

gamma_thatcher <- fminsearch(
    getThatcherSumOfSquares,
    x0 = x0,
    minimize = TRUE,
    dfree = FALSE
)$xval[1]


getMyThatcher <- function(
    
    x,
    alpha = alpha_thatcher,
    beta = beta_thatcher,
    gamma = gamma_thatcher
    
    ){
    
    # '''
    # vrací odhad intenzity úmrtnosti podle Thatchera
    # '''
    
    gamma + (alpha * exp(beta * x)) / (1 + alpha * exp(beta * x))
    
}

plot(getMyThatcher(0:110))


## ----------------------------------------------------------------------------

###############################################################################

## odhaduji Heligman-Pollardův model ------------------------------------------

## nejdříve konstruuji svoje bodové odhady pravděpodobností úmrtí -------------

q_x_estimates <- (deaths_panel[, "2014"]/population_panel[, "2014"])[1:100]


## vytvářím funkci pro součet čtverců -----------------------------------------

getHeligmanSumOfSquares <- function(x){
    
    # '''
    # vrací součet vážených čtverců reziduí pro odhady pravděpodobnosti úmrtí
    # dle modelu Heligmana-Pollarda
    # '''
        
    sum(
        (q_x_estimates -
            (x[1] * exp(x[2] * c(0:99))) / (
                1 + x[1] * exp(x[2] * c(0:99))
                )
        ) ^ 2
    )
    
}

x0 <- c(0, 0)

fminsearch(
    getHeligmanSumOfSquares,
    x0 =  x0,
    minimize = TRUE,
    dfree = FALSE
)

a_heligman <- fminsearch(
    getHeligmanSumOfSquares,
    x0 = x0,
    minimize = TRUE,
    dfree = FALSE
)$xval[1]

b_heligman <- fminsearch(
    getHeligmanSumOfSquares,
    x0 = x0,
    minimize = TRUE,
    dfree = FALSE
)$xval[2]


getMyHeligman <- function(
    
    x,
    a = a_heligman,
    b = b_heligman
    
    ){
    
    # '''
    # vrací odhad intenzity úmrtnosti podle Heligmana-Pollarda
    # '''
    
    (a * exp(b * x)) / (1 +  a * exp(b * x))
    
}


plot(q_x_estimates)
plot(getMyHeligman(0:110))


## ----------------------------------------------------------------------------

###############################################################################

## vytvářím model Coale-Kisker ------------------------------------------------

## vytvářím funkci pro součet čtverců -----------------------------------------

getCoaleSumOfSquares <- function(x){
    
    # '''
    # vrací součet vážených čtverců reziduí pro odhady specifické míry
    # úmrtnosti dle modelu Coale-Kisker
    # '''
        
    sum(
        (specific_mortality_rate[["2014"]][81:100] -
            exp(x[1] * c(80:99)^2 + x[2] * c(80:99) + x[3])
        ) ^ 2
    )
    
}

x0 <- c(0.00001, -0.0005, -10000)

fminsearch(
    getCoaleSumOfSquares,
    x0 =  x0,
    minimize = TRUE,
    dfree = FALSE
)

a_coale <- fminsearch(
    getCoaleSumOfSquares,
    x0 = x0,
    minimize = TRUE,
    dfree = FALSE
)$xval[1]

b_coale <- fminsearch(
    getCoaleSumOfSquares,
    x0 = x0,
    minimize = TRUE,
    dfree = FALSE
)$xval[2]

c_coale <- fminsearch(
    getCoaleSumOfSquares,
    x0 = x0,
    minimize = TRUE,
    dfree = FALSE
)$xval[3]


a_coale <- 0.0015
b_coale <- -0.03
c_coale <- -10


getMyCoale <- function(
    
    x,
    a = a_coale,
    b = b_coale,
    c = c_coale
    
    ){
    
    # '''
    # vrací odhad specifické míry úmrtnosti podle Coale-Kiskera
    # '''
    
    exp(a * x ^ 2 + b * x + c)
    
}

plot(getMyCoale(80:99))


## ----------------------------------------------------------------------------

###############################################################################

## naivní Gompertzův model ----------------------------------------------------

getNaiveGompertzSumOfSquares <- function(x){
    
    # '''
    # vrací součet vážených čtverců reziduí pro odhady intenzity úmrtnosti
    # pro věky 60+ let
    # '''
    
    my_indices <- c(
        which(names(specific_mortality_rate[["2014"]]) == "60"):
        which(names(specific_mortality_rate[["2014"]]) == "99")
    )
    
    sum(
        (number_of_alive[["2014"]][my_indices] +
         number_of_alive[["2015"]][my_indices]) / (
            2 * specific_mortality_rate[["2014"]][my_indices] *
            (1 - specific_mortality_rate[["2014"]][my_indices])
        ) * (specific_mortality_rate[["2014"]][my_indices] -
             (x[1] * (x[2] ^ age[my_indices]))) ^ 2
    )
    
}


## definuji funkci vracející pouze čtverec, ne součet -------------------------

x0 <- c(0.001, 1.0)

fminsearch(
    getNaiveGompertzSumOfSquares,
    x0 =  x0,
    minimize = TRUE,
    dfree = FALSE
)

a_naive <- fminsearch(
    getNaiveGompertzSumOfSquares,
    x0 = x0,
    minimize = TRUE,
    dfree = FALSE
)$xval[1]

b_naive <- fminsearch(
    getNaiveGompertzSumOfSquares,
    x0 = x0,
    minimize = TRUE,
    dfree = FALSE
)$xval[2]


getNaiveGompertz <- function(
    
    x,
    a = a_naive,
    b = b_naive
    
    ){
    
    # '''
    # vrací odhad míry intenzity úmrtnosti naivně podle Gompertze
    # '''
    
    a * b ^ x
    
}


## ----------------------------------------------------------------------------

###############################################################################

## počítám pravděpodobnostní úmrtí pro všechny vyrovnávací funkce -------------

my_alpha <- 0.85    ## zlomek nultého roku života

q_0 <- mortality_rate[["2014"]][1] / (
    births_panel[, "2013"] * (1 - my_alpha) +
    births_panel[, "2014"] * my_alpha
    )


## ----------------------------------------------------------------------------

my_p_x <- NULL
my_q_x <- NULL
my_life_lengths <- NULL

for(my_function in c(

    "getMyGompertz",
    "getNaiveGompertz",
    "getMyKannisto",
    "getMyThatcher",
    "getMyHeligman",
    "getMyCoale"

)){
    
    # '''
    # vrací tabulku s pravděpodobnostmi úmrtí pro jednotlivé modely
    # '''
    
    my_p_x <- cbind(
        my_p_x,
        fromBalancingToPx(

            age,
            "2014",
            specific_mortality_rate,
            q_0,
            get(my_function)

        )
    )
    
    my_q_x <- cbind(
        my_q_x,
        fromBalancingToQx(

            age,
            "2014",
            specific_mortality_rate,
            q_0,
            get(my_function)

        )
    )
    
    my_life_lengths <- cbind(
        my_life_lengths,
        fromBalancingToMeanLifeLength(
            my_p_x[, dim(my_p_x)[2]],            
            my_q_x[, dim(my_q_x)[2]],
            0.85
        )
    )

}


## ----------------------------------------------------------------------------

###############################################################################
###############################################################################
###############################################################################







