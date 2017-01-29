###############################################################################
###############################################################################
###############################################################################

## definuji pomocné funkce ----------------------------------------------------

getWeightedMovingAverage <- function(x, weights){
    
    # '''
    # vrací vážený klouzavý průměr z vektoru čísel "x"
    # a vektoru vah "weigts"
    # '''
    
    if(length(x) != length(weights)){
        stop(
            "Délka vektoru čísel 'x' není shodná s délkou vektoru vah 'w'!"
            )
    }
    
    return(sum(x * weights))
    
}


## ----------------------------------------------------------------------------

getMyMovingAverage <- function(
    
    x,
    type = c(3, 9, 19),
    year = "2014"
    
    ){
    
    # '''
    # vrací vážený klouzavý průměr ze 3, 9 nebo 19 hodnot
    # pro věk "x"
    # '''
    
    if(type == 3){
        my_weights <- rep(1/3, 3)
    }else{
        if(type == 9){
            my_weights <- c(
                         0.04, 0.08, 0.12, 0.16, 0.2,
                         0.16, 0.12, 0.08, 0.04
                         )
        }else{
            if(x <= 29){
                my_weights <- c(
                                 -0.037, 0, 0.0741, 0.2963, 1/3, 0.2963,
                                 0.0741, 0, -0.037
                                 )
            }else{
                my_weights <- c(
                                 -0.0032, -0.0096, -0.0144, -0.0128, 0,
                                 0.0336, 0.0848, 0.1392, 0.1824, 0.2,
                                 0.1824, 0.1392, 0.0848, 0.0336, 0, 
                                 -0.0128, -0.0144, -0.0096, -0.0032
                                 )
            }    
        }
    }
    
    sum(
        specific_mortality_rate[[year]][c(
            (which(
                names(specific_mortality_rate[[year]]) == as.character(x)) -
                ((type - 1) / 2)
            ):
            (which(
                names(specific_mortality_rate[[year]]) == as.character(x)) +
                ((type - 1) / 2))
            )] * my_weights
    )
    
}


## ----------------------------------------------------------------------------

getMyBalancing <- function(
    
    age_range,
    year,
    specific_mortality_rate,
    my_balancing_function,
    ...
    
){
    
    # '''
    # vrací vyrovnání specifické míry úmrtnosti klouzavými průmery
    # a funkcí predikující mortalitu ve vyšších věcích
    # '''
    
    balancing <- rep(NA, length(age))
    names(balancing) <- age
    
    for(i in 1:2){

        balancing[
            as.character(i)
        ] <- specific_mortality_rate[[year]][as.character(i)]
    
    }
    
    for(i in 3:5){

        balancing[
            as.character(i)
        ] <- getMyMovingAverage(i, 3, year)
         
    }
    
    for(i in 6){

    balancing[
        as.character(i)
    ] <- 0.75 * getMyMovingAverage(i, 3, year) +
         0.25 * getMyMovingAverage(i, 9, year)
    
    }

    for(i in 7){

        balancing[
            as.character(i)
        ] <- 0.50 * getMyMovingAverage(i, 3, year) +
             0.50 * getMyMovingAverage(i, 9, year)
        
    }

    for(i in 8){

        balancing[
            as.character(i)
        ] <- 0.25 * getMyMovingAverage(i, 3, year) +
             0.75 * getMyMovingAverage(i, 9, year)
        
    }

    for(i in 9:29){

        balancing[
            as.character(i)
        ] <- getMyMovingAverage(i, 9, year)
        
    }

    for(i in 30){

        balancing[
            as.character(i)
        ] <- 0.75 * getMyMovingAverage(i, 9, year) +
             0.25 * getMyMovingAverage(i, 19, year)
        
    }

    for(i in 31){

        balancing[
            as.character(i)
        ] <- 0.50 * getMyMovingAverage(i, 9, year) +
             0.50 * getMyMovingAverage(i, 19, year)
        
    }

    for(i in 32){

        balancing[
            as.character(i)
        ] <- 0.25 * getMyMovingAverage(i, 9, year) +
             0.75 * getMyMovingAverage(i, 19, year)
        
    }

    for(i in 33:59){

        balancing[
            as.character(i)
        ] <- getMyMovingAverage(i, 19, year)
        
    }

    for(i in 60){

        balancing[
            as.character(i)
        ] <- 0.75 * getMyMovingAverage(i, 19, year) +
             0.25 * my_balancing_function(x = i, ...)
        
    }

    for(i in 61){

        balancing[
            as.character(i)
        ] <- 0.50 * getMyMovingAverage(i, 19, year) +
             0.50 * my_balancing_function(x = i, ...)
        
    }

    for(i in 62){

        balancing[
            as.character(i)
        ] <- 0.25 * getMyMovingAverage(i, 19, year) +
             0.75 * my_balancing_function(x = i, ...)
        
    }

    for(i in 63:110){

        balancing[
            as.character(i)
        ] <- my_balancing_function(x = i, ...)
        
    }
    
    balancing
    
}


## ----------------------------------------------------------------------------

fromBalancingToQx <- function(

    age_range,
    year,
    specific_mortality_rate,
    q_0,
    my_balancing_function,
    ...

){
    
    # '''
    # vrací pravděpodobnost úmrtí pro zadané vyrovnání
    # '''
    
    my_balancing <- getMyBalancing(    
        age_range,
        year,
        specific_mortality_rate,
        my_balancing_function,
        ...
    )
    
    p_0 <- 1 - q_0
    
    survival_probability <- c("0" = p_0)

    for(i in 1:max(age)){
        survival_probability <- c(
            survival_probability,
            exp(-my_balancing[as.character(i)])
            )
    }
    
    death_probability <- 1 - survival_probability
    
    death_probability
    
}


## ----------------------------------------------------------------------------

fromBalancingToPx <- function(

    age_range,
    year,
    specific_mortality_rate,
    q_0,
    my_balancing_function,
    ...

){
    
    # '''
    # vrací pravděpodobnost přežití pro zadané vyrovnání
    # '''
    
    my_balancing <- getMyBalancing(    
        age_range,
        year,
        specific_mortality_rate,
        my_balancing_function,
        ...
    )
    
    p_0 <- 1 - q_0
    
    survival_probability <- c("0" = p_0)

    for(i in 1:max(age)){
        survival_probability <- c(
            survival_probability,
            exp(-my_balancing[as.character(i)])
            )
    }
    
    survival_probability
    
}


## ----------------------------------------------------------------------------

fromBalancingToMeanLifeLength <- function(
    
    survival_probability,
    death_probability,
    my_alpha

){
    
    # '''
    # vrací střední délku života pro zadané vyrovnání
    # '''
        
    expected_number_of_alive <- c("0" = 100000,
                                  100000 * survival_probability[2:111])

    expected_number_of_deaths <- expected_number_of_alive *
                                 death_probability

    L_x <- 100000 - my_alpha * expected_number_of_deaths["0"]

    for(i in 2:110){
        L_x[i] <- 
            (expected_number_of_alive[i] +
             expected_number_of_alive[i + 1]) / 2
    }

    names(L_x) <- 0:109

    T_x <- NULL

    for(i in 1:110){
        T_x[i] <- sum(L_x[i:110])
    }

    names(T_x) <- 0:109

    mean_life_length <- T_x / expected_number_of_alive[1:110]
    
    mean_life_length
    
}


## ----------------------------------------------------------------------------

###############################################################################

## pomocná funkce pro výpočet střední délky života novorozence ----------------

getMyNewbornMeanLifeLength <- function(
    
    number_of_alives_of_the_given_year,
    my_alpha
    
){
    
    # '''
    # vrací střední délku života novorozence v daném roce pro daný
    # vektor počtů dožívajících od věku 0 do nejvyššího uvažovaného, tedy
    # "number_of_alives_of_the_given_year";
    # předpokladem je, že vektor "number_of_alives_of_the_given_year"
    # je řazen vzestupně podle věků, tedy od věku 0 do nejvyššígo
    # uvažovaného;
    # koeficient "my_alpha" je podíl dolního elementárního souboru
    # a počtu zemřelých daného roku vždy pro věk 0
    # '''
    
    (sum(number_of_alives_of_the_given_year) -
     my_alpha * number_of_alives_of_the_given_year[1] +
     (my_alpha - 0.5) * number_of_alives_of_the_given_year[2]) / (
         number_of_alives_of_the_given_year[1]
     )
    
}


## ----------------------------------------------------------------------------

###############################################################################

## definuji pomocnou funkci pro výpočet generační pravděpodobnosti úmrtí ------

getMyGenerationDeathProbability <- function(
    
    x,
    year,
    death_probability_table
    
){
    
    # '''
    # vrací generační pravděpodobnost úmrtí, tj. pravděpodobnost úmrtí
    # generace narozené v roce "year" v jejím věku "x" let
    # '''
    
    sqrt(
        death_probability_table[
            as.character(x), as.character(as.integer(year) +
                                          as.integer(x))
        ] *
        death_probability_table[
            as.character(x), as.character(as.integer(year) +
                                          as.integer(x) + 1)
        ]
    )
    
}


## ----------------------------------------------------------------------------

###############################################################################

## helper funkce pro střední délku života v přesném věku ----------------------

getMyMeanLifeLengthForGivenAge <- function(
    
    x,
    year,
    alive_numbers_table
    
){
    
    # '''
    # vrací střední délku života pro daný věk "x" a rok "year"
    # na základě tabulky přežívajících "alive_numbers_table"
    # '''
    
    sum(
        alive_numbers_table[
            which(rownames(alive_numbers_table) == as.character(x)):
            dim(alive_numbers_table)[1], as.character(year)
        ]
    ) / alive_numbers_table[as.character(x), as.character(year)] - 0.5
    
}


## ----------------------------------------------------------------------------

###############################################################################
###############################################################################
###############################################################################







