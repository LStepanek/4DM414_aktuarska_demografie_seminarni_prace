###############################################################################
###############################################################################
###############################################################################

## počítám některé míry úmrtnosti pro daný věk --------------------------------

###############################################################################

## definuju proměnnou "věk" ---------------------------------------------------

age <- c(0:110)


## definuji proměnnou "střed věkového intervalu" ------------------------------

age_mid_point <- age + 0.5


## ----------------------------------------------------------------------------

###############################################################################

## definuji proměnnou "počet zemřelých" pro poslední rok, ze kterého jsou
## dostupná data, zde 2014 ----------------------------------------------------

mortality_rate <- list()

for(my_year in colnames(deaths_panel)){
    
    output <- rep(0, length(age))
    
    for(i in 1:dim(deaths_panel)[1]){

        for(j in 1:length(age)){
        
            if(rownames(deaths_panel)[i] == as.character(age[j])){
            
                output[i] <- deaths_panel[i, my_year]
                
            }
            
        }
    
    }
    
    mortality_rate[[length(mortality_rate) + 1]] <- assign(my_year,
                                                           output)
        
    names(mortality_rate)[length(mortality_rate)] <- my_year
    
}


## ----------------------------------------------------------------------------

###############################################################################

## definuji proměnnou "počet žijících k 1. 1." pro předposlední a poslední
## rok, ze kterého jsou dostupná data, zde 2014 a 2015 ------------------------

number_of_alive <- list()

for(my_year in colnames(population_panel)){
    
    output <- rep(0, length(age))
    
    for(i in 1:dim(population_panel)[1]){

        for(j in 1:length(age)){
        
            if(rownames(population_panel)[i] == as.character(age[j])){
            
                output[i] <- population_panel[i, my_year]
                
            }
            
        }
    
    }
    
    number_of_alive[[length(number_of_alive) + 1]] <- assign(
            my_year,
            output
        )
        
    names(number_of_alive)[length(number_of_alive)] <- my_year
    
}


## ----------------------------------------------------------------------------

###############################################################################

## definuji specifické míry úmrtnosti -----------------------------------------

specific_mortality_rate <- list()

for(my_year in names(mortality_rate)){
  
    if(my_year %in% colnames(population_panel) &
       as.character(as.numeric(my_year) + 1) %in%
       colnames(population_panel)){
           
           output <- mortality_rate[[my_year]][
                         1:length(which(
                             grepl("[0-9]+", rownames(population_panel))
                               ))
                         ] / (
               population_panel[
                   grepl("[0-9]+", rownames(population_panel)), my_year
                   ] +
               population_panel[
                   grepl("[0-9]+", rownames(population_panel)),
                   as.character(as.numeric(my_year) + 1)
                   ]
               ) * 2
               
            specific_mortality_rate[[
                length(specific_mortality_rate) + 1
                ]] <- assign(
                          my_year,
                          output
                          )
            
            names(specific_mortality_rate)[
                length(specific_mortality_rate)
                ] <- my_year
           
       }
    
}


## ----------------------------------------------------------------------------

###############################################################################

## počítám klouzavé průměry ---------------------------------------------------

weighted_moving_averages <- list()

for(my_year in names(specific_mortality_rate)){
        
    k3 <- rep(NA, length(specific_mortality_rate[[my_year]]))
    
    for(i in 2:(length(specific_mortality_rate[[my_year]]) - 1)){
        k3[i] <- getWeightedMovingAverage(
                     x = specific_mortality_rate[[my_year]][
                             (i - 1):(i + 1)
                             ],
                     weights = c(1/3, 1/3, 1/3)
                     )
    }
    
    k9 <- rep(NA, length(specific_mortality_rate[[my_year]]))
    
    for(i in 7:(length(specific_mortality_rate[[my_year]]) - 6)){
        k9[i] <- getWeightedMovingAverage(
                     x = specific_mortality_rate[[my_year]][
                             (i - 4):(i + 4)
                             ],
                     weights = c(
                         0.04, 0.08, 0.12, 0.16, 0.2,
                         0.16, 0.12, 0.08, 0.04
                         )
                     )
    }
    
    k19 <- rep(NA, length(specific_mortality_rate[[my_year]]))
    
    for(i in 7:(length(specific_mortality_rate[[my_year]]) - 9)){
        k19[i] <- if(as.integer(
                      names(specific_mortality_rate[[my_year]])[i]) <= 29
                      ){
                          getWeightedMovingAverage(
                             x = specific_mortality_rate[[my_year]][
                                     (i - 4):(i + 4)
                                     ],
                             weights = c(
                                 -0.037, 0, 0.0741, 0.2963, 1/3, 0.2963,
                                 0.0741, 0, -0.037
                                 )
                             )
                       }else{
                       
                          getWeightedMovingAverage(
                             x = specific_mortality_rate[[my_year]][
                                     (i - 9):(i + 9)
                                     ],
                             weights = c(
                                 -0.0032, -0.0096, -0.0144, -0.0128, 0,
                                 0.0336, 0.0848, 0.1392, 0.1824, 0.2,
                                 0.1824, 0.1392, 0.0848, 0.0336, 0, 
                                 -0.0128, -0.0144, -0.0096, -0.0032
                                 )
                             )
                       
                       }
    }
    
    weighted_moving_averages[[my_year]] <- list(
        "3" = k3, "9" = k9, "19" = k19
        )
 
}


## ----------------------------------------------------------------------------

###############################################################################

## Gompertz-Makehamova funkce pro data roku 2014 ------------------------------

## nejdříve pro věkový interval mezi 60 až 83 roky, včetně --------------------

G_1 <- sum(specific_mortality_rate[["2014"]][
    which(names(specific_mortality_rate[["2014"]]) == "60"):
    which(names(specific_mortality_rate[["2014"]]) == "67")
])

G_2 <- sum(specific_mortality_rate[["2014"]][
    which(names(specific_mortality_rate[["2014"]]) == "68"):
    which(names(specific_mortality_rate[["2014"]]) == "75")
])

G_3 <- sum(specific_mortality_rate[["2014"]][
    which(names(specific_mortality_rate[["2014"]]) == "76"):
    which(names(specific_mortality_rate[["2014"]]) == "83")
])

c <- ((G_3 - G_2) / (G_2 - G_1))^(1 / 8)

K_c <- c ^ 60.5 * ((c ^ 8) - 1) / (c - 1)

b <- (G_2 - G_1) / (K_c * ((c ^ 8) - 1))

a <- (G_1 - b * K_c) / 8


## ----------------------------------------------------------------------------

###############################################################################

## zavádím funkci součtu čtverců reziduí, kterou je nutné minimalizovat
## pro parametry "a", "b" a "c" -----------------------------------------------

getGompertzSumOfSquares <- function(x){
    
    # '''
    # vrací součet vážených čtverců reziduí pro odhady intenzity úmrtnosti
    # pro věky 60 až 83 let
    # '''
    
    my_indices <- c(
        which(names(specific_mortality_rate[["2014"]]) == "60"):
        which(names(specific_mortality_rate[["2014"]]) == "83")
    )
    
    sum(
        (number_of_alive[["2014"]][my_indices] +
         number_of_alive[["2015"]][my_indices]) / (
            2 * specific_mortality_rate[["2014"]][my_indices] *
            (1 - specific_mortality_rate[["2014"]][my_indices])
        ) * (specific_mortality_rate[["2014"]][my_indices] -
             (x[1] + x[2] * (x[3] ^ age[my_indices]))) ^ 2
    )
    
}


## definuji funkci vracející pouze čtverec, ne součet -------------------------

getGompertzSquare <- function(x, a, b, c){
    
    # '''
    # vrací součet vážených čtverců reziduí pro odhady intenzity úmrtnosti
    # pro věky 60 až 83 let
    # '''
    
    my_index <- which(names(specific_mortality_rate[["2014"]]) == x)
    
        (number_of_alive[["2014"]][my_index] +
         number_of_alive[["2015"]][my_index]) / (
            2 * specific_mortality_rate[["2014"]][my_index] *
            (1 - specific_mortality_rate[["2014"]][my_index])
        ) * (specific_mortality_rate[["2014"]][my_index] -
             (a + b * (c ^ age[my_index]))) ^ 2
    
}


## využijme nyní Davidon–Fletcher–Powellovu numerickou metodu pro nalezení
## minima funkce o více proměnných, zde funkce vracející součet vážených
## čtverců reziduí pro Gompertzovu funkci na datech roku 2014 ve věku
## 60 až 83 let

fminsearch(
    getGompertzSumOfSquares,
    x0 = c(a, b, c),
    minimize = TRUE,
    dfree = FALSE
)$xval                ## vrací nové a, b, c:
                      ## a = 4.521604e-03
                      ## b = 2.710064e-06
                      ## c = 1.130100e+00

a_new <- fminsearch(
    getGompertzSumOfSquares,
    x0 = c(a, b, c),
    minimize = TRUE,
    dfree = FALSE
)$xval[1]

b_new <- fminsearch(
    getGompertzSumOfSquares,
    x0 = c(a, b, c),
    minimize = TRUE,
    dfree = FALSE
)$xval[2]

c_new <- fminsearch(
    getGompertzSumOfSquares,
    x0 = c(a, b, c),
    minimize = TRUE,
    dfree = FALSE
)$xval[3]


## původní suma vážených čtverců (věk 60-83 let) ------------------------------

getGompertzSumOfSquares(c(a, b, c))                ## 316.7384


## nová suma vážených čtverců (věk 60-83 let) ---------------------------------

getGompertzSumOfSquares(c(a_new, b_new, c_new))    ## 56.90267


## ----------------------------------------------------------------------------

###############################################################################

## definuji Gompertzovu-Makehamovu funkci -------------------------------------

getMyGompertz <- function(
    
    x,
    a = a_new,
    b = b_new,
    c = c_new
    
    ){
    
    # '''
    # vrací odhad intenzity úmrtnosti podle Gompertze-Makehama, optimálně
    # pro věky 60-83 let
    # '''
    
    a + b * c ^ x
    
}


## ----------------------------------------------------------------------------

###############################################################################

## nyní pro věkový interval 83 let a více -------------------------------------

## zavádím funkci součtu čtverců reziduí, kterou je nutné minimalizovat
## pro parametr "d" -----------------------------------------------------------

getModifiedGompertzSumOfSquares <- function(x){
    
    # '''
    # vrací součet vážených čtverců reziduí pro odhady intenzity úmrtnosti
    # pro věky 83 až 99 let
    # '''
    
    my_indices <- c(
        which(names(specific_mortality_rate[["2014"]]) == "83"):
        which(names(specific_mortality_rate[["2014"]]) == "99")
    )
    
    sum(
        (number_of_alive[["2014"]][my_indices] +
         number_of_alive[["2015"]][my_indices]) / (
            2 * specific_mortality_rate[["2014"]][my_indices] *
            (1 - specific_mortality_rate[["2014"]][my_indices])
        ) * (specific_mortality_rate[["2014"]][my_indices] -
             (x[1] + x[2] * (
                 x[3] ^ (83 + (log(
                                  x[4] * (age[my_indices] + 0.5 - 83) + 1
                                  )) / x[4])
              ))
            ) ^ 2
    )
    
}


## využijme nyní Davidon–Fletcher–Powellovu numerickou metodu pro nalezení
## minima funkce o více proměnných, zde funkce vracející součet vážených
## čtverců reziduí pro modifikovanou Gompertzovu funkci na datech
## roku 2014 ve věku 83 až 99 let

fminsearch(
    getModifiedGompertzSumOfSquares,
    x0 = c(a, b, c, -0.02),
    minimize = TRUE,
    dfree = FALSE
)$xval                ## d = 4.714873e-02

d <- fminsearch(
    getModifiedGompertzSumOfSquares,
    x0 = c(a, b, c, -0.02),
    minimize = TRUE,
    dfree = FALSE
)$xval[4]


## ----------------------------------------------------------------------------

###############################################################################

## definuji modifikovanou Gompertzovu-Makehamovu funkci

getMyModifiedGompertz <- function(x, a, b, c, d){
    
    # '''
    # vrací odhad intenzity úmrtnosti podle Gompertze-Makehama pro věky
    # od 83 let
    # '''
    
    a + b * c ^ (83 + (log(d * (x + 0.5 - 83) + 1)) / d)
    
}


## ----------------------------------------------------------------------------

###############################################################################

## vytvářím vyrovnání pro úmrtnostní tabulky ----------------------------------

balancing <- rep(NA, length(age))
names(balancing) <- age

for(i in 1:2){

    balancing[
        as.character(i)
    ] <- specific_mortality_rate[["2014"]][as.character(i)]
    
}

for(i in 3:5){

    balancing[
        as.character(i)
    ] <- getWeightedMovingAverage(
             specific_mortality_rate[["2014"]][
                 c(
                    which(names(
                        specific_mortality_rate[["2014"]]
                        ) == as.character(i - 1)):
                    which(names(
                        specific_mortality_rate[["2014"]]
                        ) == as.character(i + 1))
                    )
             ],
             rep(1/3, 3)
         )
         
}

for(i in 6){

    balancing[
        as.character(i)
    ] <- 0.75 * getMyMovingAverage(i, 3) + 0.25 * getMyMovingAverage(i, 9)
    
}

for(i in 7){

    balancing[
        as.character(i)
    ] <- 0.50 * getMyMovingAverage(i, 3) + 0.50 * getMyMovingAverage(i, 9)
    
}

for(i in 8){

    balancing[
        as.character(i)
    ] <- 0.25 * getMyMovingAverage(i, 3) + 0.75 * getMyMovingAverage(i, 9)
    
}

for(i in 9:29){

    balancing[
        as.character(i)
    ] <- getMyMovingAverage(i, 9)
    
}

for(i in 30){

    balancing[
        as.character(i)
    ] <- 0.75 * getMyMovingAverage(i, 9) + 0.25 * getMyMovingAverage(i, 19)
    
}

for(i in 31){

    balancing[
        as.character(i)
    ] <- 0.50 * getMyMovingAverage(i, 9) + 0.50 * getMyMovingAverage(i, 19)
    
}

for(i in 32){

    balancing[
        as.character(i)
    ] <- 0.25 * getMyMovingAverage(i, 9) + 0.75 * getMyMovingAverage(i, 19)
    
}

for(i in 33:59){

    balancing[
        as.character(i)
    ] <- getMyMovingAverage(i, 19)
    
}

for(i in 60){

    balancing[
        as.character(i)
    ] <- 0.75 * getMyMovingAverage(i, 19) +
         0.25 * getMyGompertz(i, a_new, b_new, c_new)
    
}

for(i in 61){

    balancing[
        as.character(i)
    ] <- 0.50 * getMyMovingAverage(i, 19) +
         0.50 * getMyGompertz(i, a_new, b_new, c_new)
    
}

for(i in 62){

    balancing[
        as.character(i)
    ] <- 0.25 * getMyMovingAverage(i, 19) +
         0.75 * getMyGompertz(i, a_new, b_new, c_new)
    
}

for(i in 63:110){

    balancing[
        as.character(i)
    ] <- getMyGompertz(i, a_new, b_new, c_new)
    
}


## ----------------------------------------------------------------------------

###############################################################################

## dávám dohromady tisknutelný výstup -----------------------------------------

rezidua <- rep(NA, length(age))

for(x in c(60:99)){
    rezidua[which(age == as.character(x))] <-
        getGompertzSquare(as.character(x), a, b, c)        
}

printable_output <- cbind(
    "věk" = age,
    "střed věkového intervalu" = age_mid_point,
    "počet zemřelých 2014" = mortality_rate[["2014"]],
    "počet žijících k 1. 1. 2014" = number_of_alive[["2014"]],
    "počet žijících k 1. 1. 2015" = number_of_alive[["2015"]],
    "specifické míry úmrtnosti" = c(
        specific_mortality_rate[["2014"]], rep(NA, 11)
        ),
    "3-klouzavý průměr" = c(
        weighted_moving_averages[["2014"]][["3"]], rep(NA, 11)
    ),
    "9-klouzavý průměr" = c(
        weighted_moving_averages[["2014"]][["9"]], rep(NA, 11)
    ),
    "19-klouzavý průměr" = c(
        weighted_moving_averages[["2014"]][["19"]], rep(NA, 11)
    ),
    "hodnoty G-M funce" = c(
        rep(NA, 60),
        getMyGompertz(60:110, a_new, b_new, c_new)
        ),
    "vážené čtverce G-M reziduí" = rezidua,
    "vyrovnání pro úmrtnost" = balancing
)

printable_output <- rbind(printable_output, "111" = c(111, rep(NA, 11)))

print(xtable(printable_output,
             align = rep("", ncol(printable_output) + 1),
             digits = c(0, 0, 1, 0, 0, 0, 6, 6, 6, 6, 6, 6, 6)
             ),
      floating = FALSE, tabular.environment = "tabular",
      hline.after = NULL, include.rownames = FALSE,
      include.colnames = TRUE
      )
      
      
## ----------------------------------------------------------------------------

###############################################################################
###############################################################################
###############################################################################

## vytvářím druhou tabulku s biometrickými parametry --------------------------

###############################################################################

## definuji "q_0" a "p_0", pravděpodobnost úmrtí a přežití kojence ------------

my_alpha <- 0.85    ## zlomek nultého roku života

q_0 <- mortality_rate[["2014"]][1] / (
    births_panel[, "2013"] * (1 - my_alpha) +
    births_panel[, "2014"] * my_alpha
    )

p_0 <- 1 - q_0


## pravděpodobnost přežití pro věky 0-110 let ---------------------------------

survival_probability <- c("0" = p_0)

for(i in 1:110){
    survival_probability <- c(
        survival_probability,
        exp(-balancing[as.character(i)])
        )
}


## pravděpodobnost úmrtí pro věky 0-110 let -----------------------------------

death_probability <- 1 - survival_probability


## očekávaný počet přežívajících ----------------------------------------------

expected_number_of_alive <- c("0" = 100000,
                              100000 * survival_probability[2:111])

                              
## očekávaný počet úmrtí ------------------------------------------------------

expected_number_of_deaths <- expected_number_of_alive * death_probability


## počet prožitých let --------------------------------------------------------

L_x <- 100000 - my_alpha * expected_number_of_deaths["0"]

for(i in 2:110){
    L_x[i] <- 
        (expected_number_of_alive[i] + expected_number_of_alive[i + 1]) / 2
}

names(L_x) <- 0:109


## počet let života -----------------------------------------------------------

T_x <- NULL

for(i in 1:110){
    T_x[i] <- sum(L_x[i:110])
}

names(T_x) <- 0:109


## střední délka života -------------------------------------------------------

mean_life_length <- T_x / expected_number_of_alive[1:110]


## pravděpodobná délka života -------------------------------------------------

likely_life_length <- NULL

for(i in 0:110){

    my_x <- min(as.integer(names(expected_number_of_alive[
            expected_number_of_alive >=
            expected_number_of_alive[as.character(i)] / 2
        ][length(expected_number_of_alive[
            expected_number_of_alive >=
            expected_number_of_alive[as.character(i)] / 2
        ])])),
        as.integer(
            names(expected_number_of_alive)[
                length(expected_number_of_alive)
                ]
            )
        )
    
    likely_life_length <- c(
        likely_life_length,
        unname(
            my_x + (expected_number_of_alive[as.character(my_x)] -
                expected_number_of_alive[as.character(i)] / 2) / (
                    expected_number_of_alive[as.character(my_x)] -
                    expected_number_of_alive[as.character(my_x + 1)]
        ) - i)
    )
    
}

names(likely_life_length) <- 0:110


## normální délka života ------------------------------------------------------

estimate <- log(
    1 / (2 * b_new) * (
        log(c_new) - 2 * a_new + sqrt((log(c_new) - 4 * a_new) * log(c_new))
        )
    ) / log(c_new)

normal_life_length <- NULL

for(i in 0:110){
    normal_life_length <- c(
        normal_life_length,
        max(estimate, i) - i
    )
}

names(normal_life_length) <- 0:110


## ----------------------------------------------------------------------------

###############################################################################

## vytvářím tisknutelný výstup ------------------------------------------------

printable_output <- cbind(
    "věk" = age,
    "pravděpodobnost přežití" = survival_probability,
    "pravděpodobnost úmrtí" = death_probability,
    "počet dožívajících" = expected_number_of_alive,
    "počet zemřelých" = expected_number_of_deaths,
    "počet prožitých let" = c(L_x, NA),
    "počet let života" = c(T_x, NA),
    "střední délka života" = c(mean_life_length, NA),
    "pravděpodobná délka života" = likely_life_length,
    "normální délka života" = normal_life_length,
    "střední věk úmrtí" = c(mean_life_length + c(0:109), NA),
    "pravděpodobný věk úmrtí" = likely_life_length + c(0:110),
    "normální věk úmrtí" = normal_life_length + c(0:110)
)

printable_output <- rbind(printable_output, "111" = c(111, rep(NA, 12)))

print(xtable(printable_output,
             align = rep("", ncol(printable_output) + 1),
             digits = c(0, 0, 6, 6, 0, 0, 2, 0, 2, 2, 2, 2, 2, 2)
             ),
      floating = FALSE, tabular.environment = "tabular",
      hline.after = NULL, include.rownames = FALSE,
      include.colnames = TRUE
      )


## ----------------------------------------------------------------------------

###############################################################################
###############################################################################
###############################################################################







