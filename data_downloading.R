###############################################################################
###############################################################################
###############################################################################

## loaduju data ---------------------------------------------------------------

## prohledávám Eurostat, zajímají mě vždy kódy následující datasetů: ----------

#### (i)   stav populace k 1. lednu dle věku a pohlaví ------------------------
#### (ii)  živě narození dle věku matky a pohlaví novorozence -----------------
#### (iii) zemřelí dle věku a pohlaví -----------------------------------------

## příslušné názvy datasetů jsou samozřejmě v codebooku -----------------------

setwd(mother_working_directory)


my_codes <- NULL

for(my_dataset_of_interest in my_codebook$variable_full_name){

    my_codes <- c(
    
        my_codes,
        search_eurostat(
            pattern = paste("^", my_dataset_of_interest, "$", sep = ""),
            type = "dataset"
        )$code
        
    )
           
    names(my_codes)[
        length(my_codes)
        ] <- my_codebook$variable_abbreviation[
                 my_codebook$variable_full_name ==
                 my_dataset_of_interest
             ]

}


## ----------------------------------------------------------------------------

###############################################################################

## stahuji data ---------------------------------------------------------------

for(my_code in my_codes){
    
    my_data <- as.data.frame(get_eurostat(id = my_code))
    
    
    ## extrahuji z datasetů pouze pozorování vyhovující mému zadání -----------
    
    my_data <- my_data[my_data$geo == my_geo & my_data$sex == my_sex, ]
    
    
    ## přetypovávám proměnnou "geo" a "sex", aby měly jen jeden level ---------
    
    for(my_variable in c("geo", "sex")){
        
        my_data[, my_variable] <- as.factor(
            
            as.character(my_data[, my_variable])
            
            )
        
    }
    
    ## přirazuji profiltrovanému datasetu jméno a objekt ----------------------
    
    assign(
        paste(
            names(my_codes)[which(my_codes == my_code)],
            "data",
            sep = "_"
            ),
        my_data
    )
    
}


## ----------------------------------------------------------------------------

###############################################################################
###############################################################################
###############################################################################







