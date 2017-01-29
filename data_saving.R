###############################################################################
###############################################################################
###############################################################################

## ukládám data ---------------------------------------------------------------

setwd(paste(mother_working_directory, "vstupy", sep = "/"))


## zakládám sešit -------------------------------------------------------------

source_data <- createWorkbook()


for(my_code in my_codes){
    
    
    ## inicializuji data ------------------------------------------------------
    
    my_data <- get(
    
         paste(
            names(my_codes)[which(my_codes == my_code)],
            "data",
            sep = "_"
            )
    
    )
    
    
    ## přidávám data do sešitu ------------------------------------------------
    
    addWorksheet(
        wb = source_data,
        sheetName = names(my_codes)[which(my_codes == my_code)]
    )


    ## ukládám do sešitu data -------------------------------------------------

    writeData(
        wb = source_data,
        sheet = names(my_codes)[which(my_codes == my_code)],
        rowNames = TRUE,
        colNames = TRUE,
        x = my_data
    )    
        
}


saveWorkbook(
    wb = source_data,
    file = "zdrojova_data.xlsx",
    overwrite = TRUE
)


setwd(mother_working_directory)


## ----------------------------------------------------------------------------

###############################################################################
###############################################################################
###############################################################################







