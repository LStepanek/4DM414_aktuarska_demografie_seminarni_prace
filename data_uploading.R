###############################################################################
###############################################################################
###############################################################################

## nahrávám data --------------------------------------------------------------

setwd(paste(mother_working_directory, "vstupy", sep = "/"))


for(my_sheet_name in my_codebook$variable_abbreviation){
    
    my_data <- read.xlsx(
    
        xlsxFile = "zdrojova_data.xlsx",
        sheet = my_sheet_name,
        rowNames = TRUE    
        
    )
    
    for(my_variable in c("unit", "sex", "geo", "age", "indic_de")){
        
        if(my_variable %in% colnames(my_data)){
        
            my_data[, my_variable] <- as.character(
                                          my_data[, my_variable]
                                      )
                                      
        }
                
    }
    
    for(my_variable in c("time", "values")){
        
        if(my_variable %in% colnames(my_data)){
        
            my_data[, my_variable] <- as.numeric(
                                          as.character(
                                              my_data[, my_variable]
                                          )
                                      )
                                      
        }
                                      
    }
    
    if("time" %in% colnames(my_data)){
        
        my_data[, "time"] <- as.Date(
                                 my_data[, "time"], origin = "1899-12-30"
                             )
                
    }    
    
    
    assign(
        my_sheet_name,
        my_data
    )
        
}


setwd(mother_working_directory)


## ----------------------------------------------------------------------------

###############################################################################
###############################################################################
###############################################################################







