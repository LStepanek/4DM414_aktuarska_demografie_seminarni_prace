###############################################################################
###############################################################################
###############################################################################

## nastavuji pracovní složku --------------------------------------------------

while(!grepl("seminarni_prace$", getwd())){

    setwd(choose.dir())
    
}

mother_working_directory <- getwd()


## ----------------------------------------------------------------------------

###############################################################################

## spouštím sekvenci skriptů --------------------------------------------------

for(my_script in c(
    
    "initialization",      ## spouštím inicializaci
    "data_downloading",    ## stahuji data
    "data_saving",         ## ukládám data
    "data_uploading",      ## loaduji data
    "data_processing",     ## upravuji data, vytvářím některé výstupy
    "helper_functions",    ## definuji pomocné funkce
    "calculations",        ## počítám některé míry úmrtnosti pro daný věk
    "advanced_models",     ## počítám další modely predikce mortality
    "life_tables",         ## vytvářím generační úmrtnostní tabulky
    "plotting"             ## vykresluji důležité diagramy
    
)){
    
    source(paste(my_script, ".R", sep = ""),
           echo = TRUE,
           encoding = "UTF-8")
    
}


## ----------------------------------------------------------------------------

###############################################################################
###############################################################################
###############################################################################





