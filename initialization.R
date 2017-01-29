###############################################################################
###############################################################################
###############################################################################

## instaluji a inicializuji balíčky -------------------------------------------

for(package in c(
                 "eurostat",
                 "openxlsx",
                 "xtable",
                 "pracma"
                 )){
                 
    if(!(package %in% rownames(installed.packages()))){
    
        install.packages(
            package,
            dependencies = TRUE,
            repos = "http://cran.us.r-project.org"
        )
        
    }
    
    library(package, character.only = TRUE)
    
}


## ----------------------------------------------------------------------------

###############################################################################

## nastavuji handling se zipováním v R ----------------------------------------

Sys.setenv(R_ZIPCMD = "C:/Rtools/bin/zip") 


## ----------------------------------------------------------------------------

###############################################################################

## zakládám podsložku "vstupy" a "výstupy" ------------------------------------

for(my_directory in c("vstupy", "vystupy")){

    if(!file.exists(my_directory)){
    
        dir.create(file.path(
        
            mother_working_directory, my_directory
        
        ))
    
    }

}


## ----------------------------------------------------------------------------

###############################################################################

## loaduji codebook -----------------------------------------------------------

setwd(mother_working_directory)

my_codebook <- read.csv(
    
    file = "codebook.txt",
    header = TRUE,
    sep = ",",
    colClasses = "character",
    encoding = "UTF-8"
    
)


## ----------------------------------------------------------------------------

###############################################################################

## loaduji zadané hodnoty některých proměnných --------------------------------

setwd(mother_working_directory)

for(variable_of_interest in c(
                              
                              "my_geo",
                              "my_sex",
                              "start_year"
                              
                              )){

    assign(
        variable_of_interest,
        gsub(
            " +%.*",
            "",
            readLines(con = "task_variables.txt", encoding = "UTF-8")[
                grepl(
                    variable_of_interest,
                    readLines(con = "task_variables.txt",
                              encoding = "UTF-8")
                    )
                ]
        )
    )

}


## ----------------------------------------------------------------------------

###############################################################################
###############################################################################
###############################################################################





