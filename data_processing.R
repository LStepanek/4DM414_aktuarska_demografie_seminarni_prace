###############################################################################
###############################################################################
###############################################################################

## pro datasety 'births', 'population' a 'deaths' vytvářím tabulky, které
## by měly být za standardních okolností v Excelu -----------------------------

###############################################################################

## vytvářím panelovou tabulku pro dataset "population" a "deaths" -------------

for(my_data_name in c("population", "deaths", "expectancies")){
    
    my_data <- get(my_data_name)
    
    my_year <- start_year
    
    output <- NULL
    
    while(!my_year %in% format(my_data[, "time"], "%Y")){
        my_year <- my_year <- as.character(as.integer(my_year) + 1)
    }
    
    while(my_year %in% format(my_data[, "time"], "%Y")){
        
        temp_data <- my_data[format(my_data[, "time"], "%Y") == my_year, ]
        
        output <- cbind(output,
                        c(
                          temp_data[temp_data$age == "Y_LT1", "values"],
                          temp_data[
                              grepl("Y[0-9]+", temp_data[, "age"]), ][
                                  order(
                                      as.integer(
                                          gsub("Y",
                                               "",
                                               temp_data[
                                                   grepl(
                                                       "Y[0-9]+",
                                                        temp_data[, "age"]
                                                        ),
                                                        "age"
                                                        ]
                                                        )
                                                )
                                       ),"values"],
                            if("Y_OPEN" %in% temp_data$age){
                                temp_data[temp_data$age == "Y_OPEN",
                                          "values"]
                            },
                            if("Y_GE101" %in% temp_data$age){
                                temp_data[temp_data$age == "Y_GE101",
                                          "values"]
                            },
                            if("TOTAL" %in% temp_data$age){
                                temp_data[temp_data$age == "TOTAL",
                                          "values"]
                            }
                          )
                        )
                        
        colnames(output)[dim(output)[2]] <- my_year
        
        my_year <- as.character(as.integer(my_year) + 1)
        
    }
    
    rownames(output) <- c(
        
        "0",
        sort(
            as.integer(
                gsub("Y",
                     "",
                     temp_data[
                         grepl("Y[0-9]+", temp_data[, "age"]), "age"
                         ]
                    )
                )
            ),
        if("Y_OPEN" %in% temp_data$age){
            "otevřené"
        },
        if("Y_GE101" %in% temp_data$age){
            "větší než 100"
        },
        if("TOTAL" %in% temp_data$age){
            "celkem"
        }
    
    )
    
    assign(
        paste(my_data_name, "panel", sep = "_"),
        output
    )

}


## ----------------------------------------------------------------------------

###############################################################################

## vytvářím panelovou tabulku pro dataset "births" ----------------------------

for(my_data_name in c("births")){
    
    my_data <- get(my_data_name)
    
    my_year <- start_year
    
    output <- NULL
    
    while(my_year %in% format(my_data[, "time"], "%Y")){
        
        temp_data <- my_data[format(my_data[, "time"], "%Y") == my_year, ]
        
        output <- cbind(output,
                        temp_data[temp_data$age == "TOTAL", "values"]
                        )
                        
        colnames(output)[dim(output)[2]] <- my_year
        
        my_year <- as.character(as.integer(my_year) + 1)
        
    }
    
    rownames(output) <- "celkem"
    
    assign(
        paste(my_data_name, "panel", sep = "_"),
        output
    )

}


## ----------------------------------------------------------------------------

###############################################################################

## vytvářím panelovou tabulku pro dataset "life_tables" -----------------------

for(my_data_name in c("life_tables")){
    
    my_data <- get(my_data_name)
    
    output <- NULL
        
    for(my_year in sort(unique(format(my_data[, "time"], "%Y")))){
    
        temp_data <- my_data[format(my_data[, "time"], "%Y") == my_year &
                             my_data[, "indic_de"] == "LIFEXP", ]
        
        output <- cbind(output,
                        c(
                          temp_data[temp_data$age == "Y_LT1", "values"],
                          temp_data[
                              grepl("Y[0-9]+", temp_data[, "age"]), ][
                                  order(
                                      as.integer(
                                          gsub("Y",
                                               "",
                                               temp_data[
                                                   grepl(
                                                       "Y[0-9]+",
                                                        temp_data[, "age"]
                                                        ),
                                                        "age"
                                                        ]
                                                        )
                                                )
                                       ),"values"],
                            temp_data[temp_data$age == "Y_GE85", "values"]
                          )
                        )
                        
        colnames(output)[dim(output)[2]] <- my_year
        
    }
    
    rownames(output) <- c(
        
        "0",
        sort(
            as.integer(
                gsub("Y",
                     "",
                     temp_data[
                         grepl("Y[0-9]+", temp_data[, "age"]), "age"
                         ]
                    )
                )
            ),
        "větší než 84"
    
    )
    
    assign(
        paste(my_data_name, "panel", sep = "_"),
        output
    )
    
}


## ----------------------------------------------------------------------------

###############################################################################

## vytvářím tisknutelné tabulky pro dataset "population", "deaths"
## a "expectancies" -----------------------------------------------------------

for(my_data_name in c("population", "deaths", "expectancies")){
    
    my_data <- get(paste(my_data_name, "panel", sep = "_"))
    
    left_table <- my_data[
        1:(dim(my_data)[1] / 2),
    ]
    
    right_table <- my_data[
    (dim(my_data)[1] / 2 + 1):dim(my_data)[1],
    ]
    
    temp_data <- cbind(left_table, rownames(right_table), right_table)
        
    print(xtable(temp_data,
                 align = rep("", ncol(temp_data) + 1),
                 digits = 0),
          floating = FALSE, tabular.environment = "tabular",
          hline.after = NULL, include.rownames = TRUE,
          include.colnames = TRUE
          )
    
}


## ----------------------------------------------------------------------------

###############################################################################

## vytvářím tisknutelné tabulky pro dataset "births" --------------------------

for(my_data_name in c("births")){
    
    temp_data <- get(paste(my_data_name, "panel", sep = "_"))
    
    print(xtable(temp_data,
                 align = rep("", ncol(temp_data) + 1),
                 digits = 0),
          floating = FALSE, tabular.environment = "tabular",
          hline.after = NULL, include.rownames = TRUE,
          include.colnames = TRUE
          )
    
}


## ----------------------------------------------------------------------------

###############################################################################

## vytvářím tisknutelné tabulky pro dataset "life_tables" ---------------------

print(xtable(life_tables_panel[, 1:20],
             align = rep("", ncol(life_tables_panel[, 1:20]) + 1),
             digits = 1),
      floating = FALSE, tabular.environment = "tabular",
      hline.after = NULL, include.rownames = TRUE,
      include.colnames = TRUE
      )


print(xtable(life_tables_panel[, 21:40],
             align = rep("", ncol(life_tables_panel[, 21:40]) + 1),
             digits = 1),
      floating = FALSE, tabular.environment = "tabular",
      hline.after = NULL, include.rownames = TRUE,
      include.colnames = TRUE
      )


## ----------------------------------------------------------------------------

###############################################################################
###############################################################################
###############################################################################







