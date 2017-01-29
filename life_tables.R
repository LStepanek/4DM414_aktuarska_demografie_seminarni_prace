###############################################################################
###############################################################################
###############################################################################

## vytvářím generační úmrtnostní tabulky --------------------------------------

###############################################################################

## nejdříve zkoumám bodové odhady koeficientů meziročních růstů střední
## délky života plynoucích z univariátní lineární regrese ---------------------

life_tables_panel <- as.data.frame(life_tables_panel)  ## přetavuji dataset
                                                       ## se středními délkami
                                                       ## života pro jednotli-
                                                       ## vé roky a věky do
                                                       ## dataframeu

life_tables_increments <- NULL
                                                       
for(i in 1:dim(life_tables_panel)[1]){
    
    my_row <- NULL
    
    for(j in 5:dim(life_tables_panel)[2]){
        
        y <- unname(unlist(life_tables_panel[i, (max(c(1, j - 9))):j]))
        x <- as.numeric(colnames(life_tables_panel)[(max(c(1, j - 9))):j])
        my_row <- c(my_row, unname(lm(y ~ x)$coefficients[2]))
    
    }
    
    life_tables_increments <- rbind(life_tables_increments,
                                    my_row)
    
    rownames(life_tables_increments)[
        dim(life_tables_increments)[1]
        ] <- rownames(life_tables_panel)[i]
    
}

colnames(
    life_tables_increments
    ) <- colnames(life_tables_panel)[5:dim(life_tables_panel)[2]]


## vytvářím tisknutelné tabulky pro dataset meziročních koeficientů růstu -----

print(xtable(life_tables_increments[, 1:18],
             align = rep("", ncol(life_tables_increments[, 1:18]) + 1),
             digits = 3),
      floating = FALSE, tabular.environment = "tabular",
      hline.after = NULL, include.rownames = TRUE,
      include.colnames = TRUE
      )


print(xtable(life_tables_increments[, 19:36],
             align = rep("", ncol(life_tables_increments[, 19:36]) + 1),
             digits = 3),
      floating = FALSE, tabular.environment = "tabular",
      hline.after = NULL, include.rownames = TRUE,
      include.colnames = TRUE
      )


## ----------------------------------------------------------------------------

###############################################################################
###############################################################################
###############################################################################

## nyní vytvářím tabulku s pravděpodobnostmi úmrtí pro jednotlivé roky
## 2014-2165 ------------------------------------------------------------------

###############################################################################

## zavádím vektor koeficientů poklesu pravděpodobnosti úmrtí ------------------

my_k_x <- c(1,                     ## hodnota pro rok 2014
            rep(0.999, 100),        ## dummy hodnoty pro roky 2015-2114
            rep(1, 51)             ## hodnoty pro roky 2115-2165
            )

names(my_k_x) <- c(2014:2165)


## ----------------------------------------------------------------------------

###############################################################################

## zastropuji poslední hodnotu pravděpodobností úmrtí pro jednotlivé
## věky roku 2014 na hodnotu 1.0 ----------------------------------------------

my_death_probability <- death_probability
my_death_probability <- c(my_death_probability,
                          1)

names(my_death_probability)[
    length(my_death_probability)
] <- as.character(
         as.integer(
             names(my_death_probability)[length(my_death_probability) - 1]
         ) + 1
     )


## nyní vytvářím tabulku pravděpodobností úmrtí pro roky 2014-2165
## se započítanými koeficienty poklesu těchto pravděpodobností ----------------

modified_death_probability_table <- data.frame(
    "2014" = my_death_probability,
    check.names = FALSE
    )

for(i in 2:length(my_k_x)){
    
    modified_death_probability_table <- cbind(
        
        modified_death_probability_table,
        modified_death_probability_table[,
            dim(modified_death_probability_table)[2]
        ] * my_k_x[i]
        
    )
    
    colnames(modified_death_probability_table)[
        dim(modified_death_probability_table)[2]
    ] <- names(my_k_x)[i]
    
}


## ----------------------------------------------------------------------------

###############################################################################

## vytvářím tabulku s počty dožívajících pro roky 2014-2165 -------------------

modified_alive_numbers_table <- NULL

for(i in 1:dim(modified_death_probability_table)[2]){
    
    my_column <- 100000

    for(j in 2:dim(modified_death_probability_table)[1]){
        
        my_column <- c(
            my_column,
            my_column[j - 1] * (
                1 - modified_death_probability_table[j - 1, i]
            )
        )    
        
    }
    
    modified_alive_numbers_table <- as.data.frame(cbind(
        modified_alive_numbers_table,
        my_column
    ))
    
    colnames(modified_alive_numbers_table)[
        dim(modified_alive_numbers_table)[2]
    ] <- colnames(modified_death_probability_table)[i]
    
}

rownames(
    modified_alive_numbers_table
) <- rownames(modified_death_probability_table)


## ----------------------------------------------------------------------------

###############################################################################

## počítám střední délky života novorozence pro všechny roky 2014-2114 --------

my_expected_life_lengths <- NULL

for(my_year in c(2014:2114)){

    my_expected_life_lengths <- c(
    
        my_expected_life_lengths,
        getMyNewbornMeanLifeLength(    
            modified_alive_numbers_table[, as.character(my_year)],
            my_alpha
        )
    
    )
    
    names(my_expected_life_lengths)[
        length(my_expected_life_lengths)
    ] <- as.character(my_year)
    
}


## ----------------------------------------------------------------------------

###############################################################################

## vytvářím vektor středních délek života novorozence pro některé roky
## vzdálené od sebe vždy deset let --------------------------------------------

years_of_interest <- colnames(modified_alive_numbers_table)[
        as.integer(colnames(modified_alive_numbers_table)) <= 2114
    ][
        as.integer(colnames(modified_alive_numbers_table))[
        as.integer(colnames(modified_alive_numbers_table)) <= 2114
    ] %% 10 == 4
]


## ----------------------------------------------------------------------------

###############################################################################

## dopočítávám odhady střední délky novorozence pro ty roky končící
## číslicí "4", pro které nenabízí Eurostat svůj odhady -----------------------

## využívám přitom předpokladu, že střední délka života je po částech
## log-lineární, tj. že každých deset let se prodlouží o 0.9 násobek
## prodloužení během předchozích deseti let -----------------------------------

newborn_life_length <- expectancies_panel[
    "0",
    intersect(colnames(expectancies_panel), years_of_interest)
]

for(my_year in years_of_interest){
    
    if(!my_year %in% names(newborn_life_length)){
        
        newborn_life_length <- c(
            newborn_life_length,
            (newborn_life_length[as.character(as.integer(my_year) - 10)] -
             newborn_life_length[as.character(as.integer(my_year) - 20)]) *
             0.9 +
             newborn_life_length[as.character(as.integer(my_year) - 10)]
        )
    
        names(newborn_life_length)[length(newborn_life_length)] <-
            my_year        
    
    }    
    
}


## ----------------------------------------------------------------------------

###############################################################################

## nyní přepočítávm koeficienty poklesu pravděpodobnosti úmrtnosti tak,
## aby se střední délky života novorozence odhadlé (i) podle těchto
## koeficientů rovnaly ve vybraných letech středním délkám života novorozenců
## odhadnutým metodikou Eurostatu ---------------------------------------------

for(my_year in years_of_interest[years_of_interest != "2014"]){
    
    my_k_x[as.character((as.integer(my_year) - 9):as.integer(my_year))] <- 1
    
    while(
    
        getMyNewbornMeanLifeLength(
                modified_alive_numbers_table[, as.character(my_year)],
                my_alpha
        ) < newborn_life_length[my_year]
        
    ){
        
        modified_death_probability_table <- data.frame(
            "2014" = my_death_probability,
            check.names = FALSE
        )

        for(i in 2:length(my_k_x)){
        
            modified_death_probability_table <- cbind(
            
                modified_death_probability_table,
                modified_death_probability_table[,
                    dim(modified_death_probability_table)[2]
                ] * my_k_x[i]
            
            )
        
            colnames(modified_death_probability_table)[
                dim(modified_death_probability_table)[2]
            ] <- names(my_k_x)[i]
    
        }
        
        modified_alive_numbers_table <- NULL

        for(i in 1:dim(modified_death_probability_table)[2]){
            
            my_column <- 100000

            for(j in 2:dim(modified_death_probability_table)[1]){
                
                my_column <- c(
                    my_column,
                    my_column[j - 1] * (
                        1 - modified_death_probability_table[j - 1, i]
                    )
                )    
                
            }
            
            modified_alive_numbers_table <- as.data.frame(cbind(
                modified_alive_numbers_table,
                my_column
            ))
            
            colnames(modified_alive_numbers_table)[
                dim(modified_alive_numbers_table)[2]
            ] <- colnames(modified_death_probability_table)[i]
            
        }

        rownames(
            modified_alive_numbers_table
        ) <- rownames(modified_death_probability_table)
        
        my_k_x[
            as.character((as.integer(my_year) - 9):as.integer(my_year))
        ] <- my_k_x[
                 as.character((as.integer(my_year) - 9):as.integer(my_year))
             ] - 0.0001
            
    }
    
}


## ----------------------------------------------------------------------------

###############################################################################

## počítám neúplnou generační tabulku pravděpodobností úmrtí ------------------

my_generation_death_table <- NULL

for(year in c(1954:2054)){

    my_column <- NULL
    
    for(age in c(60:110)){
    
        my_column <- c(
            my_column,
            getMyGenerationDeathProbability(
                age,
                year,
                modified_death_probability_table
            )
        )
        
    }
    
    my_generation_death_table <- as.data.frame(cbind(
        my_generation_death_table,
        my_column
    ), check.names = FALSE)
    
    colnames(my_generation_death_table)[
        dim(my_generation_death_table)[2]
    ] <- year
    
}

rownames(my_generation_death_table) <- c(60:110)


## ----------------------------------------------------------------------------

###############################################################################

## nyní sestavuji generační tabulku počtů dožívajících ------------------------

my_generation_alive_table <- NULL

for(i in 1:dim(my_generation_death_table)[2]){
    
    my_column <- 100000

    for(j in 2:dim(my_generation_death_table)[1]){
        
        my_column <- c(
            my_column,
            my_column[j - 1] * (
                1 - my_generation_death_table[j - 1, i]
            )
        )
        
    }
    
    my_generation_alive_table <- as.data.frame(cbind(
        my_generation_alive_table,
        my_column
    ))
    
    colnames(my_generation_alive_table)[
        dim(my_generation_alive_table)[2]
    ] <- colnames(my_generation_death_table)[i]
    
}

rownames(
    my_generation_alive_table
) <- rownames(my_generation_death_table)


## ----------------------------------------------------------------------------

###############################################################################

## modeluji střední délky života pro konkrétní věky mezi 60-110 lety vždy
## pro dané roky; jednak pro daný přesný věk, jednak pro danou generaci -------

for(my_dataset_name in c("exact", "generation")){

    temp_data <- NULL

    for(x in rownames(my_generation_alive_table)){
        
        my_row <- NULL

        for(year in colnames(my_generation_alive_table)){
        
            my_row <- c(
            
                my_row,
                getMyMeanLifeLengthForGivenAge(
                    x,
                    year = if(my_dataset_name == "exact"){
                        as.character(as.integer(year) + 60)
                    }else{year},
                    alive_numbers_table = if(
                        my_dataset_name == "generation"
                    ){
                        my_generation_alive_table
                    }else{
                        modified_alive_numbers_table
                    }
                )
                
            )
            
        }
        
        temp_data <- as.data.frame(
                         rbind(temp_data, my_row),
                         check.names = FALSE
                     )
            
    }

    colnames(temp_data) <- colnames(my_generation_alive_table)
    rownames(temp_data) <- rownames(my_generation_alive_table)
    
    assign(
        paste(my_dataset_name, "life_lengths_table", sep = "_"),
        temp_data
    )

}


## ----------------------------------------------------------------------------

###############################################################################

## tisknutelné varianty úmrtnostních tabulek ----------------------------------

output <- generation_life_lengths_table - exact_life_lengths_table[, 1]

printable_output <- t(output)[, 35:51]

print(xtable(printable_output,
             align = rep("", ncol(printable_output) + 1),
             digits = 2
             ),
      floating = FALSE, tabular.environment = "tabular",
      hline.after = NULL, include.rownames = TRUE,
      include.colnames = TRUE
      )


## ----------------------------------------------------------------------------

###############################################################################

## ukládám postupně projekce pravděpodobností úmrtí, počtů dožívajících,
## délky života a generační úmrtnostní tabulky --------------------------------

setwd(paste(mother_working_directory, "vystupy", sep = "/"))


## specifikace tabule formou listu --------------------------------------------

my_specification <- list(
    "name" = c("my_generation_death_table",
               "my_generation_alive_table",
               "exact_life_lengths_table",
               "generation_life_lengths_table"),
    "digits" = c(6, 0, 2, 2),
    "czech_name" = c("projece pravděpodobností úmrtí",
                     "projekce počtu přežívajících",
                     "střední délka života | věk",
                     "střední délka života | generace")    
)


## vytvářím pracovní soubor ---------------------------------------------------

life_tables <- createWorkbook()


## vytvářím dva své styly - jednak tučné písmo, jednak písmo zarovnané
## doprava v rámci buňky ------------------------------------------------------

my_bold_style <- createStyle(textDecoration = "bold")
right_halign_cells <- createStyle(halign = "right")


## vytvářím sešity postupně pro čtyři tabulky ---------------------------------

for(my_table_name in c(

    "my_generation_death_table",
    "my_generation_alive_table",
    "exact_life_lengths_table",
    "generation_life_lengths_table"
    
)){

    ## inicializuji dataset ---------------------------------------------------

    my_table <- get(my_table_name)

    addWorksheet(
        wb = life_tables,
        sheetName = my_specification$czech_name[
                        my_specification$name == my_table_name
                    ]
    )


    ## ukládám do sešitu data -------------------------------------------------

    writeData(
        wb = life_tables,
        sheet = my_specification$czech_name[
                        my_specification$name == my_table_name
                    ],
        rowNames = TRUE,
        colNames = TRUE,
        x = format(
                round(
                    my_table,
                    digits = my_specification$digits[
                        my_specification$name == my_table_name
                    ]
                ),
                nsmall = my_specification$digits[
                        my_specification$name == my_table_name
                ]
            )
    )


    ## nastavuji automatickou šířku sloupce -----------------------------------
     
    setColWidths(
        wb = life_tables,
        sheet = my_specification$czech_name[
                        my_specification$name == my_table_name
                ],
        cols = 1:dim(my_table)[2],
        widths = "auto"
    )
    
    
    ## přidávám tučné písmo popiskům ------------------------------------------
    
    addStyle(
        wb = life_tables,
        sheet = my_specification$czech_name[
                        my_specification$name == my_table_name
                ],
        style = my_bold_style,
        rows = c(1:(dim(my_table)[1] + 1), rep(1, dim(my_table)[2])),
        cols = c(rep(1, dim(my_table)[1] + 1), 2:(dim(my_table)[2] + 1))
    )

    addStyle(
        wb = life_tables,
        sheet = my_specification$czech_name[
                        my_specification$name == my_table_name
                ],
        style = right_halign_cells,
        rows = 2:(dim(my_table)[1] + 1),
        cols = 2:(dim(my_table)[2] + 1),
        gridExpand = TRUE
    )

}

saveWorkbook(
    wb = life_tables,
    file = "projekce_a_umrtnostni_tabulky.xlsx",
    overwrite = TRUE
)


setwd(mother_working_directory)


## ----------------------------------------------------------------------------

###############################################################################
###############################################################################
###############################################################################







