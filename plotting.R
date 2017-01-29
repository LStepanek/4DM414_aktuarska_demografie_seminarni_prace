###############################################################################
###############################################################################
###############################################################################

## vytvářím diagramy ----------------------------------------------------------

###############################################################################

## pravděpodobnost přežití a úmrtí --------------------------------------------

setwd(paste(mother_working_directory, "vystupy", sep = "/"))

jpeg("pravdepodobnost_preziti_a_umrti.jpg",
    height = 5,
    width = 8,
    units = "in",
    res = 800
    )

par(mar = c(4.1, 4, 1, 5.9), xpd = TRUE)

plot(
    x = names(survival_probability),
    y = survival_probability,
    xaxt = "n",
    xlab = expression(paste("věk ", italic(x), ", [rok]", sep = "")),
    ylim = c(0.0, 1.0),
    ylab = "pravděpodobnost",
    col = "blue",
    type = "l"
)

axis(1, at = seq(0, 110, 10), labels = seq(0, 110, 10))

lines(
    x = names(death_probability),
    y = death_probability,
    col = "red"
)

legend(
    x = "topright",
    legend = c(
        expression(paste("přežití, ", italic(p[x]), sep = "")),
        expression(paste("úmrtí, ", italic(q[x]), sep = ""))
        ),
    title = "pravděpodobnost",
    col = c("blue", "red"),
    lty = 1,
    inset = c(-0.19, 0.00),
    cex = 0.8
)


dev.off()

setwd(mother_working_directory)


## ----------------------------------------------------------------------------

###############################################################################

## počty dožívajících a zemřelých ---------------------------------------------

setwd(paste(mother_working_directory, "vystupy", sep = "/"))

jpeg("pocty_dozivajicich_a_zemrelych.jpg",
    height = 5,
    width = 8,
    units = "in",
    res = 800
    )

par(mar = c(4.1, 4, 1, 7.0), xpd = TRUE)

plot(
    x = names(expected_number_of_alive),
    y = expected_number_of_alive,
    xaxt = "n",
    yaxt = "n",
    ylim = c(0.0, 1e5),
    xlab = expression(paste("věk ", italic(x), ", [rok]", sep = "")),
    ylab = "počet / 10000",
    col = "blue",
    type = "l"
)

axis(1, at = seq(0, 110, 10), labels = seq(0, 110, 10))
axis(2, at = c(0, 2e4, 4e4, 6e4, 8e4, 1e5), labels = seq(0, 10, 2))

lines(
    x = names(expected_number_of_deaths),
    y = expected_number_of_deaths,
    col = "red"
)

legend(
    x = "topright",
    legend = c(
        expression(paste("dožívajících, ", italic(l [x]), sep = "")),
        expression(paste("zemřelých, ", italic(d [x]), sep = ""))
        ),
    title = "počet",
    col = c("blue", "red"),
    lty = 1,
    inset = c(-0.235, 0.00),
    cex = 0.75
)

dev.off()

setwd(mother_working_directory)


## ----------------------------------------------------------------------------

###############################################################################

## střední, pravděpodobná a normální délka života -----------------------------

setwd(paste(mother_working_directory, "vystupy", sep = "/"))

jpeg("stredni_pravdepodobnost_normalni_delka_zivota.jpg",
    height = 5,
    width = 8,
    units = "in",
    res = 800
    )

par(mar = c(4.1, 4, 1, 1), xpd = FALSE)

plot(
    x = names(likely_life_length),
    y = likely_life_length,
    type = "l",
    col = "blue",
    xaxt = "n",
    ylim = c(0, 100),
    xlab = expression(paste("věk ", italic(x), ", [rok]", sep = "")),
    ylab = "zbývající věk [rok]"
)

lines(
    x = names(c(mean_life_length, NA)),
    y = c(mean_life_length, NA),
    col = "black"
)

lines(
    x = names(normal_life_length),
    y = normal_life_length,
    col = "red"
)

axis(1, at = seq(0, 110, 10), labels = seq(0, 110, 10))

legend(
    x = "topright",
    legend = c(
        expression(paste("střední, ", italic(e[x]^0), sep = "")),
        expression(paste("pravděpodobná, ", italic(tilde(x)), sep = "")),
        expression(paste("normální, ", italic(hat(x)), sep = ""))
        ),
    title = "délka života",
    col = c("black", "blue", "red"),
    lty = 1,
    cex = 0.75
)


dev.off()

setwd(mother_working_directory)


## ----------------------------------------------------------------------------

###############################################################################

## specifické míry úmrtnosti 0-109 let ----------------------------------------

setwd(paste(mother_working_directory, "vystupy", sep = "/"))

jpeg("specificke_miry_umrtnosti_0_az_109_let.jpg",
    height = 5,
    width = 8,
    units = "in",
    res = 800
    )

par(mar = c(4.1, 4.1, 1, 1), xpd = FALSE)

plot(
    x = 0:110,
    y = c(specific_mortality_rate[["2014"]], rep(NA, 11)),
    pch = 1,
    xaxt = "n",
    xlab = expression(paste("věk ", italic(x), ", [rok]", sep = "")),
    ylab = expression(paste("specifická míra úmrtnosti, ",
                            italic(m [list(t, x)]), sep = ""))
    )

lines(
    x = 0:110,
    y = c(weighted_moving_averages[["2014"]][["3"]], rep(NA, 11)),
    col = "green"
)

lines(
    x = 0:110,
    y = c(weighted_moving_averages[["2014"]][["9"]], rep(NA, 11)),
    col = "blue"
)

lines(
    x = 0:110,
    y = c(weighted_moving_averages[["2014"]][["19"]], rep(NA, 11)),
    col = "red"
)

lines(
    x = 0:110,
    y = c(rep(NA, 60), getMyGompertz(60:110, a_new, b_new, c_new)),
    col = "black"
)

axis(1, at = seq(0, 110, 10), labels = seq(0, 110, 10))

legend(
    x = "topleft",
    legend = c(
        expression(paste("specifická míra úmrtnosti, ",
                            italic(m [list(t, x)]), sep = "")),
        expression(paste("3-klouzavý průměr, ",
                            italic(m [list(t, x)] ^ tilde(3)), sep = "")),
        expression(paste("9-klouzavý průměr, ",
                            italic(m [list(t, x)] ^ tilde(9)), sep = "")),
        expression(paste("19-klouzavý průměr, ",
                            italic(m [list(t, x)] ^ tilde(19)), sep = "")),
        expression(paste("Gompertz-Makehamovo vyrovnání, ",
                            italic(m [list(t, x)] ^ tilde(GM)), sep = ""))
        ),
    col = c("black", "green", "blue", "red", "black"),
    lty = c(NA, 1, 1, 1, 1),
    pch = c(1, NA, NA, NA, NA),
    cex = 0.75
)


dev.off()

setwd(mother_working_directory)


## ----------------------------------------------------------------------------

###############################################################################

## specifické míry úmrtnosti 1-34 let -----------------------------------------

setwd(paste(mother_working_directory, "vystupy", sep = "/"))

jpeg("specificke_miry_umrtnosti_1_az_34_let.jpg",
    height = 5,
    width = 8,
    units = "in",
    res = 800
    )

par(mar = c(4.1, 4.1, 1, 1), xpd = FALSE)

plot(
    x = 1:34,
    y = specific_mortality_rate[["2014"]][2:35],
    pch = 1,
    xaxt = "n",
    yaxt = "n",
    xlab = expression(paste("věk ", italic(x), ", [rok]", sep = "")),
    ylab = expression(paste(10000 %*% "specifická míra úmrtnosti, ",
                            italic(m [list(t, x)]), sep = ""))
    )

lines(
    x = 1:34,
    y = weighted_moving_averages[["2014"]][["3"]][2:35],
    col = "green"
)

lines(
    x = 1:34,
    y = weighted_moving_averages[["2014"]][["9"]][2:35],
    col = "blue"
)

lines(
    x = 1:34,
    y = weighted_moving_averages[["2014"]][["19"]][2:35],
    col = "red"
)

axis(1, at = seq(0, 34, 2), labels = seq(0, 34, 2))
axis(2, at = seq(1e-04, 5e-04, 1e-04), labels = seq(1, 5, 1))

legend(
    x = "bottomright",
    legend = c(
        expression(paste("specifická míra úmrtnosti, ",
                            italic(m [list(t, x)]), sep = "")),
        expression(paste("3-klouzavý průměr, ",
                            italic(m [list(t, x)] ^ tilde(3)), sep = "")),
        expression(paste("9-klouzavý průměr, ",
                            italic(m [list(t, x)] ^ tilde(9)), sep = "")),
        expression(paste("19-klouzavý průměr, ",
                            italic(m [list(t, x)] ^ tilde(19)), sep = ""))
        ),
    col = c("black", "green", "blue", "red"),
    lty = c(NA, 1, 1, 1),
    pch = c(1, NA, NA, NA),
    cex = 0.75
)


dev.off()

setwd(mother_working_directory)


## ----------------------------------------------------------------------------

###############################################################################

## specifické míry úmrtnosti 30-64 let ----------------------------------------

setwd(paste(mother_working_directory, "vystupy", sep = "/"))

jpeg("specificke_miry_umrtnosti_30_az_64_let.jpg",
    height = 5,
    width = 8,
    units = "in",
    res = 800
    )

par(mar = c(4.1, 4.1, 1, 1), xpd = FALSE)

plot(
    x = 30:64,
    y = specific_mortality_rate[["2014"]][31:65],
    pch = 1,
    xaxt = "n",
    yaxt = "n",
    xlab = expression(paste("věk ", italic(x), ", [rok]", sep = "")),
    ylab = expression(paste(1000 %*% "specifická míra úmrtnosti, ",
                            italic(m [list(t, x)]), sep = ""))
    )

lines(
    x = 30:64,
    y = weighted_moving_averages[["2014"]][["3"]][31:65],
    col = "green"
)

lines(
    x = 30:64,
    y = weighted_moving_averages[["2014"]][["9"]][31:65],
    col = "blue"
)

lines(
    x = 30:64,
    y = weighted_moving_averages[["2014"]][["19"]][31:65],
    col = "red"
)

lines(
    x = 30:64,
    y = c(rep(NA, 60), getMyGompertz(60:110, a_new, b_new, c_new))[31:65],
    col = "black"
)

axis(1, at = seq(30, 64, 2), labels = seq(30, 64, 2))
axis(2, at = seq(0.002, 0.010, 0.002), labels = seq(2, 10, 2))

legend(
    x = "topleft",
    legend = c(
        expression(paste("specifická míra úmrtnosti, ",
                            italic(m [list(t, x)]), sep = "")),
        expression(paste("3-klouzavý průměr, ",
                            italic(m [list(t, x)] ^ tilde(3)), sep = "")),
        expression(paste("9-klouzavý průměr, ",
                            italic(m [list(t, x)] ^ tilde(9)), sep = "")),
        expression(paste("19-klouzavý průměr, ",
                            italic(m [list(t, x)] ^ tilde(19)), sep = "")),
        expression(paste("Gompertz-Makehamovo vyrovnání, ",
                            italic(m [list(t, x)] ^ tilde(GM)), sep = ""))
        ),
    col = c("black", "green", "blue", "red", "black"),
    lty = c(NA, 1, 1, 1, 1),
    pch = c(1, NA, NA, NA, NA),
    cex = 0.75
)


dev.off()

setwd(mother_working_directory)


## ----------------------------------------------------------------------------

###############################################################################

## specifické míry úmrtnosti 60-99 let ----------------------------------------

setwd(paste(mother_working_directory, "vystupy", sep = "/"))

jpeg("specificke_miry_umrtnosti_60_az_99_let.jpg",
    height = 5,
    width = 8,
    units = "in",
    res = 800
    )

par(mar = c(4.1, 4.1, 1, 1), xpd = FALSE)

plot(
    x = 60:99,
    y = specific_mortality_rate[["2014"]][61:100],
    pch = 1,
    xaxt = "n",
    yaxt = "n",
    xlab = expression(paste("věk ", italic(x), ", [rok]", sep = "")),
    ylab = expression(paste(100 %*% "specifická míra úmrtnosti, ",
                            italic(m [list(t, x)]), sep = ""))
    )

lines(
    x = 60:99,
    y = weighted_moving_averages[["2014"]][["3"]][61:100],
    col = "green"
)

lines(
    x = 60:99,
    y = weighted_moving_averages[["2014"]][["9"]][61:100],
    col = "blue"
)

lines(
    x = 60:99,
    y = weighted_moving_averages[["2014"]][["19"]][61:100],
    col = "red"
)

lines(
    x = 60:99,
    y = c(rep(NA, 60), getMyGompertz(60:110, a_new, b_new, c_new))[61:100],
    col = "black"
)

axis(1, at = seq(60, 98, 2), labels = seq(60, 98, 2))
axis(2, at = seq(0.00, 0.30, 0.05), labels = seq(0, 30, 5))

legend(
    x = "topleft",
    legend = c(
        expression(paste("specifická míra úmrtnosti, ",
                            italic(m [list(t, x)]), sep = "")),
        expression(paste("3-klouzavý průměr, ",
                            italic(m [list(t, x)] ^ tilde(3)), sep = "")),
        expression(paste("9-klouzavý průměr, ",
                            italic(m [list(t, x)] ^ tilde(9)), sep = "")),
        expression(paste("19-klouzavý průměr, ",
                            italic(m [list(t, x)] ^ tilde(19)), sep = "")),
        expression(paste("Gompertz-Makehamovo vyrovnání, ",
                            italic(m [list(t, x)] ^ tilde(GM)), sep = ""))
        ),
    col = c("black", "green", "blue", "red", "black"),
    lty = c(NA, 1, 1, 1, 1),
    pch = c(1, NA, NA, NA, NA),
    cex = 0.75
)


dev.off()

setwd(mother_working_directory)


## ----------------------------------------------------------------------------

###############################################################################

## pravděpodobnost úmrtí pro různé modely -------------------------------------

setwd(paste(mother_working_directory, "vystupy", sep = "/"))

jpeg("pravdepodobnosti_umrti_pro_ruzne_modely.jpg",
    height = 5,
    width = 8,
    units = "in",
    res = 800
    )

par(mar = c(4.1, 4.1, 1, 1), xpd = FALSE)

plot(
    x = rownames(my_q_x)[61:111],
    y = my_q_x[61:111, 1],
    xaxt = "n",
    ylim = c(0, 1),
    xlab = expression(paste("věk ", italic(x), ", [rok]", sep = "")),
    ylab = expression(paste("pravděpodobnost úmrtí, ",
                            italic(q [x]), sep = "")),
    type = "l"
    
)

points(
    x = names(specific_mortality_rate[["2014"]][61:111]),
    y = specific_mortality_rate[["2014"]][61:111],
    pch = 1
)

lines(
    x = rownames(my_q_x)[61:111],
    y = my_q_x[61:111, 2],
    col = "lightgrey"
)

lines(
    x = rownames(my_q_x)[61:111],
    y = my_q_x[61:111, 3],
    col = "orange"
)

lines(
    x = rownames(my_q_x)[61:111],
    y = my_q_x[61:111, 4],
    col = "green"
)

lines(
    x = rownames(my_q_x)[61:111],
    y = my_q_x[61:111, 5],
    col = "red"
)

lines(
    x = rownames(my_q_x)[61:111],
    y = my_q_x[61:111, 6],
    col = "blue"
)

axis(1, at = seq(60, 110, 5), labels = seq(60, 110, 5))

legend(
    x = "topleft",
    legend = c(
        expression(paste("Gompertz-Makehamův model", sep = "")),
        expression(paste("specifická míra úmrtnosti, ",
                            italic(m [list(t, x)]), sep = "")),
        expression(paste("Gompertzův model", sep = "")),
        expression(paste("Kannistö model", sep = "")),
        expression(paste("Thatcherův model", sep = "")),
        expression(paste("Heligmanův-Pollardův model", sep = "")),
        expression(paste("Coale-Kiskerův model", sep = ""))
        ),
    col = c("black", "black", "lightgrey", "orange", "green", "red", "blue"),
    lty = c(1, NA, 1, 1, 1, 1, 1),
    pch = c(NA, 1, NA, NA, NA, NA, NA),
    cex = 0.80
)


dev.off()

setwd(mother_working_directory)


## ----------------------------------------------------------------------------

###############################################################################

## střední délky života pro různé modely --------------------------------------

setwd(paste(mother_working_directory, "vystupy", sep = "/"))

jpeg("stredni_delky_zivota_pro_ruzne_modely.jpg",
    height = 5,
    width = 8,
    units = "in",
    res = 800
    )

par(mar = c(4.1, 4.1, 1, 1), xpd = FALSE)

plot(
    x = rownames(my_life_lengths)[61:110],
    y = my_life_lengths[61:110, 1],
    xaxt = "n",
    xlab = expression(paste("věk ", italic(x), ", [rok]", sep = "")),
    ylab = "zbývající věk [rok]",
    type = "l"
    
)

lines(
    x = rownames(my_life_lengths)[61:110],
    y = my_life_lengths[61:110, 2],
    col = "lightgrey"
)

lines(
    x = rownames(my_life_lengths)[61:110],
    y = my_life_lengths[61:110, 3],
    col = "orange",
    lty = 1
)

lines(
    x = rownames(my_life_lengths)[61:110],
    y = my_life_lengths[61:110, 4],
    col = "green",
    lty = 2
)

lines(
    x = rownames(my_life_lengths)[61:110],
    y = my_life_lengths[61:110, 5],
    col = "red"
)

lines(
    x = rownames(my_life_lengths)[61:110],
    y = my_life_lengths[61:110, 6],
    col = "blue"
)

axis(1, at = seq(60, 110, 5), labels = seq(60, 110, 5))

legend(
    x = "topright",
    legend = c(
        expression(paste("Gompertz-Makehamův model", sep = "")),
        expression(paste("Gompertzův model", sep = "")),
        expression(paste("Kannistö model", sep = "")),
        expression(paste("Thatcherův model", sep = "")),
        expression(paste("Heligmanův-Pollardův model", sep = "")),
        expression(paste("Coale-Kiskerův model", sep = ""))
        ),
    col = c("black", "lightgrey", "orange", "green", "red", "blue"),
    lty = c(1, 1, 1, 2, 1, 1),
    cex = 0.80
)

dev.off()

setwd(mother_working_directory)


## ----------------------------------------------------------------------------

###############################################################################

## střední délka života novorozence a její nárůst -----------------------------

setwd(paste(mother_working_directory, "vystupy", sep = "/"))

jpeg("stredni_delka_zivota_novorozence_a_jeji_narust.jpg",
    height = 5,
    width = 8,
    units = "in",
    res = 800
    )

par(mar = c(4.1, 4.1, 1, 5), xpd = FALSE)

plot(
    x = names(life_tables_panel[1, ]),
    y = life_tables_panel[1, ],
    xaxt = "n",
    yaxt = "n",
    ylim = c(70, 82),
    xlab = expression(paste("kalendářní rok (", italic(t), ")", sep = "")),
    ylab = "zbývající věk [rok]",
    type = "l",
    col = "blue"
)

lines(
    x = names(life_tables_increments[1, ]),
    y = 70 + 12 * (
            life_tables_increments[1, ] - min(life_tables_increments[1, ])
            ) / (max(life_tables_increments[1, ]) -
                 min(life_tables_increments[1, ])),
    col = "red"
)

my_first_tick <- 70 + 12 * (
            0.05 - min(life_tables_increments[1, ])
            ) / (max(life_tables_increments[1, ]) -
                 min(life_tables_increments[1, ]))
                 
my_last_tick <- 70 + 12 * (
            0.35 - min(life_tables_increments[1, ])
            ) / (max(life_tables_increments[1, ]) -
                 min(life_tables_increments[1, ]))

axis(1, at = seq(1975, 2015, 5), labels = seq(1975, 2015, 5))
axis(2, at = seq(70, 82, 2), labels = seq(70, 82, 2),
     col.axis = "blue")
axis(4,
     at = seq(my_first_tick,
              my_last_tick,
              (my_last_tick - my_first_tick) / 6),
     labels = format(seq(0.05, 0.35, 0.05), nsmall = 2),
     col.axis = "red"
)
mtext(side = 4, line = 4,
      text = paste("průměrný meziroční nárůst střední ",
                   "délky života \nnovorozence [rok]",
                   sep = "")
      )

legend(
    x = "bottomright",
    legend = c(
        expression(paste("střední délka života novorozence, ",
                         italic(e[0]^0), sep = "")),        
        expression(paste("její meziroční nárůst, E(",
                         italic(e[list(0, t + 1)]^0 - e[list(0, t)]^0),
                         ")", sep = ""))
        ),
    col = c("blue", "red"),
    lty = 1,
    cex = 0.80
)


dev.off()

setwd(mother_working_directory)


## ----------------------------------------------------------------------------

###############################################################################
###############################################################################
###############################################################################







