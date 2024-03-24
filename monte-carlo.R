source("./doe-gsheet.R")
source("./doe-models.R")
source("./monte-carlo-hit.R")
source("./monte-carlo-philosopher.R")
library(tidyverse)
library(wesanderson)

philosopher <- Philosopher$new();


calculated_scenarios <- philosopher$calculate_scenario(
    gear = "legendary hammers",
    damage_type = c("physical", "fire"),
    perk1 = c("attack power", "criticals", "undead damage"),
    perk2 = c("attack power", "criticals", "undead damage"),
    exo = c("Critical", "Might"));
message("calculated ", length(calculated_scenarios), " scenarios");

calculated_hits <- vector("list", length(calculated_scenarios));
out <- vector("numeric", length(calculated_scenarios));

NUM_ATTACKS_PER_DUNGEON <- 50;
COLORS = rainbow(length(calculated_scenarios)*1.25);

calc = function(hit) {
    out = NULL;
    for (n in 1:NUM_ATTACKS_PER_DUNGEON) {
        result <- hit$calculate_hit_result();
        out[n] <- result$attack();
    }
    out;
}


par(oma = c(0, 0, 0, 0), mar = c(5, 5, 5, 35), cex=0.5)
options(device = "quartz")
legend_titles <- c();
for (i in 1:length(calculated_scenarios)) {
    scenario = calculated_scenarios[[i]];
    title = scenario$title();
    legend_titles = c(legend_titles, title);
    message("scenario: ", title);
    hit <- scenario$calculate_hit();
    out <- replicate(n = 100, expr = calc(hit));
    N_mean <- apply(out, 1, mean);
    if (i == 1) {
        plot(N_mean,
             type="l",
             ylim = c(0, 500),
             col = COLORS[i],
             main = "Monte Carlo Simulation",
             sub = "Lovingly wrought at http://bit.ly/dungeons-of-eternity-statistics by Protagosus",
             xlab = "MC Iteration",
             ylab = "Actual Damage"
        );
    } else {
        lines(N_mean, lty = 1, col = COLORS[i]);
    }
##    N_quants <- apply(out, 1, function(x) quantile(x, c(0.1, 0.9)))
    #lines(N_quants[1,], lty = i, col = COLORS[i])
##    lines(N_quants[2,], lty = i, col = COLORS[i])
#    N_median <- apply(out, 1, median);
#    lines(N_median, lty = 2, col = COLORS[i])
}

#N_mean <- apply(out, 1, mean);
#N_quants <- apply(out, 1, function(x) quantile(x, c(0.1, 0.9)))

#lines(N_quants[1,], lty = 2, col = "grey")
#lines(N_quants[2,], lty = 2, col = "red")

#N_median <- apply(out, 1, median);
#lines(N_median, lty = 2, col = "green")

legend("topleft",
       inset = c(1.0, 0),
       xpd = TRUE,
       legend = legend_titles,
       fill = COLORS
)

#title();

## legend(100, 290,
##   legend=c("crit 30% chance for 300% damage",
##    "explode 30% chance for 200% area damage",
##    "damage +30%",
##    "base damage"),
##   fill=c("red", "orange", "blue", "black"))
##   title(
##    main="Monte Carlo Sim: 75 damage by Perks and EXO at level 50",
##     sub = "Lovingly wrought at http://bit.ly/dungeons-of-eternity-statistics by Protagosus",
##      xlab = "MC Iteration",
##       "Actual Damage"
##        )
