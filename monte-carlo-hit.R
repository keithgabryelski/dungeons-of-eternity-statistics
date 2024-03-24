source("./doe-models.R")
library(tidyverse)

HitResult <- R6Class(
    "HitResult",
    public = list(
        rnum1 = NULL,
        damage = NULL,
        rnum2 = NULL,
        chance = NULL,
        chanced = NULL,
        rolled_damage = NULL,
        multiplier = NULL,
        adjusted_damage = NULL,
        initialize = function(rnum1, rnum2, damage, chance, chanced,
                              rolled_damage, adjusted_damage, multiplier) {
            self$rnum1 = rnum1;
            self$rnum2 = rnum2;
            self$damage = damage;
            self$chance = chance;
            self$chanced = chanced;
            self$rolled_damage = rolled_damage;
            self$adjusted_damage = adjusted_damage;
            self$multiplier = multiplier;
        },
        attack = function() {
            self$adjusted_damage;
        },
        debugout = function() {
            paste("rnum1:", self$rnum1, "damage:", self$damage, "rnum2:", self$rnum2, "chance:", self$chance,
                  "chanced:", self$chanced, "rolled_damage:", self$rolled_damage,
                  "multiplier:", self$multiplier,
                  "adjusted_damage:", self$adjusted_damage);
        }
    )
);

Hit <- R6Class(
    "Hit",
    public = list(
        damage = NULL,
        adjustment = NULL,
        initialize = function(damage, adjustment) {
            self$damage <<- damage;
            self$adjustment <<- adjustment;
        },
        adjust = function(by) {
            chance <- self$adjustment$chance;
            multiplier <- self$adjustment$multiplier;
            damage <- self$damage;
            if (by$chance == 1) {
                damage <- damage + (by$multiplier - 1);
            } else {
                chance <- chance + by$chance;
                multiplier <- multiplier + by$multiplier
            }

            Hit$new(
                damage = damage,
                adjustment = Adjustment$new(
                    chance = chance,
                    multiplier = multiplier
                )
            );
        },
        calculate_hit_result = function() {
            rnum1 <- runif(1);
            rolled_damage <- self$damage * rnum1;
            rnum2 <- runif(1);
            chanced <- (rnum2 < self$adjustment$chance);
            if (chanced) {
                adjusted_damage <- rolled_damage * self$adjustment$multiplier;
            } else {
                adjusted_damage <- rolled_damage;
            }
            HitResult$new(
                 rnum1 = rnum1,
                 damage = self$damage,
                 rnum2 = rnum2,
                 chance = self$adjustment$chance,
                 chanced = chanced,
                 rolled_damage = rolled_damage,
                 multiplier = self$adjustment$multiplier,
                 adjusted_damage = adjusted_damage
            );
        }
    )
)
