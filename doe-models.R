library(R6)

Adjustment <- R6Class(
    "Adjustment",
    public = list(
        chance = NULL,
        multiplier = NULL,
        initialize = function(chance, multiplier) {
            self$chance <<- chance;
            self$multiplier <<- multiplier;
        },
        stats = function() {
            if (self$chance == 0) {
                return("")
            }
            if (self$chance == 1) {
                return(paste0("#", self$multiplier));
            }
            return(paste0("#", self$multiplier, "@", self$chance));
        }
    )
)

DamageType <- R6Class(
    "DamageType",
    public = list(
        name = NULL,
        adjustment = NULL,
        initialize = function(name, adjustment) {
            self$name <<- name;
            self$adjustment <<- adjustment;
        },
        title = function() {
            paste0(self$name, self$adjustment$stats());
        }
    )
)

Perk <- R6Class(
    "Perk",
    public = list(
        name = NULL,
        adjustment = NULL,
        initialize = function(name, adjustment) {
            self$name <<- name;
            self$adjustment <<- adjustment;
        },
        title = function() {
            paste0("+", self$name, self$adjustment$stats());
        }
    )
)

ArmEXO <- R6Class(
    "ArmEXO",
    public = list(
        name = NULL,
        adjustment = NULL,
        initialize = function(name, adjustment) {
            self$name <<- name;
            self$adjustment <<- adjustment;
        },
        title = function() {
            paste0(self$name, self$adjustment$stats());
        }
    )
)

Gear <- R6Class(
    "Gear",
    public = list(
        name = NULL,
        damage = NULL,
        initialize = function(name, damage) {
            self$name <<- name;
            self$damage <<- damage;
        },
        title = function() {
            paste0(gsub("legendary", "lego", self$name), "#", self$damage);
        }
    )
)

Weapon <- R6Class(
    "Weapon",
    public = list(
        gear = NULL,
        damage_type = NULL,
        perk1 = NULL,
        perk2 = NULL,
        initialize = function(gear, damage_type, perk1, perk2) {
            self$gear <<- gear;
            self$damage_type <<- damage_type;
            self$perk1 <<- perk1;
            self$perk2 <<- perk2;
        },
        title = function() {
            paste(self$gear$title(),
                  self$damage_type$title(),
                  self$perk1$title(),
                  self$perk2$title());
        }
    )
)

Suit <- R6Class(
    "Suit",
    public = list(
        arm_EXO = NULL,
        initialize = function(arm_EXO) {
            self$arm_EXO <<- arm_EXO;
        },
        title = function() {
            self$arm_EXO$title();
        }
    )
)

Scenario <- R6Class(
    "Scenario",
    public = list(
        weapon = NULL,
        suit = NULL,
        initialize = function(weapon, suit) {
            self$weapon <<- weapon;
            self$suit <<- suit;
        },
        title = function() {
            paste(self$weapon$title(), self$suit$title())
        },
        calculate_hit = function() {
            hit <- Hit$new(
                     damage = self$weapon$gear$damage,
                     adjustment = Adjustment$new(
                                      chance = 0.01,
                                      multiplier = 1.0
                                 )
                     )
            hit <- hit$adjust(self$weapon$damage_type$adjustment)
            hit <- hit$adjust(self$weapon$perk1$adjustment)
            hit <- hit$adjust(self$weapon$perk2$adjustment);
            hit <- hit$adjust(self$suit$arm_EXO$adjustment);

            hit
        }
    )
)
