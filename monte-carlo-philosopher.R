source("./doe-models.R")

Philosopher <- R6Class(
    "Philosopher",
    public = list(
        create_scenario = function(gear, damage_type, damage_perk1, damage_perk2, exo) {
            scenario <- Scenario$new(
                weapon = Weapon$new(
                    gear = Gear$new(
                        name = gear$name,
                        damage = gear$damage
                    ),
                    damage_type = DamageType$new(
                        name = damage_type$name,
                        adjustment = Adjustment$new(
                            chance = damage_type$chance,
                            multiplier = damage_type$multiplier
                        )
                    ),
                    perk1 = Perk$new(
                        name = damage_perk1$name,
                        adjustment = Adjustment$new(
                            chance = damage_perk1$chance,
                            multiplier = damage_perk1$multiplier
                        )
                    ),
                    perk2 = Perk$new(
                        name = damage_perk2$name,
                        adjustment = Adjustment$new(
                            chance = damage_perk2$chance,
                            multiplier = damage_perk2$multiplier
                        )
                    )
                ),
                suit = Suit$new(
                    arm_EXO = ArmEXO$new(
                        name = exo$name,
                        adjustment = Adjustment$new(
                            chance = exo$chance,
                            multiplier = exo$multiplier
                        )
                    )
                )
            )

            scenario;
        },

        calculate_scenarios = function() {
            perks1 <- doe_gsheet$perks();
            perks2 <- doe_gsheet$perks();
            all_damage_types <- doe_gsheet$damage_types();
            exos <- doe_gsheet$arm_exos();
            exos <- add_row(exos, name = "no exo", description = "no exo", placement = "Arms", chance = 1, multiplier = 1);
            gears <- doe_gsheet$gears();

            #damage_perks1 <- damage_perks1[1,];
            #damage_perks2 <- damage_perks2[1,];
            #gears <- gears[1,];
            #exos <- exos[1,];
            num_scenarios = nrow(gears) * nrow(all_damage_types) *
                nrow(perks1) * nrow(perks2) * nrow(exos);
            message("creating space for ", num_scenarios, " scenarios");

            all_scenarios <- vector("list", num_scenarios);
            i <- 1;
            for (gear_i in 1:nrow(gears)) {
                gear <- gears[gear_i,]
                for (damage_type_i in 1:nrow(all_damage_types)) {
                    damage_type <- all_damage_types[damage_type_i,]
                    for (perk1_i in 1:nrow(perks1)) {
                        perk1 <- perks1[perk1_i,];
                        for (perk2_i in 1:nrow(perks2)) {
                            perk2 <- perks2[perk2_i,];
                            for (exo_i in 1:nrow(exos)) {
                                exo <- exos[exo_i,];
                                scenario <- create_scenario(gear, damage_type, perk1, perk2, exo);
                                all_scenarios[[i]] <- scenario;
                                i <- i + 1;
                            }
                        }
                    }
                }
            }
            all_scenarios;
        },

        calculate_scenario = function(gear, damage_type, perk1, perk2, exo) {
            perks1 <- doe_gsheet$perks();
            perks1 <- add_row(perks1, name = "no perk", description = "no perk", chance = 1, multiplier = 1);
            perks2 <- doe_gsheet$perks();
            perks2 <- add_row(perks2, name = "no perk", description = "no perk", chance = 1, multiplier = 1);
            damage_types <- doe_gsheet$damage_types();
            exos <- doe_gsheet$arm_exos();
            exos <- add_row(exos, name = "no exo", description = "no exo", placement = "Arms", chance = 1, multiplier = 1);
            gears <- doe_gsheet$gears();

            gears <- subset(gears, name %in% gear)
            damage_types <- subset(damage_types, name %in% damage_type)
            perks1 <- subset(perks1, name %in% perk1);
            perks2 <- subset(perks2, name %in% perk2);
            exos <- subset(exos, name %in% exo);

            all_scenarios <- vector("list", 1);
            i <- 1;
            for (gear_i in 1:nrow(gears)) {
                gear <- gears[gear_i,]
                for (damage_type_i in 1:nrow(damage_types)) {
                    damage_type <- damage_types[damage_type_i,]
                    for (perk1_i in 1:nrow(perks1)) {
                        perk1 <- perks1[perk1_i,];
                        for (perk2_i in 1:nrow(perks2)) {
                            perk2 <- perks2[perk2_i,];
                            if (perk1$name == perk2$name && perk1$name != "no perk") {
                                next;
                            }
                            for (exo_i in 1:nrow(exos)) {
                                exo <- exos[exo_i,];
                                scenario <- create_scenario(gear, damage_type, perk1, perk2, exo);
                                all_scenarios[[i]] <- scenario;
                                i <- i + 1;
                            }
                        }
                    }
                }
            }
            all_scenarios;
        }
    )
)


