library(dplyr)
library(collapse)
library(AppliedPredictiveModeling)
library(caret)
library(googlesheets4)
rawreport <- read_sheet("https://docs.google.com/spreadsheets/d/15XGevBozTrsKYo2EGDgq5onnf2fHgTcJfXuA5vTeSnY/edit#gid=1163589623", "All Reports")
rawreport <- replace_NA(rawreport, cols = c("attack power",
    "criticals",
    "undead damage",
    "monster damage",
    "critter damage",
    "sorcerer damage",
    "elemental damage",
    "elite damage",
    "throw distance",
    "throw damage",
    "explosions",
    "slowing",
    "reload",
    "shot distance",
    "vampire",
    "poison",
    "area damage",
    "knockback distance",
    "absorb",
    "throwable",
    "stab damage",
    "Damage"
    ))
rawreport <- replace_NA(rawreport, value = "physical", cols = c("Damage Type"))
rawreport$'Damage Type' <- as.factor(rawreport$'Damage Type')
rawreport <- within(rawreport, {
    Category           <- factor(Category)
    Group              <- factor(Group)
    Name               <- factor(Name)
    Icon               <- factor(Icon)
    Rarity             <- factor(Rarity)
    Cost               <- as.double(Cost)
    Damage             <- as.double(Damage)
})
rawreport$'attack power' <- as.logical(rawreport$'attack power')
rawreport$'criticals' <- as.logical(rawreport$'criticals')
rawreport$'undead damage' <- as.logical(rawreport$'undead damage')
rawreport$'monster damage' <- as.logical(rawreport$'monster damage')
rawreport$'critter damage' <- as.logical(rawreport$'critter damage')
rawreport$'sorcerer damage' <- as.logical(rawreport$'sorcerer damage')
rawreport$'elemental damage' <- as.logical(rawreport$'elemental damage')
rawreport$'elite damage' <- as.logical(rawreport$'elite damage')
rawreport$'throw distance' <- as.logical(rawreport$'throw distance')
rawreport$'throw damage' <- as.logical(rawreport$'throw damage')
rawreport$'explosions' <- as.logical(rawreport$'explosions')
rawreport$'slowing' <- as.logical(rawreport$'slowing')
rawreport$'reload' <- as.logical(rawreport$'reload')
rawreport$'shot distance' <- as.logical(rawreport$'shot distance')
rawreport$'vampire' <- as.logical(rawreport$'vampire')
rawreport$'poison' <- as.logical(rawreport$'poison')
rawreport$'area damage' <- as.logical(rawreport$'area damage')
rawreport$'knockback distance' <- as.logical(rawreport$'knockback distance')
rawreport$'absorb' <- as.logical(rawreport$'absorb')
rawreport$'throwable' <- as.logical(rawreport$'throwable')
rawreport$'stab damage' <- as.logical(rawreport$'stab damage')
report = na.omit(subset(rawreport, select = -c(Perk1, Perk2, perks)))
reportX <- c("Damage", "Cost")
reportXMatrix <- as.matrix(report[,reportX])
