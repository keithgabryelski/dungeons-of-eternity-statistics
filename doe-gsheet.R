library(methods)
library(dplyr)
library(collapse)
library(AppliedPredictiveModeling)
library(caret)
library(googlesheets4)
library(R6)

DOE_GSHEET_URL = "https://docs.google.com/spreadsheets/d/15XGevBozTrsKYo2EGDgq5onnf2fHgTcJfXuA5vTeSnY/edit#gid=1163589623";

StatisticsGoogleSheets <- R6Class(
    "StatisticsGoogleSheets",
    lock_objects = FALSE,
    public = list(
        url = NULL,
        fetched_reports = NULL,
        fetched_perks = NULL,
        fetched_exos = NULL,
        fetched_gears = NULL,
        fetched_damage_types = NULL,
        fetch_reports = function() { read_sheet(self$url, "All Reports") },
        fetch_perks = function() { read_sheet(self$url, "Perks") },
        fetch_exos = function() { read_sheet(self$url, "EXO") },
        fetch_gears = function() { read_sheet(self$url, "All Gear Damage") },
        fetch_damage_types = function() { read_sheet(self$url, "Damage Types") },
        initialize = function(url,
                              fetched_reports = NULL,
                              fetched_perks = NULL,
                              fetched_exos = NULL,
                              fetched_gears = NULL,
                              fetched_damage_types = NULL) {
            self$url = url;
            self$reset();
            self$fetched_reports = fetched_reports;
            self$fetched_perks = fetched_perks;
            self$fetched_exos = fetched_exos;
            self$fetched_gears = fetched_gears;
            self$fetched_damage_types = fetched_damage_types;
        },
        reset = function() {
            self$fetched_reports = NULL;
            self$fetched_perks = NULL;
            self$fetched_exos = NULL;
            self$fetched_gears = NULL;
            self$fetched_damage_types = NULL;
        },
        perks = function() {
            if (is.null(self$fetched_perks)) {
                self$fetched_perks <- self$fetch_perks();
                self$fetched_perks$chance <- self$fetched_perks$`max chance`;
                self$fetched_perks$multiplier <- self$fetched_perks$`max multiplier`;
            }
            subset(self$fetched_perks, !is.na(`max chance`), select = c("name", "description", "chance", "multiplier"));
        },
        arm_exos = function() {
            if (is.null(self$fetched_exos)) {
                self$fetched_exos <- self$fetch_exos();
                self$fetched_exos$chance <- self$fetched_exos$`max chance`;
                self$fetched_exos$multiplier <- self$fetched_exos$`max multiplier`;
            }
            subset(
                self$fetched_exos,
                !is.na(chance) & placement == "Arms",
                select = c("name", "description", "placement", "level", "chance", "multiplier")
            );
        },
        gears = function() {
            if (is.null(self$fetched_gears)) {
                self$fetched_gears <- self$fetch_gears();
                self$fetched_gears$damage <- self$fetched_gears$max;
            }
            subset(
                self$fetched_gears,
                !is.na(name),
                select = c("name", "rarity", "category", "damage"));
        },
        damage_types = function() {
            if (is.null(self$fetched_damage_types)) {
                self$fetched_damage_types <- self$fetch_damage_types();
            }
            subset(
                self$fetched_damage_types,
                !is.na(name),
                select = c("name", "description", "chance", "multiplier"));
        }
    )
);

if (exists("doe_gsheet")) {
    # re-up
    doe_gsheet = StatisticsGoogleSheets$new(
         url = DOE_GSHEET_URL,
         fetched_reports = doe_gsheet$fetched_reports,
         fetched_perks = doe_gsheet$fetched_perks,
         fetched_exos = doe_gsheet$fetched_exos,
         fetched_gears = doe_gsheet$fetched_gears,
         fetched_damage_types = doe_gsheet$fetched_damage_types
    );
} else {
    doe_gsheet = StatisticsGoogleSheets$new(url = DOE_GSHEET_URL);
}
