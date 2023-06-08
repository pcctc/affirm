## code to prepare `DATASET` dataset goes here

RAND <-
  tibble::tribble(
    ~SUBJECT, ~RAND_GROUP, ~RAND_STRATA,
    1,        "Drug A",    "<65yr",
    2,        "Drug B",    ">=65yr",
    3,        "Drug A",    "<65yr",
    4,        "Drug B",    ">=65yr",
    5,        NA,          ">=65yr"
  )

DM <-
  tibble::tribble(
    ~SUBJECT, ~AGE, ~RACE,
    1,        40,   "Asian",
    2,        70,   "Black or African American",
    3,        50,   "Native American", # should be"American Indian or Alaska Native"
    4,        60,   "Native Hawaiian or Other Pacific Islander"
  )


usethis::use_data(RAND, DM, overwrite = TRUE)
