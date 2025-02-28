library(knitr)
library(lubridate)
library(readr)
library(zip)
library(dplyr)
library(stringr)

fahrenheit_to_celsius <- function(F) {
  return((5/9) * (F - 32))
}

clean_catch <- function(clean_data_path){
  readr::read_csv(clean_data_path) |> #updated query has visitTime2
    mutate(totalLength = as.numeric(totalLength)) |>
    arrange(subSiteName, visitTime) |>
    mutate(trap_start_date = ymd_hms(case_when(visitType %in% c("Continue trapping", "Unplanned restart", "End trapping") ~ lag(visitTime2),
                                               T ~ visitTime)),
           trap_end_date = ymd_hms(case_when(visitType %in% c("Continue trapping", "Unplanned restart", "End trapping") ~ visitTime,
                                             T ~ visitTime2))) |>
    filter(siteName != "Lower Feather River RST")
}

clean_trap <- function(clean_data_path){
  readr::read_csv(clean_data_path) |>
    mutate(discharge = as.numeric(discharge),
           waterTemp = ifelse(waterTemp > 500, NA, waterTemp),
           counterAtStart = ifelse(counterAtStart == 33303251, NA, counterAtStart)) |>  # setting outlier of 551 in waterTemp to NA
    arrange(subSiteName, visitTime) |>
    mutate(trap_start_date = ymd_hms(case_when(visitType %in% c("Continue trapping", "Unplanned restart", "End trapping") ~ lag(visitTime2),
                                               T ~ visitTime)),
           trap_end_date = ymd_hms(case_when(visitType %in% c("Continue trapping", "Unplanned restart", "End trapping") ~ visitTime,
                                             T ~ visitTime2)),
           waterTemp = ifelse(waterTemp > 25, fahrenheit_to_celsius(waterTemp), waterTemp)) |> # doing the conversion "manually"
    # waterTemp = ifelse(waterTempUnitID == 19, fahrenheit.to.celsius(waterTemp), waterTemp)) |> TODO check on the criteria for the waterTempUnit
    select(-waterTempUnitID) |>
    filter(siteName != "Lower Feather River RST")
}

clean_recapture <- function(clean_data_path){
  readr::read_csv(clean_data_path) |>
    arrange(subSiteName, visitTime) |>
    mutate(trap_start_date = ymd_hms(case_when(visitType %in% c("Continue trapping", "Unplanned restart", "End trapping") ~ lag(visitTime2),
                                               T ~ visitTime)),
           trap_end_date = ymd_hms(case_when(visitType %in% c("Continue trapping", "Unplanned restart", "End trapping") ~ visitTime,
                                             T ~ visitTime2)),
           markPosition = case_when(markPosition == "Pelvic fin, right" ~ "Pelvic fin right",
                                    markPosition == "Pelvic fin, left" ~ "Pelvic fin left",
                                    T ~ markPosition)) |>
    filter(siteName != "Lower Feather River RST")
}

clean_release <- function(clean_data_path){
  readr::read_csv(clean_data_path) |>
    mutate(appliedMarkPosition = case_when(appliedMarkPosition == "Pelvic fin, right" ~ "Pelvic fin right",
                                           appliedMarkPosition == "Pelvic fin, left" ~ "Pelvic fin left",
                                          T ~ appliedMarkPosition))
}

clean_data <- function(clean_data_path) {
  # Temporary directories for extraction and writing
  folder_path <- "data/tisdale.zip"
  temp_dir <- tempdir()
  temp_dir <- normalizePath(temp_dir, winslash = "/")
  original_wd <- getwd()

  # Unzip new_path
  unzip(folder_path, exdir = temp_dir)
  new_file <- file.path(temp_dir, basename(clean_data_path))
  # Load and filter new data
  clean_data <- if (grepl("tisdale_catch.csv", clean_data_path)) {
    clean_catch(new_file)
  } else if(grepl("tisdale_recapture.csv", clean_data_path)) {
    clean_recapture(new_file)
  }else if(grepl("tisdale_release.csv", clean_data_path)){
    clean_release(new_file)
  }else{
    clean_trap(new_file)
  }
  # Combine data
  # Write updated data back to the temporary directory
  write_csv(clean_data, new_file)
  setwd(temp_dir)
  files_to_zip <- list.files(pattern = "^tisdale", recursive = TRUE)

  zip(
    zipfile = file.path(original_wd, folder_path),
    files =  files_to_zip
  )
  setwd(original_wd)
}

clean_current_year <- function(clean_data_path){
  clean_current_year_data <- if (grepl("current_year_tisdale_catch.csv", clean_data_path)) {
    clean_catch(clean_data_path)
  } else if(grepl("current_year_tisdale_recapture.csv", clean_data_path)) {
    clean_recapture(clean_data_path)
  }else if(grepl("current_year_tisdale_release.csv", clean_data_path)){
    clean_release(clean_data_path)
  }else{
    clean_trap(clean_data_path)
  }
  # Combine data
  # Write updated data back to the temporary directory
  write_csv(clean_currenty_year, new_file)

}
# Paths for historical and new data
path <- sort(c("tisdale_catch.csv",
               "tisdale_recapture.csv",
               "tisdale_release.csv",
               "tisdale_trap.csv"))
# raw_data_path <- paste0("data/", path)
clean_data_path <- paste0("data/tisdale.zip/", path)

# Apply the function to all file pairs
mapply(clean_data, clean_data_path)

current_year_path <- sort(c("data/current_year_tisdale_catch.csv",
                            "data/current_year_tisdale_release.csv",
                            "data/current_year_tisdale_recapture.csv",
                            "data/current_year_tisdale_trap.csv"))
mapply(clean_current_year_data, current_year_path)
