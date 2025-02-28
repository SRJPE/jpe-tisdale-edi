library(knitr)
library(lubridate)
library(readr)
library(zip)
library(dplyr)
library(stringr)

fahrenheit_to_celsius <- function(F) {
  return((5/9) * (F - 32))
}

clean_catch <- function(raw_data_path){
  readr::read_csv(raw_data_path) |> #updated query has visitTime2
    mutate(totalLength = as.numeric(totalLength)) |>
    arrange(subSiteName, visitTime) |>
    mutate(trap_start_date = ymd_hms(case_when(visitType %in% c("Continue trapping", "Unplanned restart", "End trapping") ~ lag(visitTime2),
                                               T ~ visitTime)),
           trap_end_date = ymd_hms(case_when(visitType %in% c("Continue trapping", "Unplanned restart", "End trapping") ~ visitTime,
                                             T ~ visitTime2))) |>
    filter(siteName != "Lower Feather River RST")
}

clean_trap <- function(raw_data_path){
  readr::read_csv(raw_data_path) |>
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

clean_recapture <- function(raw_data_path){
  readr::read_csv(raw_data_path) |>
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

clean_release <- function(raw_data_path){
  readr::read_csv(raw_data_path) |>
    mutate(appliedMarkPosition = case_when(appliedMarkPosition == "Pelvic fin, right" ~ "Pelvic fin right",
                                           appliedMarkPosition == "Pelvic fin, left" ~ "Pelvic fin left",
                                          T ~ appliedMarkPosition))
}

clean_data <- function(raw_data_path, clean_data_path) {
  # Temporary directories for extraction and writing
  folder_path <- "data/tisdale_test.zip"
  temp_dir <- tempdir()
  temp_dir <- normalizePath(temp_dir, winslash = "/")
  original_wd <- getwd()

  # Unzip new_path
  unzip(folder_path, exdir = temp_dir)
  print(temp_dir)
  new_file <- file.path(temp_dir, basename(raw_data_path))

  # Load historic data
  # historic_data <- readr::read_csv(historic_path)
  # Load and filter new data
  clean_data <- if (grepl("tisdale_catch.csv", raw_data_path)) {
    clean_catch(raw_data_path)
  } else if(grepl("tisdale_recapture.csv", raw_data_path)) {
    clean_recapture(raw_data_path)
  }else if(grepl("tisdale_release.csv", raw_data_path)){
    clean_release(raw_data_path)
  }else{
    clean_trap(raw_data_path)
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

# Paths for historical and new data
path <- sort(c("tisdale_catch.csv",
               "tisdale_recapture.csv",
               "tisdale_release.csv",
               "tisdale_trap.csv"))
raw_data_path <- paste0("data/", path)
clean_data_path <- paste0("data/tisdale_test.zip/", path)

# Apply the function to all file pairs
mapply(clean_data, raw_data_path, clean_data_path)
