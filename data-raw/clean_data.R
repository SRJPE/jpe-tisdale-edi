library(tidyverse)
library(readxl)

catch <- read_xlsx(here::here("data-raw", "Tisdale_CatchRaw.xlsx")) |>
  mutate(totalLength = as.numeric(totalLength)) |>
  glimpse()

# atCaptureRun vs finalRun
catch |>
  mutate(run_compare = atCaptureRun == finalRun) |>
  summarise(sum(run_compare, na.rm = T)/length(run_compare)) # they are equal 47% of the time

# look into scenarios where they differ
catch |>
  filter(atCaptureRun != finalRun) |>
  select(atCaptureRun, finalRun) |>
  group_by(atCaptureRun) |>
  tally()

catch |>
  filter(atCaptureRun != finalRun) |>
  select(atCaptureRun, finalRun) |>
  group_by(finalRun) |>
  tally()
# finalRun has more not recorded, more fall run, fewer spring run, far fewer
# winter run
catch |>
  filter(atCaptureRun != finalRun) |>
  select(atCaptureRun, finalRun) |> View() # this shows you what they are changed to

write_csv(catch, here::here("data", "catch.csv"))

# TODO do we want counterAtStart?
# TODO discharge is all NAs
# TODO outlier of 551 in waterTemp
trap <- read_xlsx(here::here("data-raw", "Tisdale_TrapRaw.xlsx")) |>
  mutate(discharge = as.numeric(discharge)) |>
  glimpse()
write_csv(trap, here::here("data", "trap.csv"))


# read in clean data to check ---------------------------------------------

catch <- read_csv(here::here("data", "catch.csv")) |> glimpse()
trap <- read_csv(here::here("data", "trap.csv")) |> glimpse()


# function for checking metadata ------------------------------------------
get_class <- function(data, name) {
  data |> select(all_of(name)) |>
    pull() |> class()
}
report_metadata <- function(data) {
  names <- names(data)
  for(i in names) {
    print(i)
    if(get_class(data, i)[1] == "character") {
      values <- data |> select(all_of(i)) |> pull()
      dput(unique(values))
    }
    else if(get_class(data, i)[1] == "numeric") {
      print(data |> select(all_of(i)) |> pull() |> range(na.rm = T))
    }
    else if(get_class(data, i)[1] == "POSIXct") {
      print(data |> select(all_of(i)) |> pull() |> range(na.rm = T))
    }
    else if (get_class(data, i)[1] == "logical") {
      print(data |> select(all_of(i)) |> pull() |> unique())
    }
    else{
      print("unknown data type")
    }
  }
}
