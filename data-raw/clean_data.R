library(tidyverse)
library(readxl)

catch <- read_xlsx(here::here("data-raw", "tisdale_catch.xlsx")) |> #updated query has visitTime2
  mutate(totalLength = as.numeric(totalLength)) |>
  glimpse()

# atCaptureRun vs finalRun
catch |>
  mutate(run_compare = atCaptureRun == finalRun) |>
  summarise(sum(run_compare, na.rm = T)/length(run_compare)) # they are equal 41% of the time

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

write_csv(catch, here::here("data", "tisdale_catch.csv"))

# TODO do we want counterAtStart?
# TODO discharge is all NAs
# TODO outlier of 551 in waterTemp

#TODO check counterAtEnd values - max is 3195559 - did not update this value on metadata
trap <- read_xlsx(here::here("data-raw", "tisdale_trap.xlsx")) |>
  mutate(discharge = as.numeric(discharge)) |>
  glimpse()
write_csv(trap, here::here("data", "tisdale_trap.csv"))

recapture <- read_xlsx(here::here("data-raw", "tisdale_recapture.xlsx")) |> # note that some higher values were introduces for counterAtEnd
  glimpse()

release_fish <- read_xlsx(here::here("data-raw", "tisdale_releasefish.xlsx")) |> # all forklengths are NA
  glimpse()


# read in clean data to check ---------------------------------------------

catch <- read_csv(here::here("data", "tisdale_catch.csv")) |> glimpse()
trap <- read_csv(here::here("data", "tisdale_trap.csv")) |> glimpse()

