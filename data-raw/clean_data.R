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
trap <- read_xlsx(here::here("data-raw", "Tisdale_TrapRaw.xlsx")) |>
  mutate(discharge = as.numeric(discharge)) |>
  glimpse()
write_csv(trap, here::here("data", "trap.csv"))

