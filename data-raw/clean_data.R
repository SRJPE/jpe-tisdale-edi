library(tidyverse)
library(readxl)


catch <- read_xlsx(here::here("data-raw", "tisdale_catch.xlsx")) |> #updated query has visitTime2
  mutate(totalLength = as.numeric(totalLength)) |>
  arrange(subSiteName, visitTime) |>
  mutate(trap_start_date = ymd_hms(case_when(visitType %in% c("Continue trapping", "Unplanned restart", "End trapping") ~ lag(visitTime2),
                                             T ~ visitTime)),
         trap_end_date = ymd_hms(case_when(visitType %in% c("Continue trapping", "Unplanned restart", "End trapping") ~ visitTime,
                                           T ~ visitTime2))) |>
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

# trap -----
trap <- read_xlsx(here::here("data-raw", "tisdale_trap.xlsx")) |> # TODO note that some higher values were introduces for counterAtEnd
  mutate(discharge = as.numeric(discharge),
         waterTemp = ifelse(waterTemp > 500, NA, waterTemp)) |>  # setting outlier of 551 in waterTemp to NA
  arrange(subSiteName, visitTime) |>
  mutate(trap_start_date = ymd_hms(case_when(visitType %in% c("Continue trapping", "Unplanned restart", "End trapping") ~ lag(visitTime2),
                                             T ~ visitTime)),
         trap_end_date = ymd_hms(case_when(visitType %in% c("Continue trapping", "Unplanned restart", "End trapping") ~ visitTime,
                                           T ~ visitTime2))) |>
  glimpse() #TODO check if temp is on C of F. range is ~1-70
# write clean csv
write_csv(trap, here::here("data", "tisdale_trap.csv"))

# recapture -----

recapture <- read_xlsx(here::here("data-raw", "tisdale_recapture.xlsx")) |>
  arrange(subSiteName, visitTime) |>
  mutate(trap_start_date = ymd_hms(case_when(visitType %in% c("Continue trapping", "Unplanned restart", "End trapping") ~ lag(visitTime2),
                                             T ~ visitTime)),
         trap_end_date = ymd_hms(case_when(visitType %in% c("Continue trapping", "Unplanned restart", "End trapping") ~ visitTime,
                                           T ~ visitTime2))) |>
  glimpse()
# write clean csv
write_csv(recapture, here::here("data", "tisdale_recapture.csv"))

# release fish -----
# There is currently no data on this tableso we will not include at this time
# release_fish <- read_xlsx(here::here("data-raw", "tisdale_releasefish.xlsx")) |> # all forklengths are NA
#   glimpse()
# write clean csv
# write_csv(release_fish, here::here("data", "tisdale_release_fish.csv"))

# release -----
release <- read_xlsx(here::here("data-raw", "tisdale_release.xlsx")) |> # TODO no markedLifeStage recorded, should we delete?
  glimpse()
# write clean csv
write_csv(release, here::here("data", "tisdale_release.csv"))

# read in clean data to check ---------------------------------------------

catch <- read_csv(here::here("data", "tisdale_catch.csv")) |> glimpse()
trap <- read_csv(here::here("data", "tisdale_trap.csv")) |> glimpse()
recapture <- read_csv(here::here("data", "tisdale_recapture.csv")) |> glimpse()
# release_fish <- read_csv(here::here("data", "tisdale_release_fish.csv")) |> glimpse()
release <- read_csv(here::here("data", "tisdale_release.csv")) |> glimpse()
