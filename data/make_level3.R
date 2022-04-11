
#remotes::install_github("makinin/rdbhapi")
library(rdbhapi)
library(tidygeocoder)

#' ===========================================
#' General info for institutes
#' ===========================================
load("data/level1.RData")
load("data/level2.RData")

level3 <- dbh_data(210) |>
  filter(`Gyldig til`==99999,
         Institusjonskode %in% level1$Institusjonskode,
         Nivå==3) |>
  select(-Nivå, -Nivå_tekst, -Institusjonsnavn, 
         -`fagkode avdeling`, -`fagnavn avdeling`, -`Avdelingskode (3 siste siffer)`,
         -`Gyldig fra`, -`Gyldig til`)

save(level3, file="data/level3.RData")


#' ===========================================
#' Gender ratio/all employees universities
#' ===========================================
dbhd.222 <- dbh_data(222) # alle tilsatte

dbhd.222 |> filter(Nivå==3, Institusjonskode %in% level1$Institusjonskode) |>
  group_by(Institusjonskode) |>
  filter(Avdelingskode %in% level3$Avdelingskode[level3$Institusjonskode==first(Institusjonskode)]) |>
  ungroup() |>
  select(Institusjonskode, Avdelingskode, Årstall, starts_with("Antall")) |>
  left_join(level3) -> level3.employees
save(level3.employees, file="data/level3_employees.RData")

#' ===========================================
#' Gender ratio/all employees different stillingskoder
#' ===========================================
dbhd.225 <- dbh_data(225) # alle tilsatte
dbhd.225 |> filter(Institusjonskode %in% unique(level1$Institusjonskode)) |>
  select(Institusjonskode, Avdelingskode, Årstall, Stillingskode, 
         Benevnelse, starts_with("Antall")) |> 
  left_join(level3) |> na.omit() |>
  group_by(Institusjonskode, Fakultetskode,Avdelingskode, Årstall, Stillingskode,Benevnelse) |>
  summarise(`Antall årsverk`=sum(`Antall årsverk`),
            `Antall kvinner`=sum(`Antall kvinner`),
            `Antall menn`=sum(`Antall menn`), .groups="drop") |>
  ungroup() -> level3.employees.positions
save(level3.employees.positions, file="data/level3_employees_positions.RData")

#' ===========================================
#' Students
#' ===========================================

## does not work, see https://github.com/makinin/rdbhapi/issues/9
# dbhd.123 <- dbh_data(123)

#' Alternativel, I used the website: https://dbh.hkdir.no/dbhapiklient/
#' to select table 123 and use the query saved in dbh_123_query.json
 
#dbhd.123 <- read_delim("data/dbh_123.csv")
dbhd.123 <- dbh_data(123, group_by=c("Årstall", "Institusjonskode", "Avdelingskode"))


dbhd.123 |> filter(Institusjonskode %in% unique(level1$Institusjonskode)) |>
  filter(Avdelingskode %in% level3$Avdelingskode) |> 
  select(Institusjonskode, Avdelingskode, Årstall, starts_with("Antall")) |> 
  left_join(level3) |> na.omit() -> level3.students
save(level3.students, file="data/level3_students.RData")

