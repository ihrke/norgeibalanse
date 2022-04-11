
#remotes::install_github("makinin/rdbhapi")
library(tidyverse)
library(rdbhapi)
library(tidygeocoder)


#' Abbreviation for Fakulties at Uni level (i.e., duplicates between unis can exist)
#' 
abbrev_fac <- function(iname){
  str_to_lower(iname) |> 
    str_replace_all("[,-.:()]","") |> 
    str_replace_all("for|og|det","") |> 
    str_split(pattern = " ") |>
    map_chr(\(nn){
      map_chr(nn, \(nam){
          str_sub(nam,1,3) |> str_to_title()
      }) |> paste0(collapse = "")
    }) -> nnames
  tab<-table(nnames) 
  dups <- tab[tab> 1] # duplicates
  for(dup in names(dups)){
    nnames[nnames==dup] <- sprintf("%s%i",dup,1:dups[dup])
  }
  #any(duplicated(nnames))
  nnames
}

#' ===========================================
#' General info for universities
#' ===========================================
load("data/level1.RData")
level2 <- dbh_data(210) |>
  filter(`Gyldig til`==99999,
         Institusjonskode %in% level1$Institusjonskode,
         Nivå==2) |>
  select(-Nivå, -Nivå_tekst, -Institusjonsnavn, 
         -`fagkode avdeling`, -`fagnavn avdeling`, -`Avdelingskode (3 siste siffer)`,
         -`Gyldig fra`, -`Gyldig til`) |>
  group_by(Institusjonskode) |>
  mutate(Kortnavn=abbrev_fac(Fakultetsnavn)) |>
  ungroup()
 
save(level2, file="data/level2.RData")


#' ===========================================
#' Gender ratio/all employees universities
#' ===========================================
dbhd.222 <- dbh_data(222) # alle tilsatte

dbhd.222 |> filter(Nivå==2, Institusjonskode %in% level1$Institusjonskode) |>
  group_by(Institusjonskode) |>
  filter(Avdelingskode %in% level2$Avdelingskode[level2$Institusjonskode==first(Institusjonskode)]) |>
  ungroup() |>
  select(Institusjonskode, Avdelingskode, Årstall, starts_with("Antall")) -> level2.employees
save(level2.employees, file="data/level2_employees.RData")

#' ===========================================
#' Gender ratio/all employees different stillingskoder
#' ===========================================
#dbhd.225 <- dbh_data(225) # alle tilsatte
#dbhd.225 |> filter(Institusjonskode %in% unique(level1$Institusjonskode)) |>
#  select(Institusjonskode, Avdelingskode, Årstall, Stillingskode, 
#         Benevnelse, starts_with("Antall")) |> 
#  group_by(Institusjonskode, Årstall, Stillingskode,Benevnelse) |>
#  summarise(`Antall årsverk`=sum(`Antall årsverk`),
#            `Antall kvinner`=sum(`Antall kvinner`),
#            `Antall menn`=sum(`Antall menn`), .groups="drop") |>
#  ungroup() -> level1.employees.positions
#save(level1.employees.positions, file="data/level1_employees_positions.RData")

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
  filter(Avdelingskode %in% level2$Avdelingskode) |> 
  select(Institusjonskode, Avdelingskode, Årstall, starts_with("Antall")) |> 
  group_by(Institusjonskode, Avdelingskode, Årstall ) |>
  summarise(`Antall totalt`=sum(`Antall totalt`),
            `Antall kvinner`=sum(`Antall kvinner`),
            `Antall menn`=sum(`Antall menn`), .groups="drop") |>
  ungroup() -> level2.students
save(level2.students, file="data/level2_students.RData")

