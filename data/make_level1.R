
#remotes::install_github("makinin/rdbhapi")
library(rdbhapi)
library(tidygeocoder)

#' ===========================================
#' General info for universities
#' ===========================================
univ <- dbh_data(211) |>
  filter(`Gyldig til`==99999, Typenavn=="Universiteter")## institusjoner med adresse/postkode (use for map?)


##' GEOCODING ()
#' Careful: Does not work perfectly and needs manual tweaking! 
univ |>
  mutate(addr=sprintf("%s, %s, Norway", Institusjonsnavn, Postnummer)) |>
  geocode(addr, method="arcgis") -> univ.geo


## from Google maps
univ.geo <- within(univ.geo, {
  lat[Institusjonskode==1176]=59.369668951275465 ## univ sørøst norge
  long[Institusjonskode==1176]=10.441469332544333
  lat[Institusjonskode==1174]=67.29611763149607 ## Nord universitet
  long[Institusjonskode==1174]=14.559786871312857
  lat[Institusjonskode==1175]=59.980787050728765 ## OsloMet
  long[Institusjonskode==1175]=11.045746734327105
  lat[Institusjonskode==1173]=59.68307005150194 ## NMBU
  long[Institusjonskode==1173]=10.762188860571444
  
})

websites <- tibble::tribble(
  ~Institusjonskode, ~website,
  "1110", "https://www.uio.no/",
  "1120", "https://www.uib.no/",
  "1130", "https://uit.no?",
  "1150", "https://www.ntnu.no/",
  "1160", "https://www.uis.no/",
  "1171", "https://www.uia.no/",
  "1173", "https://www.nmbu.no/",
  "1174", "https://www.nord.no",
  "1175", "https://www.oslomet.no/",
  "1176", "https://www.usn.no/"
)
left_join(univ.geo, websites) -> level1
save(level1, file="data/level1.RData")

#' ===========================================
#' Gender ratio/all employees universities
#' ===========================================
dbhd.222 <- dbh_data(222) # alle tilsatte

dbhd.222 |> filter(Nivå==0) |>
  select(Institusjonskode, Årstall, starts_with("Antall")) |>
  filter(Institusjonskode %in% unique(level1$Institusjonskode)) -> level1.employees
save(level1.employees, file="data/level1_employees.RData")

#' ===========================================
#' Gender ratio/all employees different stillingskoder
#' ===========================================
dbhd.225 <- dbh_data(225) # alle tilsatte
dbhd.225 |> filter(Institusjonskode %in% unique(level1$Institusjonskode)) |>
  select(Institusjonskode, Avdelingskode, Årstall, Stillingskode, 
         Benevnelse, starts_with("Antall")) |> 
  group_by(Institusjonskode, Årstall, Stillingskode,Benevnelse) |>
  summarise(`Antall årsverk`=sum(`Antall årsverk`),
            `Antall kvinner`=sum(`Antall kvinner`),
            `Antall menn`=sum(`Antall menn`), .groups="drop") |>
  ungroup() -> level1.employees.positions
save(level1.employees.positions, file="data/level1_employees_positions.RData")

#' ===========================================
#' Students
#' ===========================================

## does not work, see https://github.com/makinin/rdbhapi/issues/9
##' update: the issue was fixed. Use below
# dbhd.123 <- dbh_data(123)
dbhd.123 <- dbh_data(123, group_by=c("Årstall", "Institusjonskode", "Avdelingskode"))

#' Alternativel, I used the website: https://dbh.hkdir.no/dbhapiklient/
#' to select table 123 and use the query saved in dbh_123_query.json
#dbhd.123 <- read_delim("data/dbh_123.csv")

dbhd.123 |> filter(Institusjonskode %in% unique(level1$Institusjonskode)) |>
  select(Institusjonskode, Avdelingskode, Årstall, starts_with("Antall")) |> 
  group_by(Institusjonskode, Årstall, ) |>
  summarise(`Antall totalt`=sum(`Antall totalt`),
            `Antall kvinner`=sum(`Antall kvinner`),
            `Antall menn`=sum(`Antall menn`), .groups="drop") |>
  ungroup() -> level1.students
save(level1.students, file="data/level1_students.RData")

#' ===========================================
#' Employee codes
#' ===========================================
dbh.220 <- dbh_data(220) 

positions.all <- dbh.220 |> group_by(Benevnelse) |>
  summarize() |> filter(Benevnelse!="Stipendiat") |> pull(Benevnelse) # remove Stipendiat and put back in front
positions.all <- c("Student", "Stipendiat", positions.all) 
save(positions.all, file="data/positions.RData")
