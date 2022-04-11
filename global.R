#library(tidyverse)
library(ggplot2)
library(dplyr)
library(purrr)
library(tidyr)
library(stringr)

#devtools::install_github("richarddmorey/divergingPips", subdir = "divergingPips")
library(divergingPips)
library(ggrepel)
library(glue)
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(DT)
library(yaml)
library(leaflet)
library(sf)
library(plotly)

theme_set(theme_bw())

# DEBUG
id.uni=1130 # UiT
id.fac=510 # Helsefak
id.inst="510260" # IPS

enable.level1=T
enable.level2=T
enable.level3=T

# ------------------------------------------------------------------------
# LEVEL 1
# ------------------------------------------------------------------------
get_logo <- function(id, version="quad", link_type="file"){
  if(link_type=="file"){
    return(glue::glue("www/logo/{id}_{version}.png"))
  } else {
    return(glue::glue("logo/{id}_{version}.png"))
  }
}

load("data/level1.RData")
load("data/level1_employees.RData")
load("data/level1_employees_positions.RData")
load("data/level1_students.RData")

level1 |> 
  mutate(popup=glue::glue("
<img src='{get_logo(Institusjonskode,version='wide',link_type='www')}', width=200px>
<h4>{Institusjonsnavn} ({Kortnavn})</h4>
<b>Address: </b>{addr}<br>
<b>Website: </b><a href='{website}'>{website}</a><br>

         ")) -> level1

positions <- c("Student", "Stipendiat", "Postdoktor", "Førsteamanuensis", "Professor")


#' icons on maps are universities own icons...? Looks ugly, if we want it, we need to add some work
#uni.icons <- leaflet::icons(get_logo(level1$Institusjonskode),iconWidth = 32,iconHeight = 32)

# ------------------------------------------------------------------------
# LEVEL 2
# ------------------------------------------------------------------------

load("data/level2.RData")
load("data/level2_employees.RData")
load("data/level2_students.RData")

# ------------------------------------------------------------------------
# LEVEL 3
# ------------------------------------------------------------------------

load("data/level3.RData")
load("data/level3_employees.RData")
load("data/level3_employees_positions.RData")
load("data/level3_students.RData")

# ------------------------------------------------------------------------
# FUNCTIONS
# ------------------------------------------------------------------------

if_null <- function(cond,alt){
  if(is.null(cond))
    return(alt)
  else
    return(cond)
}


