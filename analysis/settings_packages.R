# loading r packages

packages <- c( "broom", 
               "dplyr", "data.table",
               "Epi",
               "forcats", "finalfit", "forcats", 
               "ggplot2", "ggpubr", "gtools", "geepack",
               "here", "Hmisc", 
               "knitr", 
               "lubridate", "lme4",
               "markdown", "MASS", 
               "pscl",
               "rmarkdown","readr", 
               "stringr",
               "twopartm", "tidyr", "tibble",
               "VGAM")

lapply(packages, require, character.only = TRUE)
