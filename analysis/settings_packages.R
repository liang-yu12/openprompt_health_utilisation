# loading r packages

packages <- c("data.table", "lubridate", "markdown", "forcats", "Hmisc",
              "broom", "dplyr", "finalfit", "forcats", "ggplot2", "ggpubr",
              "MASS", "stringr", "tidyr", "here", "gtools")

lapply(packages, require, character.only = TRUE)