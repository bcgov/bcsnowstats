# Script for analyzing difference between this year's normals, as well as the normals calculated by Jon

# clear all
rm(list = ls())

# Get libraries
library(dplyr)
library(bcsnowdata)
library(bcsnowstats)

# ------------------------------------------------
# Get normals for 1981-2020
aswe_n_1981t2010 <- read.csv("C:/Users/AJOLLYMO/RProjects/bcsnowsbi/data/ASWE_normals.csv")

aswe_n_1981t2010 <- cbind(aswe_n_1981t2010[, 1:2], aswe_n_1981t2010[, grepl("SWE", names(aswe_n_1981t2010))])

colnames(aswe_n_1981t2010) <- gsub("_", "", colnames(aswe_n_1981t2010))
colnames(aswe_n_1981t2010) <- gsub("SWE", "", colnames(aswe_n_1981t2010))

aswe_n_1981t2010_melt <- aswe_n_1981t2010 %>%
  reshape2::melt(id = c("STATIONID", "STATIONNAME")) %>%
  dplyr::rename(date = variable) %>%
  dplyr::rename(normal_1981t2010 = value) %>%
  dplyr::mutate(date = as.Date(date, "%b%d")) %>%
  dplyr::mutate(date = format(date, "%b%d"))

# ------------------------------------------------
# Get normals calculated by Jon
aswe_n_1991t2020_excel <- openxlsx::read.xlsx("G:/Snow/Normals_2020/ASWS_Normals_1991-2020.xlsx")
colnames(aswe_n_1991t2020_excel) <- aswe_n_1991t2020_excel[1, ]
aswe_n_1991t2020_excel <- aswe_n_1991t2020_excel[-1, ] %>%
  dplyr::select(-count, -est) %>%
  dplyr::rename(STATIONID = ID, STATIONNAME = NAME)

aswe_n_1991t2020_excel_melt <- reshape2::melt(aswe_n_1991t2020_excel, id = c("STATIONID", "STATIONNAME")) %>%
  dplyr::rename(date = variable) %>%
  dplyr::rename(normal_1991t2020_excel = value) %>%
  dplyr::mutate(date = as.Date(date, "%b%d")) %>%
  dplyr::mutate(date = format(date, "%b%d"))

# ------------------------------------------------
# Get the R process data

# Read in the csv files created and get only the 1st and 15th
get1_15 <- function(site, normal_max, normal_min, force) {

  normal <- normal_site(site, normal_max, normal_min, force)

  if (all(!is.na(normal))) {
   normal_1_15 <- normal %>%
     dplyr::mutate(Day = lubridate::day(as.Date(paste0(m_d, "-2020"), format = "%m-%d-%Y"))) %>%
     dplyr::filter(Day == "1" | Day == "15") %>%
     dplyr::select(-Day)
  } else {
    normal_1_15 <- normal
  }
  return(normal_1_15)
}

# Retrieve the snow normals for all the ASWE sites. Use archived values
aswe_sites <- bcsnowdata::snow_auto_location()$LOCATION_ID

aswe_n_1991t2020_r <- read.csv(paste0("G:/Snow/Normals_2020/snownormals_1991t2020_rprocess/", "aswe_firstfiftheenth", ".csv")) %>%
#aswe_n_1991t2020_r <- normals_1t15_unfold %>%
  dplyr::select(station_id, normal_swe_mean, m_d) %>%
  dplyr::mutate(date = as.Date(m_d, "%m-%d")) %>%
  dplyr::mutate(date = format(date, "%b%d")) %>%
  dplyr::rename(STATIONID = station_id, normal_1991t2020_r = normal_swe_mean) %>%
  dplyr::select(-m_d)

# ------------------------------------------------
# Difference between 2981-2010, Excel and R versions

excel_diff <- full_join(aswe_n_1981t2010_melt %>% dplyr::select(-STATIONNAME),
                        aswe_n_1991t2020_excel_melt %>% dplyr::select(-STATIONNAME))

excel_diff_all <- full_join(excel_diff, aswe_n_1991t2020_r) %>%
  dplyr::mutate(difference_1981_excel = as.numeric(normal_1981t2010) - as.numeric(normal_1991t2020_excel)) %>%
  dplyr::mutate(difference_1981_r = as.numeric(normal_1981t2010) - as.numeric(normal_1991t2020_r)) %>%
  dplyr::mutate(difference_excel_r = as.numeric(normal_1991t2020_excel) - as.numeric(normal_1991t2020_r))

# Get only those sites where there is significant differences between the r and excel

excel_r_issues <- excel_diff_all %>%
  dplyr::filter(abs(difference_excel_r) > 20) %>%
  dplyr::arrange(-abs(difference_excel_r), date)
write.csv(excel_r_issues, file = "C:/Users/AJOLLYMO/RProjects/normal_issues.csv")

st_issues <- unique(excel_r_issues$STATIONID)

issues <- read.csv(paste0("G:/Snow/Normals_2020/snownormals_1991t2020_rprocess/", "aswe_firstfiftheenth", ".csv")) %>%
  dplyr::filter(station_id %in% st_issues)

march <- excel_r_issues %>%
  dplyr::filter(date == "Mar01")

## Investigate specific stations that are an issue
#2D14P

data_1E11P <- bcsnowdata::get_aswe_databc(
  station_id = "1E11P",
  get_year = "All",
  parameter_id = c("SWE"),
  force = FALSE,
  ask = FALSE
)

normals_2D14P <- SWE_normals(data = data_2D14P,
                              normal_max = 2020,
                              normal_min = 1991,
                              force = TRUE)
