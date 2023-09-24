## ----load-packages-------------------------
#| message = FALSE
# Load packages ----
library(tidyverse)
library(ggplot2)


## ----path----------------------------------
# Path variables ----
here_path <- here::here()  
code_path <- file.path(here_path, "src") 
docs_path <- file.path(here_path, "doc")      
data_path <- file.path(here_path, "data")
figs_path <- file.path(here_path, "results", "figures")


## ----read-data-----------------------------
#| message = FALSE
# Read data ----
world_data  <- readr::read_csv(paste0(data_path, "/raw_data/World.csv"))
kenya_data  <- readr::read_csv(paste0(data_path, "/raw_data/Kenya.csv"))
sweden_data <- readr::read_csv(paste0(data_path, "/raw_data/Sweden.csv"))


## ----variable-type-------------------------
# Convert age to factor ----
age_group <- unique(kenya_data$age)
kenya_data$age <- factor(kenya_data$age, levels = age_group, ordered = TRUE)


## ----q1-create-variables-------------------
# Create new variable py = total person years for each data set
world_data$py  <- world_data$py.men + world_data$py.women
kenya_data$py  <- kenya_data$py.men + kenya_data$py.women
sweden_data$py <- sweden_data$py.men + sweden_data$py.women


## ----q1-create-cbr-function----------------
# Function to compute the Crude Birth Rate (CBR)
compute_cbr <- function(pop_data) {
  pop_data %>% 
  group_by(period) %>% 
  summarise(cbr = sum(births) / sum(py)) %>% 
  pull()
}


## ----q1-compute-cbr------------------------
# Compute the CBR for each data set
world_cbr  <- compute_cbr(world_data)
kenya_cbr  <- compute_cbr(kenya_data)
sweden_cbr <- compute_cbr(sweden_data)

# Combine CBR into a table
cbr <- rbind(world_cbr, kenya_cbr, sweden_cbr)
row.names(cbr) <- c("World", "Kenya", "Sweden")
colnames(cbr) <- c("1950-1955", "2005-2010")
knitr::kable(cbr)


## ----q2-create-asfr-function---------------
# Function to compute Age specific fertility rate (ASFR)
compute_asfr <- function(pop_data) {
  pop_data %>% 
  mutate(asfr = births / py.women)
}


## ----q2-compute-asfr-----------------------
# Compute ASFR for each data set
world_asfr  <- compute_asfr(
  subset(
    world_data, 
    age %in% unique(sweden_data$age)[4:10]
    )
  )
kenya_asfr  <- compute_asfr(
  subset(
    kenya_data, 
    age %in% unique(sweden_data$age)[4:10]
    )
  )
sweden_asfr <- compute_asfr(
  subset(
    sweden_data, 
    age %in% unique(sweden_data$age)[4:10]
    )
  )


## ----q2-asfr-line-plot---------------------
# Create plot to compare asfr between Kenya and Sweden across two time periods
ggplot() +
  geom_line(data = kenya_asfr,
            aes(
              x = age, 
              y = asfr, 
              group = period, 
              color = "red", 
              linetype = period
              ), 
            linewidth = 1.1) +
  geom_line(data = sweden_asfr,
            aes(
              x = age,
              y = asfr, 
              group = period, 
              color = "blue", 
              linetype = period
              ), 
            linewidth = 1.1) +
  scale_color_manual(labels = c("Sweden", "Kenya"), 
                     values = c("blue", "red")) +
  guides(color = guide_legend("countries"))


## ----q3-create-tfr-function----------------
# Function to compute the total fertility rate (TFR)
compute_tfr <- function(pop_data) {
  pop_data %>% 
  group_by(period) %>% 
  summarise(tfr = 5 * sum(asfr)) %>% 
  pull()
}


## ----q3-compute-tfr------------------------
# Compute the TFR for each data set
world_tfr  <- compute_tfr(world_asfr)
kenya_tfr  <- compute_tfr(kenya_asfr)
sweden_tfr <- compute_tfr(sweden_asfr)

# Combine TFR into a table
tfr <- rbind(world_tfr, kenya_tfr, sweden_tfr)
row.names(tfr) <- c("World", "Kenya", "Sweden")
colnames(tfr) <- c("1950-1955", "2005-2010")
knitr::kable(tfr)


## ----q3-compute-total-women-and-birth------
# Compute totals of women and births in the world by period
world_data %>% 
  group_by(period) %>% 
  summarise(
    total_women = sum(py.women),
    total_births = sum(births)
    ) ->
  totals_world

# Compare how much these totals have changed
changes_totals <- totals_world[2, -1] / totals_world[1, -1]

knitr::kable(totals_world)


## ----q4-create-cdr-function----------------
# Function to compute the Crude death rate (CDR)
compute_cdr <- function(pop_data) {
  pop_data %>% 
  group_by(period) %>% 
  summarise(cbr = sum(deaths) / sum(py)) %>% 
  pull()
}


## ----q4-compute-cdr------------------------
# Compute the CDR for each data set
world_cdr  <- compute_cdr(world_data)
kenya_cdr  <- compute_cdr(kenya_data)
sweden_cdr <- compute_cdr(sweden_data)

# Combine CDR into a table
cdr <- rbind(world_cdr, kenya_cdr, sweden_cdr)
row.names(cdr) <- c("World", "Kenya", "Sweden")
colnames(cdr) <- c("1950-1955", "2005-2010")
knitr::kable(cdr)


## ----q5-create-asdr-function---------------
# Function to compute Age specific death rate (ASDR)
compute_asdr <- function(pop_data) {
  pop_data %>% 
  mutate(asdr = deaths / py)
}


## ----q5-compute-asdr-----------------------
# Compute ASDR for each data set
kenya_asdr  <- compute_asdr(
  subset(
    kenya_data, 
    period %in% c("2005-2010")
    )
  )
sweden_asdr <- compute_asdr(
  subset(
    sweden_data, 
    period %in% c("2005-2010")
    )
  )


## ----q5-asdr-line-plot---------------------
# Create plot to compare the asdr between Kenya and Sweden for 2005-2010
ggplot() +
  geom_line(data = kenya_asdr,
            aes(x = age, y = asdr, group = period, color = "red"), 
            linewidth = 1.1) +
  geom_line(data = sweden_asdr,
            aes(x = age, y = asdr, group = period, color = "blue"), 
            linewidth = 1.1) +
  scale_color_manual(labels = c("Sweden", "Kenya"), 
                     values = c("blue", "red")) +
  guides(color = guide_legend("countries"))


## ----q6-create-pop-prop-function-----------
# Function to compute population proportion by period
compute_pop_prop <- function(pop_data) {
  pop_data %>% 
  group_by(period) %>%
  mutate(pop_prop = py / sum(py)) %>%
  ungroup()
}


## ----q6-compute-pop-prop-------------------
# Compute population proportion for each data set
kenya_asdr  <- compute_pop_prop(kenya_asdr)
sweden_asdr <- compute_pop_prop(sweden_asdr)


## ----q6-compute-counterfactual-cdr---------
#| output = FALSE
# Compute Kenyas CDR when Kenya had Sweden's population distribution
mutate(kenya_asdr, temp_cdr = asdr * sweden_asdr$pop_prop) %>%
 group_by(period) %>%
 summarise(cdr_re_sweden = sum(temp_cdr))

