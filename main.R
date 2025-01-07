# Packages ----------------------------------------------------------------

require(tidyverse)
require(magrittr)


# Data --------------------------------------------------------------------
# 
dat <- read.table("dati_matteo.txt") %>% as_tibble() %>% drop_na() %>% 
  mutate(fti.x = fti.x*sqrt(1.6), bti.x = bti.x*sqrt(1.6),
         fti.y = fti.y*1.6, bti.y = bti.y*1.6)

# data in long format
dat_long <- bind_rows(
  dat %>% select(starts_with("bti")) %>% set_colnames(value = c("fall", "x", "y")) %>% 
    mutate(type = "bti"),
  dat %>% select(starts_with("fti")) %>% set_colnames(value = c("x", "y", "fall"))  %>% 
    mutate(type = "fti")
) %>% drop_na() 

# invert logistic regression curve for isoprobability curves
invd <- function(s, logod, b0, b1, b2, b3){
  (logod-b0-b2*s)/(b1+b3*s)
}

# Model -------------------------------------------------------------------
# BTI
fit_bti <- glm(fall ~ x*y, data = dat_long %>% filter(type == "bti"), family = "binomial")
summary(fit_bti)

# FTI
fit_fti <- glm(fall ~ x*y, data = dat_long %>% filter(type == "fti"), family = "binomial")
summary(fit_fti)

# JOINT
fit_all <- glm(fall ~ x*y*type, data = dat_long, family = "binomial")
summary(fit_all)



# Results -----------------------------------------------------------------

xgrid <- seq(0.2, 2.75, length = 250)
ygrid <- seq(0.1, 1.25, length = 250)
xygrid_bti <- cbind(expand.grid(x = xgrid, y = ygrid), type = "bti")
xygrid_fti <- cbind(expand.grid(x = xgrid, y = ygrid), type = "fti")
xygrid <- as.data.frame(rbind(xygrid_bti, xygrid_fti))
preds <- predict(fit_all, newdata = xygrid) # log-odds


# Predictions on a simulated flood ----------------------------------------
time_fls <- c("time500", "time1800", "time3600", "time7200")
time_data <- map_dfr(time_fls, \(ff){
  readxl::read_excel("extraction_skip_4_paper2021_levee.xlsx", sheet = ff) %>% 
    mutate(time = as.numeric(parse_number(ff)))
})
time_data <- time_data %>% rename(loc1 = x, loc2 = y, x = speed, y = deep)

# predict
preds_allto <- bind_rows(
  time_data %>% mutate(type = "bti"), time_data %>% mutate(type = "fti")
) %>% predict(fit_all, newdata = ., type = "link")
