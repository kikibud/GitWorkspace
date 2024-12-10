###########################################################################
# Comparing EC Measurement Methodologies                                  #
###########################################################################

#### Housekeeping ####
# load packages
library(readxl)
library(tidyverse)
library(lubridate)
library(hms)
library(nlme)
library(glmmTMB)
library(smatr)
library(DHARMa)
library(ggResidpanel)
library(emmeans)
library(multcomp)
library(writexl)

# set global options and default aesthetics
setwd("C:/Users/PixelLyric/Desktop/College/Grad/Research/Data")

my_pal = c('steelblue', 'darkorange', 'firebrick', 'darkorchid4')
options(contrasts = c('contr.sum', 'contr.poly'), 
        dplyr.width = Inf, pillar.print_max = 50, pillar.print_min = 50, 
        ggplot2.discrete.colour = my_pal, ggplot2.discrete.fill = my_pal)

my_theme = theme_light()+
  theme(text = element_text(size = 12), 
        strip.background = element_rect(fill = 'white', color = 'grey'), 
        strip.text = element_text(color = 'black', face = 'bold'))
theme_set(my_theme)

# Define custom functions
tryCatch.W.E <- function(expr){
     W <- NULL
     w.handler <- function(w) { # warning handler
        W <<- w
 	      invokeRestart("muffleWarning")
     }
     list(value = withCallingHandlers(tryCatch(expr, error = function(e) e),
 				     warning = w.handler), warning = W)
 }


#### EC Distribution - Data Import & Prep ####
# Position data
sme_file = 'SME_results.xlsx'
excel_sheets(sme_file)
sme = read_xlsx(sme_file, sheet = 'SME') |> 
  separate(col = 'Pot_ID', into = c('Pot_ID', 'Position'), sep = ' ') |>
  mutate(Location = factor(substr(Pot_ID, 1, 1), labels = c('Pad', 'Tunnel')),
         Position = factor(Position, levels = c('B', 'M', 'T'), 
                           labels = c('Bottom', 'Middle', 'Top')), 
         Pot_ID = factor(Pot_ID)) |> 
  dplyr::select(Pot_ID, Location, Position, SME_EC, SME_pH)
str(sme)

# Pourthrough data
sensor_file = 'cleaned_sensor_data.xlsx'
(sensor_sheets = excel_sheets(sensor_file))

pt = read_xlsx(sensor_file, sheet = 5) |> 
  mutate(Location = factor(substr(Pot_ID, 1, 1),
                           levels = c('P', 'T'),
                           labels = c('Pad', 'Tunnel')), 
         Fertilizer = factor(ifelse(nchar(Pot_ID) == 2, 'Low', 'High'), 
                             levels = c('Low', 'High')))
str(pt)
saveRDS(pt, 'pourthrough_data.rda')

pt_dates = unique(pt$date)

# Sensor data

sensor_list = lapply(1:4, read_xlsx, path = sensor_file)

# Identify duplicated time stamp and device ID combinations
which(duplicated(dplyr::select(sensor_list[[1]], c('datetime', 'Device_ID'))))
which(duplicated(dplyr::select(sensor_list[[2]], c('datetime', 'Device_ID'))))
which(duplicated(dplyr::select(sensor_list[[3]], c('datetime', 'Device_ID'))))
which(duplicated(dplyr::select(sensor_list[[4]], c('datetime', 'Device_ID'))))

# Note that there are several instances of duplicate data (i.e. all values
# are identical), but there are also cases where date, time, and device
# numbers are the same but measurements differ. For now these are just
# being averaged but you need to figure out how this happened
for (n in 1:length(sensor_list)){
  print(n)
  sensor_list[[n]] = sensor_list[[n]] |> 
    pivot_longer(cols = 3:17, 
                 names_to = 'Measure', 
                 values_to = 'Value') |>
  mutate(Measure = gsub('sensor', '', Measure)) |> 
  separate(col = 'Measure', 
           into = c('Pot_ID', 'Measurement'),
           sep = '_') |> 
  mutate(Location = factor(substr(Pot_ID, 1, 1),
                           levels = c('P', 'T'),
                           labels = c('Pad', 'Tunnel')), 
         Fertilizer = factor(ifelse(nchar(Pot_ID) == 2, 'Low', 'High'), 
                             levels = c('Low', 'High'))) |> 
  pivot_wider(names_from = 'Measurement', values_from = 'Value', 
              values_fn = mean)
}

sensor = bind_rows(sensor_list) |> 
  mutate(datetime = mdy_hm(datetime))

# Here is where the time intervals are defined, adjust values as needed
sensor = sensor |> 
  filter(as_date(datetime) %in% as_date(pt_dates)) |>
  mutate(Time = as_hms(datetime)) |> 
          # Time interval on line below is for all "Pad" measurements
  filter((Location == 'Pad' & Time > as_hms('09:45:00') & Time < as_hms('10:15:00')) |    
           # Time interval on line below is for Tunnel measurements NOT on day 2024-7-10
          (Location == 'Tunnel' & Time > as_hms('12:07:00') & Time < as_hms('12:37:00') & datetime != as_date('2024-07-10')) |
           # Time interval for tunnel on 2024-7-10
           (Location == 'Tunnel' &  Time > as_hms('13:07:00') & Time < as_hms('13:37:00') & datetime == as_date('2024-07-10')))

str(sensor)
saveRDS(sensor, 'sensor_data.rda')
write.csv(sensor, 'sensor_data.csv', row.names = FALSE) #save as csv

### chatgpt code to check for duplicate data ###
sensor <- readRDS('sensor_data.rda')

# Look at the structure of the dataset to identify key columns
str(sensor)
head(sensor)
# Find fully duplicate rows
duplicates <- sensor[duplicated(sensor), ]

# Display duplicates
print(duplicates)
n_duplicates <- sum(duplicated(sensor))
cat("Number of fully duplicate rows:", n_duplicates)

# Identify duplicates based on Date, Time, and Device_ID
partial_duplicates <- sensor[duplicated(sensor[c("datetime", "Device_ID")]) | 
                               duplicated(sensor[c("datetime", "Device_ID")], fromLast = TRUE), ]

# Display partial duplicates
print(partial_duplicates)

duplicate_counts <- sensor %>%
  group_by(datetime, Device_ID) %>%
  filter(n() > 1)  # Retain only rows with duplicates

# Count cases
duplicate_summary <- duplicate_counts %>%
  summarise(n = n(), distinct_EC = n_distinct(ec))

print(duplicate_summary)

#### Data Exploration & Validation ####
sensor = readRDS('sensor_data.rda')
pt = readRDS('pourthrough_data.rda')

# ggplot(sme, aes(y = Position, color = Location, x = SME_EC))+
#   geom_violin()+
#   geom_point(position = position_jitter(height = .1))

pt |> 
  filter(as_date(date) == ymd('2024-07-10') &
           Pot_ID == 'T31')

sensor |> 
  filter(as_date(datetime) == ymd('2024-07-10') &
           Pot_ID == 'T31') |> 
  summarise(mean(hilhorst))

sensor |> 
  filter(as_date(datetime) == ymd('2024-08-13') &
           Pot_ID == 'T32') |> 
  ggplot(aes(x = datetime, y = hilhorst, group = Pot_ID, color = Location))+
  geom_point() +
  geom_hline(yintercept = 0.92, color = 'firebrick', linewidth = 1) +
  geom_hline(yintercept = 1.47, color = 'darkorange', linewidth = 1)

# Adjust Pot_ID value in call to filter() to look at different pots
sensor |>
  filter(Pot_ID == 'P32') |> 
  ggplot(aes(x = as_hms(datetime), y = ec))+
  facet_wrap(~ as_date(datetime))+
  geom_point()

#### EC Distribution - Model Development & Evaluation ####
ec_dist_mod = glmmTMB(SME_EC ~ Location*Position, data = sme, 
                      family = Gamma(link = 'log'))

# Residuals look great
ec_dist_res = simulateResiduals(ec_dist_mod, plot = T) |> 
  resid(quantileFunction = qnorm, outlierValues = c(-5, 5))
ec_dist_fit = fitted(ec_dist_mod)
resid_auxpanel(ec_dist_res, ec_dist_fit)

#### EC Distribution - Estimation, Inference & Plotting ####
# Significant interaction means we should avoid averaging over either
# factor when reporting the results
(ec_dist_waldtest = joint_tests(ec_dist_mod))

# Calculate marginal means, using location as a grouping factor
(ec_dist_emms  = emmeans(ec_dist_mod, specs = ~ Position | Location, 
                         type = 'response'))

# Compact letter display summerizes pairwise comparisons: EC is highest
# at the bottom layer in pots on the pad but highest in the top layer
# in pots in the tunnel
(ec_dist_cld = cld(ec_dist_emms, Letters = letters, reversed = T))

# Plot results 
ec_dist_cld |> 
  as.data.frame() |> 
  mutate(.group = gsub(' ', '', .group)) |> 
  ggplot(aes(y = Position))+
    facet_grid(cols = vars(Location))+
    geom_col(aes(x = response), fill = 'seashell3', width = .8)+
    geom_errorbar(aes(xmin = asymp.LCL, xmax = asymp.UCL), width = .2) +
    geom_text(aes(x = asymp.UCL+500, label = .group)) +
    scale_x_continuous(name = 'EC')

#### Comparing EC Measurement Methodologies - Data Import & Prep #####
sensor = readRDS('sensor_data.rda') |> 
  mutate(Date = as_date(datetime)) |> 
  group_by(Date, Device_ID, Pot_ID, Location, Fertilizer) |> 
  summarise(EC = mean(0.001*ec), 
            Hilhorst = mean(hilhorst), 
            .groups = 'drop')
pt = readRDS('pourthrough_data.rda') |> 
  rename(Date = date)

comb = left_join(sensor, pt, by = join_by(Pot_ID, Date, Location, Fertilizer)) |> 
  mutate(Trt = interaction(Location, Fertilizer, sep = '-'))

ggplot(data = comb, aes(x = EC, y = PT_EC))+
  facet_wrap(Location ~ Fertilizer)+
  geom_point() +
  geom_smooth(se = F, method = 'lm')

#### Raw EC vs Pourthrough - Model Development & Evaluation  #####

# Fit a model which tests for differences in slope between the two 
# locations/irrigation systems.
loc_mod = sma(PT_EC ~ EC*Location, data = comb, method = 'SMA',
              robust = T)
# There are significant differences in slope (p = 0.014)
print(loc_mod)
# And in fact there is only evidence of a correlation in pad data (p < 0.001),
# not in the tunnel (p = 0.120)
summary(loc_mod)

# Looking only at the pad to test for differences in slope among the two 
# fertilizer treatments, we find no evidence of a difference
fert_pad_mod = sma(PT_EC ~ EC*Fertilizer, data = comb,
                   subset = comb$Location == 'Pad', 
                   method = 'SMA', robust = T)
print(fert_pad_mod)

# And thus refit the model to get average slope and elevation over the 
# two fertilizer types
pad_mod =  sma(PT_EC ~ EC, data = comb,
               subset = comb$Location == 'Pad', 
               method = 'SMA', robust = T)
# This model shows that there is a significant downward bias in the EC 
# readings, with the calibration formula: TP_EC = -0.30 + 9.37*EC
# Howerever, R2 is fairly low at 0.568. 
print(pad_mod)

# In other words, a EC measurement is only accurate to within plus or minus:
pad_res = resid(pad_mod)
1.96*sd(pad_res)
# of the pourthrough value

# Other measures of predictive accuracy:
(pad_rmse = sqrt(mean(pad_res^2)))
(pad_mad = mean(abs(pad_res)))

# Unfortunately, tools for calculating confidence bands do length (yet) exist in
# R for major axis regression, so here I will bootstrap them (adapting
# code I took from: https://stackoverflow.com/questions/39453540/how-to-plot-confidence-intervals-for-a-major-axis-fit-using-the-smatr-package-in)
npt = 200
nsim = 999
ec_vec = seq(min(comb$EC), max(comb$EC), length.out = npt)
pred_grid = data.frame(EC = ec_vec)
preds = array(dim = c(npt, nsim),
              dimnames = list(paste0('EC', ec_vec), 
                              paste0('Sim', seq(nsim))))

# bootstrap data and get predictions
nboot = 1e3
cfs = coef(pad_mod)

for (n in 1:nsim){
  print(n)
  
  # Subset full data set
  newdat = comb |> 
    filter(Location == 'Pad')
  
  # Sample, with replacement, EC values from subset, 
  # and use coefficients from fitted model to calculate predicted value
  newdat = newdat[sample(nrow(newdat), replace = T), ]
  # Create simulated PT_EC values by sampling, with replacement, the 
  # residuals from the fitted model and adding those residuals to the 
  # predicted values
  #newdat$PT_EC = newdat$lp + sample(pad_res, replace = T)
  
  # Refit the model, and skip if convergence failure, otherwise use 
  # new coefficients to calculate predictions over a grid of EC values
  newfit = tryCatch.W.E(sma(PT_EC ~ EC, data = newdat, method = 'SMA', robust = T))
  if ('error' %in% class(newfit$value)){
    preds[, n] = NA
    next
  } else {
    newcfs = coef(newfit$value)
    preds[, n] = newcfs[1]+newcfs[2]*pred_grid$EC
  }
}


# Calculate 95% confidence interval at each point in the grid of EC values
pad_ci = apply(preds, 1, tidybayes::hdci) |> 
  t() |> 
  as.data.frame() |> 
  magrittr::set_colnames(c('LCL', 'UCL')) |> 
  rownames_to_column(var = 'EC') |> 
  mutate(EC = as.numeric(gsub('EC', '', EC)), 
         Estimate = cfs[1] + cfs[2]*EC) 

# Plot results
pad_plot = ggplot(pad_ci, aes(x = EC))+
  geom_ribbon(aes(ymin = LCL, ymax = UCL), 
              alpha = .2, color = NA)+
  geom_line(aes(y = Estimate))+
  geom_point(aes(y = PT_EC), data = filter(comb, Location == 'Pad')) +
  annotate('text', x = 0.1, y = 6, label = bquote('Y = -0.3 + 9.36x')) +
  annotate('text', x = 0.1, y = 5.75, label = bquote(R^2~'= 0.56')) +
    annotate('text', x = 0.1, y = 5.5, label = bquote('p < 0.001')) +
  scale_x_continuous('Raw Sensor Reading (dS/m)')+
  scale_y_continuous('Pourthrough Measurement (dS/m)') +
  labs(title = 'Pad')
pad_plot

ggsave('results/Raw EC Sensor vs Pourthrough - Pad.tiff', plot = pad_plot,
       device = 'tiff', height = 6.5, width = 6.5)

# Turning to the tunnel, we can see that although the slopes among the 
# fertilizer treatments is significantly different, neither individual
# nor average slope is significantly different from 0, meaning no 
# association
tunnel_mod = sma(PT_EC ~ EC*Fertilizer, data = comb,
                   subset = comb$Location == 'Tunnel', 
                   method = 'SMA', robust = T)
print(tunnel_mod)
summary(tunnel_mod)

# Refit for average tunnel
tunnel_mod = sma(PT_EC ~ EC, data = comb,
                   subset = comb$Location == 'Tunnel', 
                   method = 'SMA', robust = T)

# Unfortunately, tools for calculating confidence bands do lengtht (yet) exist in
# R for major axis regression, so here I will bootstrap them (adapting
# code I took from: https://stackoverflow.com/questions/39453540/how-to-plot-confidence-intervals-for-a-major-axis-fit-using-the-smatr-package-in)
npt = 200
nsim = 999
tunnel = filter(comb, Location == 'Tunnel')
ec_vec = seq(min(tunnel$EC), max(tunnel$EC), length.out = npt)
pred_grid = data.frame(EC = ec_vec)
preds = array(dim = c(npt, nsim),
              dimnames = list(paste0('EC', ec_vec), 
                              paste0('Sim', seq(nsim))))

# bootstrap data and get predictions
nboot = 1e3
cfs = coef(tunnel_mod)

for (n in 1:nsim){
  print(n)
  
  # Subset full data set
  newdat = comb |> 
    filter(Location == 'Tunnel')
  
  # Sample, with replacement, EC values from subset, 
  # and use coefficients from fitted model to calculate predicted value
  newdat = newdat[sample(nrow(newdat), replace = T), ]
  # Create simulated PT_EC values by sampling, with replacement, the 
  # residuals from the fitted model and adding those residuals to the 
  # predicted values
  #newdat$PT_EC = newdat$lp + sample(tunnel_res, replace = T)
  
  # Refit the model, and skip if convergence failure, otherwise use 
  # new coefficients to calculate predictions over a grid of EC values
  newfit = tryCatch.W.E(sma(PT_EC ~ EC, data = newdat, method = 'SMA', robust = T))
  if ('error' %in% class(newfit$value)){
    preds[, n] = NA
    next
  } else {
    newcfs = coef(newfit$value)
    preds[, n] = newcfs[1]+newcfs[2]*pred_grid$EC
  }
}


# Calculate 95% confidence interval at each point in the grid of EC values
tunnel_ci = apply(preds, 1, tidybayes::hdci) |> 
  t() |> 
  as.data.frame() |> 
  magrittr::set_colnames(c('LCL', 'UCL')) |> 
  rownames_to_column(var = 'EC') |> 
  mutate(EC = as.numeric(gsub('EC', '', EC)), 
         Estimate = cfs[1] + cfs[2]*EC) 

# Plot results
tunnel_plot = ggplot(tunnel_ci, aes(x = EC))+
  geom_ribbon(aes(ymin = LCL, ymax = UCL), 
              alpha = .2, color = NA)+
  geom_line(aes(y = Estimate))+
  geom_point(aes(y = PT_EC), data = filter(comb, Location == 'Tunnel')) +
  annotate('text', x = 0.05, y = 4, label = bquote('Y = -0.24 + 14.56x')) +
  annotate('text', x = 0.05, y = 3.75, label = bquote(R^2~'= 0.046')) +
    annotate('text', x = 0.05, y = 3.5, label = bquote('p =0.120')) +
  scale_x_continuous('Raw Sensor Reading (dS/m)')+
  scale_y_continuous('Pourthrough Measurement (dS/m)') +
  labs(title = 'Tunnel')
tunnel_plot

ggsave('results/Raw EC Sensor vs Pourthrough - Tunnel.tiff', plot = tunnel_plot,
       device = 'tiff', height = 6.5, width = 6.5)

#### Hilhorst vs Pourthrough - Model Development & Evaluation  #####
# Fit a model which tests for differences in slope between the two 
# locations/irrigation systems.
loc_mod = sma(PT_EC ~ Hilhorst*Location, data = comb, method = 'SMA',
              robust = T)

# Significant differences among location
print(loc_mod)

## Pad Model:
# Differences in fertilizer treatment within the pad location non-sig
pad_mod = sma(PT_EC ~ Hilhorst*Fertilizer, data = comb, 
              subset = comb$Location == 'Pad', method = 'SMA',
              robust = T)
print(pad_mod)

# Refit model to average over fertilizer levels
pad_mod = sma(PT_EC ~ Hilhorst, data = comb, 
              subset = comb$Location == 'Pad', method = 'SMA',
              robust = T)
print(pad_mod)

# Hilhorst measurement is only accurate to within plus or minus:
pad_res = resid(pad_mod)
1.96*sd(pad_res)
# of the pourthrough value

# Other measures of predictive accuracy:
(pad_rmse = sqrt(mean(pad_res^2)))
(pad_mad = mean(abs(pad_res)))

# Need to use bootstrap procedure to generate confidence band for fitted line
npt = 200
nsim = 999
pad = filter(comb, Location == 'Pad')
ec_vec = seq(min(pad$Hilhorst), max(pad$Hilhorst), length.out = npt)
pred_grid = data.frame(Hilhorst = ec_vec)
preds = array(dim = c(npt, nsim),
              dimnames = list(paste0('Hilhorst', ec_vec), 
                              paste0('Sim', seq(nsim))))

# bootstrap data and get predictions
nboot = 1e3
cfs = coef(pad_mod)

for (n in 1:nsim){
  print(n)
  
  # Subset full data set
  newdat = comb |> 
    filter(Location == 'Pad')
  
  # Sample, with replacement, EC values from subset, 
  # and use coefficients from fitted model to calculate predicted value
  newdat = newdat[sample(nrow(newdat), replace = T), ]
  # Create simulated PT_EC values by sampling, with replacement, the 
  # residuals from the fitted model and adding those residuals to the 
  # predicted values
  #newdat$PT_EC = newdat$lp + sample(pad_res, replace = T)
  
  # Refit the model, and skip if convergence failure, otherwise use 
  # new coefficients to calculate predictions over a grid of EC values
  newfit = tryCatch.W.E(sma(PT_EC ~ Hilhorst, data = newdat, method = 'SMA', robust = T))
  if ('error' %in% class(newfit$value)){
    preds[, n] = NA
    next
  } else {
    newcfs = coef(newfit$value)
    preds[, n] = newcfs[1]+newcfs[2]*pred_grid$Hilhorst
  }
}

# Calculate 95% confidence interval at each point in the grid of EC values
pad_ci = apply(preds, 1, tidybayes::hdci) |> 
  t() |> 
  as.data.frame() |> 
  magrittr::set_colnames(c('LCL', 'UCL')) |> 
  rownames_to_column(var = 'Hilhorst') |> 
  mutate(Hilhorst = as.numeric(gsub('Hilhorst', '', Hilhorst)), 
         Estimate = cfs[1] + cfs[2]*Hilhorst) 

# Plot results
pad_plot = ggplot(pad_ci, aes(x = Hilhorst))+
  geom_ribbon(aes(ymin = LCL, ymax = UCL), 
              alpha = .2, color = NA)+
  geom_line(aes(y = Estimate))+
  geom_point(aes(y = PT_EC), data = filter(comb, Location == 'Pad')) +
  annotate('text', x = 1, y = 6, label = bquote('Y = -0.044 + 1.04x')) +
  annotate('text', x = 1, y = 5.75, label = bquote(R^2~'= 0.273')) +
  annotate('text', x = 1, y = 5.5, label = bquote('p < 0.001')) +
  scale_x_continuous(name = 'Hilhorst EC Readings (dS/m)')+
  scale_y_continuous(name = 'Pourthrough Measurement (dS/m)') +
  labs(title = 'Pad')
pad_plot

ggsave('results/Hilhorst EC Sensor vs Pourthrough - Pad.tiff', plot = pad_plot,
       device = 'tiff', height = 6.5, width = 6.5)

## Tunnel Model
# Nonsig diff amogn fertilizer groups in tunnel
tunnel_mod = sma(PT_EC ~ Hilhorst*Fertilizer, data = comb, 
                 subset = comb$Location == 'Tunnel', method = 'SMA',
              robust = T)
print(tunnel_mod)

# Refit to average over fertilizer levels; overall model is also nonsig
tunnel_mod = sma(PT_EC ~ Hilhorst, data = comb, 
                 subset = comb$Location == 'Tunnel', method = 'SMA',
              robust = T)
print(tunnel_mod)

# Need to use bootstrap procedure to generate confidence band for fitted line
npt = 200
nsim = 999
tunnel = filter(comb, Location == 'Tunnel')
ec_vec = seq(min(tunnel$Hilhorst), max(tunnel$Hilhorst), length.out = npt)
pred_grid = data.frame(Hilhorst = ec_vec)
preds = array(dim = c(npt, nsim),
              dimnames = list(paste0('Hilhorst', ec_vec), 
                              paste0('Sim', seq(nsim))))

# bootstrap data and get predictions
nboot = 1e3
cfs = coef(tunnel_mod)

for (n in 1:nsim){
  print(n)
  
  # Subset full data set
  newdat = comb |> 
    filter(Location == 'Tunnel')
  
  # Sample, with replacement, EC values from subset, 
  # and use coefficients from fitted model to calculate predicted value
  newdat = newdat[sample(nrow(newdat), replace = T), ]
  # Create simulated PT_EC values by sampling, with replacement, the 
  # residuals from the fitted model and adding those residuals to the 
  # predicted values
  #newdat$PT_EC = newdat$lp + sample(tunnel_res, replace = T)
  
  # Refit the model, and skip if convergence failure, otherwise use 
  # new coefficients to calculate predictions over a grid of EC values
  newfit = tryCatch.W.E(sma(PT_EC ~ Hilhorst, data = newdat, method = 'SMA', robust = T))
  if ('error' %in% class(newfit$value)){
    preds[, n] = NA
    next
  } else {
    newcfs = coef(newfit$value)
    preds[, n] = newcfs[1]+newcfs[2]*pred_grid$Hilhorst
  }
}

# Calculate 95% confidence interval at each point in the grid of EC values
tunnel_ci = apply(preds, 1, tidybayes::hdci) |> 
  t() |> 
  as.data.frame() |> 
  magrittr::set_colnames(c('LCL', 'UCL')) |> 
  rownames_to_column(var = 'Hilhorst') |> 
  mutate(Hilhorst = as.numeric(gsub('Hilhorst', '', Hilhorst)), 
         Estimate = cfs[1] + cfs[2]*Hilhorst) 

# Plot results
tunnel_plot = ggplot(tunnel_ci, aes(x = Hilhorst))+
  geom_ribbon(aes(ymin = LCL, ymax = UCL), 
              alpha = .2, color = NA)+
  geom_line(aes(y = Estimate))+
  geom_point(aes(y = PT_EC), data = filter(comb, Location == 'Tunnel')) +
  annotate('text', x = 1, y = 6, label = bquote('Y = -0.044 + 1.04x')) +
  annotate('text', x = 1, y = 5.75, label = bquote(R^2~'= 0.273')) +
  annotate('text', x = 1, y = 5.5, label = bquote('p < 0.001')) +
  scale_x_continuous(name = 'Hilhorst EC Readings (dS/m)')+
  scale_y_continuous(name = 'Pourthrough Measurement (dS/m)') +
  coord_cartesian(xlim = c(0, 5.1), ylim = c(0, 6))+
  labs(title = 'tunnel')
tunnel_plot

ggsave('results/Hilhorst EC Sensor vs Pourthrough - Tunnel.tiff', plot = tunnel_plot,
       device = 'tiff', height = 6.5, width = 6.5)

#### Power Analysis ####
sensor = readRDS('sensor_data.rda') |> 
  mutate(Date = as_date(datetime)) |> 
  group_by(Date, Device_ID, Pot_ID, Location, Fertilizer) |> 
  summarise(EC = mean(0.001*ec), 
            Hilhorst = mean(hilhorst), 
            .groups = 'drop')
pt = readRDS('pourthrough_data.rda') |> 
  rename(Date = date)

comb = left_join(sensor, pt, by = join_by(Pot_ID, Date, Location, Fertilizer)) |> 
  mutate(Trt = interaction(Location, Fertilizer, sep = '-'))

# Not actually interested in the fixed effects, so can ignore
# factorial treatment structure by creating combined treatment
comb$DateTrt = interaction(comb$Trt, comb$Date, sep = '-')
comb = droplevels(comb)

# Fit the model so we can get residual standard deviation and calculate 
# standard error
pt_mod = lme(PT_EC ~ DateTrt, data = comb, 
             random = ~1|Pot_ID)

# Clear evidence of heteroskedasticity
pt_res = resid(pt_mod, type = 'normalized')
pt_fit = fitted(pt_mod)
resid_auxpanel(pt_res, pt_fit)

# varPower is clearly the best (for details on what each means, see ?varClasses)
AIC(pt_mod, 
    update(pt_mod, weights = varExp()), 
    update(pt_mod, weights = varPower()), 
    update(pt_mod, weights = varConstProp()), 
    update(pt_mod, weights = varConstPower()))

pt_mod = lme(PT_EC ~ DateTrt, data = comb, 
             random = ~1|Pot_ID, 
             weights = varPower())

# Neither a random slopes model nor correlation 
# improve model fit
AIC(pt_mod, 
    update(pt_mod, random = ~Date|Pot_ID))

AIC(pt_mod,
    update(pt_mod, correlation = corCompSymm()),
    update(pt_mod, correlation = corAR1()))

# Residual variance is equal to (sigma^2)*abs(lp)^(2*1.872459), where lp is
# the predicted value. 
(pt_theta = coef(pt_mod$modelStruct$varStruct))
names(pt_theta) = NULL

# At the average observed of PT EC and with 5 reps, this corresponds to: 
(mean_ec = mean(comb$PT_EC))
(pt_sigma2 = sigma(pt_mod)^2)
(pt_res_var = pt_sigma2*abs(mean_ec)^(2*pt_theta))
(pt_res_se = sqrt(pt_res_var/5))

# Now for hilhorst measurements
hil_mod = lme(Hilhorst ~ DateTrt, data = comb, 
              random = ~1|Pot_ID)

# Again, clear evidence of heteroskedasticity
hil_res = resid(hil_mod, type = 'normalized')
hil_fit = fitted(hil_mod)
resid_auxpanel(hil_res, hil_fit)

# Here, varConstPower is actually slightly better than varPower, 
# but since the difference is very small, stick with the simpler model
# for consistancy
AIC(hil_mod, 
    update(hil_mod, weights = varExp()), 
    update(hil_mod, weights = varPower()), 
    update(hil_mod, weights = varConstProp()), 
    update(hil_mod, weights = varConstPower()))

hil_mod = lme(Hilhorst ~ DateTrt, data = comb, 
             random = ~1|Pot_ID, 
             weights = varPower())

# Here there is also evidence of temporal autocorrelation
AIC(hil_mod, 
    update(hil_mod, random = ~Date|Pot_ID))

AIC(hil_mod,
    update(hil_mod, correlation = corCompSymm()),
    update(hil_mod, correlation = corAR1()))

hil_mod = lme(Hilhorst ~ DateTrt, data = comb, 
             random = ~1|Pot_ID, 
             weights = varPower(), 
             correlation = corAR1())

# Residual variance is equal to (sigma^2)*abs(lp)^(2*1.872459), where lp is
# the predicted value. 
(hil_theta = coef(hil_mod$modelStruct$varStruct))
names(hil_theta) = NULL

# At the average observed of PT EC and with 5 reps, this corresponds to: 
(hil_sigma2 = sigma(hil_mod)^2)
(hil_res_var = hil_sigma2*abs(mean_ec)^(2*hil_theta))
(hil_res_se = sqrt(hil_res_var/5))

# And to get sample size at which precision of an EC estimate of 1 is equivalent to that of the pourthrough
# method with 5 replicates is:
ceiling(5*hil_sigma2/pt_sigma2)

# Standard error as a function of EC - note that the difference in 
# the mean-variance relationship for the two systems means that 
curve(sqrt(pt_sigma2*abs(x)^(2*pt_theta))/sqrt(5), 0, 3)
curve(sqrt(hil_sigma2*abs(x)^(2*hil_theta))/sqrt(5), 0, 3, add = T, col = 'blue')

# Standard error as a function of sample size when mean EC = 1
curve(sqrt(pt_sigma2*abs(1)^(2*pt_theta))/sqrt(x), 3, 8)
curve(sqrt(hil_sigma2*abs(1)^(2*hil_theta))/sqrt(x), add = T, col = 'blue')

# Standard error as a function of sample size when mean EC = 1.25
curve(sqrt(pt_sigma2*abs(1.25)^(2*pt_theta))/sqrt(x), 3, 8)
curve(sqrt(hil_sigma2*abs(1.25)^(2*hil_theta))/sqrt(x), add = T, col = 'blue')

# Standard error as a function of sample size when mean EC = 0.75
curve(sqrt(pt_sigma2*abs(1.5)^(2*pt_theta))/sqrt(x), 3, 8)
curve(sqrt(hil_sigma2*abs(1.5)^(2*hil_theta))/sqrt(x), add = T, col = 'blue')

# Standard error as a function of sample size when mean EC = 1
curve(sqrt(pt_sigma2*abs(.5)^(2*pt_theta))/sqrt(x), 3, 8)
curve(sqrt(hil_sigma2*abs(.5)^(2*hil_theta))/sqrt(x), add = T, col = 'blue')

