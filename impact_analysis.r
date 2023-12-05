rm(list = ls())
options(scipen = 999)
#Read data from csv file
library(dplyr)
library(readr)
library(plm)
library(ggplot2)
library(lmtest)
library(car)
library(stargazer)
democrac <- read_csv("/Users/jacopobinati/Desktop/impact project fdi/V-Dem-CY-Core-v13.csv")
wbdata <- read_csv("/Users/jacopobinati/Desktop/impact project fdi/WBdata.csv", col_types = cols(`GDP per capita (constant 2015 US$) [NY.GDP.PCAP.KD]` = col_double(), 
                                                                                                    `Foreign direct investment, net inflows (% of GDP) [BX.KLT.DINV.WD.GD.ZS]` = col_double(), 
                                                                                                    `Labor force, total [SL.TLF.TOTL.IN]` = col_double()))
democrac$year <- as.character(democrac$year)

#Clean data


# select necessary columns
democrac.s <- democrac %>% select(country_name,country_text_id, year, v2x_polyarchy)
wbdata.s <- wbdata %>% select(Time, `Time Code`,`Country Name`, `Country Code`, `GDP per capita (constant 2015 US$) [NY.GDP.PCAP.KD]`,`Population, total [SP.POP.TOTL]`,`Foreign direct investment, net inflows (% of GDP) [BX.KLT.DINV.WD.GD.ZS]`,`Labor force, total [SL.TLF.TOTL.IN]`)
# rename columns
colnames(democrac.s)[c(2:4)] <- c('country.code','time','democracy')
colnames(wbdata.s)[c(1,4:8)] <- c('time','country.code','gdp','population','fdi','labor')
# join both tables
datafinal <- democrac.s %>% full_join(wbdata.s)
View(datafinal)

datafinal <- full_join(democrac.s, wbdata.s, by = c("country.code", "time"))
# Assuming you want to select specific columns in the final dataset
datafinal <- datafinal %>%
  select(country_name, country.code, time, democracy, gdp, population, fdi, labor)
# remove rows where contain NA values
datafinal.f <- na.omit(datafinal)

# remove data where year is from 1997 - 2001
datafinal.y <- subset(datafinal.f, !(time %in% c('1997','1998','1999','2000','2001')))
# remove country where year count is less than 20
datafinal.y <- datafinal.y %>% group_by(country.code) %>% mutate(year.c = length(unique(time)))
datafinal.y <- subset(datafinal.y, year.c >= 20)

# define income type
income <- subset(datafinal.y, time == '2002')
income$income.type <- ifelse(income$gdp < 3358, 'low','high')
datafinal.i <- datafinal.y %>% left_join(income[c(2,10)], by = c('country.code' = 'country.code'))

####################################################################################################
combined_data <- rbind(
  cbind(datafinal.i, Year = "2002"),
  cbind(datafinal.i, Year = "2021")
)
low_income <- subset(datafinal.i, income.type == 'low')
high_income <- subset(datafinal.i, income.type == 'high')
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Create a ggplot histogram with facets for each category
ggplot(combined_data, aes(x = fdi, fill = income.type)) +
  geom_histogram(bins = 20, position = "identity", alpha = 0.7) +
  facet_grid(Year ~ income.type, scales = "free_y") +
  labs(title = "FDI Distribution by Income Type and Year", x = "FDI", y = "Frequency") +
  theme_minimal() +
  scale_fill_manual(values = c("skyblue", "lightgreen"), name = "Income Type")
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
high_income_data <- datafinal.i[datafinal.i$income.type == "high",]
low_income_data <- datafinal.i[datafinal.i$income.type == "low",]

# Calculate labor/population ratio for high and low-income data
high_income_data <- high_income_data %>%
  mutate(labor_population_ratio = labor / population)
low_income_data <- low_income_data %>%
  mutate(labor_population_ratio = labor / population)

# Create separate plots for high and low-income countries
plot_high_income <- ggplot(high_income_data, aes(x = labor_population_ratio, y = gdp, color = factor(time))) +
  geom_point() +
  facet_wrap(~country_name) +
  labs(title = "GDP vs Labor/Population Ratio for High-Income Countries by Year",
       x = "Labor/Population Ratio", y = "GDP",
       color = "Year") +
  theme_minimal()

plot_low_income <- ggplot(low_income_data, aes(x = labor_population_ratio, y = gdp, color = factor(time))) +
  geom_point() +
  facet_wrap(~country_name) +
  labs(title = "GDP vs Labor/Population Ratio for Low-Income Countries by Year",
       x = "Labor/Population Ratio", y = "GDP",
       color = "Year") +
  theme_minimal()

# Display both plots
plot_high_income
plot_low_income
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
avg_fdi <- datafinal.i %>%
  group_by(income.type, time) %>%
  summarise(avg_fdi = mean(fdi, na.rm = TRUE))

# Plotting average FDI for each year for high and low-income countries
ggplot(avg_fdi, aes(x = time, y = avg_fdi, color = income.type, group = income.type)) +
  geom_line() +
  labs(title = "Average FDI Over Time by Income Type",
       x = "Year", y = "Average FDI",
       color = "Income Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
####################################################################################################
# create a panel data object
pdata <- pdata.frame(datafinal.i, index = c("country.code", "time"))

#Run FE models

# run a fixed effect regression model for all countries
fe_model <- plm(fdi ~ log(gdp) + log(population) + log(labor) + democracy + factor(time), 
                data = pdata, model = "within")
summary(fe_model, vcov = function(x) vcovHC(x, type = 'sss'))

# run model for low-income countries
low_model <- plm(fdi ~ log(gdp) + log(population) + log(labor) + democracy + factor(time), 
                 data = pdata[pdata$income.type == 'low',], model = "within")
summary(low_model, vcov = function(x) vcovHC(x, type = 'sss'))

# run model by high-income countries
high_model <- plm(fdi ~ log(gdp) + log(population) + log(labor) + democracy + factor(time), 
                  data = pdata[pdata$income.type == 'high',], model = "within")
summary(high_model, vcov = function(x) vcovHC(x, type = 'sss'))

# MODEL DESCRIPTION
stargazer(fe_model, low_model, high_model, type = "text")

##################################################
##################################################

#Run FD models

#calculate additional metrics
fd.model <- datafinal.i %>%
  arrange(country.code, time) %>% # sort by country and year
  group_by(country.code) %>% # group by country
  mutate(fdi_diff = c(NA, diff(fdi)), # calculate the difference in fdi between current and previous year
         demo_diff = c(NA, diff(democracy))) # calculate the difference in democracy between current and previous year
fd.model <- fd.model %>% 
  arrange(country.code, time) %>% # sort by country and year
  group_by(country.code) %>% # group by country
  mutate(demo_diff2 = c(NA, diff(demo_diff)),
         lag_demo_diff = dplyr::lag(demo_diff,1),
         log_gdp_diff = c(NA, diff(log(gdp))),
         log_pop_diff = c(NA, diff(log(population))),
         log_labor_diff = c(NA, diff(log(labor))))

# create panel data
pdata2 <- pdata.frame(fd.model, index = c("country.code", "time"))

# run model for all countries
f.all <- as.formula("fdi_diff ~ demo_diff + dplyr::lag(demo_diff, 1) + dplyr::lag(demo_diff, 2) + dplyr::lag(demo_diff, 3) + dplyr::lead(demo_diff, 1) + dplyr::lead(demo_diff, 2) + log_gdp_diff + dplyr::lag(log_gdp_diff, 1) + dplyr::lag(log_gdp_diff, 2) + dplyr::lag(log_gdp_diff, 3) + log_pop_diff + dplyr::lag(log_pop_diff, 1) + dplyr::lag(log_pop_diff, 2) + dplyr::lag(log_pop_diff, 3) + log_labor_diff + dplyr::lag(log_labor_diff, 1) + dplyr::lag(log_labor_diff, 2) + dplyr::lag(log_labor_diff, 3)")
e.all <-plm(formula=f.all,data=pdata2,effect="twoways")
summary(e.all, vcov = function(x) vcovHC(x, type = 'sss'))

# run model for low-income countries
e.low <- plm(formula=f.all,data=pdata2[pdata2$income.type == 'low',],effect="twoways")
summary(e.low, vcov = function(x) vcovHC(x, type = 'sss'))

# run model for high-income countries
e.high <- plm(formula=f.all,data=pdata2[pdata2$income.type == 'high',],effect="twoways")
summary(e.high, vcov = function(x) vcovHC(x, type = 'sss'))

# MODEL DESCRIPTION
stargazer(e.all, e.low, e.high, type = "text")
##################################################

