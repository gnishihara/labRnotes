# Lab Challenge 01

library(tidyverse)
library(lubridate)
library(gnnlab) 
library(ggpubr)
library(lemon)

d1 = tibble(files = dir("~/Lab_Data/kawatea/Oxygen/", full = TRUE)) |> 
  filter(!str_detect(files, "(tainoura)|(mushima)|(calibration)|(edge)|(sand)")) |> 
  filter(str_detect(files, "(0m)|(surface)"))
d1_periods = "~/Lab_Data/kawatea/調査期間 - 調査期間.csv" |> read_csv()

d1 = d1 |> mutate(data = map(files, read_onset))

d1 = d1 |> mutate(files = basename(files)) |> 
  separate(files, c("logger", "id", "location", "position", "date", "extension")) |> 
  select(id, location, position, data) |> 
  unnest(data)

d1_interval = d1_periods |> 
  mutate(interval = interval(start_date, end_date)) |> 
  pull(interval) |> as.list()

d1 = d1 |> filter(datetime %within% d1_interval)

d1 = d1 |> mutate(date = as_date(datetime)) |> 
  group_by(id, location, position, date) |> 
  filter(near(n(), 144))

d2 = d1 |> group_by(location, position, date) |> 
  summarise(across(temperature, list(mean = mean, min = min, max = max, var = var)))

d2 = d2 |> filter(!(date %within% interval("2020-12-15", "2021-01-05")))

d2 = d2 |> mutate(location = factor(location, 
                                    levels = c("arikawaamamo", "arikawagaramo"),
                                    labels = c("Zostera", "Sargassum")))

d2 = d2 |> arrange(date, location, position) |> 
  mutate(year = year(date),
         month = month(date)) |> 
  mutate(year = factor(year, levels = 2017:2021),
         month = factor(month, levels = c(4:12, 1:3),
                        labels = month.abb[c(4:12, 1:3)])) |> 
  ungroup()

d2_jan = d2 |> filter(str_detect(month, "Jan")) 

d2_jan |>   ggplot() + 
  geom_boxplot(aes(x = year, y = temperature_mean, fill = year))  +
  facet_grid(cols = vars(position),
             rows = vars(location))


d2_jan |>   ggplot() + 
  geom_histogram(aes(x = temperature_mean), binwidth = 0.05)  +
  facet_grid(cols = vars(position, location),
             rows = vars(year))



################################################################################
library(emmeans) # for emmeans()
library(car) # for leveneTest()
library(statmod) # for qresiduals()
library(nlme)

d2_jan_0m = d2_jan |> filter(str_detect(position, "0m"))
d2_jan_sf = d2_jan |> filter(!str_detect(position, "0m"))

# 一般化線形モデル
# 0m 
m0 = glm(temperature_mean ~ 1,               data = d2_jan_0m, family = gaussian()) 
m1 = glm(temperature_mean ~ year,            data = d2_jan_0m, family = gaussian()) 
m2 = glm(temperature_mean ~ year * location, data = d2_jan_0m, family = gaussian()) 

anova(m0, m1, m2, test = "F")
AIC(m0, m1, m2)

d2_jan_0m = d2_jan_0m |> 
  mutate(resids = residuals(m2),
         fit     = fitted(m2))

p1 = d2_jan_0m |> 
  ggplot() + 
  geom_qq(aes(sample = resids)) +
  geom_qq_line(aes(sample = resids)) +
  scale_y_continuous(limits = c(-4,4)) +
  coord_equal()

p2 = d2_jan_0m |> 
  ggplot() +
  geom_histogram(aes(x = resids, y = ..density..), binwidth = 0.05) +
  geom_function(fun = dnorm, color = "red", size = 2) +
  scale_x_continuous(limits = c(-4, 4)) +
  coord_flip()

ggarrange(p1,p2, ncol = 2)

d2_jan_0m |> 
  ggplot() + 
  geom_boxplot(aes(x = as.factor(fit), y = resids))

d2_jan_0m |> 
  ggplot() + 
  geom_point(aes(x = as.factor(fit), y = resids))

d2_jan_0m |> 
  mutate(stresiduals = sqrt(abs(resids/ sd(resids)))) |> 
  ggplot() + 
  geom_boxplot(aes(x = as.factor(fit), y = stresiduals))+
  geom_point(aes(x = as.factor(fit), y = stresiduals),
             position = position_jitter(0.1))

emmeans(m1, specs = pairwise ~ year, adjust = "tukey")
emmeans(m2, specs = pairwise ~ location|year, adjust = "tukey")
emmeans(m2, specs = pairwise ~ year|location, adjust = "tukey")

summary(m1)



# Gamma 分布の場合

# 0m 
m0 = glm(temperature_mean ~ 1,               data = d2_jan_0m, family = Gamma("log")) 
m1 = glm(temperature_mean ~ year,            data = d2_jan_0m, family = Gamma("log")) 
m2 = glm(temperature_mean ~ year * location, data = d2_jan_0m, family = Gamma("log")) 

anova(m0, m1, m2, test = "Chi")
AIC(m0, m1, m2)

d2_jan_0m = d2_jan_0m |> 
  mutate(qresids = qresiduals(m2),
         fit     = fitted(m2))

p1 = d2_jan_0m |> 
  ggplot() + 
  geom_qq(aes(sample = qresids)) +
  geom_qq_line(aes(sample = qresids)) +
  scale_y_continuous(limits = c(-4,4)) +
  coord_equal()

p2 = d2_jan_0m |> 
  ggplot() +
  geom_histogram(aes(x = qresids, y = ..density..), binwidth = 0.05) +
  geom_function(fun = dnorm, color = "red", size = 2) +
  scale_x_continuous(limits = c(-4, 4)) +
  coord_flip()

ggarrange(p1,p2, ncol = 2)

d2_jan_0m |> 
  ggplot() + 
  geom_boxplot(aes(x = as.factor(fit), y = qresids))

d2_jan_0m |> 
  mutate(qstresiduals = sqrt(abs(qresids/ sd(qresids)))) |> 
  ggplot() + 
  geom_boxplot(aes(x = (fit), y = qstresiduals, 
                   group = as.factor(fit)))+
  geom_point(aes(x = (fit), y = qstresiduals),
             position = position_jitter(0.1)) +
  geom_smooth(aes(x = (fit), y = qstresiduals))


emmeans(m1, specs = pairwise ~ year, adjust = "tukey")
emmeans(m2, specs = pairwise ~ location|year, adjust = "tukey")
emmeans(m2, specs = pairwise ~ year|location, adjust = "tukey")

summary(m1)


################################################################################
g1 = gls(temperature_mean ~ year, 
         weights = varIdent(~year), data = d2_jan_0m)


emmeans(g1, specs = pairwise ~ year, adjust = "tukey")

d2_jan_0m = d2_jan_0m |> 
  mutate(resids = residuals(g1, type = "normalized"),
         fit     = fitted(g1))

p1 = d2_jan_0m |> 
  ggplot() + 
  geom_qq(aes(sample = resids)) +
  geom_qq_line(aes(sample = resids)) +
  scale_y_continuous(limits = c(-4,4)) +
  coord_equal()

p2 = d2_jan_0m |> 
  ggplot() +
  geom_histogram(aes(x = resids, y = ..density..), binwidth = 0.05) +
  geom_function(fun = dnorm, color = "red", size = 2) +
  scale_x_continuous(limits = c(-4, 4)) +
  coord_flip()

ggarrange(p1,p2, ncol = 2)


################################################################################

d2_jan_0m_no2018 = d2_jan_0m |> filter(!str_detect(year, "2018"))

g2 = gls(temperature_mean ~ year*location, 
         weights = varIdent(~year), data = d2_jan_0m_no2018)


emmeans(g2, specs = pairwise ~ year|location, adjust = "tukey")

d2_jan_0m_no2018 = d2_jan_0m_no2018 |> 
  mutate(resids = residuals(g2, type = "normalized"),
         fit     = fitted(g2))

p1 = d2_jan_0m_no2018 |> 
  ggplot() + 
  geom_qq(aes(sample = resids)) +
  geom_qq_line(aes(sample = resids)) +
  scale_y_continuous(limits = c(-4,4)) +
  coord_equal()

p2 = d2_jan_0m_no2018 |> 
  ggplot() +
  geom_histogram(aes(x = resids, y = ..density..), binwidth = 0.05) +
  geom_function(fun = dnorm, color = "red", size = 2) +
  scale_x_continuous(limits = c(-4, 4)) +
  coord_flip()

ggarrange(p1,p2, ncol = 2)

summary(g2)

#############################################################################
## ベイズ法

library(rstanarm)

# 0m 
m0 = stan_glm(temperature_mean ~ 1,               
              data = d2_jan_0m, family = Gamma("log"),
              cores = 4) 
m1 = stan_glm(temperature_mean ~ year,            
              data = d2_jan_0m, family = Gamma("log"),
              cores = 4) 
m2 = stan_glm(temperature_mean ~ year * location, 
              data = d2_jan_0m, family = Gamma("log"),
              cores = 4) 

loo0 = loo(m0)
loo1 = loo(m1)
loo2 = loo(m2)

loo_compare(loo0, loo1, loo2) |> 
  as_tibble(rownames = "model")

d2_jan_0m = d2_jan_0m |> 
  mutate(resids = residuals(m1),
         fit     = fitted(m1))

p1 = d2_jan_0m |> 
  ggplot() + 
  geom_qq(aes(sample = resids)) +
  geom_qq_line(aes(sample = resids)) +
  scale_y_continuous(limits = c(-4,4)) +
  coord_equal()

p2 = d2_jan_0m |> 
  ggplot() +
  geom_histogram(aes(x = resids, y = ..density..), binwidth = 0.05) +
  geom_function(fun = dnorm, color = "red", size = 2) +
  scale_x_continuous(limits = c(-4, 4)) +
  coord_flip()

ggarrange(p1,p2, ncol = 2)

d2_jan_0m |> 
  ggplot() + 
  geom_boxplot(aes(x = as.factor(fit), y = resids))

d2_jan_0m |> 
  mutate(stresiduals = sqrt(abs(resids/ sd(resids)))) |> 
  ggplot() + 
  geom_boxplot(aes(x = (fit), y = stresiduals, 
                   group = as.factor(fit)))+
  geom_point(aes(x = (fit), y = stresiduals),
             position = position_jitter(0.1)) +
  geom_smooth(aes(x = (fit), y = stresiduals))


emmeans(m1, specs = pairwise ~ year, adjust = "tukey")

emmeans(m2, specs = pairwise ~ location|year, adjust = "tukey")
emmeans(m2, specs = pairwise ~ year|location, adjust = "tukey")

summary(m1)
