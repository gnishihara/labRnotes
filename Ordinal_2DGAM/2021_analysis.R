# 有川湾のアマモ場
# 2021年8月6日
# Greg
#Load packages
#CTRL + D 1行削除
#CTRL + ALT + ↑ or ↓　カーソル複製　
library(tidyverse)
library(lubridate)
library(gnnlab)
library(magick)
library(showtext)
library(readxl)
library(patchwork)

#plots
color=viridis::viridis(6)
font_add_google("Noto Sans JP","notosans-jp")
font_add_google("Noto Sans","notosans")
# 図のフォントがからだったので、ここで修正した
# １）theme_set() をつかってデフォルトのフォントをかえる
# ２）ggplot() の theme() からとんとの指定をはずす。
theme_set(text = element_text(family = "notosans-jp"))
showtext_auto()

#read data file
filename="~/Lab_Data/kabeyamam/arikawa_amamo_line.xlsx"
esheet=excel_sheets(filename) #xlsxのファイルのシート名


d1=read_xlsx(filename, sheet = esheet[1],
             col_types = "text")
d2=read_xlsx(filename, sheet = esheet[2],
             col_types = "text")
d3=read_xlsx(filename, sheet = esheet[3],
             col_types = "text")

#d1の処理はここから
d1=d1 |> select(matches("ライン|距離|時刻|アマモ"))
d1=d1 |> pivot_longer(cols=everything())

d1=d1 |> 
  mutate(line=str_extract(name,"[0-9]+$")) |> 
  mutate(line=as.numeric(line))
d1=d1 |> 
  mutate(name=str_remove(name,"[0-9]+$"))
d1=d1 |> pivot_wider(names_from = name, values_from = value) |> 
  unnest(everything())
d1=d1 |> 
  mutate(距離=str_extract(距離,"[0-9]+")) |> 
  mutate(距離=as.numeric(距離)) |> 
  mutate(アマモ=factor(アマモ))
d1b=d1 |> 
  mutate(アマモ=str_remove(アマモ,"orB"))

#d2の処理はここから
d2=d2 |> select(matches("ライン|距離|時刻|アマモ"))
d2=d2 |> pivot_longer(cols=everything())
d2=d2 |> 
  mutate(line=str_extract(name,"[0-9]+$")) |> 
  mutate(line=as.numeric(line))
d2=d2 |> 
  mutate(name=str_remove(name,"[0-9]+$"))
d2=d2 |> pivot_wider(names_from = name, values_from = value) |> 
  unnest(everything())
d2=d2 |> 
  mutate(距離=str_extract(距離,"[0-9]+")) |> 
  mutate(距離=as.numeric(距離)) |> 
  mutate(アマモ=factor(アマモ))
d2b=d2 |> 
  mutate(アマモ=str_remove(アマモ,"orC")) |> 
  mutate(アマモ=ifelse(str_detect(アマモ,"^T$"),NA,アマモ))

d3=d3 |> select(matches("ライン|距離|時刻|アマモ"))
d3=d3 |> pivot_longer(cols=everything())
d3=d3 |> 
  mutate(line=str_extract(name,"[0-9]+$")) |> 
  mutate(line=as.numeric(line))
d3=d3 |> 
  mutate(name=str_remove(name,"[0-9]+$"))
d3=d3 |> pivot_wider(names_from = name, values_from = value) |> 
  unnest(everything())
d3=d3 |> 
  mutate(距離=str_extract(距離,"[0-9]+")) |> 
  mutate(距離=as.numeric(距離)) |> 
  mutate(アマモ=factor(アマモ))




##########################################################
#plot

p1=ggplot()+
  geom_tile(aes(y=距離,
                x=line,
                fill=アマモ),
            data=d1b)+
  annotate(geom = "text",
           x=1,
           y=166,
           label="Seawall side",
           vjust=0,hjust=0,
           color="white")+
  annotate(geom = "text",
           x=6,
           y=166,
           label="Entrance to seagrass meadow",
           vjust=0,hjust=1,
           color="white")+
  
  scale_y_continuous(name="Distance (m)",
                     trans="reverse",
                     limits=c(170,100),
                     breaks = seq(170,100,
                                  by=-10))+
  scale_x_discrete(name="Transect")+
  scale_fill_manual(name="Coverage",
                    values = rev(color[-6]))

p2=ggplot()+
  geom_tile(aes(y=距離,
                x=line,
                fill=アマモ),
            data=d2b)+
  annotate(geom = "text",
           x=1,
           y=166,
           label="Seawall side",
           vjust=0,hjust=0,
           color="white")+
  annotate(geom = "text",
           x=11,
           y=166,
           label="Entrance to seagrass meadow",
           vjust=0,hjust=1,
           color="white")+
  
  scale_y_continuous(name="Distance (m)",
                     trans="reverse",
                     limits=c(170,100),
                     breaks = seq(170,100,
                                  by=-10))+
  scale_x_discrete(name="Transect")+
  scale_fill_manual(name="Coverage",
                    values = rev(color[-6]))

p3=ggplot()+
  geom_tile(aes(y=距離,
                x=line,
                fill=アマモ),
            data=d3)+
  annotate(geom = "text",
           x=1,
           y=166,
           label="Seawall side",
           vjust=0,hjust=0,
           color="white")+
  annotate(geom = "text",
           x=11,
           y=166,
           label="Entrance to seagrass meadow",
           vjust=0,hjust=1,
           color="white")+
  
  scale_y_continuous(name="Distance (m)",
                     trans="reverse",
                     limits=c(170,100),
                     breaks = seq(170,100,
                                  by=-10))+
  scale_x_discrete(name="Transect")+
  scale_fill_manual(name="Coverage",
                    values = rev(color[-6]))

p1+p2
p1/p2/p3

#d1b+d2b
d1b=d1b |> mutate(month=5)
d2b=d2b |> mutate(month=6)
d3=d3 |> mutate(month=7)

dall=bind_rows(d1b,d2b,d3) |> 
  mutate(month=factor(month,
                      levels=1:12,
                      labels=month.abb[1:12]))

ggplot(dall)+
  geom_tile(aes(x=line,y=距離,fill=アマモ))+
  scale_y_continuous("Distance (m)",
                     trans = "reverse",
                     breaks=seq(170,100,by=-10),
                     limits = c(170,100))+
  scale_fill_manual(values = rev(color[-6]))+
  facet_grid(rows=vars(month))

wh=aseries(6)
plotname="kabeyama_all.pdf"
ggsave(filename = plotname,
       width=wh[2],
       height = wh[1],units="mm")

image_read_pdf(plotname) |> 
  image_resize("500x") |> 
  image_write(str_replace(plotname,"pdf","png"))

# analysis
 
dall02 = dall |> filter(str_detect(month, "May", negate = TRUE))

dall02 =
  dall02 |> 
  mutate(line2 = sprintf("L%02d", line)) |> 
  mutate(line = line * 5) 


dall02 = dall02 |> mutate(amamo = as.numeric(as.factor(アマモ)))

dall02 = dall02 %>% 
  select(line, distance = 距離, amamo = アマモ, month) %>% 
  mutate(amamo = factor(amamo, ordered = T))

################################################################################
library(brms)
library(tidybayes)
library(ggnewscale)
mgcv::t2()
seed = 2020
chains = 4
cores = chains
ctrl = list(adapt_delta = 0.99)
bmodel1 = bf(amamo ~ t2(line, distance, bs = "ts", k = c(10, 30), by = month) + month) + cumulative()

get_prior(bmodel1, data = dall02)
prior = c(prior(normal(0, 2), class = b),
          prior(student_t(3, 0, 2.5), class = Intercept),
          prior(student_t(3, 0, 2.5), class = sds))

bout1 = brm(bmodel1, data = dall02,
            prior = prior,
            seed = seed,
            chains = chains, 
            cores = cores,
            control = ctrl)

## Posterior predictive check
pp_check(bout1)

pdata = dall02 %>% 
  tidyr::expand(line = seq(min(line), max(line), length = 25),
                distance = seq(min(distance), max(distance), length = 25),
                month = c("Jun", "Jul"))

pdata = pdata %>% add_fitted_draws(model = bout1)

pdata = pdata %>% mutate(.category = factor(.category, levels = 1:5, labels = LETTERS[1:5]))

pdata = pdata %>% ungroup() %>% mutate(amamo = factor(.category, ordered = T))

pdata2 = pdata %>% group_by(line, distance, month, amamo) %>% 
  mean_hdci(.value)

ggplot(pdata2) + 
  geom_tile(aes(x = line, y = distance), data = dall02) +
  geom_contour(aes(x = line, 
                          y = distance, 
                          z = .value,
                   color = after_stat(level)), 
                      bins = 10, size = 1) + 
  scale_color_viridis_c("Probability", direction = -1) + 
  scale_y_continuous("Distance (m)",
                     trans = "reverse",
                     breaks=seq(170,100,by=-10),
                     limits = c(170,100))+
  scale_x_continuous("Distance (m)",
                     breaks = seq(0, 50, by = 25),
                     limits = c(0, 50)) +
  facet_grid(cols = vars(amamo),
             rows = vars(month),as.table = FALSE) +
  theme(legend.background = element_blank(),
        legend.position = c(0.0, 0.5),
        legend.justification = c(0, 0.5),
        legend.direction = "horizontal")


wh=aseries(5)
plotname="kabeyama_model.pdf"
ggsave(filename = plotname,
       width=wh[2],
       height = wh[1],units="mm")

image_read_pdf(plotname) |> 
  image_resize("1000x") |> 
  image_write(str_replace(plotname,"pdf","png"))











































