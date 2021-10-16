# 有川湾のアマモ場
# 更新日：2021年10月14日
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
library(mgcv)
library(lemon)
library(vegan)
library(ggpubr)

# Ranking method
# Aが71〜100%
# Bが41〜70％
# Cが11〜40%
# Dが1〜11%
# Eが0%

font_add_google("Noto Sans","notosans")
# 図のフォントがからだったので、ここで修正した
# １）theme_set() をつかってデフォルトのフォントをかえる
# ２）ggplot() の theme() からとんとの指定をはずす。
theme_pubr(base_size = 10, base_family = "notosans") |> theme_set()
showtext_auto()

#read data file
afile = "~/Lab_Data/kabeyamam/arikawa_amamo_line.xlsx"
gfile = "~/Lab_Data/kabeyamam/arikawa_garbage.xlsx"

asheet=excel_sheets(afile) #xlsxのファイルのシート名
gsheet=excel_sheets(gfile) #xlsxのファイルのシート名

adata = tibble(asheet) |> 
  mutate(data = map(asheet, function(sheet) {
    read_xlsx(afile, sheet = sheet, col_types = "text") |> 
      select(!matches("[...]")) 
  })) |> unnest(data)


gdata = tibble(gsheet) |> 
  filter(str_detect(gsheet, "^[0-9]+$")) |> 
  mutate(data = map(gsheet, function(sheet){
    read_xlsx(gfile, sheet = sheet, col_types = "text") 
  })) |> unnest(data)


# 全角数字があるとこまる。
# 全角数字は stringi::stri_trans_nfkc() で半角に変換する。
adata = adata |> 
  mutate(across(matches("ライン"), as.numeric)) |> 
  mutate(month = ymd(asheet), .after = asheet) |> 
  mutate(month = floor_date(month) |> as_date()) |> 
  mutate(across(matches("距離"), ~str_extract(.x, "[0-9]+") |> as.numeric())) |> 
  select(month, matches("アマモ"), matches("距離")) |> 
  pivot_longer(!month,
               names_to = c(".value", "id"),
               names_pattern="(.).*(.)") |> 
  mutate(id = stringi::stri_trans_nfkc(id) |> as.numeric()) |> 
  rename(rank =　ア, distance = 距, line = id) |> 
  mutate(month = month.abb[month(month)]) |> 
  mutate(line = 5 * (line -1),
         rank = factor(rank),
         month_fct = factor(month,
                            levels = month.abb[c(5:12, 1:4)])) |> 
  drop_na()


gdata = gdata |> print(n = 540) |> 
  mutate(month = ym(day) |> as_date(),
         mass = as.numeric(Weight)) |> 
  select(month, place, type, mass)



ggplot(adata)+
  geom_tile(aes(x = line, y = distance, fill = rank)) +
  scale_x_continuous("Distance (m)",
                     breaks=seq(0, 50, by = 25),
                     limits = c(-5, 55))+
  scale_y_continuous("Distance (m)",
                     trans = "reverse",
                     breaks=seq(170, 100, by=-10),
                     limits = c(170, 100)) +
  scale_fill_viridis_d(name = "Coverage", direction = -1, option = "B") +
  facet_grid(cols=vars(month_fct))

library(mgcv)
# ocat() の場合、説明は整数に変換する
adata = adata |> mutate(rank2 = as.integer(rank))
K = adata |> pull(rank2) |> max()
ctrl = gam.control(nthreads = 5)

# Null model (no month effect)
mout0 = gam(rank2 ~ t2(line, distance, bs = c("ts", "ts"), k = c(10, 30)), 
            data = adata, family = ocat(R = K), 
            method = "ML", control = ctrl)

# Full model (month effect)
mout1 = gam(rank2 ~ t2(line, distance, by = month_fct, bs = c("ts", "ts"), k = c(10, 30)) + month_fct, 
            data = adata, family = ocat(R = K), 
            method = "ML", control = ctrl)

AIC(mout0, mout1)

# mout0のAICが最も低かったので、REMLで当てはめる
mout0 = gam(rank2 ~ t2(line, distance, bs = c("ts", "ts"), k = c(10, 30)), 
            data = adata, family = ocat(R = K), 
            method = "REML", control = ctrl)

pdata = adata |>  
  tidyr::expand(line = seq(min(line), max(line), length = 25),
                distance = seq(min(distance), max(distance), length = 25),
                month_fct = month.abb[5:8])

pmat = predict(mout0, newdata = pdata, type = "response")
pmat = pmat |> as_tibble()

pdata = bind_cols(pdata, pmat) |> 
  pivot_longer(cols = starts_with("V"))

pdata = pdata |> 
  mutate(rank = factor(name, levels = str_glue("V{1:5}"),  labels = LETTERS[1:5]))


labelit = function(x) {
  case_when(x == "A" ~ "A (>70%)",
            x == "B" ~ "B (>40%)",
            x == "C" ~ "C (>10%)",
            x == "D" ~ "D (>1%)",
            TRUE~ "E (0%)")
}

ggplot() + 
  geom_contour_filled(aes(x = line, y = distance, z = value, 
                          fill = after_stat(level)),
                      bins = 10, data = pdata) +
  geom_point(aes(x = line, y = distance), data = adata,  size = 0.5, color = "grey70") +
  scale_fill_viridis_d(name = "Probability", end = 0.9, option = "A") +
  scale_y_continuous("Along transect (m)", trans = "reverse", breaks = seq(166, 100, length = 4), 
                     limits = c(166, 100))+
  scale_x_continuous("Across transect (m)", breaks = seq(0, 50, by = 25), limits = c(0, 50)) +
  guides(fill = guide_legend(ncol = 3, byrow=F), color = "none") +
  facet_rep_wrap(facets = vars(rank), labeller = as_labeller(labelit)) +
  labs(title = "May to August 2021 seagrass coverage at Arikawa Bay, Nagasaki, Japan") +
  theme(strip.background = element_rect(color = NA),
        legend.position = c(0.82,0.25),
        legend.justification = c(0.5, 0.5),
        legend.background = element_blank())

wh = gnnlab::aseries(5)
plotname = "kabeyama_all.pdf"
ggsave(filename = plotname,
       width = wh[2],
       height = wh[1], units="mm")
image_read_pdf(plotname) |> 
  image_resize("1500x") |> 
  image_write(str_replace(plotname,"pdf","png"))


gdata = gdata |> 
  mutate(type = str_replace(type, "・", "/")) |> 
  mutate(month = month.abb[month(month)]) |> 
  mutate(place = factor(place),
         type= factor(type),
         month_fct = factor(month,
                            levels = month.abb[c(5:12, 1:4)])) 

area = adata |> 
  group_by(line, distance) |> 
  summarise(seagrass = sum(ifelse(str_detect(rank, "E"), 0, 1))) |> 
  ungroup() |> 
  mutate(seagrass = ifelse(seagrass > 0, 1, 0)) |> 
  mutate(sand = 1-seagrass) |> 
  summarise(across(c(seagrass, sand), sum)) |> 
  mutate(across(c(seagrass, sand), ~.x * 5 * 2))
area = area |> gather()

gdata |> 
  full_join(area, by = c("place" = "key")) |> ungroup() |> 
  group_by(place) |> 
  summarise(mass = 1000*sum(mass/value))


gdata2 = gdata |> 
  full_join(area, by = c("place" = "key")) |> ungroup() |> 
  mutate(group = ifelse(str_detect(type, "net|cloth"), "Fisheries debris", "Other")) |> 
  mutate(mass = 1000 * mass/value) |> 
  group_by(place, group) |> 
  summarise(mass = sum(mass)) |> 
  mutate(place = str_to_sentence(place)) 

ylabel = "'Density'~(g~m^{-2})"
ggplot(gdata2) + 
  geom_col(aes(x = group, y = mass, fill = place), position = position_dodge2()) +

    geom_label(aes(x = group, y = mass, label = sprintf("%0.1f~g~m^{-2}", mass)),
               parse = T,
               position = position_dodge2(width = 0.9),
               size = 5, family = "notosans", fontface = "bold",
              label.size = 0, fill = NA,
             vjust = 0, hjust = 0.5) +
  scale_fill_viridis_d(end = 0.9) +
  scale_x_discrete("Collected debris") +
  scale_y_continuous(parse(text = ylabel), limits = c(0, 60)) +
  labs(title = "May to August 2021, Arikawa Bay, Nagasaki, Japan") +
  theme(legend.position = c(1,1),
        legend.justification = c(1,1),
        legend.title = element_blank(),
        legend.background = element_blank())

wh = gnnlab::aseries(5)
plotname = "kabeyama_debris.pdf"
ggsave(filename = plotname,
       width = wh[2],
       height = wh[1], units="mm")
image_read_pdf(plotname) |> 
  image_resize("1500x") |> 
  image_write(str_replace(plotname,"pdf","png"))
