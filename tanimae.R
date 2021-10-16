# 2021 SEP 30
library(tidyverse)
library(lubridate)
library(vegan)
library(ggvegan)
library(ggpubr)
library(ggrepel)
library(showtext)
library(magick)

font_add_google("Noto Sans","notosans")
# 図のフォントがからだったので、ここで修正した
# １）theme_set() をつかってデフォルトのフォントをかえる
# ２）ggplot() の theme() からとんとの指定をはずす。
theme_pubr(base_size = 10, base_family = "notosans") |> theme_set()
showtext_auto()


rendermath = function(x, file = "equation.tex", save_png = FALSE) {
  # Cool script to render latex math in to Noto Sans equations.
  list(
    l1 = '\\documentclass{standalone}',
    l2 = '\\usepackage{amsmath}',
    l3 = '\\usepackage[sfdefault,subscriptcorrection]{notomath}',
    l4 =  '\\begin{document}',
    l5 =  '\\(\\displaystyle{',
    l6 = x,
    l7 = '}\\)',
    l8 = '\\end{document}') |> 
    readr::write_lines(file)
  pdffile = str_replace(file, "tex", "pdf")
  pngfile = str_replace(file, "tex", "png")
  tinytex::xelatex(file)
  if(save_png) {
    image_read_pdf(pdfile, density = 600) |> 
      image_flatten() |> 
      image_border("white", "10x10") |> 
      image_write(pngfile)
  }
}


# 地図. ------------------------------------------------------------------------


library(ggspatial) # North Arrow の関数
library(sfheaders)
library(sf)

# 有川湾 #######################################################################

shpfile  = "~/Lab_Data/Nagasaki_map_data/N03-20210101_42_GML/"
shpfile2 = "~/Lab_Data/Nagasaki_map_data/W05-07_42_GML/W05-07_42-g_RiverNode.shp"
shpfile3 = "~/Lab_Data/Nagasaki_map_data/W05-07_42_GML/W05-07_42-g_Stream.shp"

nagasaki = st_read(shpfile,  as_tibble = TRUE)
crs_original = st_crs(nagasaki)
# plot longitude & latitude.

xymins = c(32.9870, 129.1170)
xymaxs = c(32.990, 129.1205)
names(xymins) = c("ymin", "xmin")
names(xymaxs) = c("ymax", "xmax")
box = c(xymins, xymaxs)
bbox = st_bbox(box, crs = crs_original)

# water    = st_read(shpfile2, as_tibble = TRUE, crs = crs_original)
# stream   = st_read(shpfile3, as_tibble = TRUE, crs = crs_original)
# water    = st_crop(water, bbox)
# stream   = st_crop(stream, bbox)

nagasaki2 = st_crop(nagasaki, bbox)
tib = tibble(station = c(1,2,3,4,5,6,7),
             long = c(129.118025, 129.118382, 129.119063, 129.118516, 129.1189, 129.119095, 129.120058),
             lat = c(32.988, 32.988, 32.98783, 32.9884, 32.98855, 32.9892, 32.98753)) |> 
  mutate(station = factor(station))
# tib 　　= st_as_sf(tib, coords = c("long", "lat"), crs = crs_original)


p0 = ggplot() + 
  geom_sf(data = nagasaki2, size = 0, alpha = 1, color = "white") +
  geom_point(aes(long, lat, color =station), data = tib, size = 5) +
  geom_label_repel(aes(long, lat, label = station), data = tib, size = 6) +
  scale_color_viridis_d(end = 0.9) +
  guides(color = "none") +
  annotation_north_arrow(style = north_arrow_minimal(text_size = 30), 
                         height = unit(30, "mm"), width = unit(30, "mm"),
                         location = "top") +
  annotation_scale(location = "tr", height = unit(5, "mm"),
                   text_family = "notosans",
                   text_cex = 1) +
  coord_sf(expand = F) +
  labs(title = "Station location") +
  theme(legend.position = "right",
        legend.title = element_blank(),
        legend.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank())
p0
# 
# pdfname = "arikawa_bay.pdf"
# pngname = str_replace(pdfname, "pdf", "png")
# ggsave(pdfname, height = 300, width = 200, units = "mm")
# img = image_read_pdf(pdfname, density = 300)
# img |> image_trim() |> image_border("white", "20x20") |> image_resize("500x") |> 
#  image_write(pngname)


fnames = dir("~/Lab_Data/tanimaes/seaweed_data/rds_write_out/", full=T)

plaster = str_subset(fnames, "plaster") |> read_rds() 
# quadrat = str_subset(fnames, "quadrat") |> read_rds() 
species = str_subset(fnames, "species") |> read_rds() 
temperature = str_subset(fnames, "temp") |> read_rds() 
sediment = str_subset(fnames, "sediment") |> read_rds() 


sediment = sediment |> select(-datetime) |> 
  pivot_longer(cols = matches("_day")) |> 
  group_by(year, month, station) |> 
  summarise(sediment = mean(value, na.rm=T)) |> print(n = Inf)

temperature = temperature |> 
  mutate(year = year(datetime),
         month = month(datetime)) |> 
  group_by(year, month, station) |> 
  summarise(across(temp, list(mean = mean, min = min, max = max, var = var))) 

plaster = plaster |> select(-c(expID, datetime)) 

edata = 
  full_join(sediment, temperature) |> 
  full_join(plaster) |> 
  group_by(station) |> 
  summarise(across(matches("temp|pla|sediment"), ~mean(.x, na.rm=T)))

species2 = species |> 
  select(date, station, species_j, existence) |> 
  pivot_wider(names_from = species_j, 
              values_from = existence) 

species2 = species2 |> arrange(station, date)
station = species2 |> pull(station)
month = species2 |> mutate(month = month(date)) |> pull(month)
# 調査地点全域
# Gamma 
gamma = 
  species |> 
  filter(existence > 0) |> 
  select(species_j, existence) |> 
  distinct() |> 
  summarise(gamma = sum(existence)) 
alpha = 
  species |> 
  filter(existence > 0) |> 
  group_by(station) |> select(species_j, existence) |> 
  distinct() |> 
  summarise(alpha = sum(existence)) 
bind_cols(alpha, gamma) |> 
  mutate(beta = gamma / alpha, proportional = 1 - alpha / gamma)

# ステーション間の類似度
# 類似度が高い場合は 0, 類似度が低い場合は 1
X = species |> 
  group_by(station, species_j) |> 
  summarise(existence = sum(existence)) |> 
  mutate(existence = ifelse(existence > 0, 1, 0)) |> ungroup() |> 
  pivot_wider(names_from = species_j, values_from = existence) |> 
  select(-station)
X = as.matrix(X)
rownames(X) = str_glue("st{1:7}")
X =betadiver(X, method = "sim")
X |> round(2)

Z = X |> as.matrix()  
Z[lower.tri(Z)] = NA
Z = Z |> as_tibble(rownames = "station") |> 
  pivot_longer(matches("st[1-7]")) |> 
  mutate(across(c(station, name),
                ~str_replace(.x, "st", "Station "))) |> 
  drop_na()


flabel = "beta*'-diversity'"
plot1 = ggplot(Z) +
  geom_tile(aes(x = station, y = name, fill = value)) +
  geom_text(aes(x = station, y = name, 
                label = sprintf("%0.3f", value)),
            color = ifelse(near(Z$value, 0), "white", "black"),
            family = "notosans",
            fontface = "bold",
            size = 6) +
  scale_fill_viridis_c(parse(text = flabel),na.value = "white") +

  guides(fill = guide_colorbar(title.position = "top", 
                               label.position = "bottom",
                               barwidth = grid::unit(50, units="mm"),
                               barheight = grid::unit(10, units = "mm"))) +
  theme(axis.title = element_blank(),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 15),
        legend.position = c(0.75, 0.25),
        legend.justification = c(0.5, 0.5),
        legend.direction = "horizontal",
        legend.background = element_blank())
plot1
#########################################
divers = bind_cols(alpha, gamma) |> 
  summarise(alpha_m = mean(alpha),
            alpha_s = sd(alpha),
            gamma = first(gamma))
  
RS = species2 |> select(!c(date, station)) |> rowSums()
Y = species2 |> select(!c(date, station)) |> 
  mutate(across(everything(), ~sqrt(.x / RS)))

X = tibble(station, month) |> full_join(edata)
r1 = rda(Y ~ temp_mean + pla_100g_day + month, data = X)

RsquareAdj(r1)$r.squared 
anova(r1, by = "terms", permutations = 99)

xlabel = "RDA[1]"
ylabel = "RDA[2]"
r1d_s1 = fortify(r1, axes = 1:2, scaling = 1)
r1d_s2 = fortify(r1, axes = 1:2, scaling = 2)
fortify.cca()
calculate_equilibrium = function(X) {
  # vegan scales output with a constant.
  p = length(X$CA$eig)
  tot = sum(X$CA$eig)
  n = nrow(X$CA$u)
  sqrt(2 / p) * ((n-1)*tot)^0.25
}
sites1 = r1d_s1 |> filter(str_detect(Score, "sites")) |> mutate(station = X$station)
sites2 = r1d_s2 |> filter(str_detect(Score, "sites")) |> mutate(station = X$station)
constraints1 = r1d_s1 |> filter(str_detect(Score, "constraints")) |> mutate(station = X$station)
constraints2 = r1d_s2 |> filter(str_detect(Score, "constraints")) |> mutate(station = X$station)
constraints1 = constraints1 |> group_by(station) |> summarise(across(c(RDA1,RDA2), mean))
constraints2 = constraints2 |> group_by(station) |> summarise(across(c(RDA1,RDA2), mean))
r0 = calculate_equilibrium(r1)

labelsize = 3

p1 = ggplot() +
  ggforce::geom_circle(aes(x0 = 0, y0 = 0, r = r0), color = "grey50") +
  geom_point(aes(x = RDA1, y = RDA2, color = station), data = sites1, alpha = 0.5) +
  geom_point(aes(x = RDA1, y = RDA2, color = station), data = constraints1, size = 3) +
  geom_segment(aes(x = 0, y = 0, xend = RDA1, yend = RDA2), data = filter(r1d_s1, str_detect(Score, "biplot")),
               arrow = arrow(15, unit(3, "mm"))) +
  geom_text(aes(x = RDA1/sqrt(RDA1^2 + RDA2^2)*r0*1.2, 
                y = RDA2/sqrt(RDA1^2 + RDA2^2)*r0*1.2,
                label = "Water temperature"), 
            size = labelsize, family = "notosans",
            data = filter(r1d_s1, str_detect(Label, "temp"))) +
  geom_text(aes(x = RDA1/sqrt(RDA1^2 + RDA2^2)*r0*1.2,
                y = RDA2/sqrt(RDA1^2 + RDA2^2)*r0*1.2,
                label = "Plaster dissolution rate"),
            size = labelsize, family = "notosans",
            data = filter(r1d_s1, str_detect(Label, "pla"))) +
  geom_text(aes(x = RDA1/sqrt(RDA1^2 + RDA2^2)*r0*1.2,
                y = RDA2/sqrt(RDA1^2 + RDA2^2)*r0*1.2,
                label = "Month"), 
            size = labelsize, family = "notosans",
            data = filter(r1d_s1, str_detect(Label, "month"))) +
  scale_color_viridis_d(end = 0.9) + guides(color = "none") + 
  scale_x_continuous(parse(text = xlabel)) +
  scale_y_continuous(parse(text = ylabel)) + 
  coord_equal(xlim = c(-0.6, 0.6), ylim = c(-0.6, 0.6)) +
  labs(title = "Distance biplot (scaling = 1)") 
p2 = ggplot() +
  geom_point(aes(x = RDA1, y = RDA2, color = station), data = sites2, alpha = 0.5) +
  geom_point(aes(x = RDA1, y = RDA2, color = station), data = constraints2, size = 3) +
  geom_segment(aes(x = 0, y = 0,　xend = RDA1, yend = RDA2), data = filter(r1d_s2, str_detect(Score, "biplot")),
               arrow = arrow(15, unit(3, "mm"))) +
  geom_text(aes(x = RDA1/sqrt(RDA1^2 + RDA2^2)*0.8, 
                y = RDA2/sqrt(RDA1^2 + RDA2^2)*0.8,
                label = "Water temperature"), 
            size = labelsize, family = "notosans",
            data = filter(r1d_s2, str_detect(Label, "temp"))) +
  geom_text(aes(x = RDA1/sqrt(RDA1^2 + RDA2^2)*1.1,
                y = RDA2/sqrt(RDA1^2 + RDA2^2)*1.1,
                label = "Plaster dissolution rate"),
            size = labelsize, family = "notosans",
            data = filter(r1d_s2, str_detect(Label, "pla"))) +
  geom_text(aes(x = RDA1/sqrt(RDA1^2 + RDA2^2)*0.9,
                y = RDA2/sqrt(RDA1^2 + RDA2^2)*1.2,
                label = "Month"), 
            size = labelsize, family = "notosans",
            data = filter(r1d_s2, str_detect(Label, "month"))) +
  scale_color_viridis_d("Station", end = 0.9) + 
  scale_x_continuous(parse(text = xlabel)) +
  scale_y_continuous(parse(text = ylabel)) + 
  guides(color = guide_legend(nrow = 1))+
  coord_equal(xlim = c(-1, 1), 
              ylim = c(-1, 1)) +
  labs(title = "Correlation biplot (scaling = 2)") + 
  theme(legend.position = "top",
        legend.background = element_blank())

ggpubr::ggarrange(p0,p2,align = "hv", nrow = 1, common.legend = T)

wh = gnnlab::aseries(5)
ggsave("tanimae.pdf", width = wh[2], height = wh[1], units = "mm")
txt1 = "Arikawa Bay, Nagasaki, Japan"
txt2 = sprintf("\\gamma\\text{-diversity} = %0.0f;~\\overline{\\alpha}\\text{-diversity} = %0.1f \\pm %0.1f", divers$gamma, divers$alpha_m, divers$alpha_s)
rendermath(txt2, "tanimae_math.tex")

img = image_read_pdf("tanimae.pdf", density = 600) 
img2 = img |> image_crop("x200") |>  image_trim() |> image_border("white", "20x20")    # Guide
img1 = img |> image_crop("+0+200") |> image_trim()  # Main image
i1wh = img1 |> image_info()
img3 = image_read_pdf("tanimae_math.pdf", density = 600)
img3 = img3 |> image_trim() |> image_border("white", "20x20")
img23 = image_append(c(img2, img3), stack = T) |> image_border("white", "50x50")
i23wh = img23 |> image_info()
imgb = image_blank(i1wh$width - i23wh$width, i23wh$height, "white")
img23b = image_append(c(imgb, img23))

image_append(c(img23b,img1), stack = T) |> 
  image_trim() |> 
  image_border("white", "0x50") |>
  image_annotate(txt1, font = "noto sans", size = 2*72) |> 
  image_trim() |> 
  image_border("white", "20x20") |>
  image_write("tanimae.png")

















