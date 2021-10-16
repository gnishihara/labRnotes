Sys.setlocale("LC_TIME", "en_US.UTF-8") # This is to set the server time locate to en_US.UTF-8

library(tidyverse)
library(lubridate)
library(magick)
library(showtext)
library(ggpubr)
font_add_google("Noto Sans","notosans")
# font_add_google("Noto Sans JP","notosans")
# 図のフォントがからだったので、ここで修正した
# １）theme_set() をつかってデフォルトのフォントをかえる
# ２）ggplot() の theme() からとんとの指定をはずす。
theme_pubr(base_size = 10, base_family = "notosans") |> theme_set()
showtext_auto()

library(ggspatial) # North Arrow の関数
library(sfheaders)
library(sf)

# demfile = dir("~/Lab_Data/Japan_map_data/DEM/", 
#               pattern = "(.*)4929-42(.*).xml", full = TRUE) |> print()
# 
# dem = tibble(demfile) |> mutate(data = map(demfile, fgdr::read_fgd_dem, resolution = 10, return_class = "stars")) 
# dem |> print(n=40)
# 
# z = dem |> slice(1:7) |> pull(data)
# x = do.call(st_mosaic, z)
# x = st_transform(x, "WGS84")
# ggplot() + geom_stars(data = x)
data = read_csv("~/Lab_Data/matsumuro/longlat_info.csv", locale = locale(encoding = "sjis"))
data = data |> 
  rename(coverage = matches("cover")) |> 
  mutate(eelgrass = ifelse(coverage > 0, T, F))
data = data |> 
  mutate(rank = 5) |> 
  mutate(rank = ifelse(coverage >5,  4, rank)) |> 
  mutate(rank = ifelse(coverage >25, 3, rank)) |> 
  mutate(rank = ifelse(coverage >40, 2, rank)) |> 
  mutate(rank = ifelse(coverage >95, 1, rank)) |> 
  mutate(rank = factor(rank, levels = 1:5, labels = LETTERS[1:5]))

# dplyr::case_when() の使い方
data = data |> mutate(rank = case_when(coverage > 95 ~ 1,
                                       coverage > 40 ~ 2,
                                       coverage > 25 ~ 3,
                                       coverage > 5 ~ 4,
                                       TRUE ~ 5)) |> 
  mutate(rank = factor(rank, levels = 1:5, labels = LETTERS[1:5]))
data = data |> mutate(rank = case_when(coverage > 70 ~ 1,
                                       coverage > 40 ~ 2,
                                       coverage > 10 ~ 3,
                                       coverage >  1~ 4,
                                       TRUE ~ 5)) |> 
  mutate(rank = factor(rank, levels = 1:5, labels = LETTERS[1:5]))

# gsi = tibble(xml = dir("~/Lab_Data/Japan_map_data/FG/", full.names = TRUE)) |>
#   mutate(fname = basename(xml)) |>
#   separate(fname, into = c("fg", "gml", "id", "type", "date", "num")) |>
#   filter(str_detect(type, "WA")) |>  # map type.
#   filter(str_detect(id, "492926|492936")) # gml id.
# 
# try_read_fgd = possibly(fgdr::read_fgd, NULL)
# out = gsi |> mutate(data = map(xml, try_read_fgd)) |> pull(data) |> bind_rows()



# このシェープファイルは国土交通省のサイトからです。
shpfile  = "~/Lab_Data/Nagasaki_map_data/N03-20210101_42_GML/"
shpfile2 = "~/Lab_Data/Nagasaki_map_data/W05-07_42_GML/W05-07_42-g_RiverNode.shp"
shpfile3 = "~/Lab_Data/Nagasaki_map_data/W05-07_42_GML/W05-07_42-g_Stream.shp"
shpfile4 = "~/Lab_Data/Nagasaki_map_data/L03/"

nagasaki = st_read(shpfile,  as_tibble = TRUE)
crs_original = st_crs(nagasaki)

water    = st_read(shpfile2, as_tibble = TRUE, crs = crs_original)
stream   = st_read(shpfile3, as_tibble = TRUE, crs = crs_original)
land     = st_read(shpfile4, as_tibble = TRUE, options = "ENCODING=CP932")

# 形上湾周辺
lonlimits = c(129.779262, 129.807706)
latlimits = c(32.897657, 32.955917)

box = c(xmin = lonlimits[1], ymin = latlimits[1],
        xmax = lonlimits[2], ymax = latlimits[2])

box = st_bbox(box, crs = crs_original) |> st_as_sfc(tmp)

water    = st_crop(water, box)
stream   = st_crop(stream, box)
land     = st_transform(land, crs = crs_original)
land     = st_crop(land, box)
data 　　= st_as_sf(data, coords = c("long", "lat"), crs = crs_original)


# labels は国土交通省のサイトからです。
# https://nlftp.mlit.go.jp/ksj/gml/codelist/LandUseCd-09.html
# 海水域が最後の因子になるように、並べている。
# land = land |> 
#   mutate(land = factor(`土地利用種`,
#                        levels = c("0100", "0200", "0500",
#                                   "0600", "0700", "0901",
#                                   "0902", "1000", "1100",
#                                   "1600", "1400", "1500"),
#                        labels = c("田", "その他農地", "森林",
#                                   "荒地", "建物用地", "道路",
#                                   "鉄道", "その他の用地", "河川地および潮沼",
#                                   "ゴルフ場", "海浜", "海水域")))

land = land |> 
  mutate(land = factor(`土地利用種`,
                       levels = c("0100", "0200", "0500",
                                  "0600", "0700", "0901",
                                  "0902", "1000", "1100",
                                  "1600", "1400", "1500"),
                       labels = c("Agriculture", "Agriculture", "Forest",
                                  "Urban", "Urban", "Urban",
                                  "Urban", "Other", "Freshwater",
                                  "Golf course", "Beach", "Marine")))
land = land |> rename(mesh = メッシュ)
land2 = land |> filter(st_is_valid(geometry)) |> filter(str_detect(land, "Freshwater"))
lnames = str_glue("st{land2$mesh}")
dnames = str_glue("st{data$Name}") 

X = st_distance(data, land2)
dimnames(X) = list(dnames, lnames)
X[1:3, 1:3]

z = tibble::as_tibble(X, rownames = "dnames") |> 
  pivot_longer(matches("^st[0-9]+")) |> 
  group_by(dnames) |> 
  summarise(value = min(value))


bind_cols(z, data) |> 
  mutate(present = ifelse(coverage > 0, 1, 0),
         absent = ifelse(coverage > 0, 0, 1)) |> 
  ggplot(aes(x = as.numeric(value), y = present / (absent + present), present = present, absent = absent)) +
  geom_point() +
  geom_smooth(method = "glm",
              formula = cbind(present, absent) ~ x,
              method.args = list(family = binomial),
              color = "black") +
  scale_x_continuous("Distance from nearest river (m)", limits = c(0, 2500)) +
  scale_y_continuous("Probability of occurrence")

wh = gnnlab::aseries(5)  
library(magick)
pdfname = "katagami_bay_glm.pdf"
pngname = str_replace(pdfname, "pdf", "png")
ggsave(pdfname, height = wh[1], width = wh[1], units = "mm")
img = image_read_pdf(pdfname, density = 300)
img |> image_trim() |> image_border("white", "20x20") |> image_resize("500x") |> 
  image_write(pngname)

labelit = function(x) {
  case_when(x == "A" ~ "A (>70%)",
            x == "B" ~ "B (>40%)",
            x == "C" ~ "C (>10%)",
            x == "D" ~ "D (>1%)",
            TRUE~ "E (0%)")
}

data = data |> 
  mutate(rank = labelit(rank))

ggplot() + 
  geom_sf(aes(fill = land), data = land, alpha = 0.5, linetype = 0, size = 0) +
  geom_sf(data = nagasaki, size = 2, alpha = 0, color = "black") +
  geom_sf(data = stream, size = 1) +
  geom_sf(aes(color = rank), data = data, size = 10, alpha = 0.9) +
  scale_color_viridis_d("Coverage", end = 0.9, drop = F, option = "C") +
  scale_fill_viridis_d("Land use", end = 0.9, direction = -1, option = "D") + 
  annotation_north_arrow(style = north_arrow_minimal(text_size = 30, text_face = "bold"), 
                         height = unit(30, "mm"), width = unit(30, "mm")) +
  coord_sf(xlim = lonlimits, ylim = latlimits, expand = F) +
  # labs(title = "Katagami Bay, Nagasaki, Japan", subtitle = "May - June 2021") +
  theme(text = element_text(size = 20),
        legend.position = "right",
        legend.background = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank())

library(magick)
pdfname = "katagami_bay.pdf"
pngname = str_replace(pdfname, "pdf", "png")
ggsave(pdfname, height = wh[1]*2, width = wh[1]*2, units = "mm")
img = image_read_pdf(pdfname, density = 300)
img |> image_trim() |> image_border("white", "20x20") |> image_resize("500x") |> 
  image_write(pngname)


img1 = image_read_pdf("katagami_bay.pdf", density = 300)
img1 = img1 |> image_trim() |> image_border("white", "20x20") |> image_resize("x1000") 
img2 = image_read_pdf("katagami_bay_glm.pdf", density = 300)
img2 = img2 |> image_trim() |> image_border("white", "20x20") |> image_resize("x1000") 

image_append(c(img2, img1)) |> 
  image_border(color = "white", geometry = "x80") |> 
  image_annotate("May - June 2021 Katagami Bay, Nagasaki, Japan",
                 font = "Noto Sans",
                 gravity = "northwest", size = 50) |> 
  image_trim() |> 
  image_border(color = "white", geometry = "10x10") |> 
  image_write("katagami_bay_all.png")






# 有川湾 #######################################################################
xymins = c(32.97427004177547, 129.0835472079611)
xymaxs = c(32.994864933396116, 129.12874162342598)
names(xymins) = c("ymin", "xmin")
names(xymaxs) = c("ymax", "xmax")
box = c(xymins, xymaxs)
box = st_bbox(box, crs = crs_original) |> st_as_sfc(tmp)

shpfile  = "~/Lab_Data/Nagasaki_map_data/N03-20210101_42_GML/"
shpfile2 = "~/Lab_Data/Nagasaki_map_data/W05-07_42_GML/W05-07_42-g_RiverNode.shp"
shpfile3 = "~/Lab_Data/Nagasaki_map_data/W05-07_42_GML/W05-07_42-g_Stream.shp"
shpfile4 = "~/Lab_Data/Nagasaki_map_data/L03/"

nagasaki = st_read(shpfile,  as_tibble = TRUE)
crs_original = st_crs(nagasaki)

water    = st_read(shpfile2, as_tibble = TRUE, crs = crs_original)
stream   = st_read(shpfile3, as_tibble = TRUE, crs = crs_original)
land     = st_read(shpfile4, as_tibble = TRUE, options = "ENCODING=CP932")

water    = st_crop(water, box)
stream   = st_crop(stream, box)
land     = st_transform(land, crs = crs_original)
land     = st_crop(land, box)


# labels は国土交通省のサイトからです。
# https://nlftp.mlit.go.jp/ksj/gml/codelist/LandUseCd-09.html
# 海水域が最後の因子になるように、並べている。
land = land |> 
  mutate(land = factor(`土地利用種`,
                       levels = c("0100", "0200", "0500",
                                  "0600", "0700", "0901",
                                  "0902", "1000", "1100",
                                  "1600", "1400", "1500"),
                       labels = c("田", "その他農地", "森林",
                                  "荒地", "建物用地", "道路",
                                  "鉄道", "その他の用地", "河川地および潮沼",
                                  "ゴルフ場", "海浜", "海水域")))


ggplot() + 
  geom_sf(aes(fill = land), data = land, alpha = 0.5, linetype = 0, size = 0) +
  geom_sf(data = nagasaki, size = 2, alpha = 0, color = "black") +
  geom_sf(data = stream, size = 1) +
  scale_color_viridis_d(end = 0.9, drop = F, option = "C") +
  scale_fill_viridis_d("土地利用", end = 0.9, direction = -1, option = "D") + 
  annotation_north_arrow(style = north_arrow_minimal(text_size = 30, text_face = "bold"), 
                         height = unit(30, "mm"), width = unit(30, "mm")) +
  annotation_scale(location = "tr", height = unit(5, "mm"),
                   text_face = "bold", text_family = "notosans",
                   text_cex = 1) +
  coord_sf(xlim = box[c(2,4)], ylim = box[c(1,3)], expand = F) +
  labs(title = "Arikawa Bay, Nagasaki, Japan") +
  theme(text = element_text(size = 20),
        legend.position = "right",
        legend.title = element_blank(),
        legend.background = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank())

pdfname = "arikawa_bay.pdf"
pngname = str_replace(pdfname, "pdf", "png")
ggsave(pdfname, height = 300, width = 200, units = "mm")
img = image_read_pdf(pdfname, density = 300)
img |> image_trim() |> image_border("white", "20x20") |> image_resize("500x") |> 
  image_write(pngname)