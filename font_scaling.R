# For some reason, when the xaringan file 
# does not run the following script properly.
# 
library(tidyverse)
library(readxl)
library(ggpubr)
library(showtext)

font_add_google(name = "Noto Sans", family = "notosans")
font_add_google(name = "Noto Sans JP", family = "notosans-jp") # 日本語フォント
showtext_auto()
theme_replace(text = element_text(family = "notosans"))

filename = rprojroot::find_rstudio_root_file("io/Table 2.xlsx")
col_names = c("month", "temperature1", "sd1", "empty","temperature2", "sd2")
exceldata = readxl::read_excel(filename, sheet=1, skip = 2, col_name = col_names)

DPI = 300
scale = DPI / 96 # 96 in this script but 72 in xaringan.

wh = list(width = 80, height = 80)
pdffile = rprojroot::find_rstudio_root_file("temperature_plot2.pdf")
pngfile = rprojroot::find_rstudio_root_file("temperature_plot2.png")

xlabel = "Month"
ylabel = "'Temperature'~(degree*C)" # plotmath expression see ?plotmath

levels = month.abb
levels = str_c(levels, ifelse(levels == "May", "", "."))


pout = exceldata |> 
  mutate(month = factor(month, levels = levels)) |> 
  ggplot() + 
  geom_point(aes(x = month, y = temperature1)) + 
  labs(x = parse(text = xlabel), 
       y = parse(text = ylabel))  +
  theme_pubr(base_family = "notosans") +
  theme(text = element_text(size = 10))

ggsave(pdffile, plot = pout, width = wh$width, height = wh$height, units = "mm", dev = cairo_pdf)

pout = exceldata |> 
  mutate(month = factor(month, levels = levels)) |> 
  ggplot() + 
  geom_point(aes(x = month, y = temperature1)) + 
  labs(x = parse(text = xlabel), 
       y = parse(text = ylabel))  +
  theme_pubr(base_family = "notosans") +
  theme(text = element_text(size = 10 * scale))

ggsave(pngfile, plot = pout, width = wh$width, height = wh$height, units = "mm", dpi = DPI)

library(magick)
i1 = image_read_pdf(pdffile, density = 600) |> image_resize("x500") 
i2 = image_read(pngfile)|> image_resize("x500") 
