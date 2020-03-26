#===============================================================================
# 2020-03-26 -- covid19
# calculate and map burden on Russain regions
# Ilya Kashnitsky, ilya.kashnitsky@gmail.com
#===============================================================================


library(tidyverse)
library(magrittr)
library(sf)
library(hrbrthemes)

options(scipen = 9999)
library(showtext)
font_add_google("Oswald", "Oswald")
font_add_google("Roboto Slab", "Roboto Slab")
showtext::showtext_auto()

library(cowplot)
library(plotly)



# load the datasets -------------------------------------------------------

load("reg_okato.rda")
load("pop10.rda")

# CFR Italian 23 March
cfr <- tibble::tribble(
    ~age, ~b, ~m, ~f,
    "0-9",      0,     0,       0,
    "10-19",      0,     0,       0,
    "20-29",      0,     0,       0,
    "30-39",    0.3,   0.5,     0.2,
    "40-49",    0.6,   0.9,     0.3,
    "50-59",    1.5,   2.1,     0.8,
    "60-69",    5.2,   6.3,     2.9,
    "70-79",   15.6,  18.4,    10.3,
    "80-89",   23.6,  27.5,    18.3,
    "90+",     24,  30.7,    20.4
) %>% 
    pivot_longer(b:f, names_to = "sex", values_to = "cfr")


# calculate ---------------------------------------------------------------


# filter out only regions
reg_only <- df_reg_10 %>% 
    filter(nchar(id)==11,
           # remove pooled population of Arkhangelk and Tumen
           !id %in% c("71000000000", "11000000000"))

# russain average proportion
ru_avg_prop <- reg_only %>% 
    left_join(cfr, c("age", "sex"))  %>% 
    filter(year ==2020, !sex=="b", type=="pooled") %>% 
    # assume 2/3 get infected
    mutate(death = (value*2/3) * (cfr/100)) %>% 
    summarise(value = value %>% sum(na.rm = TRUE),
              death = death %>% sum(na.rm = TRUE)) %>% 
    mutate(prop = death / value * 100) %>% 
    pull(prop)


# join cfr data and calculate proportion dying for total population 2019
ru_calc <- reg_only %>% 
    left_join(cfr, c("age", "sex"))  %>% 
    filter(year ==2020, !sex=="b") %>% 
    # assume 2/3 get infected
    mutate(death = (value*2/3) * (cfr/100)) %>% 
    group_by(type, id, name) %>% 
    summarise(value = value %>% sum(),
              death = death %>% sum()) %>% 
    ungroup() %>% 
    drop_na() %>%
    mutate(
        prop = death / value * 100,
        rel_prop = prop / ru_avg_prop
    ) %>% 
    # discrete
    group_by(type) %>% 
    mutate(
        rel_prop_gr = rel_prop %>% 
            cut(c(0, .5, 2/3, 4/5, .95, 100/95, 5/4, 3/2, Inf))
    ) %>% 
    ungroup() %>% 
    drop_na()

# join the calculated values to the spatial object

df_plot <- reg_okato %>% 
    left_join(
        ru_calc %>% filter(type == "pooled"),
        by = c("okato"="id")
    )



# maps ---------------------------------------------------------------

# static map
df_plot %>% 
    ggplot() + 
    geom_sf(aes(fill = prop), size = .1)+
    scale_fill_fermenter(palette = 4, direction = 1,
                         breaks = seq(.5, 1.75, .25))+
    cowplot::theme_map(font_family = font_rc)

out <- ggplot2::last_plot()

ggsave(filename = "map-prop.png", plot = out, 
       width = 7, height = 4)

# plotly -- this is quick and dirty, later I will do properly with the labels
p <- ggplotly(out)
htmlwidgets::saveWidget(p, file = "foo-map.html")


# plots -------------------------------------------------------------------

# dotplot with the ptoportions
ru_calc %>% 
    select(type, id, name, prop) %>% 
    pivot_wider(names_from = type, values_from = prop) %>% 
    arrange(pooled) %>% 
    mutate(id = id %>% as_factor(),
           name = name %>% str_trunc(20, side = "center") %>% 
               as_factor()) %>% 
    pivot_longer(pooled:urban, names_to = "type", values_to = "prop") %>%
    ggplot(aes(y = name))+
    geom_point(aes(prop, color = type))+
    theme_minimal(base_family = "Oswald")+
    labs(y = NULL)


ggsave(filename = "rank-prop-urb-rur.pdf", 
       width = 5, height = 10)  

# dotplot with the death tolls
ru_calc %>% 
    select(type, id, name, death) %>% 
    pivot_wider(names_from = type, values_from = death) %>% 
    arrange(pooled) %>% 
    mutate(id = id %>% as_factor(),
           name = name %>% str_trunc(20, side = "center") %>% 
               as_factor()) %>% 
    pivot_longer(pooled:urban, names_to = "type", values_to = "death") %>%
    ggplot(aes(y = name))+
    geom_point(aes(death, color = type))+
    scale_x_log10()+
    theme_minimal(base_family = "Oswald")+
    labs(y = NULL)


ggsave(filename = "rank-death-urb-rur.pdf", 
       width = 5, height = 10)  
