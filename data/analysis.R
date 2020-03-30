#===============================================================================
# 2020-03-28 -- covid19
# calculate and map burden on Russain regions
# Ilya Kashnitsky, ilya.kashnitsky@gmail.com
# Anton Barchuk, barchuk.anton@gmail.com
#===============================================================================

library(tidyverse)
library(magrittr) 
library(sf)
library(hrbrthemes)
library(stringi)

options(scipen = 9999)
library(showtext)
font_add_google("Roboto Condensed", "Roboto Condensed")
showtext::showtext_auto()

library(cowplot)
library(plotly)



# load the datasets -------------------------------------------------------

load("data/reg_okato.rda")
load("data/pop10.rda")

# CFR Italian 26 March
# https://www.epicentro.iss.it/coronavirus/bollettino/Bollettino-sorveglianza-integrata-COVID-19_26-marzo%202020.pdf
cfr <-  tibble::tribble(
    ~age, ~b, ~m, ~f,
    "0-9",      0.0001,     0.0001,       0.0001,
    "10-19",      0.0001,     0.0001,       0.0001,
    "20-29",      0.0001,     0.0001,       0.0001,
    "30-39",    0.3,   0.6,     0.1,
    "40-49",    0.7,   1.1,     0.4,
    "50-59",    1.7,   2.4,     0.8,
    "60-69",    5.7,   6.9,     3.5,
    "70-79",   16.9,  19.8,    11.6,
    "80-89",   24.6,  29.2,    18.9,
    "90+",     24,  30.8,    20.4
) 

cfr_sr <- cfr %>% 
    mutate(mfratio=round(m/f, 1)) %>%
    select(age,mfratio) %>% 
    filter(!age=="90+") %>% 
    mutate(age = age %>% paste %>% str_replace("80-89", "80+"))

# Ferguson report table 1
# http://spiral.imperial.ac.uk/handle/10044/1/77482
ifr <- tibble::tribble(
    ~age, ~hosp, ~i, ~ifr,
"0-9", 0.01, 5.0, 0.002,
"10-19", 0.04, 5.0, 0.006,
"20-29", 1.1, 5.0, 0.03,
"30-39", 3.4, 5.0, 0.08,
"40-49", 4.3, 6.3, 0.15,
"50-59", 8.2, 12.2, 0.60,
"60-69", 11.8, 27.4, 2.2,
"70-79", 16.6, 43.2, 5.1,
"80+", 18.4, 70.9, 9.3
) %>% 
    mutate(icu=hosp*i/100) %>%
    select(-i) %>%
    pivot_longer(hosp:icu, names_to = "variable", values_to = "prop") %>% 
    left_join(cfr_sr) %>%
    mutate(f=round(2*prop/(1+mfratio),6), #calculate male and female proportions
           m=round(2*prop*mfratio/(1+mfratio),6)) %>%
    pivot_longer(m:f, names_to = "sex", values_to = "adj_prop") %>%
    rename(new_age=age) %>% 
    select(-mfratio,-prop)



# Italian CFR plot --------------------------------------------------------

# age profile of case fatality ratios
(
    gg_cfr <- cfr  %>% 
        pivot_longer(b:f, names_to = "sex", values_to = "cfr") %>% 
        ggplot(aes(cfr, age, color = sex))+
        geom_point(size = 3, shape = c(16, 1, 1) %>% rep(10))+
        scale_color_manual(values = c("#df356b", "#009C9C", "#eec21f"), guide = NULL)+
        scale_x_continuous(position = "top")+
        theme_minimal(base_family = font_rc)+
        theme(panel.grid = element_blank(),
              axis.ticks.x.top = element_line(colour = "#7F7F7F", .22),
              axis.ticks.length.x = unit(.5, "lines"))+
        labs(x = NULL, 
             y = NULL,
             title = "Летальность COVID-19 по возрасту и полу, %",
             caption = "Итальянские данные за 26 марта 2020 г., 6801 смертей")+
        annotate("text", x = 19.4, y = 10, hjust = 1, size = 4, 
                 label = "женщины", family = font_rc, color = "#009C9C")+
        annotate("text", x = 20.8, y = 8, hjust = 0, size = 4, 
                 label = "мужчины", family = font_rc, color = "#eec21f")
)

ggsave(filename = "figures/cfr-italy.pdf", gg_cfr, width = 5, height = 5)
ggsave(filename = "figures/cfr-italy.svg", gg_cfr, width = 5, height = 5)

    
# calculate ---------------------------------------------------------------

# filter out only regions
reg_only <- df_reg_10 %>% 
    filter(nchar(id)==11,
           # remove pooled population of Arkhangelk and Tumen
           !id %in% c("71000000000", "11000000000")) %>%
    mutate(new_age=case_when(
        age=="0-9"~ "0-9",
        age=="10-19"~ "10-19",
        age=="20-29"~"20-29",
        age=="30-39"~"30-39",
        age=="40-49"~ "40-49",
        age=="50-59" ~  "50-59",
        age=="60-69" ~ "60-69",
        age=="70-79" ~ "70-79" ,
        age=="80-89" ~ "80+",
        age=="90+" ~ "80+",
        TRUE~"other")) %>%
    group_by(new_age,name,id, sex,year,type) %>%
    summarise(value=sum(value)) %>% ungroup() %>%
    filter(type=="pooled", !sex=="b", year==2020) %>%
    select(-year, -type) 

# add vent
vent <- read.csv2(
    "data/vent.csv", colClasses = c("id" = "character")
) %>% 
    group_by(id) %>% 
    mutate(vent = max(vent, vent2, vent3, na.rm = T)) %>% 
    select(reg, id, vent) %>% 
    ungroup() %>% 
    mutate(reg_en = reg %>% stringi::stri_trans_general("ru-ru_Latn/BGN"))

    
# with varying attack rate ------------------------------------------------


calc_attack_rates <- function(rate = 2/3) {
    
    dfi <- reg_only %>% 
        left_join(ifr, c("new_age", "sex"))  %>% 
        # assumption on the attack rate
        mutate(abs = round ((value*rate) * (adj_prop/100),0)) %>%
        group_by(id, name, variable) %>% 
        summarise(
            value = value %>% sum(na.rm = T),
            abs = abs %>% sum(na.rm = T)
        ) %>% 
        left_join(vent) %>%
        ungroup() %>% 
        drop_na() %>%
        group_by(variable) %>% # to calculate average prop for Russia
        mutate(
            prop = abs / value * 100,
            avg_prop = weighted.mean(prop, value), # average prop for Russia
            rel_prop = prop / avg_prop
        ) %>% 
        ungroup() %>% 
        # discrete
        group_by(variable) %>% 
        mutate(
            rel_prop_gr = rel_prop %>% 
                cut(c(0, .5, 2/3, 4/5, .95, 100/95, 5/4, 3/2, Inf))
        ) %>% 
        ungroup() %>% 
        drop_na()
    
    return(dfi)
    
}

df_calc <- seq(.4, .8, .1) %>% 
    map_df(calc_attack_rates, .id = "attack_rate") %>% 
    mutate(attack_rate = attack_rate %>% as_factor %>% 
               lvls_revalue(seq(40, 80, 10) %>% paste0("%")) %>% 
               paste) %>% 
    left_join(
        reg_okato,
        .,
        by = c("okato"="id")
    )

# map -- RUS labels
p_arates <- df_calc %>%
    filter(variable == "icu") %>%
    plot_ly() %>%
    add_sf(
        color = ~ prop,
        colors = "RdPu",
        midpoint = 1,
        split = ~ okato,
        alpha = 1,
        stroke = I("#ebebeb"),
        span = I(.1),
        text = ~ paste0(
            reg,
            "\n",
            "Интенсивная терапия понадобится: ",
            (abs / 1e3) %>% round(1),
            " тыс. чел.",
            "\n"
        ),
        frame = ~ attack_rate,
        hoverinfo = "text",
        hoveron = "fills"
    ) %>%
    hide_colorbar() %>%
    animation_opts(1000, easing = "elastic", redraw = FALSE) %>%
    animation_slider(currentvalue = list(prefix = "Доля зараженного населения: ")) %>%
    animation_button(hide = T) %>%
    hide_legend()

htmlwidgets::saveWidget(p_arates, file = "map-arates.html")


# map -- ENG labels
p_arates_en <- df_calc %>%
    filter(variable == "icu") %>%
    plot_ly() %>%
    add_sf(
        color = ~ prop,
        colors = "RdPu",
        midpoint = 1,
        split = ~ okato,
        alpha = 1,
        stroke = I("#ebebeb"),
        span = I(.1),
        text = ~ paste0(
            reg_en,
            "\n",
            "Demand for intensive care units: ",
            (abs / 1e3) %>% round(1),
            " thous.",
            "\n"
        ),
        frame = ~ attack_rate,
        hoverinfo = "text",
        hoveron = "fills"
    ) %>%
    hide_colorbar() %>%
    animation_opts(1000, easing = "elastic", redraw = FALSE) %>%
    animation_slider(currentvalue = list(prefix = "Proportion of the population infected: ")) %>%
    animation_button(hide = T) %>%
    hide_legend()

htmlwidgets::saveWidget(p_arates_en, file = "map-arates-en.html")




# venitlators -------------------------------------------------------------

# filter the 40% scenario
df_calc_40 <- df_calc %>% 
    st_drop_geometry() %>% 
    filter(attack_rate == "40%", variable == "icu") %>% 
    transmute(id = okato, reg, reg_en, value, abs, vent)

# run all the loops script -- output is ru_v df
source("R/vent_analysis.R") # it takes several minutes

save(ru_v, file = "data/ru_v.rda")

# # NOT USED in the end
# # interpolate color palete
# pal_func <- colorRamp(
#     colors = RColorBrewer::brewer.pal(n = 11, name = "Reds")
# ) 
# 
# # convert to hex colors
# pal_calc <- function(x) {
#     x %>% pal_func() %>% divide_by(255) %>% rgb
# }
# 
# 
# c(0, .1, .9) %>% pal_calc %>% color


# join the data and calculate manual colors
df_vent <- ru_v %>% 
    mutate(
        peak_week = epi_scenario %>% as_factor %>% fct_inorder() %>% 
            lvls_revalue(c("05", "10", "20")),
        log_peak_demand_0_1 = peak_patients_per_vent %>% log %>% scales::rescale(),
        log_peak_demand_0_1_rev = 1 - log_peak_demand_0_1
    ) %>% 
    # group_by(id, epi_scenario) %>% 
    # mutate(manual_color = log_peak_demand_0_1 %>% pal_calc) %>% 
    # ungroup() %>% 
    left_join(
        reg_okato,
        .,
        by = c("okato"="id")
    )

# map -- RUS labels
p_vents <- df_vent %>%
    group_by(epi_scenario) %>% 
    plot_ly() %>%
    add_sf(
        color = ~ log_peak_demand_0_1_rev,
        colors = ~ "BrBG",
        split = ~ okato,
        alpha = 1,
        stroke = I("#ebebeb"),
        span = I(.1),
        text = ~ paste0(
            reg,
            "\n",
            "При пике эпидемии на ", peak_week %>% str_replace("05", "5"), 
            " неделе", "\n",
            "Пациентов на один аппарат ИВЛ: " , 
            peak_patients_per_vent %>% round(1),
            "\n"
        ),
        frame = ~ peak_week,
        hoverinfo = "text",
        hoveron = "fills"
    ) %>%
    hide_colorbar() %>%
    animation_opts(1000, easing = "elastic", redraw = FALSE) %>%
    animation_slider(currentvalue = list(prefix = "Длительность эпидемии в неделях от начала до пика: ")) %>%
    animation_button(hide = T) %>%
    hide_legend()

htmlwidgets::saveWidget(p_vents, file = "map-vents.html")


# map -- ENG labels
p_vents_en <- df_vent %>%
    group_by(epi_scenario) %>% 
    plot_ly() %>%
    add_sf(
        color = ~ log_peak_demand_0_1_rev,
        colors = ~ "BrBG",
        split = ~ okato,
        alpha = 1,
        stroke = I("#ebebeb"),
        span = I(.1),
        text = ~ paste0(
            reg_en,
            "\n",
            "Epidemic peaks at the ", peak_week %>% str_replace("05", "5"), 
            "th week", "\n",
            "Peak number of patients per one ICU: " , 
            peak_patients_per_vent %>% round(1),
            "\n"
        ),
        frame = ~ peak_week,
        hoverinfo = "text",
        hoveron = "fills"
    ) %>%
    hide_colorbar() %>%
    animation_opts(1000, easing = "elastic", redraw = FALSE) %>%
    animation_slider(currentvalue = list(prefix = "Duration of the epidemic form the beginning to the peak, in weeks: ")) %>%
    animation_button(hide = T) %>%
    hide_legend()

htmlwidgets::saveWidget(p_vents_en, file = "map-vents-en.html")


# export tables in CSV ----------------------------------------------------

out_abs_icu <- df_calc %>%
    st_drop_geometry() %>% 
    filter(variable == "icu") %>%
    transmute(okato, reg, reg_en, attack_rate, abs) %>% 
    pivot_wider(names_from = attack_rate, values_from = abs) 

rio::export(out_abs_icu, file = "data/out-abs-icu.csv")


out_vent <- df_vent %>%
    st_drop_geometry() %>% 
    transmute(okato, reg, reg_en, epi_scenario, peak_patients_per_vent) %>% 
    pivot_wider(names_from = epi_scenario, values_from = peak_patients_per_vent) 
rio::export(out_vent, file = "data/out-vent.csv")
