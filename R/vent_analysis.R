
#vent analysis

# 
# df_calc_40<-ru_calc %>% 
#     filter(variable=="icu") %>%
#     select(id, reg, abs, vent)


library(magicfor)  

#20 weeks and 5 days per ICU stay

magic_for(silent = TRUE)

for (k in df_calc_40$abs) {
    
date<-rgamma(k,2.7,scale=30) #distribution of icu cases
length=rnorm(k,5,2) #distribution of length of stay in icu

in_icu<- rep(0,150)
#date is the day of start of stay in icu
#end is the day of end of stay
# max is 1 if date of start is before certain day and date of end is after certain day of stay
for (i in 0:150) {
    dd<-as.data.frame(cbind(date,length)) %>%
        mutate(cases=1,end=date+length) %>%
        mutate(max=case_when(
            date<=i & end>=i ~ 1,
            TRUE ~ 0
        ))
    in_icu[i]<-sum(dd$max) 
    
}
plot(in_icu)
max20<-max(in_icu)
put(max20)

}

res20<-magic_result_as_dataframe() %>% rename(abs=k)

#10 weeks and 5 days per ICU stay

magic_for(silent = TRUE)

for (k in df_calc_40$abs) {
    
    date<-rgamma(k,3.8,scale=10) #distribution of icu cases
    length=rnorm(k,5,2) #distribution of length of stay in icu
    
    in_icu<- rep(0,70)
    #date is the day of start of stay in icu
    #end is the day of end of stay
    # max is 1 if date of start is before certain day and date of end is after certain day of stay
    for (i in 0:70) {
        dd<-as.data.frame(cbind(date,length)) %>%
            mutate(cases=1,end=date+length) %>%
            mutate(max=case_when(
                date<=i & end>=i ~ 1,
                TRUE ~ 0
            ))
        in_icu[i]<-sum(dd$max) 
        
    }
    plot(in_icu)
    max10<-max(in_icu)
    put(max10)
    
}

res10<-magic_result_as_dataframe() %>% rename(abs=k)


#40 weeks and 5 days per ICU stay

magic_for(silent = TRUE)

for (k in df_calc_40$abs) {
    
    date<-rgamma(k,2.5,scale=63) #distribution of icu cases
    length=rnorm(k,5,2) #distribution of length of stay in icu
    
    in_icu<- rep(0,300)
    #date is the day of start of stay in icu
    #end is the day of end of stay
    # max is 1 if date of start is before certain day and date of end is after certain day of stay
    for (i in 0:300) {
        dd<-as.data.frame(cbind(date,length)) %>%
            mutate(cases=1,end=date+length) %>%
            mutate(max=case_when(
                date<=i & end>=i ~ 1,
                TRUE ~ 0
            ))
        in_icu[i]<-sum(dd$max) 
        
    }
    plot(in_icu)
    max40<-max(in_icu)
    put(max40)
    
}

res40<-magic_result_as_dataframe() %>% rename(abs=k)

ru_v <- df_calc_40 %>% 
    left_join(res10) %>%
    left_join(res20) %>%
    left_join(res40) %>%
    mutate(
        vent10weeks=round(max10/vent,2),
        vent20weeks=round(max20/vent,2),
        vent40weeks=round(max40/vent,2),
    ) %>% 
    # mutate(name = name %>% str_trunc(20, side = "center")) %>%
    arrange(vent40weeks) %>% 
    mutate(reg = reg %>% as_factor %>% fct_inorder) %>% 
    pivot_longer(vent10weeks:vent40weeks,
                 names_to = "epi_scenario",
                 values_to = "peak_patients_per_vent")


# ru_v %>%  
#     ggplot(aes(y = reg))+
#     geom_point(aes(x=peak_patients_per_vent, color=epi_scenario ))+
#     scale_x_log10()+
#     theme_minimal()+
#     labs(y = "") +
#     geom_vline(xintercept=1)+
#     scale_y_discrete(limits=ru_v$name)
#     
