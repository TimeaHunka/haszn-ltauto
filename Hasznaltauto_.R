df2 <- readRDS("hasznaltauto.RDS")
library(ggcorrplot)

# Különböző autómárkák átlagárai 


df <- df2 %>% 
  group_by(brand) %>% 
  summarise(vetelar_atlag= mean(vetelar)) 

df$vetelar_atlag_mf <- df$vetelar_atlag / 1000000

márkák_árak <- ggplot(df , aes(brand,vetelar_atlag_mf , fill=brand))+
  geom_col()+
  labs(x="Autómárkák",
       y="Átlagos vételár (mFt)",
       title="A különböző márkák átlagárai") +
  theme_classic(base_line_size = 0.4, base_rect_size = 0.7)+
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 

library(plotly)
  ggplotly(márkák_árak)
  
  
#Árult használtautók árai évek szerint
  
  
  
  df3 <- df2 %>% 
    group_by(brand, evjarat) %>% 
    summarise(vetelar_atlag= mean(vetelar)) 
  
  df3$vetelar_atlag_mf <- df3$vetelar_atlag / 1000000
  
 ábra1 <-   ggplot(df3 , aes(x=reorder(brand, vetelar_atlag_mf),
                    y=vetelar_atlag_mf , fill= evjarat, color =brand))+
    geom_boxplot()+
    geom_hline(aes(yintercept=mean(df3$vetelar_atlag_mf)))+
    #stat_summary(fun= mean,color="red")
     labs(x="Autómárkák",
          y="Átlagos vételár (mFt)",
          title="A különböző márkák átlagárai évjárat szerint")+
               
    theme_classic(base_line_size = 0.4, base_rect_size = 0.7)+
    theme(legend.position = "none",
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 

  ggplotly(ábra1)
  
  
  
  
  
  
  
  
  

  library(tidyverse)
getwd()

df_brand <- df2 %>% 
  group_by(brand) %>% 
  nest()

audi <-(df_brand[[2]][[26]])

#Az automata audik vételára magasabb


levels(audi$sebessegvalto_fajtaja)
unique(audi$sebessegvalto_automata)

audi$sebessegvalto_automata <- ifelse(audi$sebessegvalto_fajtaja == "automata" , 1 , 0 )

#Normalitás vizsgálat

by (audi$vetelar , audi$sebessegvalto_automata , shapiro.test)
hist (audi$vetelar [audi$sebessegvalto_automata==1])

#p<0.01 ezért szignifikánsan eltér az eloszlás a normálistól mindkét csoportnál. 
#Viszont nagy mintaelemszám miatt ( n>100 ) ettől eltekinthetünk


#Szoráshomogenitás

by (audi$vetelar , audi$sebessegvalto_automata , sd , na.rm = T)
psych::describeBy (audi$vetelar , audi$sebessegvalto_automata ,na.rm = T)

# Hipotézis
#H0: mu automata <= mu manualis
#H1: mu  automata > mu manualis

ketmintas_t <- t.test (vetelar ~ sebessegvalto_automata , 
                       data = audi , 
                       alternative = "greater" , 
                       var.equal = T) 
ketmintas_t

df_t <-  ketmintas_t [["parameter"]][["df"]]
t    <-  ketmintas_t [["statistic"]][["t"]]

r <-  sqrt ( t^2 / (t^2 + df_t) )
r


#Közepes mértékű hatásról beszélhetünk.


boxplot (vetelar ~ sebessegvalto_automata , data = audi)

ggplot (audi , aes (x = sebessegvalto_automata , y = vetelar)) +
  stat_summary (fun.data = mean_cl_normal , geom = "errorbar" , color = "red" , width = 0.5) +
  geom_jitter (alpha = 0.7 , shape = '.') +
  coord_cartesian ()











audi_modell <-audi %>% 
  group_by(modell) %>% 
  nest()

audi_a6 <- (audi_modell[[2]][[1]])

audi_a6$sebessegvalto_automata <- ifelse(audi_a6$sebessegvalto_fajtaja == "automata" , 1 , 0 )

#Normalitás vizsgálat

by (audi_a6$vetelar , audi_a6$sebessegvalto_automata , shapiro.test)
hist (audi_a6$vetelar [audi_a6$sebessegvalto_automata==1])

#p<0.01 ezért szignifikánsan eltér az eloszlás a normálistól mindkét csoportnál. 
#Viszont nagy mintaelemszám miatt ( n>100 ) ettől eltekinthetünk


#Szoráshomogenitás

by (audi_a6$vetelar , audi_a6$sebessegvalto_automata , sd , na.rm = T)
psych::describeBy (audi_a6$vetelar , audi_a6$sebessegvalto_automata ,na.rm = T)

# Hipotézis
#H0: mu automata <= mu manualis
#H1: mu  automata > mu manualis

ketmintas_t <- t.test (vetelar ~ sebessegvalto_automata , 
                       data = audi_a6 , 
                       alternative = "greater" , 
                       var.equal = T) 
ketmintas_t

df_t <-  ketmintas_t [["parameter"]][["df"]]
t    <-  ketmintas_t [["statistic"]][["t"]]

r <-  sqrt ( t^2 / (t^2 + df_t) )
r


#Közepes mértékű hatásról beszélhetünk.


boxplot (vetelar ~ sebessegvalto_automata , data = audi_a6)

ggplot (audi , aes (x = sebessegvalto_automata , y = vetelar)) +
  stat_summary (fun.data = mean_cl_normal , geom = "errorbar" , color = "red" , width = 0.5) +
  geom_jitter (alpha = 0.7 , shape = '.') +
  coord_cartesian ()

