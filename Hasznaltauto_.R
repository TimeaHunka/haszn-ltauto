library(ggcorrplot)
library(plotly)
library(tidyverse)
df2 <- readRDS("hasznaltauto.RDS")

# Különböző autómárkák átlagárai 


df <- df_final %>% 
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

ggplotly(márkák_árak)


#A különböző márkák 30 mFt alatti átlagárai"
df <- df_final %>% 
  filter(vetelar <= 30000000) %>% 
  group_by(brand) %>% 
  summarise(vetelar_atlag= mean(vetelar)) 

df$vetelar_atlag_mf <- df$vetelar_atlag / 1000000

márkák_árak1 <- ggplot(df , aes(brand,vetelar_atlag_mf , fill=brand))+
  geom_col()+
  labs(x="Autómárkák",
       y="Átlagos vételár (mFt)",
       title="A különböző márkák 30 mFt alatti átlagárai") +
  theme_classic(base_line_size = 0.4, base_rect_size = 0.7)+
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 

ggplotly(márkák_árak1)

#Árult használtautók árai évek szerint



df3 <- df_final %>% 
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



#A 30 mFt alatti használtautók átlagárai évjárat szerint
#Itt nagyon átláthatatlan a wrap miatt shinyban erdemes lenne az evjáratot lenyilo fulkent beallitani hogy az adott evben h alakulnak az árak a 30 mFt alatti kocsik esetében

df4 <- df_final %>% 
  filter(vetelar <= 30000000 , evjarat >= 2005) %>%
  group_by(brand, evjarat) %>% 
  summarise(vetelar_atlag= mean(vetelar)) 

df4$vetelar_atlag_mf <- df4$vetelar_atlag / 1000000

ábra1 <-   ggplot(df4 , aes(x=reorder(brand, vetelar_atlag_mf),
                            y=vetelar_atlag_mf , fill= evjarat, color =evjarat))+
  geom_boxplot()+
  geom_hline(aes(yintercept=mean(df3$vetelar_atlag_mf)))+
  facet_wrap(~evjarat, nrow = 8)+
  #stat_summary(fun= mean,color="red")
  labs(x="Autómárkák",
       y="Átlagos vételár (mFt)",
       title="A 30 mFt alatti használtautók átlagárai évjárat szerint")+
  
  theme_classic(base_line_size = 0.4, base_rect_size = 0.7)+
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 

ggplotly(ábra1)







getwd()

df_brand <- df2 %>% 
  group_by(brand) %>% 
  nest()

audi <-(df_brand[[2]][[26]])

#Az automata audik vételára magasabb


levels(audi$sebessegvalto_fajtaja)
unique(audi$sebessegvalto_automata)

audi1 <- audi %>% 
  filter( vetelar <= 5000000 & vetelar >= 1000000)

mean(audi1$vetelar[audi1$sebessegvalto_automata == 1])
mean(audi1$vetelar[audi1$sebessegvalto_automata == 0])

table(audi1$sebessegvalto_automata)

audi1$sebessegvalto_automata <- ifelse(audi1$sebessegvalto_fajtaja == "automata" , 1 , 0 )

#Normalitás vizsgálat

by (audi$vetelar , audi$sebessegvalto_automata , shapiro.test)
hist (audi1$vetelar [audi1$sebessegvalto_automata==1], ylim=c(0,400))
hist (audi1$vetelar [audi1$sebessegvalto_automata==0], ylim=c(0,400))

#p<0.01 ezért szignifikánsan eltér az eloszlás a normálistól mindkét csoportnál. 
#Viszont nagy mintaelemszám miatt ( n>100 ) ettől eltekinthetünk


#Szoráshomogenitás

by (audi$vetelar , audi$sebessegvalto_automata , sd , na.rm = T)
psych::describeBy (audi$vetelar , audi$sebessegvalto_automata ,na.rm = T)

# Hipotézis
#H0: mu automata <= mu manualis
#H1: mu  automata > mu manualis

ketmintas_t <- t.test (vetelar ~ sebessegvalto_automata , 
                       data = audi1 , 
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
table(audi_a6$sebessegvalto_automata)
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

# MARTIN -----------------------------------------------------------------------

# Használtautók gyártási évének eloszlása
table(df2$evjarat)
# Mivel 1990 előtt gyártott autók száma viszonylag kevés, így azokat kidobjuk, mivel outliereknek gondoljuk őket az elemzés szempontjából.
# 1990 előtti autók kiszűrése

df_final <- df2 %>% 
  filter( evjarat >= 1990)

# A gyártási év eloszlása

ggplot(df_final)+
  geom_histogram(aes(evjarat),
                 fill = "red",
                 binwidth = 1)+
  labs(
    main = "Az autók gyártási évének eloszlása",
    x = "Gyártási év",
    y = "Darab",
  )+
  theme_classic()

# Hipotézis vizsgálat
# 1) Dízel autók drágábbak-e?
table(df_final)