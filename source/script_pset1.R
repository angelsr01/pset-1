# Angel Siachoque Rodriguez, 201815355.
rm(list=ls())
R.version.string
# R version 4.2.1
#Se instala el paquete, y posteriormente se llama para tenerlo disponible.    
install.packages("pacman")
require(pacman)
#Para llamar librerías de la clase se usa la funcion p_load.
#Con esto podemos tener a la mano, al menos, la libreria tidyverse y rio.
p_load(tidyverse,
       rio,
       skimr,
       janitor)
# 1) Ejercicio de vectores
vr1=seq(1,100,by=1)
vr2=seq(1,100,by=2)
vr3=c(vr2+1)

# 2.1) Importar bases de datos, verificando primero el directorio de trabajo
getwd()
cg =import("input/Enero - Cabecera - Caracteristicas generales (Personas).csv") %>%
  clean_names()
ocu =import("input/Enero - Cabecera - Ocupados.csv") %>%
  clean_names()

# 2.2) Exportar bases de datos
export(x=cg , file="output/Caracteristicas generales (Personas).rds")
export(x=ocu , file="output/Ocupados.rds")

#3) Generar variables, aclarar que la variable p6040 representa la edad
ocu=mutate(.data=ocu,ocupado=1)
cg=mutate(.data=cg, joven=ifelse(test=p6040>=18 & p6040<=28, yes=1, no=0))

#4) Eliminar filas/columnas de un conjunto de datos
cg=cg %>% subset(p6040>=18 & p6040<=70)
#aclaración: la variable p6920 no aparece en cg...
#... y por ello se omite su selección, en cambio si aparece p6050
cg=cg %>% select(secuencia_p, orden,
              hogar, directorio, p6020, p6040, p6050,
              dpto, fex_c_2011, esc, mes)
#consecuentemente, se nota que acá sí aparece la variable p6920...
#... asi se justifica el uso de esta variable en la seleccion
ocu=ocu %>% select(secuencia_p, orden, hogar,
               directorio, ocupado, inglabo, p6920)

# 5) Combinar bases de datos
geih = left_join(x=cg, y=ocu, by=c("secuencia_p",
                                   "orden", "hogar", "directorio"))

#6) Descriptivas de un conjunto de datos
summary(geih)
#Este ejemplo representa, según el sexo de la persona encuestada...
#... el ingreso laboral que posee y si cotiza o no a pension.
geih %>% 
  select(inglabo,p6020,p6920) %>% 
  group_by(p6020) %>%  
  summarise(promedio_inglabo = mean(inglabo, na.rm = T),
            media_inglabo = median(inglabo, na.rm = T),
            promedio_p6920 = mean(p6920, na.rm = T),
            media_p6920 = median(p6920, na.rm = T))
#La grafica que se construira mostrara la relacion entre los ingresos...
#... y el sexo de la persona encuestada.
ingresos <- geih %>% 
  group_by(p6020) %>% 
  summarise(ingresos = mean(inglabo, na.rm = T)) %>% 
  ggplot(data=. , mapping = aes(x = as.factor(p6020) ,
                                y = ingresos, fill = as.factor(p6020))) + 
  geom_bar(stat = "identity") 
ingresos +
  scale_fill_manual(values = c("2"="red" , "1"="blue") ,
                    label = c("1"="Hombre" , "2"="Mujer") , name = "Sexo") +
  labs(x = "sexo") + 
  theme_classic()
#La siguiente grafica mostrara la relacion entre los ingresos...
#... y si cotiza o no a pension la persona encuestada
ingresos2 <- geih %>% 
  group_by(p6920) %>% 
  summarise(ingresos2 = mean(inglabo, na.rm = T)) %>% 
  ggplot(data=. , mapping = aes(x = as.factor(p6920) ,
                                y = ingresos, fill = as.factor(p6920))) + 
  geom_bar(stat = "identity") 
ingresos +
  scale_fill_manual(values = c("3"="yellow", "2"="red" , "1"="blue") , 
                    label = c("1"="Si" , "2"="No", "3"="Ya pensionado/a") ,
                    name = "¿Pension?") +
  labs(x = "pension") + 
  theme_classic()
# Este ejemplo agrupa a los individuos encuestados por departamento...
#... sacando una descripion basica relacionada a la edad que poseen.
geih %>% 
  select(p6040,dpto) %>% 
  group_by(dpto) %>%  
  summarise(promedio_p6040 = mean(p6040, na.rm = T),
            media_p6040 = median(p6040, na.rm = T))
# Esta grafica muestra la relacion entre la edad...
#... y el nivel de ingreso de la muestra encuestada.
graph_1 = ggplot(data = geih , mapping = aes(x = p6040 , y = inglabo)) +
  geom_point(col = "red" , size = 0.5)
graph_1
# Fin del ejercicio.