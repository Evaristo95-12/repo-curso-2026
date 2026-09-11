library(tidyverse)
library(nycflights13)

# Ejercicio 1 -------------------------------------------------------------
##We forgot to draw the relationship between weather and airports in Figure 19.1. What is the relationship and how should it appear in the diagram?----
weather |> distinct(origin)
airports |> filter(faa %in% unique(weather$origin)) |> select(faa, name)
# Ejercicio 2 ----
##weather only contains information for the three origin airports in NYC. If it contained weather records for all airports in the USA, what additional connection would it make to flights?----
flights |> distinct(dest) |> nrow()
weather |> distinct(origin) |> nrow()
# Ejercicio 3 ----
##The year, month, day, hour, and origin variables almost form a compound key for weather, but there’s one hour that has duplicate observations. Can you figure out what’s special about that hour?----
weather |>
  count(origin, year, month, day, hour) |>
  filter(n > 1)
# Ejercicio 4 ----
##We know that some days of the year are special and fewer people than usual fly on them (e.g., Christmas eve and Christmas day). How might you represent that data as a data frame? What would be the primary key? How would it connect to the existing data frames?----
special_days <- tribble(
  ~year, ~month, ~day, ~motivo,
  2013,  12,     24,   "Nochebuena",
  2013,  12,     25,   "Navidad")
flights |> left_join(special_days, by = join_by(year, month, day))
# Ejercicio 5 ----
##Draw a diagram illustrating the connections between the Batting, People, and Salaries data frames in the Lahman package. Draw another diagram that shows the relationship between People, Managers, AwardsManagers. How would you characterize the relationship between the Batting, Pitching, and Fielding data frames?----
###Batting, People, Salaries: People tiene como primary key playerID. Batting tiene una compound key playerID-yearID-stint, y usa playerID como foreign key hacia People. Salaries tiene compound key playerID-yearID-teamID, y también usa playerID como foreign key hacia People. Osea, People queda en el centro del diagrama, con Batting y Salaries colgando de ella por playerID ----
###People, Managers, AwardsManagers: mismo patrón — People$playerID es la primary key de la que cuelgan tanto Managers$playerID como AwardsManagers$playerID como foreign key. Managers y AwardsManagers no se conectan directo entre sí, sino a través de People----
###Batting, Pitching, Fielding: estas tres comparten la misma compound key (playerID-yearID-stint). No son foreign key una de otra, son como "tablas hermanas" al mismo nivel, cada una apuntando indirectamente a People vía playerID. Si quisiera juntar bateo, pitcheo y fildeo de un mismo jugador en una misma temporada, tendría que hacer un join entre ellas usando esa clave compuesta compartida----