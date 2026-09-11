
library(tidyverse)
library(nycflights13)
# Ejrecicio 1 -------------------------------------------------------------
worst_hours <-  |>
  group_by(origin, time_hour) |>
  summarize(avg_delay = mean(dep_delay, na.rm = TRUE), n = n()) |>
  ungroup() |>
  slice_max(avg_delay, n = 48)
worst_hours |> left_join(weather, join_by(origin, time_hour))
##Casi la mitad de esas horas tuvieron algo de lluvia. No es lo que esta generando las variacion, hay otros factores como el presion y buena visibilidad----
# Ejercicio 2 -------------------------------------------------------------
top_dest <- flights |> count(dest, sort = TRUE) |> head(10)
top_dest
flights |> semi_join(top_dest)
# EJercicio 3 -------------------------------------------------------------
flights |> anti_join(weather, join_by(origin, time_hour))

# Ejercicio 4 -------------------------------------------------------------

flights |>
  anti_join(planes, join_by(tailnum)) |>
  count(carrier, sort = TRUE) |>
  mutate(pct = n / sum(n) * 100)
##La variable que mas explica el problema es carrier. Esas aerolíneas no reportan bien sus tail numbers----

# Ejercicio 5 -------------------------------------------------------------
plane_carriers <- flights |> distinct(tailnum, carrier) |> drop_na(tailnum)
plane_carriers |> count(tailnum) |> filter(n > 1)
plane_carriers |> filter(tailnum == "N146PQ")
##Rechazo----

# Ejercicio 6 -------------------------------------------------------------
airports_sml <- airports |> select(faa, lat, lon)
flights2 <- flights |>
  select(year, time_hour, origin, dest, tailnum, carrier) |>
  left_join(airports_sml |> rename(origin_lat = lat, origin_lon = lon),
            join_by(origin == faa)) |>
  left_join(airports_sml |> rename(dest_lat = lat, dest_lon = lon),
            join_by(dest == faa))
##Es mas facil----

# Ejercicio 7 -------------------------------------------------------------
avg_delay_dest <- flights |>
  group_by(dest) |>
  summarize(avg_delay = mean(arr_delay, na.rm = TRUE), n = n())
avg_delay_dest |> arrange(desc(avg_delay))
airports |>
  semi_join(flights, join_by(faa == dest)) |>
  left_join(avg_delay_dest, join_by(faa == dest)) |>
  ggplot(aes(x = lon, y = lat, size = n, color = avg_delay)) +
  borders("state") +
  geom_point(alpha = 0.7) +
  coord_quickmap()

# Ejercicio 8 -------------------------------------------------------------

flights |>
  filter(year == 2013, month == 6, day == 13) |>
  summarize(mean(dep_delay, na.rm = TRUE))
flights |> summarize(mean(dep_delay, na.rm = TRUE))
weather |>
  filter(year == 2013, month == 6, day == 13) |>
  arrange(desc(wind_gust)) |>
  select(origin, hour, wind_speed, wind_gust, precip) |>
  head(5)



