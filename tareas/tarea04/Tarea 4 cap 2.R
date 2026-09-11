library(nycflights13)
library(tidyverse)
nycflights13::flights
View(flights)

# Ejercicio 1 -------------------------------------------------------------
##Had an arrival delay of two or more hours----
flights |> filter(arr_delay >= 120)
##Flew to Houston (IAH or HOU)----
flights |> filter(dest %in% c("IAH", "HOU"))
##Were operated by United, American, or Delta----
flights |> filter(carrier %in% c("UA", "AA", "DL"))
##Departed in summer (July, August, and September)----
flights |> filter(month %in% c(7, 8, 9))
##Arrived more than two hours late but didn’t leave late----
flights |> filter(arr_delay > 120 & dep_delay <= 0)
##Were delayed by at least an hour, but made up over 30 minutes in flight----
flights |> filter(dep_delay >= 60 & (dep_delay - arr_delay) > 30)


# Ejericicio 2 ----------------
##Sort flights to find the flights with the longest departure delays. Find the flights that left earliest in the morning-----
flights |> arrange(desc(dep_delay))
flights |> arrange(dep_time)


# Ejercicio 3 -----------
##Sort flights to find the fastest flights. (Hint: Try including a math calculation inside of your function.)--------
flights |> arrange(air_time / distance)



# Ejercicio 4 -------
##Was there a flight on every day of 2013?----
flights |> 
  distinct(month, day)
  

# Ejercicio 5 -------
##Which flights traveled the farthest distance? Which traveled the least distance?----
flights |> arrange(desc(distance))
flights |> arrange(distance)



# Ejercicio 6----
##Does it matter what order you used filter() and arrange() if you’re using both? Why/why not? Think about the results and how much work the functions would have to do----
### Yo diria que no importa porque si filtras primero u ordinas primero es indistinto, ambos van a terminar arrojando los mismo datos independientemente del orden en que se usen. Ahora como usar filtro elimina columans eso hace que r use menos esfuerzo, si se pone primero fliter y luego arrage, ya que va a ordenas menos datos














