# Number of cars
cars = 100

# Number of spaces in each car
space_in_a_car = 4

# Number of drivers
drivers = 30.0

# Number of passengers
passengers = 91

# Number of cars without drivers
cars_not_driven = cars - drivers

# Number of cars with drivers
cars_driven = drivers

# Number of spaces considering all cars
carpool_capacity = cars_driven * space_in_a_car

# Number of passengers per car
average_passengers_per_car = passengers/cars_driven

print "There are", cars, "cars available."
print "There are only", drivers, "drivers available."
print "There will be", cars_not_driven, "empty cars today."
print "We can transport", carpool_capacity, "people today."
print "We have", passengers, "to carpool today."
print "We need to put about", average_passengers_per_car, "in each car."