# create a string from a string and a number variable
x = "There are %d types of people." % 10

# variable called 'binary' is a string
binary = "binary"

# contraction
do_not = "don't"

# create a string from a string and a tuple(?!) of strings
y = "Those who know %s and those who %s." % (binary, do_not)

# evaluate the statement 'x' and show on console
print x
# evaluate the statement 'y' and show on console 
print y

# without creating the variable
print "I said: %r." % x
print "I also said: '%s'." % y

# variable called 'hilarious' is binary
hilarious = False

# unevaluated string with variable
joke_evaluation = "Isn't that joke so funny?! %r"

# evaluate the statement 'joke_evaluation' and show on console
print joke_evaluation % hilarious

# define a set of string variables
w = "This is the left side of..."
e = "a string with a right side."

# concatenate the strings and show on console
print w + e