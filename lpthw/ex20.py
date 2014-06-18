# get access to command-line variables
from sys import argv

# save command-line variables
script, input_file = argv

# function to print a file
def print_all(f):
	# read() reads ALL LINES
	print f.read()

# function to put cursor back to start of file
def rewind(f):
	# seek() moves cursor to given position
	f.seek(0)

# function to print a line with its number
def print_a_line(line_count, f):
	# read line number "line_count"
	print line_count, f.readline()

# create a file object given the input variable
current_file = open(input_file)

# print to console and add a line break
print "First let's print the whole file:\n"

# call function to print the contents of the input files
print_all(current_file)

# print to console
print "Now let's rewind, kind of like a tape."

# move cursor back to beginning of file object
rewind(current_file)

# print to console:
print "Let's print three lines:"

# initialize cursor
current_line = 1
# print the line the cursor is at
print_a_line(current_line, current_file)

# move the cursor
current_line += 1
# print the line the cursor is at
print_a_line(current_line, current_file)

# move the cursor
current_line += 1
# print the line the cursor is at
print_a_line(current_line, current_file)
