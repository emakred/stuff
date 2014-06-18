# # get access to the script call variables
# from sys import argv

# # unpack the script variable ('filename')
# script, filename = argv

print "Type the filename: "
filename = raw_input("> ")

# file object for 'filename'
txt = open(filename)

# print to console:  the filename given
print "Here's your file %r:" % filename
# print to console: contents of file
print txt.read()

# close file object
txt.close()


# print to console
print "Type the filename again:"
# capture the input in 'file_again'
file_again = raw_input("> ")

# file object for 'file_again'
txt_again = open(file_again)

# print to console: contents of file
print txt_again.read()

# close file object
txt_again.close()