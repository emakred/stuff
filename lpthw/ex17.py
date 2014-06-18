from sys import argv
from os.path import exists

script, from_file, to_file = argv

indata = open(from_file).read()

print "Copying the %d byte file %s to %s" % (len(indata), from_file, to_file)

# print "Does the output file exist? %r" % exists(to_file)
# print "Ready, hit RETURN to continue, CTRL-C to abort."
# raw_input()

out_file = open(to_file, 'w')
out_file.write(indata)

print "Alright, all done."

out_file.close()



