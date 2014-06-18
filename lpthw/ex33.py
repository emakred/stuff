from sys import argv

script, numinput, increment = argv

def tester(fcn_num, incr):
	i = 0
	numbers = []

	while i < fcn_num:
		print "At the top i is %d" % i
		numbers.append(i)

		i = i + incr
		print "Numbers row: ", numbers
		print "At the bottom i is %d" % i

	print "The numbers: "

	for num in numbers:
		print num

tester(int(numinput),int(increment))