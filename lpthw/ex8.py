# create a string variable
formatter = "%r %r %r %r"

# print to console:  string evaluated with four replacement numbers
print formatter % (1, 2, 3, 4)

# print to console:  string evaluated with four replacement strings
print formatter % ("one", "two", "three", "four")

# print to console:  string evaluated with four replacement binary statuses
print formatter % (True, False, False, True)

# print to console:  string evaluated with four replacement strings
print formatter % (formatter, formatter, formatter, formatter)

# print to console:  string evaluated with four replacement strings
print formatter % (
	"I had this thing.",
	"That you could type up right.",
	"But it didn't sing.",
	"So I said goodnight."
)
 