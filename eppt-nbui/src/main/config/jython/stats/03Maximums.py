from java.util import ArrayList


def getName():
	return "Maximums"


def calculate(input):
	return ArrayList(input).stream().mapToDouble(jdf(lambda e: e)).max()
