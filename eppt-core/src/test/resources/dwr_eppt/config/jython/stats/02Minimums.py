from java.util import ArrayList


def getName():
	return "Minimums"


def calculate(input):
	return ArrayList(input).stream().mapToDouble(jdf(lambda e: e)).min()
