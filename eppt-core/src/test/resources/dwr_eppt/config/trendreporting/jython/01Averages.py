from java.util import ArrayList


def getName():
	return "Averages"


def calculate(input):
	return ArrayList(input).stream().mapToDouble(jdf(lambda e: e)).average()
