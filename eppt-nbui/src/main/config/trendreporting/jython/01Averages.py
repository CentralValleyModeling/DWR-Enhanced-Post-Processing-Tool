from java.util import ArrayList


def getName():
	return "Averages"


def calculate(input):
	return ArrayList(input.values()).stream().mapToDouble(jdf(lambda e: e)).average()
