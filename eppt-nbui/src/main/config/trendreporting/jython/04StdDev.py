from gov.ca.water.calgui.busservice.impl import EpptDoubleStatistics


def getName():
	return "Standard Deviations"


def calculate(input):
	return EpptDoubleStatistics.calculateStandardDeviation(input)
