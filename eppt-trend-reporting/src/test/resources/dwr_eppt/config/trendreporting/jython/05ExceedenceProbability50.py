from rma.stats import EmpiricalDist
from java.util import ArrayList


def usesWaterYearDefinition():
	return False


def calculate(input):
	return toExceedance(ArrayList(input.values()))


def calculateExceedance(doubles):
	empiricalDist = EmpiricalDist(EmpiricalDist.InterpType.LINEAR, doubles)
	return empiricalDist.invCDF(getCdfPercent())


def toExceedance(v):
	return calculateExceedance(v.stream().mapToDouble(jdf(lambda d: d)).toArray())


def getName():
	return "50% Exceedence Probability"


def getCdfPercent():
	return .5
