from rma.stats import EmpiricalDist
from java.util.stream.Collectors import mapping
from java.util.stream.Collectors import collectingAndThen
from java.util.stream.Collectors import toList
from java.util.stream.Collectors import groupingBy

def usesWaterYearDefinition():
    return False

def calculate(input):
    return input.entrySet().stream().collect(groupingBy(jf(lambda e: e.getKey().getMonth()), mapping(
        jf(lambda e: e.getValue()), collectingAndThen(toList(), jf(lambda e: toExceedance(e))))))


def calculateExceedance(doubles):
    empiricalDist = EmpiricalDist(EmpiricalDist.InterpType.LINEAR, doubles)
    return empiricalDist.invCDF(getCdfPercent())


def toExceedance(v):
    return calculateExceedance(v.stream().mapToDouble(jdf(lambda d: d)).toArray())


def getName():
    return "25% Exceedence Probability"


def getCdfPercent():
    return .75;
