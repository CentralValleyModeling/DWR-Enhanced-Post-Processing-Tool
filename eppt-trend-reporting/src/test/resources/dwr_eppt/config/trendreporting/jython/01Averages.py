from java.util.stream.Collectors import averagingDouble
from java.util.stream.Collectors import groupingBy


def usesWaterYearDefinition():
    return False


def getName():
    return "Averages"


def calculate(input):
    return input.entrySet().stream().collect(
        groupingBy(jf(lambda e: e.getKey().getMonth()), averagingDouble(jdf(lambda e: e.getValue()))))
