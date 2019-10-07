from java.util.stream.Collectors import summarizingDouble
from java.util.stream.Collectors import collectingAndThen
from java.util.stream.Collectors import groupingBy

def usesWaterYearDefinition():
    return False

def getName():
    return "Minimums"


def calculate(input):
    return input.entrySet().stream().collect(groupingBy(jf(lambda e: e.getKey().getMonth()),
                                                        collectingAndThen(summarizingDouble(
                                                            jdf(lambda e: e.getValue())), jf(lambda s: s.getMin()))))
