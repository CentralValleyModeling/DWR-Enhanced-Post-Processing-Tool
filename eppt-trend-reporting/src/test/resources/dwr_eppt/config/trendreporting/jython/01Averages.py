from java.util.stream.Collectors import averagingDouble
from java.util.stream.Collectors import groupingBy


def getName():
    return "Averages"

def calculate(input):
    return input.entrySet().stream().collect(
        groupingBy(jf(lambda e: e.getKey().getMonth()), averagingDouble(jf(lambda e: e.getValue()))))
