from java.lang import RuntimeException
from java.util import ArrayList
from java.util.stream.Collectors import groupingBy, averagingDouble
from gov.ca.water.calgui.bo import WaterYearPeriodRangesFilter, WaterYearPeriod


def usesWaterYearDefinition():
    return True


def getName():
    return "Above Normal Years (60-20-20, ELT)"


def calculate(input):
    waterYearIndex = waterYearIndices.stream().filter(
        jp(lambda p: p.toString() == "SJR Index")).findAny().orElseThrow(
        js(lambda: RuntimeException("No SJR Index")))
    aboveNormal = waterYearIndex.getWaterYearPeriodRanges().getOrDefault(WaterYearPeriod("Above Normal"),
                                                                         ArrayList <> ())
    waterYearPeriodRangesFilter = WaterYearPeriodRangesFilter(aboveNormal, waterYearDefinition)
    return input.entrySet().stream().filter(waterYearPeriodRangesFilter).collect(groupingBy(
        jf(lambda e: e.getKey().getMonth()), averagingDouble(jdf(lambda e: e.getValue()))))
