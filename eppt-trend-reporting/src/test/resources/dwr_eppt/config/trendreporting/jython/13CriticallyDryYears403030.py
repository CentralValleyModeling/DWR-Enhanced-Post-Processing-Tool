from java.lang import RuntimeException
from java.util import ArrayList
from java.util.stream.Collectors import groupingBy, averagingDouble
from gov.ca.water.calgui.bo import WaterYearPeriodRangesFilter, WaterYearPeriod


def usesWaterYearDefinition():
    return True


def getName():
    return "Critically Dry Years (40-30-30, ELT)"


def calculate(input):
    waterYearIndex = waterYearIndices.stream().filter(
        jp(lambda p: p.toString() == "SAC Index")).findAny().orElseThrow(
        js(lambda: RuntimeException("No SAC index")))
    critical = waterYearIndex.getWaterYearPeriodRanges().getOrDefault(WaterYearPeriod("Critical"), ArrayList <> ())
    waterYearPeriodRangesFilter = WaterYearPeriodRangesFilter(critical, waterYearDefinition)
    return input.entrySet().stream().filter(waterYearPeriodRangesFilter).collect(groupingBy(
        jf(lambda e: e.getKey().getMonth()), averagingDouble(jdf(lambda e: e.getValue()))))
