from java.lang import RuntimeException
from java.util import ArrayList
from java.util.stream.Collectors import groupingBy, averagingDouble
from gov.ca.water.calgui.bo import WaterYearPeriodRangesFilter, WaterYearPeriod


def usesWaterYearDefinition():
    return True


def getName():
    return "Dry and Critically Dry Years (60-20-20, ELT)"


def calculate(input):
    waterYearIndex = waterYearIndices.stream().filter(
        jp(lambda p: p.toString() == "SJR Index")).findAny().orElseThrow(
        js(lambda: RuntimeException("No SJR Index")))
    dry = waterYearIndex.getWaterYearPeriodRanges().getOrDefault(WaterYearPeriod("Dry"), ArrayList <> ())
    dryFilter = WaterYearPeriodRangesFilter(dry, waterYearDefinition)
    critical = waterYearIndex.getWaterYearPeriodRanges().getOrDefault(WaterYearPeriod("Critical"), ArrayList <> ())
    criticalFilter = WaterYearPeriodRangesFilter(critical, waterYearDefinition)
    return input.entrySet().stream().filter(criticalFilter. or (dryFilter)).collect(groupingBy(
        jf(lambda e: e.getKey().getMonth()), averagingDouble(jdf(lambda e: e.getValue()))))
