from java.lang import RuntimeException
from java.util import ArrayList
from java.util.stream.Collectors import groupingBy, averagingDouble
from gov.ca.water.calgui.bo import WaterYearAnnualPeriodRangesFilter, WaterYearPeriod


def usesWaterYearDefinition():
    return True


def getName():
    return "Dry and Critically Dry Years (60-20-20)"


def calculate(data):
    waterYearIndex = waterYearIndices.stream().filter(
        jp(lambda p: p.toString() == "SJR Index")).findAny().orElseThrow(
        js(lambda: RuntimeException("No SJR Index")))
    dry = waterYearIndex.getAllLongWaterYearPeriodRanges().getOrDefault(WaterYearPeriod("Dry"), ArrayList <> ())
    dryFilter = WaterYearAnnualPeriodRangesFilter(dry)
    critical = waterYearIndex.getAllLongWaterYearPeriodRanges().getOrDefault(WaterYearPeriod("Critical"), ArrayList <> ())
    criticalFilter = WaterYearAnnualPeriodRangesFilter(critical)
    return data.entrySet().stream().filter(criticalFilter.or(dryFilter)).mapToDouble(jdf(lambda e:e.getValue())).average()
