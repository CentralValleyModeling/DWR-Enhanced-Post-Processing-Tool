from java.lang import RuntimeException
from java.util import ArrayList
from java.util.stream.Collectors import groupingBy, averagingDouble
from gov.ca.water.calgui.bo import WaterYearAnnualPeriodRangesFilter, WaterYearPeriod


def usesWaterYearDefinition():
    return True


def getName():
    return "Critically Dry Years (40-30-30)"


def calculate(data):
    waterYearIndex = waterYearIndices.stream().filter(
        jp(lambda p: p.toString() == "SAC Index")).findAny().orElseThrow(
        js(lambda: RuntimeException("No SAC index")))
    critical = waterYearIndex.getAllLongWaterYearPeriodRanges().getOrDefault(WaterYearPeriod("Critical"), ArrayList <> ())
    waterYearPeriodRangesFilter = WaterYearAnnualPeriodRangesFilter(critical)
    return data.entrySet().stream().filter(waterYearPeriodRangesFilter).mapToDouble(jdf(lambda e:e.getValue())).average()
