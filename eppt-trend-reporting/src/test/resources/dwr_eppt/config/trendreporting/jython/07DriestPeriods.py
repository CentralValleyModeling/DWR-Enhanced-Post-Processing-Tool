from java.util.stream.Collectors import averagingDouble
from java.util.stream.Collectors import groupingBy
from java.util import ArrayList
from gov.ca.water.calgui.bo import WaterYearPeriod, WaterYearType, WaterYearPeriodRange, WaterYearAnnualPeriodRangesFilter

def usesWaterYearDefinition():
    return True

def getName():
    return "Driest Periods (29-34, 76-77, 87-92)"


def calculate(data):
    name = "Driest Periods"
    waterYearPeriod = WaterYearPeriod(name)
    range1 = WaterYearPeriodRange(waterYearPeriod, WaterYearType(1929, waterYearPeriod),
                                  WaterYearType(1934, waterYearPeriod))
    range2 = WaterYearPeriodRange(waterYearPeriod, WaterYearType(1976, waterYearPeriod),
                                  WaterYearType(1977, waterYearPeriod))
    range3 = WaterYearPeriodRange(waterYearPeriod, WaterYearType(1987, waterYearPeriod),
                                  WaterYearType(1992, waterYearPeriod))
    waterYearPeriodRanges = ArrayList()
    waterYearPeriodRanges.add(range1)
    waterYearPeriodRanges.add(range2)
    waterYearPeriodRanges.add(range3)
    waterYearPeriodRangesFilter = WaterYearAnnualPeriodRangesFilter(waterYearPeriodRanges)
    return data.entrySet().stream().filter(waterYearPeriodRangesFilter).mapToDouble(jdf(lambda e:e.getValue())).average()
