from java.util.stream.Collectors import averagingDouble
from java.util.stream.Collectors import groupingBy
from java.util import ArrayList
from gov.ca.water.calgui.bo import WaterYearPeriod, WaterYearType, WaterYearPeriodRange, WaterYearPeriodRangesFilter


def usesWaterYearDefinition():
    return True

def getName():
    return "VIC Bias Correction Period (70-03)"


def calculate(data):
    name = "Driest Periods"
    waterYearPeriod = WaterYearPeriod(name)
    range1 = WaterYearPeriodRange(waterYearPeriod, WaterYearType(1970, waterYearPeriod),
                                  WaterYearType(2003, waterYearPeriod))
    waterYearPeriodRanges = ArrayList()
    waterYearPeriodRanges.add(range1)
    waterYearPeriodRangesFilter = WaterYearPeriodRangesFilter(waterYearPeriodRanges, waterYearDefinition)
    return data.entrySet().stream().filter(waterYearPeriodRangesFilter).collect(
        groupingBy(jf(lambda e: e.getKey().getMonth()), averagingDouble(jdf(lambda e: e.getValue()))))
