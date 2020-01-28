from java.util.stream.Collectors import averagingDouble
from java.util.stream.Collectors import groupingBy
from java.util import ArrayList
from gov.ca.water.calgui.bo import WaterYearPeriod, WaterYearType, WaterYearPeriodRange, \
	WaterYearAnnualPeriodRangesFilter


def getName():
	return "VIC Bias Correction Period (70-03)"


def calculate(data):
	name = "Driest Periods"
	waterYearPeriod = WaterYearPeriod(name)
	range1 = WaterYearPeriodRange(waterYearPeriod, WaterYearType(1970, waterYearPeriod),
								  WaterYearType(2003, waterYearPeriod))
	waterYearPeriodRanges = ArrayList()
	waterYearPeriodRanges.add(range1)
	waterYearPeriodRangesFilter = WaterYearAnnualPeriodRangesFilter(waterYearPeriodRanges)
	return data.entrySet().stream().filter(waterYearPeriodRangesFilter).mapToDouble(
		jdf(lambda e: e.getValue())).average()
