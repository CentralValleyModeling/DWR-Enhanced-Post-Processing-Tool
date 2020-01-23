from java.lang import RuntimeException
from java.util import ArrayList
from java.util.stream.Collectors import groupingBy, averagingDouble
from gov.ca.water.calgui.bo import WaterYearAnnualPeriodRangesFilter, WaterYearPeriod


def getName():
	return "Below Normal Years (60-20-20)"


def calculate(data):
	waterYearIndex = waterYearIndices.stream().filter(
		jp(lambda p: p.toString() == "SJR Index")).findAny().orElseThrow(
		js(lambda: RuntimeException("No SJR Index")))
	belowNormal = waterYearIndex.getAllLongWaterYearPeriodRanges().getOrDefault(WaterYearPeriod("Below Normal"),
																				ArrayList())
	waterYearPeriodRangesFilter = WaterYearAnnualPeriodRangesFilter(belowNormal)
	return data.entrySet().stream().filter(waterYearPeriodRangesFilter).mapToDouble(
		jdf(lambda e: e.getValue())).average()
