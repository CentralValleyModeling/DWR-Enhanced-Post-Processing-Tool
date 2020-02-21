from java.lang import RuntimeException
from java.util import ArrayList
from java.util.stream.Collectors import groupingBy, averagingDouble
from gov.ca.water.calgui.bo import WaterYearAnnualPeriodRangesFilter, WaterYearPeriod


def getName():
	return "Dry Years (60-20-20)"


def calculate(data):
	waterYearIndex = waterYearIndices.stream().filter(
		jp(lambda p: p.toString().lower() == "sjrindex")).findAny().orElseThrow(
		js(lambda: RuntimeException("No SJR Index")))
	dry = waterYearIndex.getAllLongWaterYearPeriodRanges().getOrDefault(WaterYearPeriod("Dry"), ArrayList())
	waterYearPeriodRangesFilter = WaterYearAnnualPeriodRangesFilter(dry)
	return data.entrySet().stream().filter(waterYearPeriodRangesFilter).mapToDouble(
		jdf(lambda e: e.getValue())).average()
