from gov.ca.water.calgui.bo import WaterYearDefinition, WaterYearPeriodRangesFilter, WaterYearPeriod, \
    WaterYearPeriodRange, WaterYearType, MonthPeriodFilter
from java.util import ArrayList
from java.util import TreeMap
from java.util.stream.Collectors import *
from java.time.format import TextStyle
from java.util import Locale
from java.lang import String, Double
from rma.stats import EmpiricalDist
from gov.ca.water.calgui.scripts import JythonScriptRunner
from gov.ca.water.calgui.scripts.JythonScriptRunner import *
from java.time import Month

def getPeriodStartYear(date, startOfPeriod):
    period = WaterYearPeriod("")
    waterYearType = WaterYearType(date.getYear(), period)
    range = WaterYearPeriodRange(period, waterYearType, waterYearType)
    term = WaterYearDefinition("", startOfPeriod, startOfPeriod.minus(1))
    return range.getStart(term).getYear()


def calculateExceedance(values):
    retval = TreeMap()
    doubles = values.stream().mapToDouble(jdf(lambda v: v)).sorted().toArray()
    empiricalDist = EmpiricalDist(EmpiricalDist.InterpType.LINEAR, doubles)
    i = 0
    while i < len(doubles):
        retval.put(empiricalDist.getExceed(i), doubles[i])
        i = i + 1
    return retval

def generateExceedanceValues():
    return jf(lambda v: calculateExceedance(ArrayList(v.values())))


def mapToPeriodStartYear(startOfPeriodMonth):
    return jf(lambda v: getPeriodStartYear(v.getKey(), startOfPeriodMonth))


def buildPeriodFilterForEndMonth(waterYearPeriodRange, endMonth):
    return WaterYearPeriodRangesFilter(waterYearPeriodRange, WaterYearDefinition("", endMonth.plus(1), endMonth))

def getMatchingGuiLinkEntry(guiLinkId, entry):
    return dssReader.getGuiLinkData(guiLinkId).entrySet().stream().filter(jf(lambda e : e.getKey().equals(entry.getKey()))).findAny()

def buildListPrefix(entry):
    return buildMonthYearEntry(entry) + ":"

def buildMonthYearEntry(entry):
    return entry.getKey().getMonth().minus(1).getDisplayName(TextStyle.FULL,
                                                             Locale.getDefault()) + " " + String.valueOf(entry.getKey().getYear())

def formatAsString(value):
    return " " + String.format("%.0f", value) + " (TAF)"