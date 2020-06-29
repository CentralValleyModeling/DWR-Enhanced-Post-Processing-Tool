#  Enhanced Post Processing Tool (EPPT) Copyright (c) 2020.
#
#   EPPT is copyrighted by the State of California, Department of Water Resources. It is licensed
#   under the GNU General Public License, version 2. This means it can be
#   copied, distributed, and modified freely, but you may not restrict others
#   in their ability to copy, distribute, and modify it. See the license below
#   for more details.
#
#   GNU General Public License

from gov.ca.water.calgui.bo import WaterYearDefinition, WaterYearPeriodRangesFilter, WaterYearPeriod, \
    WaterYearPeriodRange, WaterYearType, MonthPeriodFilter
from java.util import ArrayList
from java.util import TreeMap
from java.util.stream.Collectors import *
from java.time.format import TextStyle
from java.util import Locale
from java.lang import String, Double, System
from rma.stats import EmpiricalDist
from java.time import Month
from gov.ca.water.calgui.constant import Constant

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
    localDate = entry.getKey().minusMonths(1)
    return String.valueOf(localDate.getYear()) + " " + \
           localDate.getMonth().getDisplayName(TextStyle.FULL, Locale.getDefault())

def formatAsString(value, units):
    if units is None:
        units = "N/A"
    return String.format("%.0f", value)

def join(array):
    return ",".join(array)
