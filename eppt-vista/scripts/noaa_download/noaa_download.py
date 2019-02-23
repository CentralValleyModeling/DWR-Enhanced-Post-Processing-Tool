# Use their web service to download data a month at time :(
# To see which stations have data use this http://tidesandcurrents.noaa.gov/gmap3/index.shtml?type=VerifiedData&region= to get station ID
# Then checkout their web service availability here
# http://opendap.co-ops.nos.noaa.gov/axis/
# Example retrieving data for Port Chicago: Station ID 9415144
# San Francisco  station id is 9414290
# http://opendap.co-ops.nos.noaa.gov/axis/webservices/waterlevelverifiedsixmin/response.jsp?stationId=9415144&beginDate=19960101&endDate=19960131&datum=NAVD&unit=1&timeZone=1&format=text&Submit=Submit
from hec.heclib.dss import *
from hec.heclib.util import *
from hec.io import *
from hec.script import *
from java.text import SimpleDateFormat
from java.util import Calendar

sdf = SimpleDateFormat("yyyyMMdd")


def get_noaa_data_as_array_of_lines(stationId, beginDate, endDate):
    import urllib
    # url for 6 min verified data
    # url = "http://opendap.co-ops.nos.noaa.gov/axis/webservices/waterlevelverifiedsixmin/response.jsp?stationId=%s&beginDate=%s&endDate=%s&datum=MSL&unit=1&timeZone=1&format=text&Submit=Submit"%(stationId, beginDate, endDate)
    # url for hourly verified data
    url = "https://tidesandcurrents.noaa.gov/api/datagetter?product=hourly_height&application=NOS.COOPS.TAC.WL&begin_date=%s&end_date=%s&datum=NAVD&station=%s&time_zone=GMT&units=english&format=csv" % (
        beginDate, endDate, stationId)
    print url
    response = urllib.urlopen(url)
    result = response.readlines()
    response.close()
    return result


def parse_noaa_data(result):
    find_8th_equals = False
    find_equals_counter = 0
    line_counter = 0
    while find_equals_counter < 8:
        if result[line_counter].startswith("========="):
            find_equals_counter = find_equals_counter + 1
        line_counter = line_counter + 1
    data = []
    while line_counter < len(result):
        fields = result[line_counter].split()
        line_counter = line_counter + 1
        if len(fields) > 4:
            data.append(fields[1:4])
    return data


def incr_by_month(date):
    c = Calendar.getInstance()
    c.setTime(sdf.parse(date))
    c.add(Calendar.MONTH, 1)
    return sdf.format(c.getTime())


def month_end_date(date):
    c = Calendar.getInstance()
    c.setTime(sdf.parse(date))
    days = c.getActualMaximum(Calendar.DAY_OF_MONTH)
    c.set(Calendar.DAY_OF_MONTH, days)
    return sdf.format(c.getTime())


def get_noaa_data(stationId, sdate, edate, fileh):
    # do one month at time
    fetch_date = sdate
    while fetch_date != edate:
        beginDate = fetch_date
        endDate = month_end_date(beginDate)
        print 'Fetching starting: %s' % fetch_date
        result = get_noaa_data_as_array_of_lines(stationId, beginDate, endDate)
        # print result
        data = parse_noaa_data(result)
        for line in data:
            if len(line) >= 3:
                fileh.write("%s,%s,%s\n" % (line[0], line[1], line[2]))
        fetch_date = incr_by_month(fetch_date)
        fileh.flush()


def test_get_1_month():
    stationId = '9415144'
    beginDate = '19960101'
    endDate = '19960131'
    result = get_noaa_data_as_array_of_lines(stationId, beginDate, endDate)
    data = parse_noaa_data(result)
    return data


def test_get_1_year():
    stationId = '9415144'
    beginDate = '19960101'
    endDate = '20090101'
    return get_noaa_data(stationId, beginDate, endDate)


def to_hectime(datestr, timestr):
    return HecTime(datestr, timestr)


def load_ts(file, fullName):
    fh = open(file)
    lines = fh.readlines()
    tsc = TimeSeriesContainer()
    tsc.fullName = fullName
    tsc.interval = -1
    values = []
    times = []
    for line in lines:
        try:
            fields = line.split(",")
            curtime = to_hectime(fields[0], fields[1])
            if curtime.value() == -777777:
                continue
            times.append(curtime.value())
            values.append(float(fields[2]))
        except:
            print 'Could not parse line: %s' % line
            continue
    fh.close()
    tsc.times = times
    tsc.values = values
    tsc.numberValues = len(values)
    tsc.units = "FT"
    tsc.type = "INST-VAL"
    return tsc


def download_to_file(stationId, beginDate, endDate, filename):
    fileh = open(filename, 'w')
    try:
        get_noaa_data(stationId, beginDate, endDate, fileh)
    finally:
        fileh.close();


if __name__ == '__main__':
    station_info = {
        # 'PORT CHICAGO':['9415144'],
        'SF': ['9414290']
        # 'MTZ':['9415102'] #
        # 'RICHMOND':['9414863']
    }
    beginDate = '19900101'
    endDate = '20170101'
    for name in station_info.keys():
        print 'Downloading data for %s' % name
        stationId = station_info[name][0]
        filename = 'z:/temp/%s_noaa_stage.csv' % (name.lower().replace(' ', '_'))
        fullName = "/NOAA/%s/STAGE//IR-DAY/MSL/" % (name.upper())
        download_to_file(stationId, beginDate, endDate, filename)
        tsc = load_ts(filename, fullName)
        dss = HecDss.open('z:/temp/noaa.dss')
        dss.put(tsc)
    dss.done()
#
