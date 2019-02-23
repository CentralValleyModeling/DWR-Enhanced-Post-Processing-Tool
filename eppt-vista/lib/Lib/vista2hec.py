# Conversion from vista data sets to hec
def toHEC(rts):
    """
    Convert a Vista regular time series data set to hec.io.TimeSeriesContainer
    """
    pathname = rts.name
    values = rts.getYArray()
    import jarray
    times = jarray.zeros(len(values), 'i')
