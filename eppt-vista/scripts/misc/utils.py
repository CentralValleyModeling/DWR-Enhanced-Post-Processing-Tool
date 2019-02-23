from vista.set import RegularTimeSeries

from vtimeseries import apply


# Define some utility functions
def bound(ts, lower, upper):
    """
    bound(ts,lower,upper)
        creates a new time series with bounds of lower and upper (if defined, pass None if undefined) and returns it
    """
    tsnew = ts * 1

    def bounding_function(y):
        if upper is not None and y > upper:
            return upper
        elif lower is not None and y < lower:
            return lower
        else:
            return y

    #
    apply(tsnew, bounding_function)
    return tsnew


#
def _test_bound():
    ts = RegularTimeSeries("test_bound", "01JAN1990 0100", "1HOUR", [0, 1.5, 2, -3, 3, 2, -5, 0])
    tsl0 = bound(ts, 0, None)
    tsg0 = bound(ts, None, 0)


#
if __name__ == '__main__':
    _test_bound()
    print "Passed"
