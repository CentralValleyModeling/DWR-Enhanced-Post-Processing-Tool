from java.util import ArrayList

def usesWaterYearDefinition():
    return False

def getName():
    return "Maximums"


def calculate(input):
    return ArrayList(input.values()).stream().mapToDouble(jdf(lambda e:e)).max()
