# Add up boundary flows for DSM2
from gov.ca.dsm2.input.parser import Parser
from hecutils import *

from vutils import *


def sumFlowsFromTable(table):
    sum = 0
    for index in range(table.getNumberOfRows()):
        sign = table.getValue(index, "SIGN")
        file = table.getValue(index, "FILE")
        path = table.getValue(index, "PATH")
        rts = findpath(opendss(file).findpath(path), exact=1)
        if sign == "-1":
            rts = -rts
            sum = sum + rts
    return sum


def sumAllFlows():
    p = Parser()
    tables = p.parseModel("hydro_echo.inp");
    boundaryTable = tables.getTableNamed("BOUNDARY_FLOW")
    sourceFlowTable = tables.getTableNamed("SOURCE_FLOW")
    sourceFlowReservoirTable = tables.getTableNamed("SOURCE_FLOW_RESERVOIR")
    sum = sumFlowsFromTable(boundaryTable)
    sum = sum + sumFlowsFromTable(sourceFlowTable)
    sum = sum + sumflowsFromTable(sourceFlowReservoirTable)
    return sum


if __name__ == '__main__':
    sum = sumAllFlows()
    plot(sum)
