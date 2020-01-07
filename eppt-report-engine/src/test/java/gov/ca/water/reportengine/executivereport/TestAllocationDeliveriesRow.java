/*
 * Enhanced Post Processing Tool (EPPT) Copyright (c) 2019.
 *
 * EPPT is copyrighted by the State of California, Department of Water Resources. It is licensed
 * under the GNU General Public License, version 2. This means it can be
 * copied, distributed, and modified freely, but you may not restrict others
 * in their ability to copy, distribute, and modify it. See the license below
 * for more details.
 *
 * GNU General Public License
 */

package gov.ca.water.reportengine.executivereport;


import java.time.LocalDateTime;
import java.util.Map;
import java.util.TreeMap;

import org.junit.jupiter.api.Test;


public class TestAllocationDeliveriesRow extends ExecutiveReportTestBase
{
    @Test
    void testAllocationDeliveriesRowWithBaseOnly() throws Exception
    {
        testBaseOnly( "Allocations and Deliveries");
    }

    @Test
    void testAllocationDeliveriesRowWithOneAlternativeSameModel() throws Exception
    {
        testOneAlternativeSameModel( "Allocations and Deliveries");
//        Map<LocalDateTime, Double> values = new TreeMap<>();
//        values.entrySet().stream()
//              .peek(e->System.out.println(e.getKey().toString() + " " + e.getValue()))
    }
}
