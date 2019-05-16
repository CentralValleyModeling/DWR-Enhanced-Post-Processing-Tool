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

package gov.ca.water.reportengine.detailedissues;

import gov.ca.water.reportengine.TestQAQCReportBase;

import java.io.File;
import java.net.URL;
import java.nio.file.Path;

public class TestDetailedIssuesReportBase extends TestQAQCReportBase
{

    public Path getWyTypeTablePath()
    {
        URL resource = this.getClass().getClassLoader().getResource("wytypes.table");
        return new File(resource.getPath()).toPath();
    }

    public Path getWyTypeNameLookupTablePath()
    {
        URL resource = this.getClass().getClassLoader().getResource("WYTypesLookup.csv");
        return new File(resource.getPath()).toPath();
    }

}
