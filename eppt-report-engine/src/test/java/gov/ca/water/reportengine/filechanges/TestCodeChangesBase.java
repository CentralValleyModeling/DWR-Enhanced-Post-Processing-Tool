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

package gov.ca.water.reportengine.filechanges;

import gov.ca.water.reportengine.TestQAQCReportBase;

import java.io.File;
import java.net.URL;
import java.nio.file.Path;

public class TestCodeChangesBase extends TestQAQCReportBase
{

    protected Path getCodeChangesCsvPath()
    {
        URL codeChangesPath = this.getClass().getClassLoader().getResource("CodeChangesDSSPaths.csv");
        return new File(codeChangesPath.getPath()).toPath();
    }

    protected Path getBaseOutputPath()
    {
        URL baseOutputPath = this.getClass().getClassLoader().getResource("BaseOutputDirectory\\run\\Wytypes\\wytypes.wresl");
        return new File(baseOutputPath.getPath()).toPath();

    }
    protected Path getAltOutputPath()
    {
        URL altOutputPath = this.getClass().getClassLoader().getResource("AltOutputDirectory\\run\\Wytypes\\wytypes.wresl");
        return new File(altOutputPath.getPath()).toPath();
    }


}
