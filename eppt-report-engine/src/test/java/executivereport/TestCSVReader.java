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

package executivereport;

import org.junit.Test;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import java.io.File;
import java.net.URL;
import java.nio.file.Path;
import java.nio.file.Paths;

public class TestCSVReader
{
    private Path getCSVPath()
    {
        URL resource = this.getClass().getClassLoader().getResource("Category_V1.01.csv");
        return new File( resource.getPath()).toPath();
    }


//    @Test
//    public void testReadingCSV() throws Exception
//    {
//        URL resource = this.getClass().getClassLoader().getResource("SamplePstPrcss_Base_v1.01.dss");
//
//        ExecutiveReportProcessor processor = new ExecutiveReportProcessor();
//        Path dssFilePath = new File( resource.getPath()).toPath();
//        processor.processFile(getCSVPath(), dssFilePath);
//    }





}
