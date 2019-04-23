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

package gov.ca.water.eppt.nbui.actions;

import java.awt.Desktop;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.NoSuchFileException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 04-19-2019
 */
class TestRunWreslScript
{
	private static final Logger LOGGER = Logger.getLogger(TestRunWreslScript.class.getName());

	@Test
	void actionPerformed() throws Exception
	{
		RunWreslScript runWreslScript = new RunWreslScript();
		Path outputPath = Paths.get("C:\\Users\\adam\\Documents\\EPPT\\Project1\\Reports\\QAQC_Report_Test.pdf");
		try
		{
			Files.delete(outputPath);
		}
		catch(NoSuchFileException ex)
		{
			LOGGER.log(Level.FINE, "PDF already removed", ex);
		}
		Path jrxmlPath = Paths.get("C:\\Users\\adam\\Documents\\EPPT\\Project1\\Reports\\QAQC_Report.jrxml");
		runWreslScript.runReportWithOutputFile(outputPath, jrxmlPath);
		assertTrue(outputPath.toFile().isFile(), "PDF Must be generated");
	}
}