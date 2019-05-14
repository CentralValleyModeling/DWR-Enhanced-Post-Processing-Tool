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

package gov.ca.water.eppt.jasperengine;

import java.io.File;
import java.net.URL;
import java.nio.file.Files;
import java.nio.file.NoSuchFileException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 04-26-2019
 */
public class TestJasperReportRunner
{
	private static final Logger LOGGER = Logger.getLogger(TestJasperReportRunner.class.getName());

	@Test
	public void testReportRunner() throws Exception
	{
		JasperReportRunner jasperReportRunner = new JasperReportRunner();
		URL resource = getClass().getClassLoader().getResource("QAQC_Report.jrxml");
		assertNotNull(resource, "JRXML File must be defined");
		Path jrxmlPath = new File(resource.getPath()).toPath();
		Path output = Paths.get("Output.pdf");
		try
		{
			if(output.toFile().exists())
			{
				Files.delete(output);
			}
		}
		catch(NoSuchFileException ex)
		{
			LOGGER.log(Level.SEVERE, "File already deleted", ex);
		}
		jasperReportRunner.runReportWithOutputFile(output, jrxmlPath);
		assertTrue(output.toFile().exists(), "Output PDF should be generated");
	}
}
