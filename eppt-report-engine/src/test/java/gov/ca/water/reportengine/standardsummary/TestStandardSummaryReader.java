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

package gov.ca.water.reportengine.standardsummary;

import java.net.URI;
import java.net.URISyntaxException;
import java.net.URL;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.List;
import java.util.Map;

import gov.ca.water.reportengine.EpptReportException;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 08-26-2019
 */
public class TestStandardSummaryReader
{
	@Test
	public void testReader() throws URISyntaxException, EpptReportException
	{
		URL resource = TestStandardSummaryReader.class.getClassLoader().getResource("dwr_eppt/config/SummaryComponents_Types-Draft_8-26.csv");
		assertNotNull(resource);
		URI uri = resource.toURI();
		Path path = Paths.get(
				uri);
		StandardSummaryReader standardSummaryReader = new StandardSummaryReader(path, new StandardSummaryErrors());
		List<String> orderedChartIds = standardSummaryReader.getOrderedChartIds();
		Map<String, EpptChart> stringEpptChartMap = standardSummaryReader.readLines();
		assertEquals(170, stringEpptChartMap.size());
		assertEquals(170, orderedChartIds.size());
	}
}
