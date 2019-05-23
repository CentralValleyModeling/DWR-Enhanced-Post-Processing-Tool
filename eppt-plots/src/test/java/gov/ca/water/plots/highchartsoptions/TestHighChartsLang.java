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

package gov.ca.water.plots.highchartsoptions;

import java.util.logging.Level;
import java.util.logging.Logger;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.SerializationFeature;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 05-22-2019
 */
public class TestHighChartsLang
{
	private static final Logger LOGGER = Logger.getLogger(TestHighChartsLang.class.getName());
	@Test
	void testDefault() throws Exception
	{
		String result = HighChartsLang.toJson(new HighChartsLang());
		LOGGER.log(Level.INFO, result);
		assertTrue(result.contains("\"decimalPoint\":\".\""));
		assertTrue(result.contains("\"thousandsSep\":\",\""));
		assertTrue(result.contains("\"lang\":{"));
	}
}
