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

package gov.ca.water.reportengine;

import gov.ca.water.calgui.EpptInitializationException;
import gov.ca.water.reportengine.jython.JythonScriptBuilder;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 09-09-2019
 */
public class TestJythonScriptBuilder
{
	@Test
	public void testScriptBuilder() throws EpptInitializationException
	{
		String original = System.getProperty("user.dir");
		System.setProperty("user.dir", original + "\\target\\test-classes");
		JythonScriptBuilder.createInstance();
//		String function = JythonScriptBuilder.getInstance().buildFunctionFromTemplate("PeriodLessThan0AverageFunction(105)");
//		assertEquals("dssReader.getDtsData(105).entrySet().stream().filter(commonPeriodFilter).mapToDouble(jdf(lambda e : e.getValue())).filter(jpDouble(lambda v : v < 0)).count()", function);
	}
}
