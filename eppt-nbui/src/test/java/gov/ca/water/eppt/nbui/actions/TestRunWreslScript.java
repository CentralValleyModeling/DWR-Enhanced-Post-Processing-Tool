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

import java.util.logging.Logger;

import org.junit.jupiter.api.Test;

import hec.heclib.dss.HecDSSFileAccess;

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
		HecDSSFileAccess.setMessageLevel(HecDSSFileAccess.MESS_LEVEL_GENERAL);
		RunWreslScript runWreslScript = new RunWreslScript();
		runWreslScript.initReport(new String[]{"-config=J:\\DWR\\QA_QC\\SupportingDocs040219\\EPPT Supporting Doc 040219\\DEFAULT.config"});
	}
}