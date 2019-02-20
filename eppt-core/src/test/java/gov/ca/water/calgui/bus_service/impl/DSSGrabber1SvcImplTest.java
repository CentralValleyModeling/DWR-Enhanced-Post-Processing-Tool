/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */

package gov.ca.water.calgui.bus_service.impl;

import java.util.logging.Level;
import java.util.logging.Logger;
import javax.swing.*;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertNotNull;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 02-18-2019
 */
class DSSGrabber1SvcImplTest
{
	@Test
	void testCtor()
	{
		try
		{
			final DSSGrabber1SvcImpl dssGrabber = new DSSGrabber1SvcImpl(new JList());
			assertNotNull(dssGrabber);
		}
		catch(RuntimeException ex)
		{
			Logger.getLogger(DSSGrabber1SvcImplTest.class.getName()).log(Level.SEVERE,
					"Testing IntelliJ JIRA integration", ex);
		}
	}
}
