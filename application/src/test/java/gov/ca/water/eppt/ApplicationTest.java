/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */

package gov.ca.water.eppt;

import java.util.logging.Level;

import junit.framework.Test;
import org.netbeans.junit.NbModuleSuite;
import org.netbeans.junit.NbTestCase;

public class ApplicationTest extends NbTestCase
{

	public ApplicationTest(String n)
	{
		super(n);
	}

	public static Test suite()
	{
		return NbModuleSuite.createConfiguration(ApplicationTest.class).
				gui(false).
									failOnMessage(Level.WARNING). // works at least in RELEASE71
																		  failOnException(Level.INFO).
																		  enableClasspathModules(false).
																		  clusters(".*").
																		  suite(); // RELEASE71+, else use NbModuleSuite.create(NbModuleSuite.createConfiguration(...))
	}

	public void testApplication()
	{
		// pass if there are merely no warnings/exceptions
        /* Example of using Jelly Tools (additional test dependencies required) with gui(true):
        new ActionNoBlock("Help|About", null).performMenu();
        new NbDialogOperator("About").closeByButton();
         */
	}

}
