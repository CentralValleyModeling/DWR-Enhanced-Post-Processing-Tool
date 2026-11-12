/*
 * Enhanced Post Processing Tool (EPPT) Copyright (c) 2026.
 *
 *  EPPT is copyrighted by the State of California, Department of Water Resources. It is licensed
 *  under the GNU General Public License, version 2. This means it can be
 *  copied, distributed, and modified freely, but you may not restrict others
 *  in their ability to copy, distribute, and modify it. See the license below
 *  for more details.
 *
 *  GNU General Public License
 */

package gov.ca.water.calgui.busservice.impl;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.nio.file.Paths;

import calsim.app.DerivedTimeSeries;
import calsim.app.MultipleTimeSeries;
import gov.ca.water.calgui.EpptInitializationException;
import gov.ca.water.calgui.bo.GUILinksAllModelsBO;
import gov.ca.water.calgui.bo.ResultUtilsBO;
import gov.ca.water.calgui.project.NamedDssPath;

import hec.io.TimeSeriesContainer;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertNull;

/**
 * Regression tests for GH-257: a DTS/MTS operand that references another DTS by name
 * which is not present in the WRIMS project must be skipped gracefully rather than
 * causing a NullPointerException.
 */
class DSSGrabber2SvcImplTest
{
	static
	{
		if(!System.getProperty("user.dir").replace('\\', '/').endsWith("target/test-classes"))
		{
			String userDir = Paths.get(System.getProperty("user.dir")).resolve("target/test-classes")
									.toAbsolutePath().toString();
			System.setProperty("user.dir", userDir);
		}
	}

	@BeforeAll
	static void setup() throws EpptInitializationException
	{
		GuiLinksSeedDataSvcImpl.createSeedDataSvcImplInstance();
	}

	private static TimeSeriesContainer invokeGetOneSeriesWRIMS(DSSGrabber2SvcImpl grabber, DerivedTimeSeries dts2)
			throws NoSuchMethodException, IllegalAccessException
	{
		Method method = DSSGrabber2SvcImpl.class.getDeclaredMethod("getOneSeriesWRIMS",
				NamedDssPath.class, GUILinksAllModelsBO.Model.class, DerivedTimeSeries.class);
		method.setAccessible(true);
		return invoke(method, grabber, null, null, dts2);
	}

	private static TimeSeriesContainer invokeGetOneSeriesWRIMS(DSSGrabber2SvcImpl grabber, int i, MultipleTimeSeries mts2)
			throws NoSuchMethodException, IllegalAccessException
	{
		Method method = DSSGrabber2SvcImpl.class.getDeclaredMethod("getOneSeriesWRIMS",
				NamedDssPath.class, GUILinksAllModelsBO.Model.class, int.class, MultipleTimeSeries.class);
		method.setAccessible(true);
		return invoke(method, grabber, null, null, i, mts2);
	}

	private static TimeSeriesContainer invoke(Method method, DSSGrabber2SvcImpl grabber, Object... args)
			throws IllegalAccessException
	{
		try
		{
			return (TimeSeriesContainer) method.invoke(grabber, args);
		}
		catch(InvocationTargetException ite)
		{
			Throwable cause = ite.getCause();
			throw new AssertionError("getOneSeriesWRIMS threw: " + cause, cause);
		}
	}

	@Test
	void dtsOperandReferencingUnknownDtsIsSkippedInsteadOfThrowing() throws Exception
	{
		assertNull(ResultUtilsBO.getResultUtilsInstance().getProject().getDTS("MISSING_DTS_TEST.DTS"),
				"Precondition: the referenced DTS must not exist in the project");

		DerivedTimeSeries parentDts = new DerivedTimeSeries("PARENT_DTS_TEST");
		parentDts.setDTSNameAt(0, "MISSING_DTS_TEST");

		DSSGrabber2SvcImpl grabber = new DSSGrabber2SvcImpl(parentDts, new MultipleTimeSeries());

		TimeSeriesContainer result = invokeGetOneSeriesWRIMS(grabber, parentDts);

		assertNull(result, "No operand resolved, so the overall result should be null");
	}

	@Test
	void mtsOperandReferencingUnknownDtsIsSkippedInsteadOfThrowing() throws Exception
	{
		assertNull(ResultUtilsBO.getResultUtilsInstance().getProject().getDTS("MISSING_DTS_TEST_2.DTS"),
				"Precondition: the referenced DTS must not exist in the project");

		MultipleTimeSeries mts = new MultipleTimeSeries("PARENT_MTS_TEST");
		mts.setDTSNameAt(0, "MISSING_DTS_TEST_2");

		DSSGrabber2SvcImpl grabber = new DSSGrabber2SvcImpl(new DerivedTimeSeries("UNUSED_DTS_TEST"), mts);

		TimeSeriesContainer result = invokeGetOneSeriesWRIMS(grabber, 0, mts);

		assertNull(result, "The dangling reference should be skipped, yielding a null result");
	}
}
