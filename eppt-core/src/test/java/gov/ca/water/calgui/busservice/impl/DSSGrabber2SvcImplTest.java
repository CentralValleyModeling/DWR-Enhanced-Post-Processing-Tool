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

import java.lang.reflect.Method;

import calsim.app.DerivedTimeSeries;
import calsim.app.MultipleTimeSeries;
import gov.ca.water.calgui.bo.GUILinksAllModelsBO;
import gov.ca.water.calgui.bo.ResultUtilsBO;
import gov.ca.water.calgui.project.NamedDssPath;

import hec.io.TimeSeriesContainer;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertNull;

/**
 * Regression tests for GH-257: a DTS/MTS operand that references another DTS by name
 * which is not present in the WRIMS project must be skipped gracefully rather than
 * causing a NullPointerException.
 */
class DSSGrabber2SvcImplTest
{
	@Test
	void dtsOperandReferencingUnknownDtsIsSkippedInsteadOfThrowing() throws Exception
	{
		assertNull(ResultUtilsBO.getResultUtilsInstance().getProject().getDTS("MISSING_DTS_TEST.DTS"),
				"Precondition: the referenced DTS must not exist in the project");

		DerivedTimeSeries parentDts = new DerivedTimeSeries("PARENT_DTS_TEST");
		parentDts.setDTSNameAt(0, "MISSING_DTS_TEST");

		DSSGrabber2SvcImpl grabber = new DSSGrabber2SvcImpl(parentDts, new MultipleTimeSeries());

		Method getOneSeriesWRIMS = DSSGrabber2SvcImpl.class.getDeclaredMethod("getOneSeriesWRIMS",
				NamedDssPath.class, GUILinksAllModelsBO.Model.class, DerivedTimeSeries.class);
		getOneSeriesWRIMS.setAccessible(true);

		TimeSeriesContainer result;
		try
		{
			result = (TimeSeriesContainer) getOneSeriesWRIMS.invoke(grabber,
					new Object[]{null, null, parentDts});
		}
		catch(java.lang.reflect.InvocationTargetException ite)
		{
			throw new AssertionError("Underlying method threw: " + ite.getCause(), ite.getCause());
		}

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

		Method getOneSeriesWRIMS = DSSGrabber2SvcImpl.class.getDeclaredMethod("getOneSeriesWRIMS",
				NamedDssPath.class, GUILinksAllModelsBO.Model.class, int.class, MultipleTimeSeries.class);
		getOneSeriesWRIMS.setAccessible(true);

		TimeSeriesContainer result = (TimeSeriesContainer) assertDoesNotThrow(
				() -> getOneSeriesWRIMS.invoke(grabber, null, null, 0, mts),
				"Resolving an MTS with a dangling DTS reference must not throw");

		assertNull(result, "The dangling reference should be skipped, yielding a null result");
	}
}
