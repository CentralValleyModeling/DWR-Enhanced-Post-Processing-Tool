/*
 * Enhanced Post Processing Tool (EPPT) Copyright (c) 2020.
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

import java.io.File;
import java.time.LocalDate;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Optional;
import java.util.logging.Handler;
import java.util.logging.Level;
import java.util.logging.LogRecord;
import java.util.logging.Logger;

import gov.ca.water.calgui.project.EpptDssContainer;
import gov.ca.water.calgui.project.EpptScenarioRun;
import javafx.scene.paint.Color;
import mil.army.usace.hec.metadata.constants.NumericalConstants;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import hec.heclib.dss.HecDss;
import hec.heclib.util.HecTime;
import hec.io.TimeSeriesContainer;
import rma.util.RMAConst;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Checks the incomplete-time-window detection in {@link DSSGrabber1SvcImpl} against a real DSS record.
 * The record runs from 31 October 1921 24:00 through 30 September 2003 24:00 (water years 1922-2003).
 */
class TestDssGrabberCoverage
{
	private static final String PATH = "/CALSIM/DEM_D893_PLS/DEMAND-SWP-LS/01JAN1920/1MON/2020D09E/";
	private static final LocalDate WY1922_START = LocalDate.of(1921, 10, 1);
	private static final LocalDate WY2003_END = LocalDate.of(2003, 9, 30);
	private static String dssFile;

	private final List<LogRecord> _records = new ArrayList<>();
	private final Logger _logger = Logger.getLogger(DSSGrabber1SvcImpl.class.getName());
	private final Handler _handler = new Handler()
	{
		@Override
		public void publish(LogRecord logRecord)
		{
			_records.add(logRecord);
		}

		@Override
		public void flush()
		{
		}

		@Override
		public void close()
		{
		}
	};

	@BeforeAll
	static void setup()
	{
		dssFile = new File(TestDssGrabberCoverage.class.getClassLoader().getResource("SampleSV_Base.dss").getFile()).getAbsolutePath();
	}

	@BeforeEach
	void addHandler()
	{
		_logger.addHandler(_handler);
	}

	@AfterEach
	void removeHandler()
	{
		_logger.removeHandler(_handler);
	}

	/**
	 * Reads the record the same way DSSGrabber1SvcImpl does, with the padded window from DisplayFrames.
	 */
	private static TimeSeriesContainer read(LocalDate paddedStart, LocalDate paddedEnd) throws Exception
	{
		HecTime start = new HecTime();
		start.setYearMonthDay(paddedStart.getYear(), paddedStart.getMonthValue(), paddedStart.getDayOfMonth(), 0);
		HecTime end = new HecTime();
		end.setYearMonthDay(paddedEnd.getYear(), paddedEnd.getMonthValue(), paddedEnd.getDayOfMonth(), 0);
		HecDss hecDss = HecDss.open(dssFile);
		try
		{
			return (TimeSeriesContainer) hecDss.get(PATH, start.toString(), end.toString());
		}
		finally
		{
			hecDss.close();
		}
	}

	@Test
	void fullPeriodIsComplete() throws Exception
	{
		// DisplayFrames pads the read window: one month early at the start, two days short at the end
		TimeSeriesContainer tsc = read(LocalDate.of(1921, 9, 3), LocalDate.of(2003, 9, 29));
		assertEquals(984, tsc.numberValues);
		assertFalse(DSSGrabber1SvcImpl.findCoverageGap(tsc, WY1922_START, WY2003_END).isPresent());
	}

	@Test
	void subPeriodIsComplete() throws Exception
	{
		TimeSeriesContainer tsc = read(LocalDate.of(1949, 9, 3), LocalDate.of(1960, 9, 29));
		assertFalse(DSSGrabber1SvcImpl.findCoverageGap(tsc, LocalDate.of(1949, 10, 1), LocalDate.of(1960, 9, 30)).isPresent());
	}

	@Test
	void periodPastEndOfRecordIsIncomplete() throws Exception
	{
		TimeSeriesContainer tsc = read(LocalDate.of(1921, 9, 3), LocalDate.of(2005, 9, 29));
		Optional<String> gap = DSSGrabber1SvcImpl.findCoverageGap(tsc, WY1922_START, LocalDate.of(2005, 9, 30));
		assertTrue(gap.isPresent());
		assertTrue(gap.get().contains("30 September 2003"), gap.get());
	}

	@Test
	void periodBeforeStartOfRecordIsIncomplete() throws Exception
	{
		TimeSeriesContainer tsc = read(LocalDate.of(1920, 9, 3), LocalDate.of(2003, 9, 29));
		Optional<String> gap = DSSGrabber1SvcImpl.findCoverageGap(tsc, LocalDate.of(1920, 10, 1), WY2003_END);
		assertTrue(gap.isPresent());
		assertTrue(gap.get().contains("31 October 1921"), gap.get());
	}

	@Test
	void missingValuesAtEndAreIncomplete() throws Exception
	{
		TimeSeriesContainer tsc = read(LocalDate.of(1921, 9, 3), LocalDate.of(2003, 9, 29));
		tsc.values[tsc.numberValues - 1] = NumericalConstants.HEC_UNDEFINED_DOUBLE;
		tsc.values[tsc.numberValues - 2] = -901.0;
		assertTrue(DSSGrabber1SvcImpl.findCoverageGap(tsc, WY1922_START, WY2003_END).isPresent());
	}

	@Test
	void missingValuesAtStartAreIncomplete() throws Exception
	{
		TimeSeriesContainer tsc = read(LocalDate.of(1921, 9, 3), LocalDate.of(2003, 9, 29));
		tsc.values[0] = NumericalConstants.HEC_UNDEFINED_DOUBLE;
		assertTrue(DSSGrabber1SvcImpl.findCoverageGap(tsc, WY1922_START, WY2003_END).isPresent());
	}

	@Test
	void logsSevereWithScenarioName() throws Exception
	{
		TimeSeriesContainer tsc = read(LocalDate.of(1921, 9, 3), LocalDate.of(2003, 9, 29));
		EpptScenarioRun scenarioRun = new EpptScenarioRun("Alt Scenario", "", null, null, null, null,
				new EpptDssContainer(null, null, null, null, Collections.emptyList()), Color.BLUE);
		DSSGrabber1SvcImpl grabber = new DSSGrabber1SvcImpl();
		grabber.setRequiredCoverage(WY1922_START, LocalDate.of(2005, 9, 30));
		grabber.checkTimeWindowCoverage(scenarioRun, "DEM_D893_PLS/DEMAND-SWP-LS", tsc);
		assertEquals(1, _records.size());
		LogRecord logRecord = _records.getFirst();
		assertEquals(Level.SEVERE, logRecord.getLevel());
		assertEquals(scenarioRun, logRecord.getParameters()[0]);
		assertEquals("DEM_D893_PLS/DEMAND-SWP-LS", logRecord.getParameters()[1]);
	}

	@Test
	void noLogWhenCoverageNotRequested() throws Exception
	{
		TimeSeriesContainer tsc = read(LocalDate.of(1921, 9, 3), LocalDate.of(2003, 9, 29));
		DSSGrabber1SvcImpl grabber = new DSSGrabber1SvcImpl();
		grabber.checkTimeWindowCoverage(null, "DEM_D893_PLS/DEMAND-SWP-LS", tsc);
		assertTrue(_records.isEmpty());
	}

	@Test
	void noLogWhenComplete() throws Exception
	{
		TimeSeriesContainer tsc = read(LocalDate.of(1921, 9, 3), LocalDate.of(2003, 9, 29));
		DSSGrabber1SvcImpl grabber = new DSSGrabber1SvcImpl();
		grabber.setRequiredCoverage(WY1922_START, WY2003_END);
		grabber.checkTimeWindowCoverage(null, "DEM_D893_PLS/DEMAND-SWP-LS", tsc);
		assertTrue(_records.stream().noneMatch(r -> r.getLevel() == Level.SEVERE));
	}
}
