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

package gov.ca.water.calgui.scripts;

import java.time.LocalDateTime;
import java.time.ZoneId;
import java.util.Collections;
import java.util.Date;
import java.util.NavigableMap;
import java.util.Objects;
import java.util.TimeZone;
import java.util.TreeMap;
import java.util.concurrent.TimeUnit;
import java.util.logging.Level;
import java.util.regex.Pattern;

import com.google.common.flogger.FluentLogger;
import gov.ca.water.calgui.bo.DetailedIssue;
import gov.ca.water.calgui.busservice.impl.DSSGrabber1SvcImpl;
import gov.ca.water.calgui.busservice.impl.DetailedIssuesReader;
import gov.ca.water.calgui.project.EpptScenarioRun;
import gov.ca.water.calgui.project.NamedDssPath;

import hec.heclib.dss.DSSPathname;
import hec.heclib.dss.HecDataManager;
import hec.heclib.dss.HecDss;
import hec.heclib.util.HecTime;
import hec.io.DataContainer;
import hec.io.TimeSeriesContainer;
import rma.util.RMAConst;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 08-14-2019
 */
public class DssReader
{
	private static final FluentLogger LOGGER = FluentLogger.forEnclosingClass();
	private static final Pattern DSS_PATH_SPLITTER_PATTERN = Pattern.compile("/");
	private final EpptScenarioRun _scenarioRun;
	private String _units;

	public DssReader(EpptScenarioRun scenarioRun)
	{
		_scenarioRun = scenarioRun;
	}

	public NavigableMap<LocalDateTime, Double> getGuiLinkData(int guiID)
	{
		DssCache instance = DssCache.getInstance();
		NavigableMap<LocalDateTime, Double> retval = instance.readGuiLinkFromCache(_scenarioRun, guiID);
		if(retval == null)
		{
			try
			{
				DSSGrabber1SvcImpl dssGrabber1Svc = buildDssGrabber(_scenarioRun, guiID, 0);
				TimeSeriesContainer[] primarySeries = dssGrabber1Svc.getPrimarySeries();
				retval = timeSeriesContainerToMap(primarySeries);
				instance.addGuiLinkToCache(_scenarioRun, guiID, retval);
			}
			catch(RuntimeException e)
			{
				LOGGER.atSevere().withCause(e).log("Error reading GUI Links Data: " + guiID);
			}
		}
		return retval;
	}

	private NavigableMap<LocalDateTime, Double> timeSeriesContainerToMap(TimeSeriesContainer[] primarySeries)
	{
		NavigableMap<LocalDateTime, Double> retval = new TreeMap<>();
		if(primarySeries != null && primarySeries[0] != null)
		{
			TimeSeriesContainer tsc = primarySeries[0];
			if(tsc.times != null)
			{
				_units = tsc.getUnits();
				for(int i = 0; i < tsc.times.length; i++)
				{
					HecTime hecTime = new HecTime();
					hecTime.set(tsc.times[i], tsc.timeGranularitySeconds, tsc.julianBaseDate);
					double value = tsc.getValue(i);
					int offset = (int) TimeUnit.MILLISECONDS.toMinutes(TimeZone.getDefault().getRawOffset());
					Date javaDate = hecTime.getJavaDate(offset);
					LocalDateTime localDateTime = LocalDateTime.ofInstant(javaDate.toInstant(), ZoneId.systemDefault());
					if(RMAConst.isValidValue(value))
					{
						if(tsc.getParameterName().toLowerCase().contains("percent"))
						{
							value *= 100;
						}
						retval.put(localDateTime, value);
					}
					else
					{
						retval.put(localDateTime, Double.NaN);
						LOGGER.at(Level.FINE).log("Invalid value %d found at: %s", localDateTime);
					}
				}
			}
		}
		return retval;
	}

	private DSSGrabber1SvcImpl buildDssGrabber(EpptScenarioRun epptScenarioRun, int guiID, int thresholdId)
	{
		DSSGrabber1SvcImpl grabber1Svc = new DSSGrabber1SvcImpl();
		grabber1Svc.setScenarioRuns(epptScenarioRun, Collections.emptyList());
		grabber1Svc.setLocation(Integer.toString(guiID));
		grabber1Svc.setThresholdId(thresholdId);
		return grabber1Svc;
	}

	@SuppressWarnings("unchecked")
	public NavigableMap<LocalDateTime, Double> getDtsData(int dtsId) throws DssMissingRecordException
	{
		DssCache instance = DssCache.getInstance();
		NavigableMap<LocalDateTime, Double> retval = instance.readDtsLinkFromCache(_scenarioRun, dtsId);
		if(retval == null)
		{
			retval = new TreeMap<>();
			String bPart = DetailedIssuesReader.getInstance().getDetailedIssues()
											   .stream()
											   .filter(di -> di.getDetailedIssueId() == dtsId)
											   .map(DetailedIssue::getLinkedVar)
											   .findAny()
											   .orElse("");
			NamedDssPath dtsDssFile = _scenarioRun.getDssContainer().getDtsDssFile();
			String aPart = dtsDssFile.getAPart();
			String fPart = dtsDssFile.getFPart();
			String ePart = dtsDssFile.getEPart();
			DSSPathname pathname = new DSSPathname();
			pathname.setAPart(aPart);
			pathname.setFPart(fPart);
			pathname.setEPart(ePart);
			pathname.setBPart(bPart);
			String fileName = dtsDssFile.getDssPath().toString();
			HecDss hecDss = null;
			try
			{
				if(HecDataManager.doesDSSFileExist(fileName))
				{
					hecDss = HecDss.open(fileName);
					String dssPath = hecDss.getCatalogedPathnames()
										   .stream()
										   .filter(Objects::nonNull)
										   .map(s -> new DSSPathname(s.toString()))
										   .filter(d -> ((DSSPathname) d).getAPart().equalsIgnoreCase(aPart))
										   .filter(d -> ((DSSPathname) d).getBPart().equalsIgnoreCase(bPart))
										   .filter(d -> ((DSSPathname) d).getFPart().equalsIgnoreCase(fPart))
										   .findAny()
										   .orElse("")
										   .toString();
					if(!dssPath.isEmpty())
					{
						DataContainer dataContainer = hecDss.get(dssPath, true);
						if(dataContainer instanceof TimeSeriesContainer)
						{
							retval = timeSeriesContainerToMap(new TimeSeriesContainer[]{(TimeSeriesContainer) dataContainer});
							instance.addDtsLinkToCache(_scenarioRun, dtsId, retval);
						}
						else
						{
							throw new DssMissingRecordException("Unable to find matching DTS path for: " + bPart);
						}
					}
					else
					{
						throw new DssMissingRecordException("Unable to find matching DTS path for: " + bPart);
					}
				}
			}
			catch(DssMissingRecordException e)
			{
				throw e;
			}
			catch(Exception e)
			{
				LOGGER.atSevere().withCause(e).log("Unable to read DSS file: %s", fileName);
			}
			finally
			{
				if(hecDss != null)
				{
					hecDss.close();
				}
			}
		}
		return retval;
	}

	public NavigableMap<LocalDateTime, Double> getThresholdData(int thresholdId)
	{
		DssCache instance = DssCache.getInstance();
		NavigableMap<LocalDateTime, Double> retval = instance.readThresholdLinkFromCache(_scenarioRun, thresholdId);
		if(retval == null)
		{
			try
			{
				DSSGrabber1SvcImpl dssGrabber1Svc = buildDssGrabber(_scenarioRun, 102, thresholdId);
				TimeSeriesContainer[] threshold = dssGrabber1Svc.getThresholdTimeSeries();
				retval = timeSeriesContainerToMap(threshold);
				instance.addThresholdLinkToCache(_scenarioRun, thresholdId, retval);
			}
			catch(RuntimeException e)
			{
				LOGGER.atSevere().withCause(e).log("Error reading Threshold Links Data: " + thresholdId);
			}
		}
		return retval;
	}

	public String getUnits()
	{
		return _units;
	}
}
