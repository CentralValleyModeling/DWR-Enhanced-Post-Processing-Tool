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

import java.util.Objects;
import java.util.concurrent.ConcurrentHashMap;

import gov.ca.water.calgui.project.EpptScenarioRun;

import hec.io.TimeSeriesContainer;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 09-09-2019
 */
public class DssCache
{
	private final ConcurrentHashMap<Key, TSValue> _guiLinkCache = new ConcurrentHashMap<>();
	private final ConcurrentHashMap<Key, TSValue> _thresholdCache = new ConcurrentHashMap<>();
	private final ConcurrentHashMap<Key, TSValue> _dtsCache = new ConcurrentHashMap<>();

	public void clearCache()
	{
		_guiLinkCache.clear();
		_thresholdCache.clear();
		_dtsCache.clear();
	}

	TSValue readGuiLinkFromCache(EpptScenarioRun epptScenarioRun, int guiLink)
	{
		return _guiLinkCache.get(new Key(epptScenarioRun, guiLink));
	}

	void addGuiLinkToCache(EpptScenarioRun epptScenarioRun, int guiLink, TimeSeriesContainer data, String originalUnits)
	{
		_guiLinkCache.put(new Key(epptScenarioRun, guiLink), new TSValue(data, originalUnits));
	}

	TSValue readDtsLinkFromCache(EpptScenarioRun epptScenarioRun, int dtsLink)
	{
		return _dtsCache.get(new Key(epptScenarioRun, dtsLink));
	}

	void addDtsLinkToCache(EpptScenarioRun epptScenarioRun, int dtsLink, TimeSeriesContainer data, String originalUnits)
	{
		_dtsCache.put(new Key(epptScenarioRun, dtsLink), new TSValue(data, originalUnits));
	}

	TSValue readThresholdLinkFromCache(EpptScenarioRun epptScenarioRun, int dtsLink)
	{
		return _thresholdCache.get(new Key(epptScenarioRun, dtsLink));
	}

	void addThresholdLinkToCache(EpptScenarioRun epptScenarioRun, int thresholdLink, TimeSeriesContainer data, String originalUnits)
	{
		_thresholdCache.put(new Key(epptScenarioRun, thresholdLink), new TSValue(data, originalUnits));
	}

	private static final class Key
	{
		private final EpptScenarioRun _scenarioRun;
		private final int _link;

		private Key(EpptScenarioRun scenarioRun, int link)
		{
			_scenarioRun = scenarioRun;
			_link = link;
		}

		@Override
		public boolean equals(Object o)
		{
			if(this == o)
			{
				return true;
			}
			if(o == null || getClass() != o.getClass())
			{
				return false;
			}
			final Key key = (Key) o;
			return _link == key._link &&
					Objects.equals(_scenarioRun, key._scenarioRun);
		}

		@Override
		public int hashCode()
		{
			return Objects.hash(_scenarioRun, _link);
		}
	}

	static final class TSValue
	{
		private final TimeSeriesContainer _tsc;
		private final String _originalUnits;

		private TSValue(TimeSeriesContainer tsc, String originalUnits)
		{
			_tsc = tsc;
			_originalUnits = originalUnits;
		}

		TimeSeriesContainer getTsc()
		{
			return _tsc;
		}

		String getOriginalUnits()
		{
			return _originalUnits;
		}
	}
}
