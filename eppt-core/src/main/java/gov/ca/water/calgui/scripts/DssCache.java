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
import java.util.Map;
import java.util.NavigableMap;
import java.util.Objects;
import java.util.concurrent.ConcurrentHashMap;

import gov.ca.water.calgui.project.EpptScenarioRun;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 09-09-2019
 */
public class DssCache
{
	private static final DssCache INSTANCE = new DssCache();
	private final ConcurrentHashMap<Key, NavigableMap<LocalDateTime, Double>> GUI_LINK_CACHE = new ConcurrentHashMap();
	private final ConcurrentHashMap<Key, NavigableMap<LocalDateTime, Double>> THRESHOLD_CACHE = new ConcurrentHashMap();
	private final ConcurrentHashMap<Key, NavigableMap<LocalDateTime, Double>> DTS_CACHE = new ConcurrentHashMap();

	/**
	 *  Class is not thread safe
	 * @return
	 */
	public static DssCache getInstance()
	{
		return INSTANCE;
	}

	public void clearCache()
	{
		GUI_LINK_CACHE.clear();
		THRESHOLD_CACHE.clear();
		DTS_CACHE.clear();
	}

	public NavigableMap<LocalDateTime, Double> readGuiLinkFromCache(EpptScenarioRun epptScenarioRun, int guiLink)
	{
		return GUI_LINK_CACHE.get(new Key(epptScenarioRun, guiLink));
	}

	public void addGuiLinkToCache(EpptScenarioRun epptScenarioRun, int guiLink, NavigableMap<LocalDateTime, Double> data)
	{
		GUI_LINK_CACHE.put(new Key(epptScenarioRun, guiLink), data);
	}

	public NavigableMap<LocalDateTime, Double> readDtsLinkFromCache(EpptScenarioRun epptScenarioRun, int dtsLink)
	{
		return DTS_CACHE.get(new Key(epptScenarioRun, dtsLink));
	}

	public void addDtsLinkToCache(EpptScenarioRun epptScenarioRun, int dtsLink, NavigableMap<LocalDateTime, Double> data)
	{
		DTS_CACHE.put(new Key(epptScenarioRun, dtsLink), data);
	}

	public NavigableMap<LocalDateTime, Double> readThresholdLinkFromCache(EpptScenarioRun epptScenarioRun, int dtsLink)
	{
		return THRESHOLD_CACHE.get(new Key(epptScenarioRun, dtsLink));
	}

	public void addThresholdLinkToCache(EpptScenarioRun epptScenarioRun, int thresholdLink, NavigableMap<LocalDateTime, Double> data)
	{
		THRESHOLD_CACHE.put(new Key(epptScenarioRun, thresholdLink), data);
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
}
