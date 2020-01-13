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

package gov.ca.water.calgui.presentation;

import gov.ca.water.calgui.bo.GUILinksAllModelsBO;

import hec.io.TimeSeriesContainer;

public class DisplayInput
{
	private final GUILinksAllModelsBO _guiLink;
	private final TimeSeriesContainer[] _primaryResults;
	private final TimeSeriesContainer[] _secondaryResults;
	private final TimeSeriesContainer[] _diffResults;
	private final TimeSeriesContainer[][] _excResults;
	private final TimeSeriesContainer[][] _sexcResults;
	private final TimeSeriesContainer[][] _dexcResults;

	public DisplayInput(GUILinksAllModelsBO guiLink, TimeSeriesContainer[] primaryResults, TimeSeriesContainer[] secondaryResults,
						TimeSeriesContainer[] diffResults, TimeSeriesContainer[][] excResults, TimeSeriesContainer[][] sexcResults,
						TimeSeriesContainer[][] dexcResults)
	{
		_guiLink = guiLink;
		_primaryResults = primaryResults;
		_secondaryResults = secondaryResults;
		_diffResults = diffResults;
		_excResults = excResults;
		_sexcResults = sexcResults;
		_dexcResults = dexcResults;
	}

	public GUILinksAllModelsBO getGuiLink()
	{
		return _guiLink;
	}

	public TimeSeriesContainer[] getPrimaryResults()
	{
		return _primaryResults;
	}

	public TimeSeriesContainer[] getSecondaryResults()
	{
		return _secondaryResults;
	}

	public TimeSeriesContainer[] getDiffResults()
	{
		return _diffResults;
	}

	public TimeSeriesContainer[][] getExcResults()
	{
		return _excResults;
	}

	public TimeSeriesContainer[][] getSexcResults()
	{
		return _sexcResults;
	}

	public TimeSeriesContainer[][] getDexcResults()
	{
		return _dexcResults;
	}
}
