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

package gov.ca.water.plots.timeseries;

import com.fasterxml.jackson.annotation.JsonGetter;
import javafx.geometry.Pos;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 05-27-2019
 */
public class CreditsOption
{
	private boolean _enabled = false;
	private String _creditsText = "";
	private String _creditsUrl = "";

	@JsonGetter("enabled")
	public boolean isEnabled()
	{
		return _enabled;
	}

	public void setEnabled(boolean enabled)
	{
		_enabled = enabled;
	}

	@JsonGetter("text")
	public String getCreditsText()
	{
		return _creditsText;
	}

	public void setCreditsText(String creditsText)
	{
		_creditsText = creditsText;
	}

	@JsonGetter("href")
	public String getCreditsUrl()
	{
		return _creditsUrl;
	}

	public void setCreditsUrl(String creditsUrl)
	{
		_creditsUrl = creditsUrl;
	}

}
