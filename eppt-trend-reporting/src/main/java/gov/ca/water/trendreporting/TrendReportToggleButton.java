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

package gov.ca.water.trendreporting;

import javafx.scene.control.ToggleButton;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 09-24-2019
 */
class TrendReportToggleButton extends ToggleButton
{
	private final TrendReportTabConfig _trendReportTabConfig;

	TrendReportToggleButton(TrendReportTabConfig trendReportTabConfig)
	{
		super(trendReportTabConfig.getText());
		super.setMaxWidth(Double.MAX_VALUE);
		_trendReportTabConfig = trendReportTabConfig;
	}

	TrendReportTabConfig getTrendReportTabConfig()
	{
		return _trendReportTabConfig;
	}
}
