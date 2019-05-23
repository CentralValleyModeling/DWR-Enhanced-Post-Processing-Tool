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
import com.fasterxml.jackson.annotation.JsonRootName;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 05-22-2019
 */
@JsonRootName("chart")
public class TsChartOption
{
	public enum ZoomType
	{
		X("x"), Y("y"), XY("xy");
		private final String _type;


		private ZoomType(String type)
		{
			_type = type;
		}

		@Override
		public String toString()
		{
			return _type;
		}

	}

	private ZoomType _zoomType = ZoomType.XY;

	@JsonGetter("zoomType")
	public String getZoomeType()
	{
		return _zoomType.toString();
	}

	public void setZoomType(ZoomType zoomType)
	{
		_zoomType = zoomType;
	}
}
