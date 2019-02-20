/*
    Copyright (C) 1996, 1997, 1998 State of California, Department of 
    Water Resources.

    VISTA : A VISualization Tool and Analyzer. 
	Version 1.0beta
	by Nicky Sandhu
    California Dept. of Water Resources
    Division of Planning, Delta Modeling Section
    1416 Ninth Street
    Sacramento, CA 95814
    (916)-653-7552
    nsandhu@water.ca.gov

    Send bug reports to nsandhu@water.ca.gov

    This program is licensed to you under the terms of the GNU General
    Public License, version 2, as published by the Free Software
    Foundation.

    You should have received a copy of the GNU General Public License
    along with this program; if not, contact Dr. Francis Chung, below,
    or the Free Software Foundation, 675 Mass Ave, Cambridge, MA
    02139, USA.

    THIS SOFTWARE AND DOCUMENTATION ARE PROVIDED BY THE CALIFORNIA
    DEPARTMENT OF WATER RESOURCES AND CONTRIBUTORS "AS IS" AND ANY
    EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
    IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
    PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE CALIFORNIA
    DEPARTMENT OF WATER RESOURCES OR ITS CONTRIBUTORS BE LIABLE FOR
    ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
    CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT
    OR SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA OR PROFITS; OR
    BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
    LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
    (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE
    USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
    DAMAGE.

    For more information about VISTA, contact:

    Dr. Francis Chung
    California Dept. of Water Resources
    Division of Planning, Delta Modeling Section
    1416 Ninth Street
    Sacramento, CA  95814
    916-653-5601
    chung@water.ca.gov

    or see our home page: http://wwwdelmod.water.ca.gov/

    Send bug reports to nsandhu@water.ca.gov or call (916)-653-7552

 */
package vista.db.dss;

import hec.heclib.util.HecTime;
import hec.heclib.util.Heclib;
import hec.heclib.util.stringContainer;
import vista.set.DataSet;
import vista.set.DataSetAttr;
import vista.set.SetUtils;
import vista.time.TimeInterval;

/**
 * 
 * The class with native function calls to the HEC-DSS library. Only limited
 * functionality of retrieving data is available at this time. Storing data
 * options will be added later.
 * 
 * Does not allow multithreaded access. However multi-user access is allowed.
 * 
 * @author Nicky Sandhu
 * @version $Id: DSSDataWriter.java,v 1.1 2003/10/02 20:48:45 redwood Exp $
 */
public class DSSDataWriter {
	public static final boolean DOUBLE_PRECISION = true;
	private String filename;
	private int[] ifltab;
	private int[] nvals;

	/**
    *
    */
	public DSSDataWriter(String filename) {
		this.filename = filename;
	}

	/**
   *
   */
	public void storeData(String pathname, long startJulmin, long endJulmin,
			DataSet ds, boolean storeFlags) {
		DataSetAttr attr = ds.getAttributes();
		DSSData data = new DSSData();
		int dataType = attr.getType();
		data._dataType = dataType;
		data._xType = attr.getXType();
		data._yType = attr.getYType();
		data._xUnits = attr.getXUnits();
		data._yUnits = attr.getYUnits();
		data._numberRead = ds.size();
		data._yValues = SetUtils.createYArray(ds);
		if (pathname.length() >= 1800)
			throw new IllegalArgumentException("Pathname: " + pathname
					+ " is too long (>1800chars)");
		if (dataType == DSSUtil.REGULAR_TIME_SERIES) {
			if (storeFlags)
				data._flags = SetUtils.createFlagArray(ds);
			storeTimeSeriesData(pathname, startJulmin, data,
					storeFlags);
		} else if (dataType == DSSUtil.IRREGULAR_TIME_SERIES) {
			data._xValues = SetUtils.createXArray(ds);
			if (storeFlags)
				data._flags = SetUtils.createFlagArray(ds);
			storeIrregularTimeSeriesData(pathname, 0, 0, data, storeFlags);
		} else if (dataType == DSSUtil.PAIRED) {
			data._xValues = SetUtils.createXArray(ds);
			storePairedData(pathname, data, storeFlags);
		} else {
			throw new IllegalArgumentException("Data type: " + dataType
					+ " is invalid");
		}
	}

	public void openDSSFile() {
		if (ifltab == null) {
			ifltab = DSSUtil.openDSSFile(filename, true);
		}
	}

	public void closeDSSFile() {
		if (ifltab != null) {
			DSSUtil.closeDSSFile(ifltab);
		}
	}

	/**
	 * time series data
	 */
	public synchronized void storeTimeSeriesData(String pathname,
			long startJulmin, DSSData data, boolean storeFlags) {
		int idate = (int) startJulmin / 1440;
		int startTime = (int) startJulmin % 1440;
		String startDate = Heclib.juldat(idate, 104);
		stringContainer hourMinutes = new stringContainer();
		Heclib.m2ihm(startTime, hourMinutes);
		int istoreFlags = storeFlags ? 1 : 0;
		int[] flags = storeFlags ? data._flags : new int[1];
		int[] userHeader = new int[] { 0 };
		int numberHeader = 0;
		// TODO: figure out values for compression, plan, etc.
		int plan = 0;
		int compression = 0;
		int baseValueSet = 0;
		float baseValue = 0;
		int highDelta = 0;
		int delatPrecision = 0;
		int[] status = new int[] { 0 };
		if (DOUBLE_PRECISION) {
			Heclib.zsrtsxd(ifltab, pathname, startDate, hourMinutes.toString(),
					data._numberRead, data._yValues, flags, istoreFlags,
					data._yUnits, data._yType, userHeader, numberHeader, plan,
					compression, baseValue, baseValueSet, highDelta,
					delatPrecision, status);
		} else {
			float[] yvalues = extractYValuesAsFloat(data);
			Heclib.zsrtsx(ifltab, pathname, startDate, hourMinutes.toString(),
					data._numberRead, yvalues, flags, istoreFlags,
					data._yUnits, data._yType, userHeader, numberHeader, plan,
					compression, baseValue, baseValueSet, highDelta,
					delatPrecision, status);

		}
		//TODO: check status flag and throw exception
	}

	public float[] extractYValuesAsFloat(DSSData data) {
		float[] yvalues = new float[data._numberRead];
		for (int i = 0; i < yvalues.length; i++) {
			yvalues[i] = (float) data._yValues[i];
		}
		return yvalues;
	}

	/**
	 * store irregular time series
	 */
	public synchronized void storeIrregularTimeSeriesData(String pathname,
			long startJulmin, long endJulmin, DSSData data, boolean storeFlags) {
		double[] values = data._yValues;
		int[] times = new int[data._numberRead];
		for (int i = 0; i < data._numberRead; i++) {
			times[i] = (int) data._xValues[i];
		}
		int numberVals = data._numberRead;
		int startDate = (int) startJulmin / 1440;
		int[] flags = data._flags;
		int storeFlagData = 0;
		if (storeFlags) {
			flags = data._flags;
			storeFlagData = 1;
			if (flags == null) {
				flags = new int[data._numberRead];
			}
		} else {
			storeFlagData = 0;
			flags = new int[] { 0 };
		}
		String unitsX = data._yUnits;
		String typeX = data._yType;
		int[] userHeader = new int[] { 0 };
		int numberHeader = 0;
		int inFlag = 0; // 1 is to replace data not merge data
		int[] status = new int[1];
		if (DOUBLE_PRECISION) {
			Heclib.zsitsxd(ifltab, pathname, times, values, numberVals,
					startDate, flags, storeFlagData, unitsX, typeX, userHeader,
					numberHeader, inFlag, status);
		} else {
			float[] yvalues = extractYValuesAsFloat(data);
			Heclib.zsitsx(ifltab, pathname, times, yvalues, numberVals,
					startDate, flags, storeFlagData, unitsX, typeX, userHeader,
					numberHeader, inFlag, status);

		}
	}

	/**
	 * store paired data
	 */
	public synchronized void storePairedData(String pathname, DSSData data,
			boolean storeFlags) {
		throw new RuntimeException("Not yet implemented!");
	}

	private static final boolean DEBUG = false;
}
