/*
    Copyright (C) 1996-2000 State of California, Department of 
    Water Resources.

    VISTA : A VISualization Tool and Analyzer. 
	Version 1.0
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
package vista.app.schematic;

import vista.db.dss.DSSUtil;
import vista.set.DataReference;
import vista.set.DataSetIterator;
import vista.set.Group;
import vista.set.Pathname;
import vista.set.RegularTimeSeries;
import vista.set.TimeSeriesMath;
import vista.time.Time;
import vista.time.TimeFactory;
import vista.time.TimeInterval;
import vista.time.TimeWindow;

/**
   * 
   */
public class ChannelData {
	/**
    * 
    */
	public ChannelData(int channelNumber, String filename, String pathname,
			String startTime, String timeInterval) {
		_filename = filename;
		_pathname = pathname;
		_stime = TimeFactory.getInstance().createTime(startTime);
		_ti = TimeFactory.getInstance().createTimeInterval(timeInterval);
		getNextChunkOfData();
		_channelId = channelNumber;
	}

	/**
    *
    */
	public int getChannelId() {
		return _channelId;
	}

	/**
    * 
    */
	public float getNextValue() {
		if (_iterator == null)
			return 0;
		if (_iterator.atEnd())
			getNextChunkOfData();
		if (_iterator == null)
			return 0;
		//
		float val = (float) _iterator.getElement().getY();
		_iterator.advance();
		return val;
	}

	/**
    * 
    */
	private void getNextChunkOfData() {
		if (_currentTimeWindow == null) {
			_currentTimeWindow = TimeFactory.getInstance().createTimeWindow(
					_stime, _stime.__add__("1YEAR"));
		} else {
			Time st = _currentTimeWindow.getStartTime();
			Time et = _currentTimeWindow.getEndTime();
			if (et.compare(_stime) < 0) { // if beyond end of time window...
				_currentTimeWindow = TimeFactory.getInstance()
						.createTimeWindow(_stime, _stime.__add__("1YEAR"));
			}
		}
		if (_reference == null) {
			_pathname = _pathname.replace('+', '.');
			Pathname path = Pathname.createPathname(_pathname);
			path.setPart(Pathname.D_PART, ".*");
			Group g = DSSUtil.createGroup("local", _filename);
			g.filterBy(path.toString());
			if (g.getNumberOfDataReferences() == 0) {
				throw new RuntimeException("No data found for "
						+ path.toString());
			} else if (g.getNumberOfDataReferences() > 1) {
				throw new RuntimeException("More than one data set found for "
						+ path.toString());
			}
			_reference = g.getDataReference(0);
		}
		//
		DataReference ref = DataReference
				.create(_reference, _currentTimeWindow);
		if (ref == null) {
			_iterator = null;
			return;
		}
		RegularTimeSeries ds = (RegularTimeSeries) ref.getData();
		if (ds.getTimeInterval().compare(_ti) < 0) {
			ds = TimeSeriesMath.doPeriodOperation(ds, _ti,
					TimeSeriesMath.PERIOD_AVERAGE);
		} else if (ds.getTimeInterval().compare(_ti) > 0) {
			ds = null;
		}
		if (ds != null)
			_iterator = ds.getIterator();
		else
			_iterator = null;
	}

	private String _filename, _pathname;
	private DataReference _reference;
	private int _channelId;
	DataSetIterator _iterator;
	String _timeWindow;
	TimeWindow _currentTimeWindow;
	Time _stime;
	TimeInterval _ti;
	private Group _group;
}
