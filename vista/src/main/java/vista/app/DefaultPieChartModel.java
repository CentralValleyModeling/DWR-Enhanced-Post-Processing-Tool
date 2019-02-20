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
package vista.app;

import vista.graph.PieChartModel;
import vista.graph.PieModel;
import vista.set.DataSetElement;

/**
 * 
 * 
 * @author Nicky Sandhu
 * @version $Id: DefaultPieChartModel.java,v 1.1 2003/10/02 20:48:28 redwood Exp
 *          $
 */
public class DefaultPieChartModel implements PieChartModel {
	private int _index;
	private DataSetElement _dse;
	private String[] _labels;
	private DefaultPieModel _pm;

	public DefaultPieChartModel(DataSetElement dse, String[] labels) {
		_dse = dse.createClone();
		_labels = labels;
		_pm = new DefaultPieModel();
		reset();
	}

	/**
   *
   */
	public Object getReferenceObject() {
		return _dse;
	}

	/**
   *
   */
	public void setReferenceObject(Object obj) {
		if (!(obj instanceof DataSetElement))
			return;
		_dse = (DataSetElement) obj;
	}

	/**
	 * get title of pie chart
	 */
	public String getTitle() {
		return "PIE CHART";
	}

	/**
	 * get maximum value, to scale all values to this value
	 */
	public double getSumOfValues() {
		double sum = 0.0;
		for (int i = 0; i < _dse.getDimension(); i++)
			sum += _dse.getX(i);
		return sum;
	}

	/**
	 * true if more value are to follow
	 */
	public boolean hasMorePies() {
		return _index < _dse.getDimension();
	}

	/**
	 * returns information in the pie model interface
	 */
	public PieModel nextPie() {
		_pm._value = _dse.getX(_index);
		_pm._label = _labels[_index];
		_index++;
		return _pm;
	}

	/**
	 * resets to the beginning
	 */
	public void reset() {
		_index = 0;
	}

	/**
	 * 
	 * 
	 * @author Nicky Sandhu
	 * @version $Id: DefaultPieChartModel.java,v 1.1 2003/10/02 20:48:28 redwood
	 *          Exp $
	 */
	class DefaultPieModel implements PieModel {
		double _value;
		String _label;

		public String getLabel() {
			return _label;
		}

		public double getValue() {
			return _value;
		}

		public Object getReferenceObject() {
			return null;
		}
	}
}
