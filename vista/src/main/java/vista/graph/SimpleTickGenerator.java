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
package vista.graph;

/**
 * Uses labeling by converting the tick value to its float representation of a
 * string.
 * 
 * @author Nicky Sandhu
 * @version $Id: SimpleTickGenerator.java,v 1.1 2003/10/02 20:49:08 redwood Exp
 *          $
 */
public class SimpleTickGenerator implements TickGenerator {
	/**
	 * debuggin'
	 */
	public static final boolean DEBUG = false;

	/**
	 * implements generate to generate major and minor tick value arrays along
	 * with labels for each of the major ticks.
	 */
	public void generate(double minimum, double maximum) {
		if (minimum > maximum) {
			_reverseIt = true;
			_maximum = minimum;
			_minimum = maximum;
		} else {
			_reverseIt = false;
			_maximum = maximum;
			_minimum = minimum;
		}

		if (DEBUG)
			System.out.println("Generating for " + _maximum + ", " + _minimum);
		try {
			generateAxisTicksAndLabels();
		} catch (java.lang.ArithmeticException ae) {
			throw new RuntimeException("Arithmetic Exception: "
					+ ae.getMessage() + " : possible NaN's in data!");
		}
	}

	/**
	 * returns the major and minor tick data
	 */
	public TickData[] getTickData() {
		TickData[] td = new TickData[2];
		td[0] = new TickData(_majorValue, _labels, _majorValue[0],
				_majorValue[_majorValue.length - 1]);
		td[1] = new TickData(_minorValue, null, _majorValue[0],
				_majorValue[_majorValue.length - 1]);
		return td;
	}

	/**
	 * gets the generated labels
	 */
	public String[] getLabels() {
		return _labels;
	}

	/**
	 * gets the format object used in generating the labels
	 */
	public java.text.Format getFormatter() {
		return null;
	}

	/**
	 * returns the exponent for all the labels
	 */
	public int getExponent() {
		return _labelExponent;
	}

	/**
	 * Use a specific value for the number of labels generated. This aligns the
	 * axes grids.
	 */
	public void useLabelCount(int lc) {
		_fixedLabelCount = lc;
	}

	/**
	 * calculate the labels, major and minor tick values
	 */
	private void generateAxisTicksAndLabels() {
		double val;
		int i;
		int j;

		if (Math.abs(_minimum) > Math.abs(_maximum))
			_labelExponent = ((int) Math.floor(log10(Math.abs(_minimum)) / 3.0)) * 3;
		else
			_labelExponent = ((int) Math.floor(log10(Math.abs(_maximum)) / 3.0)) * 3;

		double range = getNiceNumber(_maximum - _minimum, false);

		if (_fixLabelCount)
			_labelStep = getNiceNumber(range / (_fixedLabelCount - 1), false);
		else
			_labelStep = getNiceNumber(range / (_initialLabelCount - 1), true);

		_labelStartingValue = Math.floor(_minimum / _labelStep) * _labelStep;

		val = _labelStartingValue;
		_labelCount = 1;
		while (val < _maximum) {
			val += _labelStep;
			_labelCount++;
		}

		int diffLabelCount = _fixedLabelCount - _labelCount;
		if (_fixLabelCount) {
			_labelStartingValue = _labelStartingValue
					- (_fixedLabelCount - _labelCount) / 2 * _labelStep;
			val = _labelStartingValue;
			_labelCount = 1;
			while (val < _maximum) {
				val += _labelStep;
				_labelCount++;
			}
			if (_labelCount < _fixedLabelCount)
				_labelCount = _fixedLabelCount;
		}

		_labels = new String[_labelCount];
		_majorValue = new double[_labelCount];
		_minorValue = new double[(_labelCount - 1) * _minorPerMajor + 1];

		if (DEBUG) {
			System.out.println("_labelStep=" + _labelStep);
			System.out.println("_labelStartingValue=" + _labelStartingValue);
			System.out.println("_labelCount=" + _labelCount);
			System.out.println("_labelExponent" + _labelExponent);
		}

		for (i = 0; i < _labelCount; i++) {

			val = _labelStartingValue + i * _labelStep;
			_labels[i] = _nf.format(val);
			_majorValue[i] = val;

			if (i < (_labelCount - 1)) {
				for (int k = 0; k < _minorPerMajor; k++) {
					if (DEBUG)
						System.out.println(val + k * _labelStep
								/ _minorPerMajor);
					_minorValue[i * _minorPerMajor + k] = val + k * _labelStep
							/ _minorPerMajor;
				}
			} else {
				_minorValue[i * _minorPerMajor] = val;
			}

		}

		if (_useDataMinMax) {
			boolean niceMin = false, niceMax = false;

			int minIndex = 0;
			do {
				minIndex++;
			} while (_majorValue[minIndex] < _minimum);
			minIndex--;

			if (Math.abs(_majorValue[minIndex] - _minimum) < 1e-08)
				niceMin = true;

			_majorValue[minIndex] = _minimum;

			int maxIndex = _majorValue.length - 1;
			do {
				maxIndex--;
			} while (_majorValue[maxIndex] > _maximum);
			maxIndex++;
			if (Math.abs(_majorValue[maxIndex] - _maximum) < 1e-08)
				niceMax = true;
			_majorValue[maxIndex] = _maximum;

			double[] newValues = new double[maxIndex - minIndex + 1];
			System.arraycopy(_majorValue, minIndex, newValues, 0,
					newValues.length);
			_majorValue = newValues;

			String[] newStrings = new String[maxIndex - minIndex + 1];
			System.arraycopy(_labels, minIndex, newStrings, 0,
					newStrings.length);
			_labels = newStrings;
			if (!niceMin)
				_labels[0] = "";
			if (!niceMax)
				_labels[_labels.length - 1] = "";

			minIndex = 0;
			do {
				minIndex++;
			} while (_minorValue[minIndex] < _minimum);
			minIndex--;
			_minorValue[minIndex] = _minimum;

			maxIndex = _minorValue.length - 1;
			do {
				maxIndex--;
			} while (_minorValue[maxIndex] > _maximum);
			maxIndex++;
			_minorValue[maxIndex] = _maximum;

			newValues = new double[maxIndex - minIndex + 1];
			System.arraycopy(_minorValue, minIndex, newValues, 0,
					newValues.length);
			_minorValue = newValues;

		}

		if (_reverseIt) {
			int revIndex = 0;
			for (revIndex = 0; revIndex < _majorValue.length / 2; revIndex++) {
				double tmp = _majorValue[revIndex];
				_majorValue[revIndex] = _majorValue[_majorValue.length - 1
						- revIndex];
				_majorValue[_majorValue.length - 1 - revIndex] = tmp;
			}
			for (revIndex = 0; revIndex < _minorValue.length / 2; revIndex++) {
				double tmp = _minorValue[revIndex];
				_minorValue[revIndex] = _minorValue[_minorValue.length - 1
						- revIndex];
				_minorValue[_minorValue.length - 1 - revIndex] = tmp;
			}
			for (revIndex = 0; revIndex < _labels.length / 2; revIndex++) {
				String tmp = _labels[revIndex];
				_labels[revIndex] = _labels[_labels.length - 1 - revIndex];
				_labels[_labels.length - 1 - revIndex] = tmp;
			}
		}

	}

	/**
	 * generates a nice number value from given value and rounding criteria
	 */
	private double getNiceNumber(double x, boolean round) {

		int exponentOfX;
		double xFraction;
		double roundedFraction;

		exponentOfX = (int) Math.floor(log10(Math.abs(x)));
		xFraction = x / Math.pow(10., exponentOfX);

		if (round) {
			if (xFraction < 1.5)
				roundedFraction = 1.;
			else if (xFraction < 2.5)
				roundedFraction = 2.;
			else if (xFraction < 3.)
				roundedFraction = 2.5;
			else if (xFraction < 4.)
				roundedFraction = 3.0;
			else if (xFraction < 5.)
				roundedFraction = 4.0;
			else if (xFraction < 7.)
				roundedFraction = 5.;
			else
				roundedFraction = 10.;
		} else {
			if (xFraction <= 1.)
				roundedFraction = 1.;
			else if (xFraction <= 2.5)
				roundedFraction = 2.;
			else if (xFraction <= 3.)
				roundedFraction = 2.5;
			else if (xFraction <= 4.)
				roundedFraction = 3.0;
			else if (xFraction <= 5.)
				roundedFraction = 4.0;
			else if (xFraction <= 7.)
				roundedFraction = 5.0;
			else
				roundedFraction = 10.;
		}
		return roundedFraction * Math.pow(10., exponentOfX);

	}

	/**
	 * Calculates log to the base 10
	 */
	private static double log10(double x) throws ArithmeticException {
		if (x <= 0.0)
			throw new ArithmeticException("range exception");
		return Math.log(x) / 2.30258509299404568401;
	}

	/**
	 * set boundary of generated tick marks to either to use given data min/max
	 * or set a nice value for them
	 */
	public void useDataMinMax(boolean b) {
		_useDataMinMax = b;
	}

	private static java.text.NumberFormat _nf = new java.text.DecimalFormat();
	static {
		_nf.setGroupingUsed(false);
		_nf.setMinimumFractionDigits(0);
	}
	/**
	 * minimum data value
	 */
	private double _minimum;
	/**
	 * maximum data value
	 */
	private double _maximum;
	/**
	 * exponent of label. Not used presently.
	 */
	private int _labelExponent;
	/**
	 * labels for major tick marks
	 */
	private String[] _labels;
	/**
	 * data values for major tick marks
	 */
	private double[] _majorValue;
	/**
	 * data values for minor tick marks
	 */
	private double[] _minorValue;
	/**
	 * number of labels
	 */
	private int _labelCount;
	/**
	 * starting label value
	 */
	private double _labelStartingValue;
	/**
	 * Initial guess for the number of labels required
	 */
	private int _initialLabelCount = 4;
	/**
	 * number of minor ticks for every major tick
	 */
	private int _minorPerMajor = 4;
	/**
	 * step size for next label or major tick
	 */
	private double _labelStep;
	/**
	 * fixes labels generated
	 */
	private boolean _fixLabelCount = false;
	/**
	 * number of labels to be generated
	 */
	private int _fixedLabelCount = 6;
	/**
	 * use the data minimum and maximum as tick mark begining and ending
	 * respectively.
	 */
	private boolean _useDataMinMax = false;
	/**
   *
   */
	private boolean _reverseIt = false;
}
