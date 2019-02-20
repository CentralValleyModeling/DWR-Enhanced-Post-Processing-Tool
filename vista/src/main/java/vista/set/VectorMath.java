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
package vista.set;


/**
 * This class defines vector operations and vector - scalar operation on data
 * sets. Most data sets can have vector-scalar operations defined quite easily
 * however some assumptions have to be made when doing vector-vector operations.
 * One requirement is that for non-indexed data sets the values If value is
 * missing the sum has a missing value as well for that data set element
 * 
 */
public class VectorMath {
	/**
	 * does math operation defined by operation id on the two data sets d1 and
	 * d2;
	 */
	public static DataSet doMathOperation(DataSet d1, DataSet d2,
			int operationId) {
		check(d1, d2);
		RegularTimeSeries ts1 = (RegularTimeSeries) d1;
		RegularTimeSeries ts2 = (RegularTimeSeries) d2;
		switch (operationId) {
		case DataReferenceMath.ADD:
			return TimeSeriesMath.doBinaryOperation(ts1, ts2,
					TimeSeriesMath.ADD);
		case DataReferenceMath.MUL:
			return TimeSeriesMath.doBinaryOperation(ts1, ts2,
					TimeSeriesMath.MUL);
		case DataReferenceMath.SUB:
			return TimeSeriesMath.doBinaryOperation(ts1, ts2,
					TimeSeriesMath.SUB);
		case DataReferenceMath.DIV:
			return TimeSeriesMath.doBinaryOperation(ts1, ts2,
					TimeSeriesMath.DIV);
		default:
			return null;
		}
	}

	/**
	 * does a vector and scalar operation
	 */
	public static DataSet doMathOperation(DataSet d1, double scalar,
			int operationId, boolean reverseArgs) {
		check(d1);
		TimeSeries ts = (TimeSeries) d1;
		switch (operationId) {
		case DataReferenceMath.ADD:
			return TimeSeriesMath.doBinaryOperation(ts, scalar,
					TimeSeriesMath.ADD, reverseArgs);
		case DataReferenceMath.MUL:
			return TimeSeriesMath.doBinaryOperation(ts, scalar,
					TimeSeriesMath.MUL, reverseArgs);
		case DataReferenceMath.SUB:
			return TimeSeriesMath.doBinaryOperation(ts, scalar,
					TimeSeriesMath.SUB, reverseArgs);
		case DataReferenceMath.DIV:
			return TimeSeriesMath.doBinaryOperation(ts, scalar,
					TimeSeriesMath.DIV, reverseArgs);
		default:
			return null;
		}
	}

	/**
	 * check viability of operation on data sets
	 */
	public static void check(DataSet d1, DataSet d2) {
		check(d1);
		check(d2);
		if (!(d1 instanceof RegularTimeSeries)
				&& !(d2 instanceof RegularTimeSeries)) {
			throw new IllegalArgumentException(
					"Math defined only between regular time series");
		} else {
			return;
		}
	}
	
	public static void check(DataSet d){
		if (d == null) 
			throw new IllegalArgumentException("Data Set may be null");
		if (!(d instanceof TimeSeries)){
			throw new IllegalArgumentException("Data set needs to be a time series");
		}
	}
	
}
