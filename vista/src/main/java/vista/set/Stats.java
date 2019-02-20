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
 * 
 * 
 * @author Nicky Sandhu
 * @version $Id: Stats.java,v 1.1 2003/10/02 20:49:33 redwood Exp $
 */
public class Stats {
	/**
   *
   */
	public static double max(DataSet ds) {
		return new ElementFilterIterator(ds.getIterator(),
				Constants.DEFAULT_FLAG_FILTER).getMaximum().getY();
	}

	/**
   *
   */
	public static double min(DataSet ds) {
		return new ElementFilterIterator(ds.getIterator(),
				Constants.DEFAULT_FLAG_FILTER).getMinimum().getY();
	}

	/**
   *
   */
	public static double avg(DataSet ds) {
		DataSetIterator dsi = new ElementFilterIterator(ds.getIterator(),
				Constants.DEFAULT_FLAG_FILTER);
		double sum = 0.0;
		int nsums = 0;
		for (dsi.resetIterator(); !dsi.atEnd(); dsi.advance()) {
			sum += dsi.getElement().getY();
			nsums++;
		}
		if (nsums == 0)
			throw new IllegalArgumentException("No values for data set: "
					+ ds.getName());
		return sum / nsums;
	}

	/**
	 * total of all values in the data set
	 */
	public static double total(DataSet ds) {
		DataSetIterator dsi = new ElementFilterIterator(ds.getIterator(),
				Constants.DEFAULT_FLAG_FILTER);
		double sum = 0.0;
		int nsums = 0;
		for (dsi.resetIterator(); !dsi.atEnd(); dsi.advance()) {
			sum += dsi.getElement().getY();
			nsums++;
		}
		if (nsums == 0)
			throw new IllegalArgumentException("No values for data set: "
					+ ds.getName());
		return sum;
	}

	/**
	 * standard deviation of all the acceptable elements y values
	 */
	public static double sdev(DataSet ds) {
		double xm = avg(ds);
		DataSetIterator dsi = new ElementFilterIterator(ds.getIterator(),
				Constants.DEFAULT_FLAG_FILTER);
		int count = 0;
		double sum = 0;
		for (; !dsi.atEnd(); dsi.advance()) {
			double d = dsi.getElement().getY() - xm;
			sum += d * d;
			count++;
		}
		return Math.sqrt(sum / (count - 1));
		// ??? have to check this ^
	}

	/**
   *
   */
	public static int countAcceptable(DataSet ds, ElementFilter f) {
		DataSetIterator dsi = ds.getIterator();
		int sum = 0;
		for (dsi.resetIterator(); !dsi.atEnd(); dsi.advance()) {
			if (f.isAcceptable(dsi.getElement()))
				sum++;
		}
		return sum;
	}

	/**
   *
   */
	public static int countUnacceptable(DataSet ds, ElementFilter f) {
		DataSetIterator dsi = ds.getIterator();
		int sum = 0;
		for (dsi.resetIterator(); !dsi.atEnd(); dsi.advance()) {
			if (!f.isAcceptable(dsi.getElement()))
				sum++;
		}
		return sum;
	}

	/**
   *
   */
	public static int countMissing(DataSet ds) {
		return countUnacceptable(ds, Constants.DEFAULT_FILTER);
	}

	/**
   *
   */
	public static int countOK(DataSet ds) {
		return countUnacceptable(ds, FlagUtils.OK_FILTER);
	}

	/**
   *
   */
	public static int countQuestionable(DataSet ds) {
		return countUnacceptable(ds, FlagUtils.QUESTIONABLE_FILTER);
	}

	/**
   *
   */
	public static int countReject(DataSet ds) {
		return countUnacceptable(ds, FlagUtils.REJECT_FILTER);
	}
}
