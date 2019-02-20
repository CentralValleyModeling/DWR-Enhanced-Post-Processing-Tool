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

import vista.time.TimeInterval;
import vista.time.TimeWindow;

/**
 * A proxy for period averaging operations
 * 
 * @author Nicky Sandhu
 * @version $Id: PeriodMaxProxy.java,v 1.1 2003/10/02 20:49:29 redwood Exp $
 */
public class PeriodMaxProxy extends PeriodOperationProxy {
	/**
   *
   */
	protected String getOperationName() {
		return operationName;
	}

	/**
	 * returns the value for this period from the values in the array
	 */
	protected double doPeriodOperation(double[] yvals, int nvals) {
		if (nvals <= 0)
			return 0;
		double max = yvals[0];
		for (int i = 1; i < nvals; i++) {
			max = Math.max(max, yvals[i]);
		}
		return max;
	}

	/**
	 * returns the flag for the value of this period from the values in the
	 * array.
	 */
	protected int doFlagOperation(int[] flags, int nvals) {
		return 0;
	}

	/**
   *
   */
	public PeriodMaxProxy(DataReference ref, TimeInterval ti) {
		super(ref, ti);
	}

	/**
   *
   */
	protected String getProxyName(DataReference ref, TimeInterval ti) {
		return ref.getName() + operationName;
	}

	/**
   *
   */
	protected String getProxyServerName(DataReference ref, TimeInterval ti) {
		return "";
	}

	/**
   *
   */
	protected String getProxyFileName(DataReference ref, TimeInterval ti) {
		return "";
	}

	/**
   *
   */
	protected Pathname getProxyPathname(DataReference ref, TimeInterval ti) {
		Pathname path = ref.getPathname();
		// create pathname from operation + pathname -- > should be delegated
		String[] parts = new String[Pathname.MAX_PARTS];
		for (int i = 0; i < parts.length; i++) {
			parts[i] = path.getPart(i);
		}
		parts[Pathname.B_PART] = parts[Pathname.B_PART] + operationName;
		parts[Pathname.E_PART] = ti.getIntervalAsString();
		parts[Pathname.F_PART] = parts[Pathname.F_PART] + "(" + operationName
				+ ")";
		return Pathname.createPathname(parts);
	}

	/**
   *
   */
	protected TimeWindow getProxyTimeWindow(DataReference ref, TimeInterval ti) {
		return ref.getTimeWindow();
	}

	/**
	 * create a clone of itself
	 */
	public DataReference createClone() {
		return new PeriodMaxProxy(getUnderlyingReference(),
				getPeriodTimeInterval());
	}

	/**
   *
   */
	private static String operationName = "PER-MAX";
}
