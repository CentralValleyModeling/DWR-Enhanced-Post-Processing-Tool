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

/**
 * A factory to create a proxy for a certain math operation.
 * 
 * @author Nicky Sandhu
 * @version $Id: ProxyFactory.java,v 1.1 2003/10/02 20:49:30 redwood Exp $
 */
public class ProxyFactory {
	/**
   *
   */
	public final static int PERIOD_AVERAGE = 1;
	public final static int PERIOD_MAX = 2;
	public final static int PERIOD_MIN = 3;

	/**
	 * creates a proxy for a period operation
	 */
	public static DataReference createPeriodOperationProxy(int periodOpId,
			DataReference ref, TimeInterval ti) {
		try {
			switch (periodOpId) {
			case PERIOD_AVERAGE:
				return new PeriodAverageProxy(ref, ti);
			case PERIOD_MIN:
				return new PeriodMinProxy(ref, ti);
			case PERIOD_MAX:
				return new PeriodMaxProxy(ref, ti);
			default:
				throw new IllegalArgumentException("Period operation Id: "
						+ periodOpId + " is invalid");
			}
		} catch (IllegalArgumentException e) {
			return null;
		}
	}

	/**
   *
   */
	public static DataReference createMergingProxy(DataReference[] refs) {
		try {
			if (refs == null)
				return null;
			boolean allRTS = true;
			for (int i = 0; i < refs.length; i++) {
				if (refs[i] == null)
					continue; // will ignore in merge later
				DataSet ds = refs[i].getData();
				if (ds != null && !(ds instanceof RegularTimeSeries)) {
					allRTS = false;
				}
			}
			if (allRTS)
				return new MergingProxy(refs);
			else
				return new ITSMergingProxy(refs);
		} catch (IllegalArgumentException e) {
			return null;
		}
	}

	/**
   *
   */
	public static DataReference createPairedTimeSeriesProxy(DataReference refx,
			DataReference refy) {
		try {
			return new PairedTimeSeriesProxy(refx, refy);
		} catch (IllegalArgumentException e) {
			return null;
		}
	}

	/**
   *
   */
	public static DataReference createLRFilledTimeSeriesProxy(
			DataReference refx, DataReference refy) {
		try {
			return new LRFilledTimeSeriesProxy(refx, refy);
		} catch (IllegalArgumentException e) {
			return null;
		}
	}

	/**
   *
   */
	public static DataReference createRegressionLineProxy(DataReference refx,
			DataReference refy) {
		try {
			return new RegressionLineProxy(refx, refy);
		} catch (IllegalArgumentException e) {
			return null;
		}
	}

	/**
   *
   */
	public static DataReference createMovingAverageProxy(DataReference ref,
			int backLength, int forwardLength) {
		try {
			return new MovingAverageProxy(ref, backLength, forwardLength);
		} catch (IllegalArgumentException e) {
			return null;
		}
	}
}
