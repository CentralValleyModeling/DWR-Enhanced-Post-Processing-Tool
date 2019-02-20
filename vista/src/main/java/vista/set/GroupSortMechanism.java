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
 * Sorts an array of named by their names The default sorting order is
 * increasing.
 * 
 * @author Nicky Sandhu
 * @version $Id: NameSort.java,v 1.1 2003/10/02 20:49:27 redwood Exp $
 */
public class GroupSortMechanism implements SortMechanism<Group> {
	private int sortOrder;

	/**
	 * sorts using the partId specified and increasing order of sort.
	 */
	public GroupSortMechanism() {
		this(INCREASING);
	}

	/**
	 * Initializes the sorting method for a certain part id.
	 * 
	 * @param partId
	 *            Id of the part by which to sort as defined by Pathname.?_PART
	 *            constants
	 * @see Pathname
	 */
	public GroupSortMechanism(int sortOrder) {
		this.sortOrder = sortOrder;
	}

	/**
	 * checks if order of sort is ascending or descending...
	 */
	public boolean isAscendingOrder() {
		return sortOrder == INCREASING;
	}

	/**
	 * sets ascending / descending order
	 */
	public void setAscendingOrder(boolean ascending) {
		sortOrder = ascending ? INCREASING : DECREASING; 
	}
	@Override
	public int compare(Group o1, Group o2) {
		if (isAscendingOrder()){
			return o1.getName().compareTo(o2.getName());
		} else {
			return o2.getName().compareTo(o1.getName());
		}
	}
}
