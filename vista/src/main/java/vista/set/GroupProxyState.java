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

import java.util.Comparator;

/**
 * This a proxy for group object
 * 
 * @author Nicky Sandhu
 * @version $Id: GroupProxyState.java,v 1.1 2003/10/02 20:49:24 redwood Exp $
 */
public abstract class GroupProxyState extends Group {
	/**
	 * initializes state object with given proxy. This proxy is the initial
	 * delegate and informs this state object on being initialized.
	 */
	public GroupProxyState(GroupProxy proxy) {
		proxy.addStateListener(this);
		_delegate = proxy;
	}

	/**
	 * get number of data references
	 */
	public int getNumberOfDataReferences() {
		return _delegate.getNumberOfDataReferences();
	}

	/**
	 * gets data reference by index
	 */
	public DataReference getDataReference(int index) {
		return _delegate.getDataReference(index);
	}

	/**
	 * gets data reference by name
	 */
	public DataReference getDataReference(String dataName) {
		return _delegate.getDataReference(dataName);
	}

	/**
	 * gets all the data references
	 */
	public DataReference[] getAllDataReferences() {
		return _delegate.getAllDataReferences();
	}

	/**
	 * copies this group into given group.
	 */
	public void copyInto(Group group) {
		_delegate.copyInto(group);
	}

	/**
	 * returns a copy of itself.
	 */
	public Object clone() {
		return _delegate.clone();
	}

	/**
	 * Creates a new group which is the union of this group and the specified
	 * groupg
	 */
	public Group unionWith(Group group) {
		return _delegate.unionWith(group);
	}

	/**
	 * Creates a new group which is the intersection of this group and the
	 * specified group. Returns null if intersection is the null set.
	 */
	public Group intersectionWith(Group group) {
		return _delegate.intersectionWith(group);
	}

	/**
	 * Sorts group by calling upon the SortAlgorithm to return an array of data
	 * references by some criteria. The reference list is being directly
	 * manipulated by the sorting mechanism.
	 */
	public void sortBy(Comparator<DataReference> comparator) {
		_delegate.sortBy(comparator);
	}

	/**
	 * filters on this group using filter. This causes the reference list held
	 * by this group to be modified. This change is physically reflected by
	 * appending the filter expression string to the name of this group.
	 */
	public void filterBy(Predicate predicate, boolean selecting) {
		_delegate.filterBy(predicate, selecting);
	}

	/**
	 * point delegate to the new group object.
	 */
	void groupIsInitialized() {
		Group g = new Group();
		_delegate.copyInto(g);
		_delegate = g;
	}

	/**
	 * true if initialized
	 */
	private boolean _initialized = false;
	/**
	 * state object listening for initialization on this proxy
	 */
	private Group _delegate;
}
