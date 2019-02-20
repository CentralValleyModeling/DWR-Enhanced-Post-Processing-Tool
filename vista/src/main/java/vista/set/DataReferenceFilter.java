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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Enumeration;
import java.util.Iterator;

/**
 * Uses a regular expression to filter a group of pathnames. It defines either
 * Perl5 or Awk Expressions
 */
public class DataReferenceFilter implements Enumeration<DataReference> {
	/**
	 * Iterates over an array of data references using a filter as defined by
	 * regex
	 */
	public DataReferenceFilter(DataReference[] refs, Predicate<DataReference> predicate) {
		_filteringFunction = predicate;
		setSelecting(true);
		setReferences(refs);
		filter();
	}

	/**
	 * sets the references and filters them.
	 */
	public void setReferences(DataReference[] refs) {
		_references = refs;
	}

	/**
	 * set to true if values are to be selected according to filtering function
	 * criteria or false for rejecting. Selecting is the default.
	 */
	public final void setSelecting(boolean select) {
		_selecting = select;
	}

	/**
	 * true if values are selected accorrding to filtering function.
	 */
	public final boolean isSelecting() {
		return _selecting;
	}

	/**
	 * true if it has more elements
	 */
	public boolean hasMoreElements() {
		return iterator.hasNext();
	}

	/**
	 * next element in the sequence
	 */
	public DataReference nextElement() {
		return iterator.next();
	}

	/**
	 * filters the methods
	 */
	public final void filter() {
		_filtered = true;
		CollectionUtils.filter(Arrays.asList(_references), _filteringFunction, _selecting);
		resetIterator();
	}

	/**
	 * returns the filtered array of references
	 */
	public DataReference[] getFilteredArray() {
		DataReference[] refs = new DataReference[_filteredArray.size()];
		return _filteredArray.toArray(refs);
	}

	/**
	 * resets the iterator.
	 */
	public void resetIterator() {
		iterator = _filteredArray.iterator();
	}

	/**
	 * true if filter selects by given criteria
	 */
	private boolean _selecting;
	/**
	 * true if filtering is already done.
	 */
	private boolean _filtered;
	/**
	 * array containing the filtered result.
	 */
	private ArrayList<DataReference> _filteredArray;
	/**
	 * The data reference array
	 */
	private DataReference[] _references;
	/**
	 * The function used to define what values are to be rejected
	 */
	private Predicate<DataReference> _filteringFunction;
	/**
	 * iterator for the filtered array.
	 */
	private Iterator<DataReference> iterator;
}
