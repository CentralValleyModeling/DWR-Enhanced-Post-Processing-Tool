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
 * Filters the given iterator's values using a filter function. This filter
 * iterator caches the index of acceptable value and the length of acceptable
 * values following that and so on. This leads to faster iteration times for
 * this iterator at the cost of extra memory expense.
 * 
 * However one must keep in mind that this iterator is extremely inefficient
 * when the data is in the process of being changed as the cache has to be
 * repeatedly updated.
 * 
 * @author Nicky Sandhu
 * @version $Id: ElementFilterCachedIterator.java,v 1.4 1999/07/11 19:37:40
 *          nsandhu Exp $
 */
public class ElementFilterCachedIterator implements DataSetIterator {
	/**
	 * Filters the given iterator through the given filter.
	 */
	public ElementFilterCachedIterator(DataSetIterator iterator,
			ElementFilter filter) {
		_dsi = iterator;
		_filter = filter;
		ElementFilterIterator efi = new ElementFilterIterator(_dsi, _filter);
		_dse = _dsi.getElement().createClone();
		_indexArray = getDataIndices(efi);
		int l = _indexArray.length;
		_lastTwoSum = _indexArray[l - 2] + _indexArray[l - 1];
		resetIterator();
	}

	/**
	 * Resets the iterator to the beginning of data
	 */
	public void resetIterator() {
		_chunkIndex = 0;
		_chunkStart = _indexArray[_chunkIndex];
		_chunkSize = _indexArray[_chunkIndex + 1];
		_index = _chunkStart;
		_skipped = _chunkStart - 0;
		_firstNextCall = true;
		_dsi.positionAtIndex(_index);
		_index0 = 0;
		_dse = _dsi.getElement();
	}

	/**
	 * Checks to see if the current index is at the starting of a new chunk of
	 * data
	 */
	public int hasSkipped() {
		return _skipped;
	}

	/**
	 * Returns with the next element and the iterator positioned at the next
	 * element.
	 */
	public DataSetElement nextElement() {
		if (DEBUG)
			System.out.println("Index: " + _index);
		if (DEBUG)
			System.out.println(_dse);
		if (_index > 0 && !_firstNextCall)
			advance();
		else {
			if (!_firstNextCall)
				advance();
			else
				_firstNextCall = false;
		}
		return getElement();
	}

	/**
	 * returns the previous element in the sequence
	 */
	public DataSetElement previousElement() {
		retreat();
		return getElement();
	}

	/**
	 * gets the element at the current location
	 */
	public DataSetElement getElement() {
		return _dse = _dsi.getElement();
	}

	/**
	 * puts the element at the current location
	 */
	public void putElement(DataSetElement e) {
		_dsi.putElement(e);
		// if this affects the arrays re initialize
		if (!_filter.isAcceptable(e)) {
			ElementFilterIterator efi = new ElementFilterIterator(_dsi, _filter);
			_dse = _dsi.getElement().createClone();
			_indexArray = getDataIndices(efi);
			int l = _indexArray.length;
			_lastTwoSum = _indexArray[l - 2] + _indexArray[l - 1];
			resetIterator();
			advanceUntil(this, e);
		}
	}

	/**
	 * extremely inefficient search from beginning of set.
	 */
	public static void advanceUntil(DataSetIterator dsi, DataSetElement dse) {
		while (!dsi.atEnd() && !dsi.getElement().equals(dse)) {
			dsi.advance();
		}
	}

	/**
	 * Advance by one.
	 */
	public void advance() {
		// check if iterator already at end
		if (atEnd())
			return;
		// increment to next chunk if needed...
		if (DEBUG) {
			System.out.println("_index: " + _index);
			System.out.println("_chunkIndex: " + _chunkIndex);
			System.out.println("_chunkStart: " + _chunkStart);
			System.out.println("_chunkSize: " + _chunkSize);
		}
		_skipped = 0;
		if (_index < _chunkStart + _chunkSize - 1)
			_index++;
		else {
			_chunkIndex += 2;
			if (atEnd()) {
				_chunkStart = _lastTwoSum + 1;
			} else {
				_skipped = _indexArray[_chunkIndex]
						- (_chunkStart + _chunkSize);
				_chunkStart = _indexArray[_chunkIndex];
				_chunkSize = _indexArray[_chunkIndex + 1];
				_index = _chunkStart;
			}
		}
		if (atEnd()) { // take care of missing elements at end of set
			while (!_dsi.atEnd()) {
				_dse = _dsi.getElement();
				_dsi.advance();
			}
			_index = _dsi.getIndex();
		} else {
			_dsi.positionAtIndex(_index);
			_dse = _dsi.getElement();
		}
		_index0++;
	}

	/**
	 * Retreate by one
	 */
	public void retreat() {
		if (atStart())
			return;
		// increment to next chunk if needed...
		_skipped = 0;
		// if missing values at end of set, rewind to one more than good value.
		int l = _indexArray.length;
		if (_index > _lastTwoSum) {
			_index = _lastTwoSum;
			_chunkIndex = _indexArray.length;
		}
		//
		if (_index > _chunkStart)
			_index--;
		else {
			if (atStart()) {
				return;
			} else {
				_chunkIndex -= 2;
				_skipped = _chunkStart
						- (_indexArray[_chunkIndex] + _indexArray[_chunkIndex + 1]);
				_chunkStart = _indexArray[_chunkIndex];
				_chunkSize = _indexArray[_chunkIndex + 1];
				_index = _chunkStart + _chunkSize - 1;
			}
		}
		_dsi.positionAtIndex(_index);
		_dse = _dsi.getElement();
		_index0--;
	}

	/**
	 * positions the iterator at correct index
	 */
	public void positionAtIndex(int index) {
		int currentIndex = getIndex();
		int diff = index - currentIndex;
		if (diff > 0) {
			for (int i = 0; i < diff; i++)
				advance();
		} else if (diff < 0) {
			diff = -diff;
			for (int i = 0; i < diff; i++)
				retreat();
		}
	}

	/**
	 * Gets the current index for the iterator. This is the index of this
	 * iterator on the underlying iterator's index of that of the data set
	 */
	public int getIndex() {
		return _index0;
	}

	/**
	 *gets the underlying iterator
	 */
	public int getUnderlyingIndex() {
		return _index;
	}

	/**
	 * true if iterator at end
	 */
	public boolean atEnd() {
		int l = _indexArray.length;
		return (_chunkIndex == l) && (_index >= _lastTwoSum - 1);
	}

	/**
	 * if iterator is at start of data
	 */
	public boolean atStart() {
		return _index == _indexArray[0];
	}

	/**
	 * The maximum of x and y range encapsulated as a data set element.
	 */
	public DataSetElement getMaximum() {
		if (_maximum == null) {
			calcMinMax();
		}
		return _maximum;
	}

	/**
	 * The minimum of x and y range encapsulated as a data set element.
	 */
	public DataSetElement getMinimum() {
		if (_minimum == null) {
			calcMinMax();
		}
		return _minimum;
	}

	/**
	 * this is done to save time so that one iteration over the entire set of
	 * elements can calculate max and min.
	 */
	private void calcMinMax() {
		double xmin = Float.MAX_VALUE;
		double ymin = Float.MAX_VALUE;
		double xmax = -Float.MAX_VALUE;
		double ymax = -Float.MAX_VALUE;
		int prevIndex = this.getIndex();
		DataSetIterator dsi = this;
		dsi.resetIterator();
		_minimum = dsi.getElement().createClone();
		_maximum = dsi.getElement().createClone();
		while (!dsi.atEnd()) {
			DataSetElement dse = dsi.getElement();
			double x = dse.getX();
			double y = dse.getY();
			xmin = Math.min(xmin, x);
			ymin = Math.min(ymin, y);
			xmax = Math.max(xmax, x);
			ymax = Math.max(ymax, y);
			dsi.advance();
		}
		this.positionAtIndex(prevIndex);
		_minimum.setX(xmin);
		_minimum.setY(ymin);
		_maximum.setX(xmax);
		_maximum.setY(ymax);
	}

	/**
	 * An array A where A[2k] = starting index of data in the arrays and A[2k+1]
	 * = length of acceptable data, k = 0,1,....
	 * 
	 * @returns an array of int containing starting index followed by length for
	 *          each set of consecutive data points to be referenced in this
	 *          data set.
	 */
	public final int[] getDataIndices(ElementFilterIterator efi) {

		int[] indices = new int[MAX_DATA_BREAKS];
		// intialize the elements
		efi.resetIterator();
		DataSetElement dse = efi.getElement();
		int startIndex = efi.getUnderlyingIndex();
		efi.advance();
		int currentLength = 0;
		int nIndices = 0;
		// record length of good data at skips and set new start index
		while (!efi.atEnd()) {
			dse = efi.getElement();
			if (efi.hasSkipped() > 0) {
				currentLength = efi.getUnderlyingIndex() - startIndex
						- efi.hasSkipped();
				indices[nIndices] = startIndex;
				indices[nIndices + 1] = currentLength;
				nIndices += 2;
				if (nIndices >= indices.length)
					indices = expandArray(indices);
				startIndex = efi.getUnderlyingIndex();
			}
			efi.advance();
		}
		// get to end of data...
		// reset filter iterator back to previous good value
		efi.retreat();
		currentLength = efi.getUnderlyingIndex() + 1 - startIndex;
		indices[nIndices] = startIndex;
		indices[nIndices + 1] = currentLength;
		nIndices += 2;
		if (nIndices >= indices.length)
			indices = expandArray(indices);

		if (nIndices > 0) {
			int[] newArray = new int[nIndices];
			System.arraycopy(indices, 0, newArray, 0, nIndices);
			indices = newArray;
		} else {
			indices = null;
		}
		if (DEBUG) {
			int index = 0;
			System.out.println(nIndices);
			while (index < nIndices) {
				System.out.println("Start[" + index + "] = " + indices[index]);
				System.out.println("Length[" + index + "] = "
						+ indices[index + 1]);
				index += 2;
			}
		}

		return indices;
	}

	/**
	 * expands array by certain amount & returns new array...
	 */
	private int[] expandArray(int[] indices) {
		int[] newArray = new int[indices.length + MAX_DATA_BREAKS];
		System.arraycopy(indices, 0, newArray, 0, indices.length);
		return newArray;
	}

	/**
	 * The underlying iterator
	 */
	private DataSetIterator _dsi;
	/**
	 * The current element
	 */
	private DataSetElement _dse;
	/**
	 * The function used to filter values.
	 */
	private ElementFilter _filter;
	/**
	 * true if some values have been skipped by this iterator
	 */
	private int _skipped;
	/**
	 * The current index on the iterator
	 */
	private int _index, _index0;
	/**
	 * the first call to the nextElement() method since the resetIterator call.
	 */
	private boolean _firstNextCall;
	/**
   *
   */
	private static final boolean DEBUG = false;
	/**
	 * An array containing the starting index and sizes of data chunks. This
	 * version takes more memory than if the value was filtered out during
	 * iteration but is faster. Also in the worst case scenario
	 */
	private int[] _indexArray;
	/**
	 * An index to the start of the current chunk of data
	 */
	private int _chunkStart;
	/**
	 * An index to the size of the current chunk of data
	 */
	private int _chunkSize;
	/**
	 * The index of the current chunk of data
	 */
	private int _chunkIndex;
	/**
	 * the maximum and minimum values
	 */
	private DataSetElement _maximum, _minimum;
	/**
	 * index of the last good value + 1
	 */
	private int _lastTwoSum;
	/**
	 * A guess at the maximum number of breaks in the data caused by the missing
	 * value.
	 */
	private static final int MAX_DATA_BREAKS = 1000;

}
