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

import java.util.Enumeration;
import java.util.Vector;

import vista.set.DataSetElement;

/**
 * Defines the order of drawing elements
 * 
 * @author Nicky Sandhu (DWR).
 * @version $Id: DrawIterator.java,v 1.1 2003/10/02 20:48:53 redwood Exp $
 */
public class DrawIterator implements CompositeIterator {
	/**
   *
   */
	public DrawIterator(Vector array) {
		_array = array;
		_index = 0;
		_iterator = _array.elements();
	}

	/**
	 * Resets the iterator to the beginning of data
	 */
	public void resetIterator() {
		_index = 0;
		_iterator = _array.elements();
	}

	/**
	 * This gets the next set of values from the data set.
	 * 
	 * @return The next set of values
	 * @see DataSetElement
	 */
	public GraphicElement getNext() {
		_index++;
		return (GraphicElement) _iterator.nextElement();
	}

	/**
	 * Advance by one.
	 */
	public void advance() {
		_index++;
		_iterator.nextElement();
	}

	/**
	 * Advance by a specified amount.
	 * 
	 * @param n
	 *            The amount to advance.
	 */
	public void advance(int n) {
		for (int i = 0; i < n; i++)
			advance();
	}

	/**
	 * Return the index of my current position.
	 */
	public int index() {
		return _index;
	}

	/**
	 * @return true if more elements are available in the iteration
	 */
	public boolean hasMoreElements() {
		return _iterator.hasMoreElements();
	}

	/**
	 * gets the next element and advances by one.
	 */
	public Object nextElement() {
		_index++;
		return _iterator.nextElement();
	}

	/**
   *
   */
	int _index;
	/**
   *
   */
	Vector _array;
	/**
   *
   */
	Enumeration _iterator = null;
}
