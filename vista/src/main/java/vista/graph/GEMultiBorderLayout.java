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

import java.awt.Dimension;
import java.util.ArrayList;

/**
 * Lays out the elements in North, South , East , West and Center positions. The
 * elements are sized by their preferred sizes for the NSEW positions with the
 * remaining space going to the Center position. This is useful for laying out
 * the axis elements in a Plot
 * 
 * @see Plot
 * @author Nicky Sandhu (DWR).
 * @version $Id: GEMultiBorderLayout.java,v 1.1 2003/10/02 20:48:58 redwood Exp
 *          $
 */
public class GEMultiBorderLayout extends GEBorderLayout {
	/**
	 * for debuggin'
	 */
	public static final boolean DEBUG = false;

	/**
	 * Constructor
	 */
	public GEMultiBorderLayout(LayoutMediator mediator) {
		setMediator(mediator);
	}

	/**
   *
   */
	public void setMediator(LayoutMediator mediator) {
		_mediator = mediator;
		if (_mediator != null)
			_mediator.addLayoutManager(this);
	}

	/**
	 * returns the maximum of preferred required dimensions for all graphical
	 * elements in the array times the number of elements in that array.
	 */
	protected Dimension getPreferredSize(ArrayList array) {
		Dimension size = null;
		if (_mediator == null)
			return super.getPreferredSize(array);
		if (array == north)
			size = _mediator.getPreferredSize(NORTH);
		if (array == south)
			size = _mediator.getPreferredSize(SOUTH);
		if (array == east)
			size = _mediator.getPreferredSize(EAST);
		if (array == west)
			size = _mediator.getPreferredSize(WEST);
		if (array == center)
			size = _mediator.getPreferredSize(CENTER);

		return size;
	}

	/**
	 * returns the maximum of minimum required dimensions for all graphical
	 * elements in the array.
	 */
	private Dimension getMinimumSize(ArrayList array) {
		return getPreferredSize(array);
	}

	/**
	 * returns the preferred dimensions to the mediator
	 * 
	 * @see GEBorderLayoutMediator
	 */
	Dimension getPreferredDimensions(String position) {
		Dimension size = new Dimension(0, 0);
		if (position.equals(NORTH)) {
			size = super.getPreferredSize(north);
		}
		if (position.equals(SOUTH)) {
			size = super.getPreferredSize(south);
		}
		if (position.equals(EAST)) {
			size = super.getPreferredSize(east);
		}
		if (position.equals(WEST)) {
			size = super.getPreferredSize(west);
		}
		if (position.equals(CENTER)) {
			size = super.getPreferredSize(center);
		}
		return size;
	}

	/**
   *
   */
	private LayoutMediator _mediator = null;
}
