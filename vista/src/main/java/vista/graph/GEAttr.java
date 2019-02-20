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

import java.awt.Color;

/**
 * Encapsulates all attributes of a GraphicElement. This sets up a parallel
 * hierarchy to that of the GraphicElement with a attribute class for every
 * GraphicElement subclass.
 * <p>
 * Currently all attributes are declared to be public with no accessor or
 * modifier functions. This has a problem in that something like LegendItem
 * which has both LineElement and TextLine cannot inherit those characteristics
 * for both classes at once. This can be solved by declaring each property as
 * setX and getX functions and declaring each attribute class to be a collection
 * of such functions.
 * 
 * @author Nicky Sandhu (DWR).
 * @version $Id: GEAttr.java,v 1.1 2003/10/02 20:48:56 redwood Exp $
 */
public class GEAttr implements Attributes {
	/**
	 * horizontal orientation
	 */
	public final static int HORIZONTAL = 1;
	/**
	 * vertical orientation
	 */
	public final static int VERTICAL = HORIZONTAL + 1;
	/**
	 * background color. When this is set for a graphic element it usually
	 * overwrites the already drawn stuff in the drawing region
	 */
	public Color _backgroundColor = null;// new Color(210,210,210);
	/**
	 * foreground color
	 */
	public Color _foregroundColor = Color.black;
	/**
	 * orientation
	 */
	public int _orientation = HORIZONTAL;
	/**
	 * true if graphics should be copied. The alternative is to reset the
	 * graphics after changing attributes such as font, color, etcetra.
	 * Obviously creating graphic copies are not cheap.
	 */
	public boolean _createGraphicsCopy = false;
	/**
	 * true if visible.
	 */
	public boolean _isVisible = true;
	/**
	 * true if clipped to within bounds
	 */
	public boolean _clipWithinBounds = false;

	/**
   *
   */
	public void setBackgroundColor(Color backgroundColor) {
		_backgroundColor = backgroundColor;
	}

	/**
   *
   */
	public Color getBackgroundColor() {
		return _backgroundColor;
	}

	/**
   *
   */
	public void setForegroundColor(Color foregroundColor) {
		_foregroundColor = foregroundColor;
	}

	/**
   *
   */
	public Color getForegroundColor() {
		return _foregroundColor;
	}

	/**
	 * sets Orientation
	 */
	public void setOrientation(int orientation) {
		_orientation = orientation;
	}

	/**
	 * gets Orientation
	 */
	public int getOrientation() {
		return _orientation;
	}

	/**
	 * sets CreateGraphicsCopy
	 */
	public void setCreateGraphicsCopy(boolean createGraphicsCopy) {
		_createGraphicsCopy = createGraphicsCopy;
	}

	/**
	 * gets CreateGraphicsCopy
	 */
	public boolean getCreateGraphicsCopy() {
		return _createGraphicsCopy;
	}

	/**
	 * sets IsVisible
	 */
	public void setIsVisible(boolean isVisible) {
		_isVisible = isVisible;
	}

	/**
	 * gets IsVisible
	 */
	public boolean getIsVisible() {
		return _isVisible;
	}

	/**
	 * sets ClipWithinBounds
	 */
	public void setClipWithinBounds(boolean clipWithinBounds) {
		_clipWithinBounds = clipWithinBounds;
	}

	/**
	 * gets ClipWithinBounds
	 */
	public boolean getClipWithinBounds() {
		return _clipWithinBounds;
	}

	// /**
	// * sets Pxx
	// */
	// public void setPxx( tp pxx){
	// _pxx = pxx;
	// }
	// /**
	// * gets Pxx
	// */
	// public tp getPxx(){
	// return _pxx;
	// }
	/**
	 * copies the fields into the given GEAttr object
	 */
	public void copyInto(GEAttr ga) {
		ga._backgroundColor = this._backgroundColor;
		ga._foregroundColor = this._foregroundColor;
		ga._orientation = this._orientation;
		ga._createGraphicsCopy = this._createGraphicsCopy;
		ga._isVisible = this._isVisible;
		ga._clipWithinBounds = this._clipWithinBounds;
	}
}
