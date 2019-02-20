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
package vista.app.schematic;

import java.awt.Color;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Rectangle;
import java.awt.geom.AffineTransform;

import vista.app.TimeData;
import vista.graph.AnimationObservable;
import vista.graph.AnimationObserver;
import vista.graph.GEBorderLayout;
import vista.graph.GEContainer;
import vista.graph.Scale;
import vista.graph.TextLine;
import vista.graph.TextLineAttr;

/**
   * 
   */
public class TimeDisplayElement extends GEContainer implements
		AnimationObserver {
	/**
   * 
   */
	public TimeDisplayElement(TextLineAttr tla, TimeData data) {
		super(tla);
		_textLine = new TextLine(tla, "        ");
		setLayout(new GEBorderLayout());
		add("Center", _textLine);

		_data = data;

	}

	/**
   *
   */
	public void update(AnimationObservable o, Object arg) {
		_value = _data.getNextValue().trim();
	}

	/**
   * 
   */
	public void Draw() {
	}

	/**
   * 
   */
	public void setGrid(DSMGridElement grid) {
		_grid = grid;
	}

	/**
   * 
   */
	public void setBaseNode(int nodeId) {
		_node = _grid.getNetwork().getNode(nodeId);
	}

	/**
   * 
   */
	public void setBounds(Rectangle r) {
		if (_grid != null) {
			Rectangle rb = getBounds();
			Scale xS = _grid.getXScale();
			Scale yS = _grid.getYScale();

			rb.x = xS.scaleToUC(_node.getX());
			rb.y = yS.scaleToUC(_node.getY());
			super.setBounds(rb);
		} else {
			super.setBounds(r);
		}
	}

	/**
   * 
   */
	public void animateNext() {
		_value = _data.getNextValue();
		Graphics gc = getGraphics();
		((Graphics2D)gc).setTransform(new AffineTransform());
		Color previousColor = gc.getColor();
		gc.setColor(Color.lightGray);
		Rectangle r = getInsetedBounds();
		gc.fillRect(r.x, r.y, r.width, r.height);
		gc.setColor(previousColor);
		if (_value == null)
			return;
		_textLine.setText(_value.trim());
		if (DEBUG)
			System.out.println(_value);
		_textLine.draw();
	}

	/**
   * 
   */
	TextLine _textLine;
	/**
   * 
   */
	String _value;
	/**
   * 
   */
	TimeData _data;
	/**
   * 
   */
	private DSMGridElement _grid;
	/**
   * 
   */
	private Node _node;
	/**
   * 
   */
	private boolean DEBUG = false;
}
