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

import java.awt.Font;
import java.awt.Insets;
import java.awt.Rectangle;

import vista.app.BarElement;
import vista.app.FluxInput;
import vista.graph.AnimationObservable;
import vista.graph.AnimationObserver;
import vista.graph.GEAttr;
import vista.graph.GEBorderLayout;
import vista.graph.GEContainer;
import vista.graph.Scale;
import vista.graph.TextLine;
import vista.graph.TextLineAttr;

/**
  * 
   */
public class FluxElement extends GEContainer implements AnimationObserver {
	/**
    * 
    */
	public FluxElement(GEAttr attr, String filename, String pathname,
			String name) {
		super(attr);
		_input = new FluxInput(filename, pathname);

		TextLineAttr tla = new TextLineAttr();
		tla._font = new Font("Times Roman", Font.BOLD, 10);

		// StringTokenizer st = new StringTokenizer(pathname,"/");
		// st.nextToken();
		// st = new StringTokenizer( st.nextToken(), "_" );
		// st.nextToken();
		_title = new TextLine(tla, name);

		_barElement = new BarElement(new GEAttr());
		_barElement.setFluxInput(_input);
		_barElement.setInsets(new Insets(10, 5, 10, 5));

		setLayout(new GEBorderLayout());
		add("South", _title);
		add("Center", _barElement);

	}

	/**
    * 
    */
	public void setRange(float min, float max) {
		_barElement.setRange(min, max);
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
			rb.width = 25;
			rb.height = 70;
			super.setBounds(rb);
		} else {
			super.setBounds(r);
		}
	}

	/**
    *
    */
	public void update(AnimationObservable o, Object arg) {
		_barElement.update(o, arg);
	}

	/**
    * 
    */
	public void animateNext() {
		// System.out.println(this.getClass().getName() + ".animateNext()");
		_title.draw();
		super.animateNext();
	}

	/**
    * 
    */
	public void resetInput() {
		_input.resetInput();
	}

	/**
    * 
    */
	DSMGridElement _grid;
	/**
    * 
    */
	BarElement _barElement;
	/**
    * 
    */
	TextLine _title;
	/**
    * 
    */
	Node _node;
	/**
    * 
    */
	FluxInput _input;
}
