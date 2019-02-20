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
 * A factory for producing GraphicElement objects.
 */
public class DefaultGraphFactory implements GraphFactory {
	/**
   *
   */
	public Axis createAxis() {
		return new Axis(new AxisAttr(), AxisAttr.BOTTOM);
	}

	/**
   *
   */
	public GEContainer createGEContainer() {
		return new GEContainer(new GEAttr());
	}

	/**
   *
   */
	public Legend createLegend() {
		LegendAttr legendAttr = new LegendAttr();
		legendAttr._resizeProportionally = true;
		return new Legend(legendAttr);
	}

	/**
   * 
   */
	public LegendItem createLegendItem() {
		return new LegendItem(new LegendItemAttr(), null);
	}

	/**
   *
   */
	public TextLine createTextLine() {
		return new TextLine(new TextLineAttr(), "");
	}

	/**
   *
   */
	public TickText createTickText() {
		return new TickText(new TickTextAttr(), null, null);
	}

	/**
   *
   */
	public LineElement createLineElement() {
		return new LineElement(new LineElementAttr());
	}

	/**
   *
   */
	public TickLine createTickLine() {
		return new TickLine(new TickLineAttr(), null);
	}

	/**
   *
   */
	public Plot createPlot() {
		return new Plot(new PlotAttr());
	}

	/**
   *
   */
	public Curve createCurve() {
		return new DefaultCurve(new CurveAttr(), null);
	}

	/**
	 * creates a default empty graph with default attributes
	 */
	public Graph createGraph() {
		GraphAttr graphAttr = new GraphAttr();
		return new Graph(graphAttr);
	}

	/**
	 * creates an empty multi plot container with default attributes.
	 */
	public MultiPlot createMultiPlot() {
		return new MultiPlot(new PlotAttr());
	}

	/**
   *
   */
	public Color getNextColor() {
		_colorIndex++;
		return _colorTable[_colorIndex % _colorTable.length];
	}

	private int _colorIndex = -1;
	private static Color[] _colorTable = { Color.red, Color.green, Color.blue,
			Color.pink, Color.cyan, Color.orange, Color.magenta,
			new Color(0, 206, 209), // Dark Turquoise
			new Color(85, 107, 47), // Dark Olive Green
			new Color(176, 48, 96), // maroon
			new Color(95, 158, 160), // Cadet Blue
			new Color(218, 112, 214), // orchid
			new Color(160, 32, 240) // purple
	};

}
