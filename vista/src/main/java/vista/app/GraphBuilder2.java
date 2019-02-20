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
package vista.app;

import java.awt.Insets;
import java.util.Date;
import java.util.Enumeration;
import java.util.Vector;

import vista.graph.AxisAttr;
import vista.graph.Curve;
import vista.graph.GEBorderLayout;
import vista.graph.GEContainer;
import vista.graph.Graph;
import vista.graph.GraphAttr;
import vista.graph.Legend;
import vista.graph.LegendAttr;
import vista.graph.LegendItem;
import vista.graph.LegendItemAttr;
import vista.graph.MultiPlot;
import vista.graph.Plot;
import vista.graph.PlotAttr;
import vista.graph.TextLine;
import vista.graph.TextLineAttr;
import vista.set.DataReference;

/**
 * Builds a graph from given data sets.
 * 
 * @author Nicky Sandhu
 * @version $Id: GraphBuilder2.java,v 1.1.1.1 1998/09/30 03:57:45 nsandhu Exp $
 */
public class GraphBuilder2 implements GraphBuilder {
	private int _nrows, _ncols, _curvesPerPlot;

	/**
   *
   */
	public GraphBuilder2() {
		_nrows = 2;
		_ncols = 2;
		_curvesPerPlot = 2;
	}

	/**
	 * initializes the graph builder with no data sets
	 */
	public GraphBuilder2(int nrows, int ncols, int curvesPerPlot) {
		_dataRefs = new Vector();
		_curvesPerPlot = curvesPerPlot;
		_nrows = nrows;
		_ncols = ncols;
	}

	/**
	 * adds a data set to the graph builder
	 */
	public void addData(DataReference ref) {
		if (ref == null)
			return;
		_dataRefs.addElement(ref);
	}

	/**
	 * removes the dat set
	 */
	public void removeData(DataReference ref) {
		if (ref == null)
			return;
		_dataRefs.removeElement(ref);
	}

	/**
	 * removes all the data sets
	 */
	public void removeAll() {
		if (_dataRefs.size() > 0)
			_dataRefs.removeAllElements();
	}

	/**
	 * creates graph from existing data sets
	 */
	public Graph[] createGraphs() {
		if (_dataRefs.size() == 0)
			return null;
		// get the number of graphs and initialize them.
		int ngraphs = (_dataRefs.size() - 1)
				/ (_curvesPerPlot * _nrows * _ncols) + 1;
		Graph[] graphs = new Graph[ngraphs];
		MultiPlot[] plotContainers = new MultiPlot[ngraphs];
		for (int i = 0; i < ngraphs; i++) {
			graphs[i] = new Graph(new GraphAttr());
			plotContainers[i] = new MultiPlot(new PlotAttr());
			// plotContainers[i].setLayout( new GEGridLayout(_nrows, _ncols) );
			graphs[i].addPlot(plotContainers[i]);
		}
		// create and add curves to plots to the graphs
		int gindex = 0;
		Graph graph = null;
		GEContainer pc = null;
		Plot plot = null;
		Legend lg = null;
		int numberAdded = 0;
		for (Enumeration e = _dataRefs.elements(); e.hasMoreElements();) {
			DataReference ref = (DataReference) e.nextElement();
			graph = graphs[gindex];
			pc = plotContainers[gindex];
			graph.setTitle(GraphTemplate.generateTitleText(_dataRefs));
			graph.setInsets(new Insets(10, 25, 10, 25));
			Curve c = CurveFactory.createCurve(ref, AxisAttr.BOTTOM,
					AxisAttr.LEFT, GraphTemplate.generateLegendText(ref));
			if (plot == null || plot.getNumberOfCurves() > _curvesPerPlot) {
				if (plot != null)
					plot.addLegend(lg, AxisAttr.LEFT);
				plot = new Plot(new PlotAttr());
				lg = new Legend(new LegendAttr());
				if (numberAdded == _nrows * _ncols) {
					gindex++;
					if (graph != null) {
						// add date and time.
						TextLineAttr dateAttr = new TextLineAttr();
						dateAttr._font = new java.awt.Font("Times Roman",
								java.awt.Font.PLAIN, 8);
						dateAttr._foregroundColor = java.awt.Color.red;
						dateAttr._resizeProportionally = true;
						dateAttr._justification = TextLineAttr.RIGHT;
						graph.add(GEBorderLayout.SOUTH, new TextLine(dateAttr,
								new Date().toString()));
					}
					graph = graphs[gindex];
					pc = plotContainers[gindex];
					numberAdded = 0;
				}
				pc.add(plot);
				numberAdded++;
			} else {
			}
			plot.addCurve(c);
			lg.add(new LegendItem(new LegendItemAttr(), c));
		}
		if (plot != null) {
			// add date and time only if missed adding in above loop
			if (graph != null) {
				TextLineAttr dateAttr = new TextLineAttr();
				dateAttr._font = new java.awt.Font("Times Roman",
						java.awt.Font.PLAIN, 8);
				dateAttr._foregroundColor = java.awt.Color.red;
				dateAttr._resizeProportionally = true;
				dateAttr._justification = TextLineAttr.RIGHT;
				graph.add(GEBorderLayout.SOUTH, new TextLine(dateAttr,
						new Date().toString()));
			}
			plot.addLegend(lg, AxisAttr.LEFT);
		}
		return graphs;
	}

	/**
   *
   */
	private Vector _dataRefs;
}
