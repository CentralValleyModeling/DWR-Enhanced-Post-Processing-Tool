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
import java.util.Vector;

import vista.graph.Axis;
import vista.graph.AxisAttr;
import vista.graph.Curve;
import vista.graph.GEAttr;
import vista.graph.GEBorderLayout;
import vista.graph.GEContainer;
import vista.graph.GEGridLayout;
import vista.graph.GEOverlayLayout;
import vista.graph.Legend;
import vista.graph.LegendItem;
import vista.gui.VistaUtils;
import vista.set.DataReference;
import vista.set.DataRetrievalException;
import vista.set.ProxyFactory;
import vista.set.RegularTimeSeries;

/**
 * 
 * 
 * @author Nicky Sandhu
 * @version $Id: MultiScatterPlot.java,v 1.1 2003/10/02 20:48:36 redwood Exp $
 */
public class MultiScatterPlot extends GEContainer {
	private DataReference[] _ts;
	private Legend _legend;

	/**
   *
   */
	public MultiScatterPlot(DataReference[] ts) {
		super(new GEAttr());
		Vector tsArray = new Vector();
		for (int i = 0; i < ts.length; i++) {
			try {
				if (ts[i].getData() instanceof RegularTimeSeries)
					tsArray.addElement(ts[i]);
			} catch (Exception e) {
				VistaUtils.displayException(null, e);
			}
		}
		int sz = tsArray.size();
		if (sz <= 0)
			throw new IllegalArgumentException(
					"No time series found for creating scatter plot");
		_ts = new DataReference[sz];
		tsArray.copyInto(_ts);
		setLayout(new GEBorderLayout());
		setInsets(new Insets(50, 50, 50, 50));
		add(GEBorderLayout.CENTER, setUpPlot(_ts));
		add(GEBorderLayout.SOUTH, _legend);
	}

	/**
   *
   */
	public GEContainer setUpPlot(DataReference[] ts) {
		GEContainer cc = new GEContainer(new GEAttr());
		cc.setLayout(new GEOverlayLayout());
		// create
		GEContainer laxisc = new GEContainer(new GEAttr());
		laxisc.setLayout(new GEGridLayout(ts.length, 1, 40, 40));
		GEContainer baxisc = new GEContainer(new GEAttr());
		baxisc.setLayout(new GEGridLayout(1, ts.length, 40, 40));
		try {
			for (int i = 0; i < ts.length; i++) {
				Axis axis = new Axis(new AxisAttr(), AxisAttr.LEFT);
				laxisc.add(axis);
				axis.setAxisLabel("Data # " + (i + 1));
			}
			//
			for (int i = 0; i < ts.length; i++) {
				Axis axis = new Axis(new AxisAttr(), AxisAttr.BOTTOM);
				baxisc.add(axis);
				axis.setAxisLabel("Data # " + (i + 1));
			}
		} catch (DataRetrievalException dre) {
		}
		//
		_legend = new Legend();
		//
		for (int i = 0; i < ts.length; i++) {
			for (int j = 0; j < ts.length; j++) {
				DataReference ref = ProxyFactory.createPairedTimeSeriesProxy(
						ts[i], ts[j]);
				String legname = "Data # " + (j + 1) + " : "
						+ ts[j].getData().getName();
				Curve crvij = CurveFactory.createCurve(ref, AxisAttr.BOTTOM,
						AxisAttr.LEFT, legname);
				Axis xAxis = (Axis) baxisc.getElement(i);
				Axis yAxis = (Axis) laxisc.getElement(j);
				crvij.setXAxis(xAxis);
				crvij.setYAxis(yAxis);
				cc.add(crvij);
				// only add legend on the last cycle.
				if (j == i)
					_legend.add(new LegendItem(crvij));
				if (j != i) {
					ref = ProxyFactory
							.createPairedTimeSeriesProxy(ts[j], ts[i]);
					Curve crvji = CurveFactory.createCurve(ref, AxisAttr.LEFT,
							AxisAttr.BOTTOM, ref.getName());
					crvji.setAttributes(crvij.getAttributes());
					crvji.setXAxis((Axis) baxisc.getElement(j));
					crvji.setYAxis((Axis) laxisc.getElement(i));
					cc.add(crvji);
				}
			}
		}
		GEContainer mp = new GEContainer(new GEAttr());
		mp.setLayout(new GEBorderLayout());
		mp.add(GEBorderLayout.CENTER, cc);
		mp.add(GEBorderLayout.WEST, laxisc);
		mp.add(GEBorderLayout.SOUTH, baxisc);
		mp.drawFirst(cc);
		return mp;
	}

}
