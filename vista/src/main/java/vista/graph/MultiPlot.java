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

import java.awt.Rectangle;
import java.util.Properties;
import java.util.Vector;

/**
 * A class containing a set of Plot objects. All operations similar to plot are
 * delegated to the current plot object.
 * 
 * @see Plot
 * @author Nicky Sandhu
 * @version $Id: MultiPlot.java,v 1.1 2003/10/02 20:49:05 redwood Exp $
 */
public class MultiPlot extends Plot {
	/**
	 * Constructor for MultiPlot
	 */
	public MultiPlot() {
		this(new PlotAttr());
	}

	/**
	 * Constructor for MultiPlot
	 */
	public MultiPlot(PlotAttr attributes) {
		super(attributes);
		removeAll();
		setLayout(new GELineLayout(GELineLayout.VERTICAL,
				GELineLayout.CENTERED_BETWEEN_BOUNDS));
		_mediator = new GEBorderLayoutMediator();
	}

	/**
	 * Constructor for MultiPlot
	 */
	public MultiPlot(int rows, int cols) {
		this(new PlotAttr(), rows, cols);
	}

	/**
	 * Constructor for MultiPlot
	 */
	public MultiPlot(PlotAttr attributes, int rows, int cols) {
		super(attributes);
		removeAll();
		setLayout(new GEGridLayout(rows, cols, 25, 25));
		_mediator = new GEBorderLayoutMediator();
	}

	/**
	 * Gets the total number of plots
	 */
	public int getNumberOfPlots() {
		return _plots.size();
	}

	/**
	 * returns an array of all the contained plots
	 */
	public Plot[] getAllPlots() {
		Plot[] plots = new Plot[getNumberOfPlots()];
		_plots.copyInto(plots);
		return plots;
	}

	/**
	 * Sets the operations of adding curves/data to the plot window
	 * 
	 * @param index
	 *            A number 0 --> n-1 where n is the total number of plots
	 */
	public void setCurrentPlot(int index) {
		if ((index >= 0) && (index < getNumberOfPlots()))
			_currentPlotIndex = index;
		else
			throw new ArrayIndexOutOfBoundsException("Plot index incorrect");
	}

	/**
	 * Gets the plot object on which operations are currently being done.
	 */
	public Plot getCurrentPlot() {
		return (Plot) _plots.elementAt(_currentPlotIndex);
	}

	/**
	 * Adds graphic elements to itself. Also keeps track of plot objects being
	 * added.
	 * 
	 * @param ge
	 *            The graphic element being added to the
	 */
	public void add(GraphicElement ge) {
		if (ge instanceof Plot) {
			Plot plot = (Plot) ge;
			((GEMultiBorderLayout) plot.getLayout()).setMediator(_mediator);
			super.add(ge);
			ge.getAttributes()._foregroundColor = getAttributes()._foregroundColor;
			_plots.addElement(ge);
			// plot.setName( new Integer(getNumberOfPlots()).toString() );
		} else {
			// throw new IllegalAdditionException(...);
		}
	}

	/**
	 * returns the Axis object for given position. If Axis object is null it
	 * returns null.
	 */
	public Axis getAxis(int axisPosition) {
		return getCurrentPlot().getAxis(axisPosition);
	}

	/**
	 * adds grid lines for the axis (lines perpendicular to axis running the
	 * distance of the plot area). The grid lines are placed at major tick marks
	 * for the given axis.
	 * 
	 * @param position
	 *            The position of the axis
	 */
	public void addGrid(int position) {
		getCurrentPlot().addGrid(position);
	}

	/**
	 * returns a rectangle in which curves are drawn for the current plot
	 */
	public Rectangle getDrawingRegion() {
		return getCurrentPlot().getDrawingRegion();
	}

	/**
   *
   */
	public GEContainer getCurveContainer() {
		return getCurrentPlot().getCurveContainer();
	}

	/**
   *
   */
	public void addCurve(Curve c) {
		getCurrentPlot().addCurve(c);
	}

	/**
   *
   */
	public void removeCurve(Curve c) {
		getCurrentPlot().removeCurve(c);
	}

	/**
	 * gets the number of curves in plot
	 */
	public int getNumberOfCurves() {
		return getCurrentPlot().getNumberOfCurves();
	}

	/**
	 * get curve by number
	 */
	public Curve getCurve(int i) {
		return getCurrentPlot().getCurve(i);
	}

	/**
	 * Returns its properties in a Properties object. These are saved on disk.
	 * 
	 * @param prefixTag
	 *            A tag to assign the context for these properties e.g. if these
	 *            properties belong to Axis class then prefixTag will be "Axis."
	 */
	public void toProperties(Properties p, String prefixTag) {

		if (p == null)
			return;
		PlotAttr attr = (PlotAttr) getAttributes();

		CompositeIterator iterator = getIterator();
		while (iterator.hasMoreElements()) {
			GraphicElement ge = (GraphicElement) iterator.nextElement();
			ge.toProperties(p, getPrefixTag(prefixTag));
		}

		// super.toProperties(p, prefixTag + "MultiPlot.");
	}

	/**
   *
   */
	public String getPrefixTag(String prefixTag) {
		return prefixTag + "MultiPlot.";
	}

	/**
	 * initializes attributes and state from Properties object.
	 */
	public void fromProperties(Properties p, String prefixTag) {
		CompositeIterator iterator = getIterator();
		while (iterator.hasMoreElements()) {
			GraphicElement ge = (GraphicElement) iterator.nextElement();
			ge.fromProperties(p, getPrefixTag(prefixTag));
		}
		// super.fromProperties(p, getPrefixTag(prefixTag));
	}

	/**
	 * The set of plots in this object.
	 */
	private Vector _plots = new Vector();
	/**
	 * The index to the current plot
	 */
	private int _currentPlotIndex = 0;
	/**
	 * The mediator between the layouts for the different plots.
	 */
	private LayoutMediator _mediator;
}
