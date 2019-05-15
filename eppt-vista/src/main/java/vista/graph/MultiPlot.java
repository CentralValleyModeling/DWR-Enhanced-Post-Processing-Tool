/*
 * Enhanced Post Processing Tool (EPPT) Copyright (c) 2019.
 *
 * EPPT is copyrighted by the State of California, Department of Water Resources. It is licensed
 * under the GNU General Public License, version 2. This means it can be
 * copied, distributed, and modified freely, but you may not restrict others
 * in their ability to copy, distribute, and modify it. See the license below
 * for more details.
 *
 * GNU General Public License
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
