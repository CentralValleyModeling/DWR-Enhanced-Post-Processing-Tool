/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */
package vista.graph;

import java.awt.event.MouseEvent;
import java.text.DecimalFormat;
import javax.swing.*;

import vista.time.TimeFactory;
import vista.time.TimeFormat;

/**
 * Displays the co-ordinates of a point in the drawing region of the plot.
 * 
 * @author Nicky Sandhu
 * @version $Id: CoordinateDisplayInteractor.java,v 1.1 2003/10/02 20:48:51
 *          redwood Exp $
 */
public class CoordinateDisplayInteractor extends ElementInteractor {
	/**
   *
   */
	private static DecimalFormat _df;
	private static TimeFormat _dtf;
	private TimeFactory _tf;
	private GECanvas _gC;

	/**
   *
   */
	public CoordinateDisplayInteractor(GECanvas gC) {
		_gC = gC;
		_df = new DecimalFormat();
		_df.setMaximumFractionDigits(2);
		_tf = TimeFactory.getInstance();
		_dtf = _tf.getTimeFormatInstance().create("ddMMMyyyy HHmm");
		ToolTipManager.sharedInstance().setDismissDelay(100000);
		ToolTipManager.sharedInstance().setInitialDelay(1000);
		ToolTipManager.sharedInstance().setReshowDelay(2000);
		ToolTipManager.sharedInstance().registerComponent(_gC);
		ToolTipManager.sharedInstance().setEnabled(true);
	}

	/**
    *
    */
	public void doneDisplaying() {
		ToolTipManager.sharedInstance().unregisterComponent(_gC);
		ToolTipManager.sharedInstance().setEnabled(false);
	}

	/**
   *
   */
	public void mouseMoved(MouseEvent e) {
		int x = e.getX();
		int y = e.getY();
		// System.out.println(x+","+y);
		Plot plot = getPlot(e.getX(), e.getY());
		if (plot.getDrawingRegion().contains(x, y)) {
			StringBuffer buf = new StringBuffer(100);
			Axis axis = plot.getAxis(AxisAttr.BOTTOM);
			if (axis != null) {
				buf.append("b = ");
				if (axis.getTickGenerator().getFormatter() instanceof TimeFormat)
					buf.append(_tf.createTime(
							(long) axis.getScale().scaleToDC(x)).format(_dtf));
				else
					buf.append(_df.format(axis.getScale().scaleToDC(x)));
			}
			//
			axis = plot.getAxis(AxisAttr.LEFT);
			if (axis != null) {
				buf.append(", l = ");
				buf.append(_df.format(axis.getScale().scaleToDC(y)));
			}
			//
			axis = plot.getAxis(AxisAttr.TOP);
			if (axis != null) {
				buf.append(" | t = ");
				if (axis.getTickGenerator().getFormatter() instanceof TimeFormat)
					buf.append(_tf.createTime(
							(long) axis.getScale().scaleToDC(x)).format(_dtf));
				else
					buf.append(_df.format(axis.getScale().scaleToDC(x)));
			}
			//
			axis = plot.getAxis(AxisAttr.RIGHT);
			if (axis != null) {
				buf.append(" , r = ");
				buf.append(_df.format(axis.getScale().scaleToDC(y)));
			}
			String txt = buf.toString();
			// System.out.println(txt);
			_gC.setToolTipText(txt);
		}
	}

	/**
   *
   */
	private Plot getPlot(int x, int y) {
		Graph graph = (Graph) _gC.getGraphicElement();
		Plot plot = graph.getPlot();
		GraphicElement[] leafs = graph.getElements(MultiPlot.class);
		if (leafs == null)
			return plot;
		if (leafs.length == 1) {
			MultiPlot mp = (MultiPlot) leafs[0];
			leafs = mp.getElements(Plot.class);
		}
		if (leafs == null)
			return plot;
		for (int i = 0; i < leafs.length; i++) {
			if (leafs[i] instanceof MultiPlot)
				continue;
			Plot p = (Plot) leafs[i];
			if (p != null && p.contains(x, y))
				plot = p;
			if (DEBUG)
				System.out.println(i + ": " + leafs[i]);
		}
		if (DEBUG)
			System.out.println("Plot @ " + "(" + x + "," + y + "): "
					+ plot.getClass().getName() + "@"
					+ Integer.toHexString(plot.hashCode()));
		return plot;
	}
}
