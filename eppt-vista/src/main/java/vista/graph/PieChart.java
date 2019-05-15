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

import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Rectangle;

/**
 * 
 * 
 * @author Nicky Sandhu
 * @version $Id: PieChart.java,v 1.1 2003/10/02 20:49:05 redwood Exp $
 */
public class PieChart extends GEContainer {
	private GEContainer _pc;
	private PieChartModel _pcm;

	public PieChart(PieChartModel pcm) {
		this(new GEAttr(), pcm);
	}

	public PieChart(GEAttr attr, PieChartModel pcm) {
		super(attr);
		_pcm = pcm;
		setLayout(new GEBorderLayout());
		add(GEBorderLayout.NORTH, new TextLine(new TextLineAttr(), pcm
				.getTitle()));
		add(GEBorderLayout.CENTER, _pc = new GEContainer(new GEAttr()));
	}

	/**
   *
   */
	public void Draw() {
		Graphics g = getGraphics();
		Rectangle r = _pc.getBounds();
		_pcm.reset();
		double max = _pcm.getSumOfValues();
		int startAngle = 0;
		double sum = 0;
		int index = 0;
		int cl = GraphUtils._colorTable.length;
		while (_pcm.hasMorePies()) {
			PieModel pm = _pcm.nextPie();
			String label = pm.getLabel();
			double val = pm.getValue();
			int w = Math.min(r.width, r.height);
			int arcAngle = (int) Math.round(val / max * 360.0);
			g.setColor(GraphUtils._colorTable[index % cl]);
			g.fillArc(r.x, r.y, w, w, startAngle, arcAngle);
			startAngle += arcAngle;
			sum += val;
			index++;
		}
	}

	/**
	 * draws with update pie chart model if any
	 */
	public void animateNext() {
		Draw();
	}

	/**
	 * calculates the preferred size of this element
	 * 
	 * @return the preferred size
	 */
	public Dimension getPreferredSize() {
		return new Dimension(125, 125);
	}

	/**
	 * calculates the minimum size of this element
	 * 
	 * @return the minimum size
	 */
	public Dimension getMinimumSize() {
		return new Dimension(125, 125);
	}
}
