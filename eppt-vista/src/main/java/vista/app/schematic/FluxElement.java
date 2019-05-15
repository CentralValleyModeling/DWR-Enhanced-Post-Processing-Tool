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
