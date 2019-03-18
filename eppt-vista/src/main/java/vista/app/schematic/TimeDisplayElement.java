/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
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
