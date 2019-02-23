/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */
package vista.graph;

import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Rectangle;
import java.util.Properties;

/**
 * A curve which can toggle between displaying and not displaying flags
 * 
 * @author Nicky Sandhu
 * @version $Id: ReferenceCurve.java,v 1.1 2003/10/02 20:49:07 redwood Exp $
 */
public class ReferenceCurve extends Curve {
	private boolean _showFlags = false;
	private Curve _dc, _fc;

	/**
   *
   */
	public ReferenceCurve(CurveDataModel cdmNormal, CurveDataModel cdmFlagged) {
		this(new CurveAttr(), cdmNormal, cdmFlagged);
	}

	/**
   *
   */
	public ReferenceCurve(CurveAttr attributes, CurveDataModel cdmNormal,
			CurveDataModel cdmFlagged) {
		super(attributes, cdmNormal);
		_dc = new DefaultCurve(attributes, cdmNormal);
		_fc = new FlaggedCurve(attributes, cdmFlagged);
	}

	/**
   *
   */
	public Curve getDefaultCurve() {
		return _dc;
	}

	/**
   *
   */
	public Curve getFlaggedCurve() {
		return _fc;
	}

	/**
   *
   */
	public boolean isShowingFlags() {
		return _showFlags;
	}

	/**
   *
   */
	public void setShowingFlags(boolean b) {
		_showFlags = b;
	}

	/**
	 * set the new model and inform axes of change
	 */
	public void setModel(CurveDataModel cdm) {
		_dc.setModel(cdm);
	}

	/**
	 * set x axis used for scaling the x data value
	 */
	public void setXAxis(Axis xAxis) {
		_fc.setXAxis(xAxis);
		_dc.setXAxis(xAxis);
	}

	/**
	 * set y axis for scaling the y data value.
	 */
	public void setYAxis(Axis yAxis) {
		_dc.setYAxis(yAxis);
		_fc.setYAxis(yAxis);
	}

	/**
	 * get x axis used for scaling the x data value
	 */
	public Axis getXAxis() {
		return _dc.getXAxis();
	}

	/**
	 * get y axis for scaling the y data value.
	 */
	public Axis getYAxis() {
		return _dc.getYAxis();
	}

	/**
	 * no back ground/ transparent background
	 */
	public void preDraw() {
		_fc.preDraw();
		_dc.preDraw();
	}

	/**
	 * Draws the data by scaling it and joining consecutive data points and/or
	 * plotting symbols for data points.
	 */
	protected void drawCurve() {
		if (_showFlags)
			_fc.drawCurve();
		else
			_dc.drawCurve();
	}
	
	public void setAttributes(GEAttr attr){
		super.setAttributes(attr);
		_fc.setAttributes(attr);
		_dc.setAttributes(attr);
	}

	/**
	 * gets preferred size
	 */
	public Dimension getPreferredSize() {
		if (_showFlags)
			return _fc.getPreferredSize();
		else
			return _dc.getPreferredSize();
	}

	/**
	 * gets minimum size
	 */
	public Dimension getMinimumSize() {
		if (_showFlags)
			return _fc.getMinimumSize();
		else
			return _dc.getMinimumSize();
	}

	/**
   *
   */
	public void setSymbol(Symbol s) {
		if (_showFlags)
			_fc.setSymbol(s);
		else
			_dc.setSymbol(s);
	}

	/**
   *
   */
	public Symbol getSymbol() {
		if (_showFlags)
			return _fc.getSymbol();
		else
			return _dc.getSymbol();
	}

	/**
   *
   */
	public String getPrefixTag(String prefixTag) {
		if (_showFlags)
			return _fc.getPrefixTag(prefixTag);
		else
			return _dc.getPrefixTag(prefixTag);
	}

	/**
	 * Returns its properties in a Properties object. These are saved on disk.
	 * 
	 * @param prefixTag
	 *            A tag to assign the context for these properties e.g. if these
	 *            properties belong to Axis class then prefixTag will be "Axis."
	 */
	public void toProperties(Properties p, String prefixTag) {
		if (_showFlags)
			_fc.toProperties(p, prefixTag);
		else
			_dc.toProperties(p, prefixTag);
	}

	/**
	 * initializes attributes and state from Properties object.
	 */
	public void fromProperties(Properties p, String prefixTag) {
		if (_showFlags)
			_fc.fromProperties(p, prefixTag);
		else
			_dc.fromProperties(p, prefixTag);
	}

	/**
	 * create dialog panel for this element.
	 */
	public GEDialogPanel createDialogPanel() {
		if (_showFlags)
			return _fc.createDialogPanel();
		else
			return _dc.createDialogPanel();
	}

	/**
	 * sets the graphics of itself and children to the same graphics context.
	 */
	public final void setGraphics(Graphics gc) {
		super.setGraphics(gc);
		_fc.setGraphics(gc);
		_dc.setGraphics(gc);
	}

	/**
	 * set this elements bounds
	 */
	public void setBounds(Rectangle r) {
		super.setBounds(r);
		_fc.setBounds(r);
		_dc.setBounds(r);
	}
}
