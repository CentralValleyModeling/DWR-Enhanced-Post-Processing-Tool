/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */
package vista.app;

import vista.set.DataReference;

/**
 * A class encapsulating the graphing information for a data set
 * 
 * @author Nicky Sandhu
 * @version $Id: DataSetInfo.java,v 1.1 2003/10/02 20:48:27 redwood Exp $
 */
class DataSetInfo {
	private DataReference _ref;
	private int _graphIndex;
	private int _plotIndex;
	private int _xPos, _yPos;
	private String _legendLabel;

	/**
	 * creates a data set information for the given reference
	 */
	public DataSetInfo(DataReference ref) {
		_ref = ref;
	}

	/**
   *
   */
	public DataReference getReference() {
		return _ref;
	}

	/**
   *
   */
	void setGraphIndex(int gi) {
		_graphIndex = gi;
	}

	/**
   *
   */
	public int getGraphIndex() {
		return _graphIndex;
	}

	/**
   *
   */
	void setPlotIndex(int pi) {
		_plotIndex = pi;
	}

	/**
   *
   */
	public int getPlotIndex() {
		return _plotIndex;
	}

	/**
   *
   */
	void setXAxisPosition(int pos) {
		_xPos = pos;
	}

	/**
   *
   */
	public int getXAxisPosition() {
		return _xPos;
	}

	/**
   *
   */
	void setYAxisPosition(int pos) {
		_yPos = pos;
	}

	/**
   *
   */
	public int getYAxisPosition() {
		return _yPos;
	}

	/**
   *
   */
	void setLegendLabel(String str) {
		_legendLabel = str;
	}

	/**
   *
   */
	public String getLegendLabel() {
		return _legendLabel;
	}
}
