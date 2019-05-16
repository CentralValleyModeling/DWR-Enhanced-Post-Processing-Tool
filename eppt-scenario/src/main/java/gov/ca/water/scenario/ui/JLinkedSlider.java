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

package gov.ca.water.scenario.ui;
//! JLinkedSlider.java

import javax.swing.*;

/**
 * This class extends the Swing JSlider class by linking a slider to a "left"
 * and a "right" JTextBox. The right textbox displays the value of the slider
 * and the left JTextBox displays the value (_maximum - slider value). The
 * JTextBoxes do not need to be visible.
 *
 * @author Originally authored by Dan Rucinski (LimnoTech)
 */
public class JLinkedSlider extends JSlider
{

	private static final long serialVersionUID = 7532351267994562680L;
	private double _maximum;
	private double _lVal;
	private double _rVal;
	private String _lTextBoxID = "";
	private String _rTextBoxID = "";

	public JLinkedSlider()
	{
		//this class doesn't appear to be used anywhere - cody 2/21/19
	}

	public String getLTextBoxID()
	{
		return _lTextBoxID;
	}

	public void setLTextBoxID(String textBoxID)
	{
		_lTextBoxID = textBoxID;
	}

	public String getRTextBoxID()
	{
		return _rTextBoxID;
	}

	public void setRTextBoxID(String textBoxID)
	{
		_rTextBoxID = textBoxID;
	}

	public void setVals(float slideVal)
	{
		_lVal = _maximum - slideVal;
		_rVal = slideVal;
	}

	public double getLVal()
	{
		return _lVal;
	}

	public double getRVal()
	{
		return _rVal;
	}

}
