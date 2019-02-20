/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */

package gov.ca.water.calgui.bo;
//! JLinkedSlider.java

import javax.swing.*;

/**
 * This class extends the Swing JSlider class by linking a slider to a "left"
 * and a "right" JTextBox. The right textbox displays the value of the slider
 * and the left JTextBox displays the value (maximum - slider value). The
 * JTextBoxes do not need to be visible.
 *
 * @author Originally authored by Dan Rucinski (LimnoTech)
 */
public class JLinkedSlider extends JSlider
{

	private static final long serialVersionUID = 7532351267994562680L;
	private float maximum;
	private float lVal;
	private float rVal;
	private String lTextBoxID = "";
	private String rTextBoxID = "";

	public JLinkedSlider()
	{
	}

	public String getLTextBoxID()
	{
		return lTextBoxID;
	}

	public void setLTextBoxID(String textBoxID)
	{
		lTextBoxID = textBoxID;
	}

	public String getRTextBoxID()
	{
		return rTextBoxID;
	}

	public void setRTextBoxID(String textBoxID)
	{
		rTextBoxID = textBoxID;
	}

	public void setVals(float slideVal)
	{
		lVal = maximum - slideVal;
		rVal = slideVal;
	}

	public float getLVal()
	{
		return lVal;
	}

	public float getRVal()
	{
		return rVal;
	}

}