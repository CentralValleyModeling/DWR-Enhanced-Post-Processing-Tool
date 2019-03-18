/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */

package gov.ca.water.bo;
//! Derived class modifying JTextField to constrain input and range

import java.util.regex.Pattern;
import javax.swing.*;
import javax.swing.text.AttributeSet;
import javax.swing.text.BadLocationException;
import javax.swing.text.Document;
import javax.swing.text.PlainDocument;

/**
 * This class is used to only enter the numeric values in the text field.
 *
 * @author LimnoTech, GTP
 */
public class NumericTextField extends JTextField
{
	private static final long serialVersionUID = 2314410705390899848L;
	private static final Pattern DIGITS = Pattern.compile("(\\d*)|[0-9]{0,15}[.]{1}[0-9]{0,15}");
	private float _minimum;
	private float _maximum;

	public NumericTextField()
	{
	}

	@Override
	protected Document createDefaultModel()
	{
		return new NumericDocument();
	}

	public float getMinVal()
	{
		return _minimum;
	}

	public void setMinVal(float minval)
	{
		_minimum = minval;
	}

	public float getMaxVal()
	{
		return _maximum;
	}

	public void setMaxVal(float maxval)
	{
		_maximum = maxval;
	}

	/**
	 * This class will check whether the entered text is number or not.
	 */
	private class NumericDocument extends PlainDocument
	{
		private static final long serialVersionUID = 234219857118535655L;
		// The regular expression to match input against (zero or more digits)


		@Override
		public void insertString(int offs, String value, AttributeSet a) throws BadLocationException
		{
			// Only insert the text if it matches the regular expression
			String s = super.getText(0, super.getLength());
			String s1 = s.substring(0, offs);
			String s2 = s.substring(offs, super.getLength());
			String sfinal = s1 + value + s2;
			// EDIT DKR 4/8/14 Corrected from previous
			value = value.trim();
			try
			{
				float f = Float.valueOf(sfinal);
				float min = getMinVal();
				float max = getMaxVal();
				if(DIGITS.matcher(value).matches() && f >= min && f <= max)
				{
					// EDIT DKR 4/8/14 from
					super.insertString(offs, value, a);
				}
				// previous "sfinal"
			}
			catch(NumberFormatException ex)
			{

			}
		}
	}
}
