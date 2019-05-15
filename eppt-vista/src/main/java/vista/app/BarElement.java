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
package vista.app;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.Graphics;
import java.awt.Rectangle;
import java.text.NumberFormat;

import vista.graph.AnimateElement;
import vista.graph.AnimationObservable;
import vista.graph.AnimationObserver;
import vista.graph.GEAttr;
import vista.graph.Scale;

/**
 *
 */
public class BarElement extends AnimateElement implements AnimationObserver
{
	static NumberFormat _nf = NumberFormat.getInstance();

	static
	{
		_nf.setMaximumFractionDigits(1);
	}

	/**
	 * Value in units of percentage
	 */
	protected float _value;
	/**
	 *
	 */
	protected FluxInput fluxInput;
	/**
	 *
	 */
	protected Scale _s;
	/**
	 *
	 */
	protected Color _lineColor = Color.yellow;
	/**
	 *
	 */
	protected Color _barColor = Color.red;
	/**
	 *
	 */
	protected Font _valueFont = new Font("Times Roman", Font.PLAIN, 10);

	/**
	 *
	 */
	public BarElement(GEAttr attr)
	{
		super(attr);
		_s = new Scale(0, 100, 0, 10);
	}

	/**
	 *
	 */
	public void setRange(float min, float max)
	{
		_s.setDCRange(min, max);
	}

	/**
	 *
	 */
	public void setFluxInput(FluxInput input)
	{
		fluxInput = input;
	}

	/**
	 *
	 */
	public void update(AnimationObservable o, Object arg)
	{
		_value = fluxInput.getNextValue();
	}

	/**
	 *
	 */
	public void Draw()
	{
	}

	/**
	 *
	 */
	public void animateNext()
	{
		// System.out.println(this.getClass().getName() + ".animateNext()");
		Graphics gc = getGraphics();
		Color previousColor = gc.getColor();
		Font previousFont = gc.getFont();

		Rectangle r = getInsetedBounds();
		Rectangle barRectangle = r;

		_s.setUCRange(r.y + r.height, r.y);

		gc.setColor(_barColor);

		gc.fillRect(r.x, _s.scaleToUC(_value), r.width, _s.scaleToUC(0)
				- _s.scaleToUC(_value));
		gc.setColor(_lineColor);
		gc.setFont(_valueFont);

		float max = (float) _s.getDataMaximum();
		float min = (float) _s.getDataMinimum();
		drawLineWithValue(min, r);
		drawLineWithValue((max + min) / 2, r);
		drawLineWithValue(max, r);
		gc.drawString(_nf.format(_value) + "%", r.x + 2 * r.width, r.y);
		gc.setColor(previousColor);
		gc.setFont(previousFont);
	}

	/**
	 *
	 */
	private void drawLineWithValue(float value, Rectangle r)
	{
		Graphics gc = getGraphics();
		gc.drawLine(r.x, _s.scaleToUC(value), r.x + r.width, _s
				.scaleToUC(value));
		gc.drawString(new Integer((int) value).toString().trim(), r.x + 2, _s
				.scaleToUC(value));
	}

	/**
	 *
	 */
	public Dimension getPreferredSize()
	{
		return new Dimension(50, 100);
	}

	/**
	 *
	 */
	public Dimension getMinimumSize()
	{
		return getPreferredSize();
	}
}
