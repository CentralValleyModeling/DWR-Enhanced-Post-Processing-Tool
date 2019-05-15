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

import java.awt.Color;
import java.awt.Insets;
import java.util.Properties;

/**
 * A legend item for each Curve on the Plot consisting of a LineElement and
 * associated TextLine. The GELineLayout is used for managing layout
 * 
 * @author Nicky Sandhu
 * @version $Id: LegendItem.java,v 1.1 2003/10/02 20:49:04 redwood Exp $
 */
public class LegendItem extends GEContainer implements ScaledElement {
	/**
	 * constructor
	 */
	public LegendItem(Curve curve) {
		this(new LegendItemAttr(), curve);
	}

	/**
	 * constructor
	 */
	public LegendItem(LegendItemAttr attributes, Curve curve) {

		super(attributes);
		GEBorderLayout layout = new GEBorderLayout();
		layout.setScaleComponents(true);
		setLayout(layout);
		//
		setCurve(curve);
		//
		if (curve != null)
			setLegendText(curve.getModel().getLegendText());
		else
			setLegendText("");
	}

	/**
   *
   */
	public void setLegendText(String txt) {
		if (txt == null)
			return;
		if (_textLine == null) {
			TextLineAttr attr = (TextLineAttr) getAttributes();
			attr._justification = TextLineAttr.LEFT;
			attr._foregroundColor = Color.black;
			_textLine = new TextLine(attr, txt);
			_textLine.setInsets(new Insets(2, 10, 2, 10));
			add(GEBorderLayout.CENTER, _textLine);
		} else {
			_textLine.setText(txt);
		}
	}

	/**
   *
   */
	public String getLegendText() {
		return _textLine.getText();
	}

	/**
   *
   */
	public Curve getCurve() {
		return _curve;
	}

	/**
   *
   */
	public void setCurve(Curve curve) {
		if (curve == null)
			return;
		if (_cll != null)
			remove(_cll);
		_curve = curve;
		add(GEBorderLayout.WEST, _cll = _curve.getLegendLine());
	}

	/**
   *
   */
	public void setLegendName(String name) {
		_textLine.setText(name);
	}

	/**
   *
   */
	public String getLegendName() {
		return _textLine.getText();
	}

	/**
	 * Returns its properties in a Properties object. These are saved on disk.
	 * 
	 * @param prefixTag
	 *            A tag to assign the context for these properties e.g. if these
	 *            properties belong to Axis class then prefixTag will be "Axis."
	 */
	public void toProperties(Properties p, String prefixTag) {
		super.toProperties(p, getPrefixTag(prefixTag));
	}

	/**
	 * initializes attributes and state from Properties object.
	 */
	public void fromProperties(Properties p, String prefixTag) {
		super.fromProperties(p, getPrefixTag(prefixTag));
	}

	/**
   *
   */
	public String getPrefixTag(String prefixTag) {
		return prefixTag + getCurve().getLocalName() + "LegendItem.";
	}

	/**
	 * The text associated with the line element
	 */
	private TextLine _textLine;
	/**
   *
   */
	private Curve _curve;
	/**
   *
   */
	private CurveLegendLine _cll;
}
