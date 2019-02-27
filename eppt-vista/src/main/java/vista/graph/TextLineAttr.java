/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */
package vista.graph;

import java.awt.Font;

/**
 * Attributes of a TextLine
 * 
 * @see TextLine
 * @author Nicky Sandhu (DWR).
 * @version $Id: TextLineAttr.java,v 1.1 2003/10/02 20:49:10 redwood Exp $
 */
public class TextLineAttr extends GEAttr {
	/**
	 * Arrangement of text within rectangle is as follows
	 * <p>
	 * T<br>
	 * E<br>
	 * X<br>
	 * T<br>
	 */
	public static final int TOP_ON_TOP = 1;
	/**
	 * Arrangement of text within rectangle is as follows
	 * <p>
	 * T E X T<br>
	 */
	public static final int SIDE_BY_SIDE = TOP_ON_TOP + 1;
	/**
	 * Text justification
	 */
	public static final int LEFT = 10;
	/**
	 * Text justification
	 */
	public static final int RIGHT = LEFT + 1;
	/**
	 * Text justification
	 */
	public static final int CENTER = RIGHT + 1;
	/**
	 * font of the text
	 */
	public Font _font = _fontTable[(int) (_fontTable.length * 0.7)];
	/**
	 * justification of text within bounds
	 */
	public int _justification = CENTER;
	/**
	 * arrangement of text
	 */
	public int _textArrangement = SIDE_BY_SIDE;
	/**
	 * resize the text by assigning different fonts such that text fits in the
	 * given bounds
	 */
	public boolean _resizeOnTheFly = false;
	/**
	 * resize the text heuristically.
	 */
	public boolean _resizeHeuristically = false;
	/**
	 * resize the text in proportion to its original dimensions and font.
	 */
	public boolean _resizeProportionally = false;
	/**
	 * original font when first created.
	 */
	public int _originalFontSize = _font.getSize();

	/**
	 * sets Font
	 */
	public void setFont(Font font) {
		_font = font;
	}

	/**
	 * gets Font
	 */
	public Font getFont() {
		return _font;
	}

	/**
	 * sets Justification
	 */
	public void setJustification(int justification) {
		_justification = justification;
	}

	/**
	 * gets Justification
	 */
	public int getJustification() {
		return _justification;
	}

	/**
	 * sets TextArrangement
	 */
	public void setTextArrangement(int textArrangement) {
		_textArrangement = textArrangement;
	}

	/**
	 * gets TextArrangement
	 */
	public int getTextArrangement() {
		return _textArrangement;
	}

	/**
	 * sets ResizeOnTheFly
	 */
	public void setResizeOnTheFly(boolean resizeOnTheFly) {
		_resizeOnTheFly = resizeOnTheFly;
	}

	/**
	 * gets ResizeOnTheFly
	 */
	public boolean getResizeOnTheFly() {
		return _resizeOnTheFly;
	}

	/**
	 * sets ResizeHeuristically
	 */
	public void setResizeHeuristically(boolean resizeHeuristically) {
		_resizeHeuristically = resizeHeuristically;
	}

	/**
	 * gets ResizeHeuristically
	 */
	public boolean getResizeHeuristically() {
		return _resizeHeuristically;
	}

	/**
	 * sets ResizeProportionally
	 */
	public void setResizeProportionally(boolean resizeProportionally) {
		_resizeProportionally = resizeProportionally;
	}

	/**
	 * gets ResizeProportionally
	 */
	public boolean getResizeProportionally() {
		return _resizeProportionally;
	}

	/**
	 * sets OriginalFontSize
	 */
	public void setOriginalFontSize(int originalFontSize) {
		_originalFontSize = originalFontSize;
	}

	/**
	 * gets OriginalFontSize
	 */
	public int getOriginalFontSize() {
		return _originalFontSize;
	}

	/**
	 * copies the fields into the given GEAttr object. Also copies in the
	 * TextLineAttr if the object is of that type.
	 */
	public void copyInto(GEAttr ga) {
		super.copyInto(ga);
		if (ga instanceof TextLineAttr) {
			TextLineAttr tla = (TextLineAttr) ga;
			tla._font = this._font;
			tla._justification = this._justification;
			tla._textArrangement = this._textArrangement;
			tla._resizeOnTheFly = this._resizeOnTheFly;
			tla._resizeHeuristically = this._resizeHeuristically;
			tla._resizeProportionally = this._resizeProportionally;
			tla._originalFontSize = this._originalFontSize;
		}
	}

	/**
	 * font table used to select fonts from.
	 */
	public static Font[] _fontTable = { new Font("Times Roman", Font.PLAIN, 6),
			new Font("Times Roman", Font.PLAIN, 6),
			new Font("Times Roman", Font.PLAIN, 6),
			new Font("Times Roman", Font.PLAIN, 6),
			new Font("Times Roman", Font.PLAIN, 6),
			new Font("Times Roman", Font.PLAIN, 6),
			new Font("Times Roman", Font.PLAIN, 7),
			new Font("Times Roman", Font.PLAIN, 7),
			new Font("Times Roman", Font.PLAIN, 8),
			new Font("Times Roman", Font.PLAIN, 8),
			new Font("Times Roman", Font.PLAIN, 8),
			new Font("Times Roman", Font.PLAIN, 9),
			new Font("Times Roman", Font.PLAIN, 9),
			new Font("Times Roman", Font.PLAIN, 9),
			new Font("Times Roman", Font.PLAIN, 10),
			new Font("Times Roman", Font.PLAIN, 10),
			new Font("Times Roman", Font.PLAIN, 10),
			new Font("Times Roman", Font.PLAIN, 11),
			new Font("Times Roman", Font.PLAIN, 12),
			new Font("Times Roman", Font.PLAIN, 13),
			new Font("Times Roman", Font.PLAIN, 14),
			new Font("Times Roman", Font.PLAIN, 14),
			new Font("Times Roman", Font.PLAIN, 15),
			new Font("Times Roman", Font.PLAIN, 15),
			new Font("Times Roman", Font.PLAIN, 16),
			new Font("Times Roman", Font.PLAIN, 17),
			new Font("Times Roman", Font.PLAIN, 17),
			new Font("Times Roman", Font.PLAIN, 18),
			new Font("Times Roman", Font.PLAIN, 18),
			new Font("Times Roman", Font.PLAIN, 19),
			new Font("Times Roman", Font.PLAIN, 20),
			new Font("Times Roman", Font.PLAIN, 20),
			new Font("Times Roman", Font.PLAIN, 21),
			new Font("Times Roman", Font.PLAIN, 22),
			new Font("Times Roman", Font.PLAIN, 22),
			new Font("Times Roman", Font.PLAIN, 23) };
}