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

import java.awt.Dimension;
import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.Graphics;
import java.awt.Insets;
import java.awt.Rectangle;
import java.util.Properties;
import java.util.StringTokenizer;

/**
 * Draws a line of text within given bounds. This element uses the drawString
 * function of the java.awt.Graphics class to get its job done.
 * <p>
 * <p>
 * It also has the ability to choose a font size such that the given text will
 * fit within a given bounds. Although this works it is relatively inefficient
 * as it does a binary search for the closest fit.
 * <p>
 * <p>
 * TextLineAttr object handles the attributes of the text such as font, color,
 * size etcetra.
 *
 * @author Nicky Sandhu
 * @version $Id: TextLine.java,v 1.1 2003/10/02 20:49:10 redwood Exp $
 */
public class TextLine extends GraphicElement implements FontResizable
{
	/**
	 * debuggin'
	 */
	public static final boolean DEBUG = false;
	/**
	 * The text to be drawn
	 */
	private String _text;

	/**
	 * Constructor
	 */
	public TextLine(String text)
	{
		this(new TextLineAttr(), text);
	}

	/**
	 * Constructor
	 */
	public TextLine(TextLineAttr attributes, String text)
	{
		super(attributes);
		_text = "";
		setText(text);
		_doRotate = true;
	}

	/**
	 * @deprecated As of VISTA - June 99, replaced by
	 * <code>String getText()</code>. sets the text to be plotted
	 */
	public String getTextString()
	{
		return getText();
	}

	/**
	 * @deprecated As of VISTA - June 99, replaced by
	 * <code>setText(String)</code>. sets the text to be plotted
	 */
	public void setTextString(String text)
	{
		setText(text);
	}

	/**
	 * gets the text to be plotted
	 */
	public String getText()
	{
		return _text;
	}

	/*
	 * sets the text to be plotted
	 */
	public void setText(String text)
	{
		if(text == null)
		{
			return;
		}
		_text = text;
	}

	/**
	 * returns height of object for given graphics and font
	 */
	public int getHeight(Graphics g, Font font)
	{
		int height = 0;
		if(g == null)
		{
			return 1;
		}
		Font previousFont = g.getFont();
		g.setFont(font);
		FontMetrics fm = g.getFontMetrics();
		if(((TextLineAttr) getAttributes())._textArrangement == TextLineAttr.SIDE_BY_SIDE)
		{
			height = fm.getHeight();
		}
		else
		{
			height = (_text.length() - 1) * (fm.getHeight() - fm.getLeading())
					+ fm.getHeight() - fm.getDescent();
		}
		g.setFont(previousFont);
		return height;
	}

	/**
	 * Calculates a font which has the given height in the graphics context.
	 * This functions is expensive as it has to repeatedly try different font
	 * point sizes to get the right match. The setFont operation on the graphics
	 * context is expensive!
	 *
	 * @return The font with the specified height.
	 */
	public Font getFontByHeight(Graphics g, int height)
	{

		int fontSizeGuess = 0;
		int fontGuessIncrement = 20;
		fontGuessIncrement = Math.max(fontSizeGuess / 2, fontGuessIncrement);
		TextLineAttr attr = (TextLineAttr) getAttributes();
		if(attr._textArrangement == TextLineAttr.SIDE_BY_SIDE)
		{
			fontSizeGuess = height;
		}
		else if(attr._textArrangement == TextLineAttr.TOP_ON_TOP)
		{
			fontSizeGuess = height / _text.length();
		}
		Font trialFont = null;
		int i = 0;
		if(fontSizeGuess > 0)
		{
			trialFont = new Font(attr._font.getName(), attr._font.getStyle(),
					fontSizeGuess);
			while(fontGuessIncrement > 1 && fontSizeGuess > 0)
			{
				while(getHeight(g, trialFont) < height
						&& fontGuessIncrement > 1 && fontSizeGuess > 0)
				{
					fontSizeGuess += fontGuessIncrement;
					trialFont = new Font(attr._font.getName(), attr._font
							.getStyle(), fontSizeGuess);
					i++;
				}
				fontGuessIncrement /= 2;
				while(getHeight(g, trialFont) > height
						&& fontGuessIncrement > 1 && fontSizeGuess > 0)
				{
					fontSizeGuess -= fontGuessIncrement;
					trialFont = new Font(attr._font.getName(), attr._font
							.getStyle(), fontSizeGuess);
					i++;
				}
				fontGuessIncrement /= 2;
			}
		}
		if(DEBUG)
		{
			System.out.println("Number of trials " + i);
		}
		if(fontSizeGuess < 1 || trialFont == null)
		{
			trialFont = new Font(attr._font.getName(), attr._font.getStyle(), 1);
		}
		return trialFont;
	}

	/**
	 * Calculates the width of the text in pixels given the font and the
	 * graphics context.
	 */
	public int getWidth(Graphics g, Font font)
	{
		int width = 0;
		if(g == null)
		{
			return 1;
		}
		Font previousFont = g.getFont();
		g.setFont(font);
		FontMetrics fm = g.getFontMetrics();
		TextLineAttr attr = (TextLineAttr) getAttributes();
		if(attr._textArrangement == TextLineAttr.SIDE_BY_SIDE)
		{
			width = fm.stringWidth(_text);
		}
		else
		{
			// width of widest character
			width = fm.stringWidth("W");
		}
		g.setFont(previousFont);
		return width;
	}

	/**
	 * Calculates a font which has the given width in the graphics context. This
	 * functions is expensive as it has to repeatedly try different font point
	 * sizes to get the right match. The setFont operation on the graphics
	 * context is expensive!
	 *
	 * @return The font with the specified width
	 */
	public Font getFontByWidth(Graphics g, int width)
	{
		int fontSizeGuess = 0;
		int fontGuessIncrement = 20;
		TextLineAttr attr = (TextLineAttr) getAttributes();
		if(attr._textArrangement == TextLineAttr.SIDE_BY_SIDE)
		{
			fontSizeGuess = width / _text.length();
		}
		else if(attr._textArrangement == TextLineAttr.TOP_ON_TOP)
		{
			fontSizeGuess = width;
		}
		Font trialFont = null;
		fontGuessIncrement = Math.max(fontSizeGuess / 2, fontGuessIncrement);
		int i = 0;
		if(fontSizeGuess > 0)
		{
			trialFont = new Font(attr._font.getName(), attr._font.getStyle(),
					fontSizeGuess);
			while(fontGuessIncrement > 1 && fontSizeGuess > 0)
			{
				while(getWidth(g, trialFont) < width && fontGuessIncrement > 1
						&& fontSizeGuess > 0)
				{
					fontSizeGuess += fontGuessIncrement;
					trialFont = new Font(attr._font.getName(), attr._font
							.getStyle(), fontSizeGuess);
					i++;
				}
				fontGuessIncrement /= 2;
				while(getWidth(g, trialFont) > width && fontGuessIncrement > 1
						&& fontSizeGuess > 0)
				{
					fontSizeGuess -= fontGuessIncrement;
					trialFont = new Font(attr._font.getName(), attr._font
							.getStyle(), fontSizeGuess);
					i++;
				}
				fontGuessIncrement /= 2;
			}
		}
		else
		{
			System.out.println(fontSizeGuess + " is less than zero");
		}
		if(DEBUG)
		{
			System.out.println("Number of trials " + i);
		}
		if(fontSizeGuess < 1 || trialFont == null)
		{
			trialFont = new Font(attr._font.getName(), attr._font.getStyle(), 1);
		}

		return trialFont;
	}

	/**
	 * sets font by ratio
	 */
	public void setFontByRatio(double fontResizeRatio)
	{
		TextLineAttr attr = (TextLineAttr) getAttributes();
		if(!attr._resizeOnTheFly)
		{
			return;
		}
		int newFontSize = (int) (attr._originalFontSize * fontResizeRatio);
		if(newFontSize < 0)
		{
			newFontSize = 0;
		}
		if(newFontSize > TextLineAttr._fontTable.length - 1)
		{
			newFontSize = TextLineAttr._fontTable.length - 1;
		}
		if(attr._font.getFamily().equals("Times Roman"))
		{
			attr._font = TextLineAttr._fontTable[newFontSize];
		}
		else
		{
			attr._font = new Font(attr._font.getFamily(),
					attr._font.getStyle(), newFontSize);
		}
	}

	/**
	 * sets font size by heuristics or guessing
	 */
	public Font getFontByHeuristics()
	{
		TextLineAttr attr = (TextLineAttr) getAttributes();
		Font font = null;
		int len = TextLineAttr._fontTable.length;
		int i = 0;
		Rectangle drawingArea = getDrawBounds();
		font = TextLineAttr._fontTable[i];
		while((getWidth(getGraphics(), font) < drawingArea.width || getHeight(
				getGraphics(), font) < drawingArea.height)
				&& i < len)
		{
			i++;
			font = TextLineAttr._fontTable[i];
		}
		i--;
		return TextLineAttr._fontTable[i];
	}

	/**
	 * sets font. Previous font already cached and uncached by graphic element.
	 */
	public void preDraw()
	{
		super.preDraw();
		TextLineAttr attr = (TextLineAttr) getAttributes();
		getGraphics().setFont(attr._font);
	}

	/**
	 * implements the actual drawing of text on the graphics context. Currently
	 * the background color filling is off as drawing order of elements in a
	 * container cannot be specified yet.
	 */
	public void Draw()
	{

		TextLineAttr attr = (TextLineAttr) getAttributes();

		FontMetrics fm = getGraphics().getFontMetrics();
		int height = fm.getHeight();
		int leading = fm.getLeading();
		int descent = fm.getDescent();

		int x = 0;
		int y = 0;
		Graphics gr = getGraphics();
		Rectangle drawingArea = getDrawBounds();

		switch(attr._justification)
		{
			case TextLineAttr.CENTER:
				x = drawingArea.x + (drawingArea.width - getWidth(gr, attr._font))
						/ 2;
				y = drawingArea.y + height - descent
						+ (drawingArea.height - getHeight(gr, attr._font)) / 2;
				break;
			case TextLineAttr.LEFT:
				x = drawingArea.x;
				y = drawingArea.y + height - descent
						+ (drawingArea.height - getHeight(gr, attr._font)) / 2;
				break;
			case TextLineAttr.RIGHT:
				x = drawingArea.x + drawingArea.width - getWidth(gr, attr._font);
				y = drawingArea.y + height - descent
						+ (drawingArea.height - getHeight(gr, attr._font)) / 2;
				break;
		}

		if(attr._textArrangement == TextLineAttr.SIDE_BY_SIDE)
		{
			gr.drawString(_text, x, y);
		}
		else
		{
			for(int i = 0; i < _text.length(); i++)
			{
				gr.drawString(_text.substring(i, i + 1), x, y);
				y += height - leading;
			}
		}
	}

	/**
	 * calculates a font size such that the given text would fit into the given
	 * bounds. This is expensive and thus there is an option of resizing on the
	 * fly to turn on/off this capability.
	 */
	public Font getFontByDimension(Graphics g)
	{
		Font font = null;
		TextLineAttr attr = (TextLineAttr) getAttributes();
		int height = getHeight(g, attr._font);
		Rectangle drawingArea = getDrawBounds();
		Font byHeight = null;
		if((height > drawingArea.height)
				|| (height < 0.9 * drawingArea.height))
		{
			byHeight = getFontByHeight(g, drawingArea.height);
		}

		int width = getWidth(g, font);
		Font byWidth = null;
		if((width > drawingArea.width) || (width < 0.90 * drawingArea.width))
		{
			byWidth = getFontByWidth(g, drawingArea.width);
		}

		int byHeightSize = 0;
		if(byHeight != null)
		{
			byHeightSize = byHeight.getSize();
		}
		int byWidthSize = 0;
		if(byWidth != null)
		{
			byWidthSize = byWidth.getSize();
		}

		if(byHeightSize < byWidthSize)
		{
			if(byHeight != null)
			{
				font = byHeight;
			}
			else
			{
				if(DEBUG)
				{
					System.out.println("Null font returned: byHeight");
				}
			}
		}
		else
		{
			if(byWidth != null)
			{
				font = byWidth;
			}
			else
			{
				if(DEBUG)
				{
					System.out.println("Null font returned: byWidth");
				}
			}
		}
		return font;
	}

	/**
	 * returns the preferred size based on current font size.
	 */
	public Dimension getPreferredSize()
	{
		Graphics gr = getGraphics();
		TextLineAttr attr = (TextLineAttr) getAttributes();
		Insets insets = getInsets();
		if(gr != null)
		{
			int minWidth = getWidth(gr, attr._font) + insets.left
					+ insets.right;
			int minHeight = getHeight(gr, attr._font) + insets.top
					+ insets.bottom;

			if(DEBUG)
			{
				System.out.println(" TextLine preferred size is "
						+ new Dimension(minWidth, minHeight));
			}
			if(attr._orientation == GEAttr.HORIZONTAL)
			{
				return new Dimension(minWidth, minHeight);
			}
			else
			{
				return new Dimension(minHeight, minWidth);
			}
		}
		else
		{
			return new Dimension(10, 10);
		}
	}

	/**
	 * returns the minimum size based on current font
	 */
	public Dimension getMinimumSize()
	{
		Graphics gr = getGraphics();
		if(gr != null)
		{
			Font smallFont = getMinimumFont();
			Insets insets = getInsets();
			int minWidth = getWidth(gr, smallFont) + insets.left + insets.right;
			int minHeight = getHeight(gr, smallFont) + insets.top
					+ insets.bottom;
			return new Dimension(minWidth, minHeight);
		}
		else
		{
			return new Dimension(10, 10);
		}
	}

	/**
	 * smallest font allowable?
	 */
	public Font getMinimumFont()
	{
		return new Font("Times Roman", Font.PLAIN, 3);
	}

	/**
	 * Returns its properties in a Properties object. These are saved on disk.
	 *
	 * @param prefixTag A tag to assign the context for these properties e.g. if these
	 *                  properties belong to Axis class then prefixTag will be "Axis."
	 */
	public void toProperties(Properties p, String prefixTag)
	{
		String localTag = getPrefixTag(prefixTag);
		if(p == null)
		{
			return;
		}
		TextLineAttr attr = (TextLineAttr) getAttributes();

		if(attr._justification == TextLineAttr.LEFT)
		{
			p.put(localTag + "justification", "TextLineAttr.LEFT");
		}
		else if(attr._justification == TextLineAttr.RIGHT)
		{
			p.put(localTag + "justification", "TextLineAttr.RIGHT");
		}
		else if(attr._justification == TextLineAttr.CENTER)
		{
			p.put(localTag + "justification", "TextLineAttr.CENTER");
		}

		if(attr._textArrangement == TextLineAttr.TOP_ON_TOP)
		{
			p.put(localTag + "textArrangement", "TextLineAttr.TOP_ON_TOP");
		}
		else if(attr._textArrangement == TextLineAttr.SIDE_BY_SIDE)
		{
			p.put(localTag + "textArrangement", "TextLineAttr.SIDE_BY_SIDE");
		}

		p.put(localTag + "font", attr._font.toString());
		p.put(localTag + "originalFontSize",
				new Integer(attr._originalFontSize).toString());

		p.put(localTag + "resizeOnTheFly", new Boolean(attr._resizeOnTheFly)
				.toString());
		p.put(localTag + "resizeProportionally", new Boolean(
				attr._resizeProportionally).toString());

		super.toProperties(p, localTag);
	}

	/**
	 * prefix tag for saving properties
	 */
	public String getPrefixTag(String prefixTag)
	{
		return prefixTag + getName() + "TextLine.";
	}

	/**
	 * initializes attributes and state from Properties object.
	 */
	public void fromProperties(Properties p, String prefixTag)
	{
		String localTag = getPrefixTag(prefixTag);
		TextLineAttr attr = (TextLineAttr) getAttributes();
		String property;

		property = p.getProperty(localTag + "justification");
		if(property != null)
		{
			attr._justification = parseJustificationProperty(property);
		}

		/*
		 * property = p.getProperty( localTag + "textArrangement" ); if
		 * (property != null) { attr._textArrangement =
		 * parseTextArrangementProperty( property ); }
		 */
		property = p.getProperty(localTag + "font");
		if(property != null)
		{
			attr._font = parseFontProperty(property);
		}

		property = p.getProperty(localTag + "originalFontSize");
		if(property != null)
		{
			attr._originalFontSize = new Integer(property).intValue();
		}

		property = p.getProperty(localTag + "resizeOnTheFly");
		if(property != null)
		{
			attr._resizeOnTheFly = new Boolean(property).booleanValue();
		}

		property = p.getProperty(localTag + "resizeProportionally");
		if(property != null)
		{
			attr._resizeProportionally = new Boolean(property).booleanValue();
		}

		super.fromProperties(p, localTag);
	}

	/**
	 * returns the font associated with this element
	 */
	public Font getFont()
	{
		return ((TextLineAttr) getAttributes())._font;
	}

	/**
	 * sets the font for this element
	 */
	public void setFont(Font f)
	{
		if(f != null)
		{
			((TextLineAttr) getAttributes())._font = f;
		}
	}

	/**
	 *
	 */
	public void setFontSize(int size)
	{
		setResizeOnTheFly(false);
		Font font = getFont();
		setFont(new Font(font.getName(), font.getStyle(), size));
	}

	/**
	 * gets the justification of the text
	 */
	public int getJustification()
	{
		return ((TextLineAttr) getAttributes())._justification;
	}

	/**
	 * sets the justification of the text to left, right or center
	 */
	public void setJustification(int j)
	{
		if((j == TextLineAttr.LEFT) || (j == TextLineAttr.RIGHT)
				|| (j == TextLineAttr.CENTER))
		{
			((TextLineAttr) getAttributes())._justification = j;
		}
	}

	/**
	 * @returns an integer for top on top character arrangement vs a side by
	 * side arrangement
	 */
	public int getTextArrangement()
	{
		return ((TextLineAttr) getAttributes())._textArrangement;
	}

	/**
	 * gets the text arrangement
	 */
	public void setTextArrangement(int ta)
	{
		if(ta == TextLineAttr.TOP_ON_TOP)
		{
			((TextLineAttr) getAttributes())._textArrangement = TextLineAttr.TOP_ON_TOP;
		}
		else
		{
			((TextLineAttr) getAttributes())._textArrangement = TextLineAttr.SIDE_BY_SIDE;
		}
	}

	/**
	 * gets ResizeOnTheFly
	 */
	public boolean getResizeOnTheFly()
	{
		return ((TextLineAttr) getAttributes())._resizeOnTheFly;
	}

	/**
	 * sets ResizeOnTheFly
	 */
	public void setResizeOnTheFly(boolean resizeOnTheFly)
	{
		((TextLineAttr) getAttributes())._resizeOnTheFly = resizeOnTheFly;
	}

	/**
	 * gets ResizeHeuristically
	 */
	public boolean getResizeHeuristically()
	{
		return ((TextLineAttr) getAttributes())._resizeHeuristically;
	}

	/**
	 * sets ResizeHeuristically
	 */
	public void setResizeHeuristically(boolean resizeHeuristically)
	{
		((TextLineAttr) getAttributes())._resizeHeuristically = resizeHeuristically;
	}

	/**
	 * gets ResizeProportionally
	 */
	public boolean getResizeProportionally()
	{
		return ((TextLineAttr) getAttributes())._resizeProportionally;
	}

	/**
	 * sets ResizeProportionally
	 */
	public void setResizeProportionally(boolean resizeProportionally)
	{
		((TextLineAttr) getAttributes())._resizeProportionally = resizeProportionally;
	}

	/**
	 * gets OriginalFontSize which is the font size used when resizing this font
	 * proportionally
	 */
	public int getOriginalFontSize()
	{
		return ((TextLineAttr) getAttributes())._originalFontSize;
	}

	/**
	 * sets OriginalFontSize which is used as the base font size when resizing
	 * this font proportionally
	 */
	public void setOriginalFontSize(int originalFontSize)
	{
		if(originalFontSize < 1)
		{
			return;
		}
		((TextLineAttr) getAttributes())._originalFontSize = originalFontSize;
	}

	/**
	 * justification property.
	 */
	public final int parseJustificationProperty(String property)
	{
		int o = 0;
		if(property.equals("TextLineAttr.LEFT"))
		{
			o = TextLineAttr.LEFT;
		}
		else if(property.equals("TextLineAttr.CENTER"))
		{
			o = TextLineAttr.CENTER;
		}
		else if(property.equals("TextLineAttr.RIGHT"))
		{
			o = TextLineAttr.RIGHT;
		}
		return o;
	}

	/**
	 *
	 */
	public final int parseTextArrangementProperty(String property)
	{
		int o = 0;
		if(property.equals("TextLineAttr.TOP_ON_TOP"))
		{
			o = TextLineAttr.TOP_ON_TOP;
		}
		else if(property.equals("TextLineAttr.SIDE_BY_SIDE"))
		{
			o = TextLineAttr.SIDE_BY_SIDE;
		}
		return o;
	}

	/**
	 *
	 */
	public final Font parseFontProperty(String property)
	{

		StringTokenizer chopper = new StringTokenizer(property, ",");
		chopper.nextToken();

		String name = chopper.nextToken();
		name = name.substring(name.indexOf("=") + 1);

		String styleStr = chopper.nextToken();
		styleStr = styleStr.substring(styleStr.indexOf("=") + 1);
		int style = Font.PLAIN;
		if(styleStr.indexOf("bold") >= 0)
		{
			style = Font.BOLD;
		}
		if(styleStr.indexOf("italic") >= 0)
		{
			style = style & Font.ITALIC;
		}

		String sizeStr = chopper.nextToken();
		sizeStr = sizeStr.substring(sizeStr.indexOf("=") + 1, sizeStr
				.indexOf("]"));
		int size = new Integer(sizeStr).intValue();

		return new Font(name, style, size);
	}

	/**
	 * create dialog panel for this element.
	 */
	public GEDialogPanel createDialogPanel()
	{
		return new TextDialogPanel(this);
	}
}
