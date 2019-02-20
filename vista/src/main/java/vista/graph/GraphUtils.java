/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */
package vista.graph;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.Graphics;
import java.util.Vector;

/**
 * A set of static methods for utility functions
 *
 * @author Nicky Sandhu
 * @version $Id: GraphUtils.java,v 1.1 2003/10/02 20:49:00 redwood Exp $
 */
public class GraphUtils
{
	private static final boolean DEBUG = false;
	static Color[] _colorTable = {Color.red, Color.green, Color.blue,
			Color.pink, Color.yellow, Color.cyan, Color.orange, Color.magenta,
			Color.gray, new Color(0, 206, 209), // Dark Turquoise
			new Color(85, 107, 47), // Dark Olive Green
			new Color(176, 48, 96), // maroon
			new Color(95, 158, 160), // Cadet Blue
			new Color(218, 112, 214), // orchid
			new Color(160, 32, 240) // purple
	};
	static Symbol[] _symbolTable = {
			SymbolFactory.createCircle(false, Color.black, 2),
			SymbolFactory.createTriangle(false, Color.black, 2),
			SymbolFactory.createSquare(false, Color.black, 2),
			SymbolFactory.createCross(false, Color.black, 2),
			SymbolFactory.createButterfly(false, Color.black, 2),
			SymbolFactory.createHourGlass(false, Color.black, 2),
			SymbolFactory.createCircle(true, Color.black, 2),
			SymbolFactory.createTriangle(true, Color.black, 2),
			SymbolFactory.createSquare(true, Color.black, 2),
			SymbolFactory.createCross(true, Color.black, 2),
			SymbolFactory.createButterfly(true, Color.black, 2),
			SymbolFactory.createHourGlass(true, Color.black, 2),
			SymbolFactory.createCircle(false, Color.black, 4),
			SymbolFactory.createTriangle(false, Color.black, 4),
			SymbolFactory.createSquare(false, Color.black, 4),
			SymbolFactory.createCross(false, Color.black, 4),
			SymbolFactory.createButterfly(false, Color.black, 4),
			SymbolFactory.createHourGlass(false, Color.black, 4)};
	static ColorRGB _colorDefinitions;
	private static int[] xtl = new int[4], ytl = new int[4];

	/**
	 * parses string to get orientation
	 *
	 * @return the orientation as GEAttr.VERTICAL | .HORIZONTAL
	 */
	public final static int parseOrientationProperty(String property)
	{
		int o = 0;
		if(property.equals("GEAttr.VERTICAL"))
		{
			o = GEAttr.VERTICAL;
		}
		else if(property.equals("GEAttr.HORIZONTAL"))
		{
			o = GEAttr.HORIZONTAL;
		}
		return o;
	}

	/**
	 * parses string to get color. The string either contains the Color default
	 * toString representation or the name of the color as defined in the
	 * rgb.properties file.
	 */
	public final static Color parseColorProperty(String colorProperty)
	{
		java.util.StringTokenizer chopper;
		String rgbString = "";
		if(colorProperty.indexOf("[") == -1)
		{
			rgbString = _colorDefinitions.getRGBString(colorProperty);
			// if (DEBUG) _colorDefinitions.list(System.out);
		}
		else
		{
			chopper = new java.util.StringTokenizer(colorProperty, "[");
			chopper.nextToken();
			rgbString = chopper.nextToken();
			rgbString = rgbString.substring(0, rgbString.indexOf("]"));
		}
		if(rgbString == null)
		{
			return new Color(0, 0, 0);
		}
		if(DEBUG)
		{
			System.out.println("RGB : " + rgbString);
		}

		chopper = new java.util.StringTokenizer(rgbString, ",");
		String colorComponent = chopper.nextToken().trim();
		if(DEBUG)
		{
			System.out.println(colorComponent);
		}
		int red = new Integer(colorComponent.trim().substring(
				colorComponent.indexOf("=") + 1)).intValue();
		colorComponent = chopper.nextToken().trim();
		if(DEBUG)
		{
			System.out.println(colorComponent);
		}
		int green = new Integer(colorComponent.substring(colorComponent
				.indexOf("=") + 1)).intValue();
		colorComponent = chopper.nextToken().trim();
		if(DEBUG)
		{
			System.out.println(colorComponent);
		}
		int blue = new Integer(colorComponent.substring(colorComponent
				.indexOf("=") + 1)).intValue();
		if(DEBUG)
		{
			System.out.println("r = " + red + " g = " + green + " b = " + blue);
		}

		return new Color(red, green, blue);
	}

	/**
	 * parses string to get dimension from the default toString() of Dimension
	 */
	public final static Dimension parseDimensionProperty(
			String dimensionProperty)
	{
		java.util.StringTokenizer chopper;
		String dstr = null;
		if(dimensionProperty.indexOf("[") == -1)
		{
		}
		else
		{
			chopper = new java.util.StringTokenizer(dimensionProperty, "[");
			chopper.nextToken();
			dstr = chopper.nextToken();
			dstr = dstr.substring(0, dstr.indexOf("]"));
		}
		if(dstr == null)
		{
			return new Dimension(100, 100);
		}
		chopper = new java.util.StringTokenizer(dstr, ",");
		String dimensionComponent = chopper.nextToken().trim();
		int width = new Integer(dimensionComponent.trim().substring(
				dimensionComponent.indexOf("=") + 1)).intValue();
		dimensionComponent = chopper.nextToken().trim();
		int height = new Integer(dimensionComponent
				.substring(dimensionComponent.indexOf("=") + 1)).intValue();
		if(DEBUG)
		{
			System.out.println("w = " + width + " h = " + height);
		}
		return new Dimension(width, height);
	}

	/**
	 * returns all elements of the class cl in the given container This does a
	 * deep search into the heirarchy of the container
	 */
	public static GraphicElement[] getElements(GEContainer gec, Class cl)
	{
		Vector array = new Vector();
		int count = gec.getElementCount();
		for(int i = 0; i < count; i++)
		{
			GraphicElement ge = gec.getElement(i);
			if(ge instanceof GEContainer)
			{
				GraphicElement[] ges = getElements((GEContainer) ge, cl);
				if(ges != null)
				{
					for(int k = 0; k < ges.length; k++)
					{
						array.addElement(ges[k]);
					}
				}
			}
			if(cl.isInstance(ge))
			{
				array.addElement(ge);
			}
		}
		int sz = array.size();
		if(sz == 0)
		{
			return null;
		}
		GraphicElement[] gecl = new GraphicElement[sz];
		array.copyInto(gecl);
		return gecl;
	}

	/**
	 * returns true if JVM is jdk2 or more.
	 */
	public static boolean isJDK2()
	{
		return (System.getProperty("java.version").compareTo("1.1z") > 0);
	}

	/**
	 * Simulates drawing of different thickness lines by using filled polygon.
	 *
	 * @param g  Graphics on which to draw
	 * @param x1 The starting x co-ordinate of line
	 * @param y1 The starting y co-ordinate of line
	 * @param x2 The ending x co-ordinate of line
	 * @param y2 The ending y co-ordinate of line
	 * @param t  The thickness of the line in pixels
	 */
	public static final void drawThickLine(Graphics g, int x1, int y1, int x2,
										   int y2, double t)
	{
		double theta;
		if(Math.abs(x2 - x1) > 0.01)
		{
			theta = Math.atan((y2 - y1) / (x2 - x1));
		}
		else
		{
			theta = Math.PI / 2;
		}
		double ct = Math.cos(theta), st = Math.sin(theta);
		// faster as it does not create any new objects
		xtl[0] = (int) (x1 - t / 2 * st);
		ytl[0] = (int) (y1 + t / 2 * ct);
		xtl[1] = (int) (x1 + t / 2 * st);
		ytl[1] = (int) (y1 - t / 2 * ct);
		xtl[2] = (int) (x2 + t / 2 * st);
		ytl[2] = (int) (y2 - t / 2 * ct);
		xtl[3] = (int) (x2 - t / 2 * st);
		ytl[3] = (int) (y2 + t / 2 * ct);
		g.fillPolygon(xtl, ytl, 4);
	}

	/**
	 * loads color names to RGB mapping from file The format is
	 * color_name=r=xxx,g=xxx,b=xxx where 0 <= xxx <= 255 If the properties
	 * could not be loaded default color names and mappings are created.
	 */
	public final void loadColorProperties(String filename)
	{
		_colorDefinitions = new ColorRGB(filename);
	}
}
