/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */
package vista.graph;

import java.awt.Color;
import java.awt.Polygon;
import java.awt.Rectangle;

/**
 * 
 * 
 * @author Nicky Sandhu
 * @version $Id: SymbolFactory.java,v 1.1 2003/10/02 20:49:09 redwood Exp $
 */
public class SymbolFactory {
	/**
   *
   */
	public static Symbol createCircle(boolean fill, Color color, int size) {
		SymbolAttr sa = new SymbolAttr();
		sa._isFilled = fill;
		sa._foregroundColor = color;
		Symbol s = new CircleSymbol(sa);
		s.setBounds(new Rectangle(0, 0, size, size));
		return s;
	}

	/**
	 * polygon based drawing
	 */
	public static Symbol createTriangle(boolean fill, Color color, int size) {
		SymbolAttr sa = new SymbolAttr();
		Symbol s = new Symbol(sa);
		sa._isFilled = fill;
		sa._foregroundColor = color;
		sa.setSymbol(createTriangleShape(size));
		return s;
	}

	/**
	 * polygon based drawing
	 */
	public static Symbol createUprightTriangle(boolean fill, Color color,
			int size) {
		SymbolAttr sa = new SymbolAttr();
		Symbol s = new Symbol(sa);
		sa._isFilled = fill;
		sa._foregroundColor = color;
		sa.setSymbol(createUprightTriangleShape(size));
		return s;
	}

	/**
	 * square symbol
	 */
	public static Symbol createSquare(boolean fill, Color color, int size) {
		SymbolAttr sa = new SymbolAttr();
		Symbol s = new Symbol(sa);
		sa._isFilled = fill;
		sa._foregroundColor = color;
		sa.setSymbol(createSquareShape(size));
		return s;
	}

	/**
	 * cross hair symbol
	 */
	public static Symbol createCross(boolean fill, Color color, int size) {
		SymbolAttr sa = new SymbolAttr();
		Symbol s = new Symbol(sa);
		sa._isFilled = fill;
		sa._foregroundColor = color;
		sa.setSymbol(createCrossShape(size));
		return s;
	}

	/**
	 * cross hair symbol
	 */
	public static Symbol createButterfly(boolean fill, Color color, int size) {
		SymbolAttr sa = new SymbolAttr();
		Symbol s = new Symbol(sa);
		sa._isFilled = fill;
		sa._foregroundColor = color;
		sa.setSymbol(createButterflyShape(size));
		return s;
	}

	/**
	 * cross hair symbol
	 */
	public static Symbol createHourGlass(boolean fill, Color color, int size) {
		SymbolAttr sa = new SymbolAttr();
		Symbol s = new Symbol(sa);
		sa._isFilled = fill;
		sa._foregroundColor = color;
		sa.setSymbol(createHourGlassShape(size));
		return s;
	}

	/**
	 * creates triangle shape
	 */
	static Polygon createUprightTriangleShape(int size) {
		return new Polygon(new int[] { -size, size, 0, -size }, new int[] {
				size, size, -size, size }, 4);
	}

	/**
	 * creates triangle shape
	 */
	static Polygon createTriangleShape(int size) {
		return new Polygon(new int[] { -size, size, 0, -size }, new int[] {
				-size, -size, size, -size }, 4);
	}

	/**
	 * creates triangle shape
	 */
	static Polygon createSlashShape(int size) {
		return new Polygon(new int[] { -size, size, 0 }, new int[] { -size,
				size, 0 }, 3);
	}

	/**
	 * creates triangle shape
	 */
	static Polygon createXShape(int size) {
		return new Polygon(new int[] { -size, size, 0, size, -size, 0 },
				new int[] { -size, size, 0, -size, size, 0 }, 6);
	}

	/**
	 * creates butterfly shape
	 */
	static Polygon createButterflyShape(int size) {
		return new Polygon(new int[] { -size, size, size, -size }, new int[] {
				-size, size, -size, size }, 4);
	}

	/**
	 * creates butterfly shape
	 */
	static Polygon createHourGlassShape(int size) {
		return new Polygon(new int[] { -size, size, -size, size }, new int[] {
				-size, -size, size, size }, 4);
	}

	/**
	 * creates triangle shape
	 */
	static Polygon createSquareShape(int size) {
		return new Polygon(new int[] { -size, size, size, -size, -size },
				new int[] { -size, -size, size, size, -size }, 5);
	}

	/**
	 * creates triangle shape
	 */
	static Polygon createCrossShape(int size) {
		return new Polygon(new int[] { -size, size, 0, 0, 0, 0 }, new int[] {
				0, 0, 0, -size, size, 0 }, 6);
	}
}
