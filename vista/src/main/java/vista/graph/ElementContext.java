/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */
package vista.graph;

/**
 * This interface is implemented by any Canvas container that contains a
 * GraphicElement object
 */
public interface ElementContext
{
	/**
	 * gets the reference to the graph object contained herein.
	 */
	GraphicElement getGraphicElement();

	/**
	 * checks to see if this container is using double buffering
	 */
	boolean isDoubleBuffered();

	/**
	 * sets double buffering for this container
	 */
	void setDoubleBuffered(boolean doubleBuffer);

	/**
	 * Redoes the next painting completely. This usually means both layout and
	 * drawing of the graph is done.
	 */
	void redoNextPaint();

	/**
	 * Clear the previous graph and paint again
	 */
	void repaint();

	/**
	 * Method for drawing and/or laying out graph
	 */
	void paint(java.awt.Graphics g);

	/**
	 * Returns the object for drawing primitives
	 */
	java.awt.Graphics getGraphics();

	/**
	 * returns the image of the graph
	 */
	java.awt.Image getGraphicElementImage();

	/**
	 * creates image of given dimensions
	 */
	java.awt.Image createImage(int width, int height);

	/**
	 * Gets the bounds for this container
	 */
	java.awt.Rectangle getBounds();
}
