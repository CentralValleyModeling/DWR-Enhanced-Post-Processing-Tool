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

import java.awt.event.ComponentEvent;
import java.awt.event.ComponentListener;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.event.MouseMotionListener;

/**
 * This class handles interaction of mouse and mouse motion with Element class
 * and its components.
 * 
 * @author Nicky Sandhu
 * @version $Id: ElementInteractor.java,v 1.1 2003/10/02 20:48:54 redwood Exp $
 */
public class ElementInteractor implements MouseListener, MouseMotionListener,
		ComponentListener, KeyListener {
	/**
	 * for debuggin' purposes
	 */
	public static final boolean DEBUG = false;

	/**
	 * Invoked when the mouse has been clicked on a component.
	 */
	public void mouseClicked(MouseEvent e) {
		if (DEBUG)
			System.out.println("Mouse Clicked at ( " + e.getX() + ", "
					+ e.getY() + " )");
	}

	/**
	 * Invoked when a mouse button has been pressed on a component.
	 */
	public void mousePressed(MouseEvent e) {
		if (DEBUG)
			System.out.println("Mouse Pressed at ( " + e.getX() + ", "
					+ e.getY() + " )");
	}

	/**
	 * Invoked when a mouse button has been released on a component.
	 */
	public void mouseReleased(MouseEvent e) {
		if (DEBUG)
			System.out.println("Mouse Released at ( " + e.getX() + ", "
					+ e.getY() + " )");
	}

	/**
	 * Invoked when the mouse enters a component.
	 */
	public void mouseEntered(MouseEvent e) {
		if (DEBUG)
			System.out.println("Mouse Entered at ( " + e.getX() + ", "
					+ e.getY() + " )");
	}

	/**
	 * Invoked when the mouse exits a component.
	 */
	public void mouseExited(MouseEvent e) {
		if (DEBUG)
			System.out.println("Mouse Exited at ( " + e.getX() + ", "
					+ e.getY() + " )");
	}

	/**
	 * Invoked when a mouse button is pressed on a component and then dragged.
	 * Mouse drag events will continue to be delivered to the component where
	 * the first originated until the mouse button is released (regardless of
	 * whether the mouse position is within the bounds of the component).
	 */
	public void mouseDragged(MouseEvent e) {
		if (DEBUG)
			System.out.println("Mouse Dragged at ( " + e.getX() + ", "
					+ e.getY() + " )");
	}

	/**
	 * Invoked when the mouse button has been moved on a component (with no
	 * buttons no down).
	 */
	public void mouseMoved(MouseEvent e) {
		if (DEBUG)
			System.out.println("Mouse Moved at ( " + e.getX() + ", " + e.getY()
					+ " )");
	}

	/**
	 * Invoked when component has been resized.
	 */
	public void componentResized(ComponentEvent e) {
		if (DEBUG)
			System.out.println("Component Event: " + e.paramString());
	}

	/**
	 * Invoked when component has been moved.
	 */
	public void componentMoved(ComponentEvent e) {
		if (DEBUG)
			System.out.println("Component Event: " + e.toString());
	}

	/**
	 * Invoked when component has been shown.
	 */
	public void componentShown(ComponentEvent e) {
		if (DEBUG)
			System.out.println("Component Event: " + e.toString());
	}

	/**
	 * Invoked when component has been hidden.
	 */
	public void componentHidden(ComponentEvent e) {
		if (DEBUG)
			System.out.println("Component Event: " + e.toString());
	}

	/**
	 * Invoked when a key has been typed. This event occurs when a key press is
	 * followed by a key release.
	 */
	public void keyTyped(KeyEvent e) {
		if (DEBUG)
			System.out.println("Key Event: " + e.toString());
	}

	/**
	 * Invoked when a key has been pressed.
	 */
	public void keyPressed(KeyEvent e) {
		if (DEBUG)
			System.out.println("Key Event: " + e.toString());
	}

	/**
	 * Invoked when a key has been released.
	 */
	public void keyReleased(KeyEvent e) {
		if (DEBUG)
			System.out.println("Key Event: " + e.toString());
	}
}
