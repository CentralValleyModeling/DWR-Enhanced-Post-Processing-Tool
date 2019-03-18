/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */
package vista.graph;

import java.awt.Component;
import java.awt.Rectangle;
import java.awt.event.ComponentEvent;

/**
 * Handles component resize events and calls Graph.setBounds to redo layout of
 * graph
 * 
 * @see Graph
 * @author Nicky Sandhu (DWR).
 * @version $Id: ResizeInteractor.java,v 1.1 2003/10/02 20:49:07 redwood Exp $
 */
public class ResizeInteractor extends ElementInteractor {
	/**
	 * for debuggin' purposes.
	 */
	public static final boolean DEBUG = false;

	/**
	 * Constructor takes the reference to the graph object
	 */
	public ResizeInteractor(ElementContext eC) {
		_ge = eC.getGraphicElement();
	}

	/**
    *
    */
	public void releaseResources() {
		_ge = null;
	}

	/**
	 * Resets bounds for graph whenever the canvas is resized.
	 */
	public void componentResized(ComponentEvent e) {
		Component comp = e.getComponent();
		ElementContext eC = null;
		if (comp instanceof ElementContext) {
			eC = (ElementContext) comp;
			if (eC.getGraphicElement() != _ge)
				return;
		}

		Rectangle r = (eC != null ? eC.getBounds() : null);
		eC.redoNextPaint();
		eC.paint(eC.getGraphics());
	}

	/**
	 * A reference to the graph canvas
	 */
	protected GraphicElement _ge;
}
