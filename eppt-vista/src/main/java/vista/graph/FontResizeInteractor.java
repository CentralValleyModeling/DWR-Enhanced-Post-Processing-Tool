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
 * Resizes font proportionally when graph gets resized
 * 
 * @author Nicky Sandhu (DWR).
 * @version $Id: FontResizeInteractor.java,v 1.1 2003/10/02 20:48:55 redwood Exp
 *          $
 */
public class FontResizeInteractor extends ResizeInteractor {
	/**
	 * for debugin'
	 */
	public static final boolean DEBUG = false;

	/**
	 * Takes the reference to the graph being resized.
	 */
	public FontResizeInteractor(ElementContext eC) {
		super(eC);
	}

	/**
    *
    */
	public void releaseResources() {
		_originalBounds = null;
		super.releaseResources();
	}

	/**
	 * Handles font resizing when component is resized.
	 */
	public void componentResized(ComponentEvent evt) {
		Component comp = evt.getComponent();
		if (comp == null)
			return;
		ElementContext eC = null;
		if (comp instanceof ElementContext) {
			eC = (ElementContext) comp;
			if (eC.getGraphicElement() != _ge)
				return;
		}

		Rectangle r = (eC != null ? eC.getBounds() : null);

		if (r != null) {
			if (DEBUG)
				System.out.println("Resized to " + r);
			if (_originalBounds == null)
				_originalBounds = r;

			double resizeRatio = Math.min((r.width * 1.0)
					/ _originalBounds.width, (r.height * 1.0)
					/ _originalBounds.height);
			if (DEBUG)
				System.out.println("Resize Ratio " + resizeRatio);
			if (_ge instanceof FontResizable && _doResize)
				((FontResizable) _ge).setFontByRatio(resizeRatio);
		}

		super.componentResized(evt);
	}

	/**
    *
    */
	public void setDoResize(boolean b) {
		_doResize = b;
	}

	/**
    *
    */
	public boolean isDoResize() {
		return _doResize;
	}

	/**
	 * The original size from which resizing ratio is calculated.
	 */
	private Rectangle _originalBounds = null;
	private boolean _doResize = true;
}
