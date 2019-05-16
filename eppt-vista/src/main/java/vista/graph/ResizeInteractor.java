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

import java.awt.Component;
import java.awt.Rectangle;
import java.awt.event.ComponentEvent;

/**
 * Handles component resize events and calls Graph.setBounds to redo layout of
 * graph
 *
 * @author Nicky Sandhu (DWR).
 * @version $Id: ResizeInteractor.java,v 1.1 2003/10/02 20:49:07 redwood Exp $
 * @see Graph
 */
public class ResizeInteractor extends ElementInteractor
{
	/**
	 * for debuggin' purposes.
	 */
	public static final boolean DEBUG = false;
	/**
	 * A reference to the graph canvas
	 */
	protected GraphicElement _ge;

	/**
	 * Constructor takes the reference to the graph object
	 */
	public ResizeInteractor(ElementContext eC)
	{
		_ge = eC.getGraphicElement();
	}

	/**
	 *
	 */
	public void releaseResources()
	{
		_ge = null;
	}

	/**
	 * Resets bounds for graph whenever the canvas is resized.
	 */
	public void componentResized(ComponentEvent e)
	{
		Component comp = e.getComponent();
		ElementContext eC = null;
		if(comp instanceof ElementContext)
		{
			eC = (ElementContext) comp;
			if(eC.getGraphicElement() != _ge)
			{
				return;
			}
		}

		Rectangle r = (eC != null ? eC.getBounds() : null);
		eC.redoNextPaint();
		eC.paint(eC.getGraphics());
	}
}
