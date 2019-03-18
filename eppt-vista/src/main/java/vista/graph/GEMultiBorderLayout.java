/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */
package vista.graph;

import java.awt.Dimension;
import java.util.ArrayList;

/**
 * Lays out the elements in North, South , East , West and Center positions. The
 * elements are sized by their preferred sizes for the NSEW positions with the
 * remaining space going to the Center position. This is useful for laying out
 * the axis elements in a Plot
 * 
 * @see Plot
 * @author Nicky Sandhu (DWR).
 * @version $Id: GEMultiBorderLayout.java,v 1.1 2003/10/02 20:48:58 redwood Exp
 *          $
 */
public class GEMultiBorderLayout extends GEBorderLayout {
	/**
	 * for debuggin'
	 */
	public static final boolean DEBUG = false;

	/**
	 * Constructor
	 */
	public GEMultiBorderLayout(LayoutMediator mediator) {
		setMediator(mediator);
	}

	/**
   *
   */
	public void setMediator(LayoutMediator mediator) {
		_mediator = mediator;
		if (_mediator != null)
			_mediator.addLayoutManager(this);
	}

	/**
	 * returns the maximum of preferred required dimensions for all graphical
	 * elements in the array times the number of elements in that array.
	 */
	protected Dimension getPreferredSize(ArrayList array) {
		Dimension size = null;
		if (_mediator == null)
			return super.getPreferredSize(array);
		if (array == north)
			size = _mediator.getPreferredSize(NORTH);
		if (array == south)
			size = _mediator.getPreferredSize(SOUTH);
		if (array == east)
			size = _mediator.getPreferredSize(EAST);
		if (array == west)
			size = _mediator.getPreferredSize(WEST);
		if (array == center)
			size = _mediator.getPreferredSize(CENTER);

		return size;
	}

	/**
	 * returns the maximum of minimum required dimensions for all graphical
	 * elements in the array.
	 */
	private Dimension getMinimumSize(ArrayList array) {
		return getPreferredSize(array);
	}

	/**
	 * returns the preferred dimensions to the mediator
	 * 
	 * @see GEBorderLayoutMediator
	 */
	Dimension getPreferredDimensions(String position) {
		Dimension size = new Dimension(0, 0);
		if (position.equals(NORTH)) {
			size = super.getPreferredSize(north);
		}
		if (position.equals(SOUTH)) {
			size = super.getPreferredSize(south);
		}
		if (position.equals(EAST)) {
			size = super.getPreferredSize(east);
		}
		if (position.equals(WEST)) {
			size = super.getPreferredSize(west);
		}
		if (position.equals(CENTER)) {
			size = super.getPreferredSize(center);
		}
		return size;
	}

	/**
   *
   */
	private LayoutMediator _mediator = null;
}
