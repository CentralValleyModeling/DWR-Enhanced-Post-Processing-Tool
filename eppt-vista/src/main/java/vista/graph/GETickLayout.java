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
import java.awt.Insets;
import java.awt.Rectangle;

/**
 * Lays out Bounded(s) such that their bounds are centered on the ticks as
 * specified in the given TickData. This layout is superior to GELineLayout in
 * that it is not bound by equidistant placing of components.
 *
 * @author Nicky Sandhu (DWR).
 * @version $Id: GETickLayout.java,v 1.1 2003/10/02 20:48:59 redwood Exp $
 */
public class GETickLayout implements GELayoutManager
{
	/**
	 * for debuggin'
	 */
	public static final boolean DEBUG = false;
	/**
	 * Element centered on boundaries
	 */
	public static final int CENTERED_ON_BOUNDS = 1;
	/**
	 * Elements centered between boundaries.
	 */
	public static final int CENTERED_BETWEEN_BOUNDS = CENTERED_ON_BOUNDS + 1;
	/**
	 * horizontal layout
	 */
	public final static int HORIZONTAL = 10;
	/**
	 * vertical layout
	 */
	public final static int VERTICAL = HORIZONTAL + 1;
	/**
	 * element positioning
	 */
	private int _elementPosition = CENTERED_ON_BOUNDS;
	/**
	 * layout orientation
	 */
	private int _orientation = HORIZONTAL;
	/**
	 * tick information
	 */
	private TickData _tickData;

	/**
	 * constructor
	 */
	public GETickLayout(int orientation, int elementPosition, TickData tickData)
	{
		_orientation = orientation;
		_elementPosition = elementPosition;
		_tickData = tickData;
	}

	/**
	 * Adds the specified element with the specified name to the layout.
	 *
	 * @param name the element name
	 * @param comp the element to be added
	 */
	public void addLayoutElement(Object obj, Bounded comp)
	{

	}

	/**
	 * Removes the specified element from the layout.
	 *
	 * @param comp the element ot be removed
	 */
	public void removeLayoutElement(Bounded comp)
	{
	}

	/**
	 * Calculates the preferred size dimensions for the specified panel given
	 * the Boundeds in the specified parent BoundedComposite.
	 *
	 * @param parent the Bounded to be laid out
	 * @see #minimumLayoutSize
	 */
	public Dimension preferredLayoutSize(BoundedComposite parent)
	{
		int n = parent.getElementCount();
		Insets insets = parent.getInsets();

		int pWidth = 0;
		int pHeight = 0;
		Bounded ge;
		for(int i = 0; i < n; i++)
		{
			ge = parent.getElement(i);
			Dimension d = ge.getPreferredSize();
			pHeight = Math.max(pHeight, d.height);
			pWidth = Math.max(pWidth, d.width);
		}
		if(_orientation == HORIZONTAL)
		{
			int extraWidth = 0;
			if(_elementPosition == CENTERED_ON_BOUNDS)
			{
				extraWidth = pWidth;
			}
			pWidth = n * pWidth + extraWidth;

			return new Dimension(pWidth + (insets.left + insets.right), pHeight
					+ (insets.top + insets.bottom));
		}
		else if(_orientation == VERTICAL)
		{
			int extraHeight = 0;
			if(_elementPosition == CENTERED_ON_BOUNDS)
			{
				extraHeight = pHeight;
			}
			pHeight = n * pHeight + extraHeight;

			return new Dimension(pWidth + (insets.left + insets.right), pHeight
					+ (insets.top + insets.bottom));
		}
		return null;
	}

	/**
	 * Calculates the minimum size dimensions for the specified panel given the
	 * Boundeds in the specified parent BoundedComposite.
	 *
	 * @param parent the Bounded to be laid out
	 * @see #preferredLayoutSize
	 */
	public Dimension minimumLayoutSize(BoundedComposite parent)
	{
		int n = parent.getElementCount();
		int pWidth = 0;
		int pHeight = 0;
		Bounded ge;
		for(int i = 0; i < n; i++)
		{
			ge = parent.getElement(i);
			Dimension d = ge.getMinimumSize();
			pHeight = Math.max(pHeight, d.height);
			pWidth = Math.max(pWidth, d.width);
		}
		if(_orientation == HORIZONTAL)
		{
			pWidth = n * pWidth;
			return new Dimension(pWidth, pHeight);
		}
		else if(_orientation == VERTICAL)
		{
			pHeight = n * pHeight;
			return new Dimension(pWidth, pHeight);
		}
		return null;
	}

	/**
	 * Lays out the BoundedComposite in the specified panel. Lays out all the
	 * elements with their bounds centered on ticks or centered between the
	 * ticks.
	 *
	 * @param parent the Bounded which needs to be laid out
	 */
	public void layoutContainer(BoundedComposite parent)
	{
		int n = parent.getElementCount();
		Bounded ge = null;

		Rectangle parentBounds = parent.getBounds();
		if(DEBUG)
		{
			System.out.println("Parent " + parent + " bounds " + parentBounds);
		}
		Rectangle geBounds = new Rectangle(0, 0, 0, 0);

		double nextX = 0.0, nextY = 0.0;

		for(int i = 0; i < n; i++)
		{

			double elementWidth = 0.0;
			double elementHeight = 0.0;
			double offset = 0.0;

			switch(_orientation)
			{
				case HORIZONTAL:
					nextY = parentBounds.y;
					elementHeight = (double) (parentBounds.height);

					elementWidth = getElementLength(i, n);
					if(elementWidth == 0)
					{
						nextX = parentBounds.x + parentBounds.width / 2;
					}
					else
					{
						nextX = getElementAnchor(i, elementWidth);
					}

					break;

				case VERTICAL:
					nextX = parentBounds.x;
					elementWidth = (double) parentBounds.width;
					elementHeight = getElementLength(i, n);
					nextY = getElementAnchor(i, elementHeight);
					break;
			}

			geBounds.setBounds((int) Math.round(nextX),
					(int) Math.round(nextY), (int) Math.round(elementWidth),
					(int) Math.round(elementHeight));

			ge = parent.getElement(i);
			ge.setBounds(geBounds);

			if(DEBUG)
			{
				System.out.println("Element " + i + " " + ge + " has bounds "
						+ geBounds);
			}

		}
	}

	/**
	 * gets the positioning of the elements
	 */
	public int getElementPositioning()
	{
		return _elementPosition;
	}

	/**
	 * sets the position of elements to be centered on bounds or between bounds
	 */
	public void setElementPositioning(int position)
	{
		_elementPosition = position;
	}

	/**
	 * gets orientation of layout
	 */
	public int getOrientation()
	{
		return _orientation;
	}

	/**
	 * sets orientation of layout
	 */
	public void setOrientation(int orientation)
	{
		_orientation = orientation;
	}

	/**
	 *
	 */
	public void setTickData(TickData tickData)
	{
		_tickData = tickData;
	}

	/**
	 * gets the width of the element at index
	 */
	private double getElementLength(int index, int n)
	{
		double elementLength = 0.0;
		if(_tickData.getMaxDCValue() == _tickData.getMinDCValue())
		{
			return 0.0;
		}
		if(_tickData.getLabels().length == 1)
		{
			return _tickData.getMaxDCValue() - _tickData.getMinDCValue();
		}
		if(index == 0)
		{
			elementLength = _tickData.getDCValue(1) - _tickData.getDCValue(0);
		}
		else if(index == n - 1)
		{
			elementLength = _tickData.getDCValue(n - 1)
					- _tickData.getDCValue(n - 2);
		}
		else
		{
			if(_elementPosition == CENTERED_ON_BOUNDS)
			{
				double upH = _tickData.getDCValue(index + 1)
						- _tickData.getDCValue(index);
				double downH = _tickData.getDCValue(index)
						- _tickData.getDCValue(index - 1);
				elementLength = Math.abs(Math.min(upH, downH));
			}
			else
			{
				elementLength = Math.abs(_tickData.getDCValue(index + 1)
						- _tickData.getDCValue(index));
			}
		}
		elementLength = Math.abs(_tickData.getScale().scaleToUC(elementLength)
				- _tickData.getScale().scaleToUC(0.0));
		return elementLength;
	}

	/**
	 *
	 */
	private double getElementAnchor(int index, double length)
	{
		double position = 0.0;
		if(_elementPosition == CENTERED_ON_BOUNDS)
		{
			position = _tickData.getUCValue(index);
		}

		if(_elementPosition == CENTERED_ON_BOUNDS)
		{
			position -= length / 2.0;
		}

		return position;
	}
}
