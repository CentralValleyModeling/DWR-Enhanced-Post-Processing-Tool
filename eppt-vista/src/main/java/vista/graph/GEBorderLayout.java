/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */
package vista.graph;

import java.awt.Dimension;
import java.awt.Insets;
import java.awt.Rectangle;
import java.util.ArrayList;

/**
 * Lays out the elements in North, South , East , West and Center positions. The
 * elements are sized by their preferred sizes for the NSEW positions with the
 * remaining space going to the Center position. This is useful for laying out
 * the axis elements in a Plot
 *
 * @author Nicky Sandhu (DWR).
 * @version $Id: GEBorderLayout.java,v 1.1 2003/10/02 20:48:56 redwood Exp $
 * @see Plot
 */
public class GEBorderLayout implements GELayoutManager
{
	/**
	 * for debuggin'
	 */
	public static final boolean DEBUG = false;
	/**
	 * The north layout constraint.
	 */
	public static final String NORTH = "North";

	/**
	 * The south layout constraint.
	 */
	public static final String SOUTH = "South";

	/**
	 * The east layout constraint.
	 */
	public static final String EAST = "East";

	/**
	 * The west layout constraint.
	 */
	public static final String WEST = "West";

	/**
	 * The center layout constraint.
	 */
	public static final String CENTER = "Center";
	/**
	 * The northern element
	 */
	protected ArrayList<Bounded> north = new ArrayList<Bounded>();
	/**
	 * The southern element
	 */
	protected ArrayList<Bounded> south = new ArrayList<Bounded>();
	/**
	 * The eastern element
	 */
	protected ArrayList<Bounded> east = new ArrayList<Bounded>();
	/**
	 * The western element
	 */
	protected ArrayList<Bounded> west = new ArrayList<Bounded>();
	/**
	 * The central element
	 */
	protected ArrayList<Bounded> center = new ArrayList<Bounded>();
	/**
	 * if true then scale components according to the preferred and actual sizes
	 * of the container.
	 */
	private boolean _doScaleComponents = false;
	private boolean _scaleComponents = true;

	/**
	 *
	 */
	public GEBorderLayout()
	{
	}

	/**
	 * Adds the specified element to the layout, using the specified constraint
	 * object.
	 *
	 * @param comp        the element to be added
	 * @param constraints where/how the element is added to the layout.
	 */
	public void addLayoutElement(Bounded comp, Object constraints)
	{
		if((constraints == null) || (constraints instanceof String))
		{
			addLayoutElement(constraints, comp);
		}
		else
		{
			throw new IllegalArgumentException(
					"cannot add to layout: constraint must be a string (or null)");
		}
	}

	/**
	 * Replaced by addLayoutElement(Element, Object).
	 *
	 * @deprecated
	 */
	public void addLayoutElement(Object obj, Bounded comp)
	{
		String name = null;
		if(obj instanceof String)
		{
			name = (String) obj;
		}
		else
		{
			// throw exception
			throw new IllegalArgumentException(
					"addLayoutElement only takes String directives");
		}
		/* Special case: treat null the same as GEBorderLayout.CENTER. */
		if(name == null)
		{
			name = GEBorderLayout.CENTER;
		}

		/*
		 * Assign the element to one of the known regions of the layout.
		 */
		if(GEBorderLayout.CENTER.equals(name))
		{
			center.add(comp);
		}
		else if(GEBorderLayout.NORTH.equals(name))
		{
			north.add(comp);
		}
		else if(GEBorderLayout.SOUTH.equals(name))
		{
			south.add(comp);
		}
		else if(GEBorderLayout.EAST.equals(name))
		{
			east.add(comp);
		}
		else if(GEBorderLayout.WEST.equals(name))
		{
			west.add(comp);
		}
		else
		{
			throw new IllegalArgumentException(
					"cannot add to layout: unknown constraint: " + name);
		}
	}

	/**
	 *
	 */
	public Dimension getPreferredSize(String name)
	{

		ArrayList<Bounded> comp = null;
		/* Special case: treat null the same as GEBorderLayout.CENTER. */
		if(name == null)
		{
			name = GEBorderLayout.CENTER;
		}

		/*
		 * Assign the element to one of the known regions of the layout.
		 */
		if(GEBorderLayout.CENTER.equals(name))
		{
			comp = center;
		}
		else if(GEBorderLayout.NORTH.equals(name))
		{
			comp = north;
		}
		else if(GEBorderLayout.SOUTH.equals(name))
		{
			comp = south;
		}
		else if(GEBorderLayout.EAST.equals(name))
		{
			comp = east;
		}
		else if(GEBorderLayout.WEST.equals(name))
		{
			comp = west;
		}
		else
		{
			throw new IllegalArgumentException(
					"cannot add to layout: unknown constraint: " + name);
		}

		return getPreferredSize(comp);
	}

	/**
	 * Removes the specified element from the layout.
	 *
	 * @param comp the element to be removed
	 */
	public void removeLayoutElement(Bounded comp)
	{
		center.remove(comp);
		north.remove(comp);
		south.remove(comp);
		east.remove(comp);
		west.remove(comp);
	}

	/**
	 * Returns the minimum dimensions needed to layout the elements contained in
	 * the specified target BoundedComposite.
	 *
	 * @param target the BoundedComposite on which to do the layout
	 * @see BoundedComposite
	 * @see #preferredLayoutSize
	 */
	public Dimension minimumLayoutSize(BoundedComposite target)
	{
		Dimension dim = new Dimension(0, 0);
		Dimension ed = new Dimension(0, 0), wd = new Dimension(0, 0), nd = new Dimension(
				0, 0), sd = new Dimension(0, 0), cd = new Dimension(0, 0);

		if(!east.isEmpty())
		{
			ed = getMinimumSize(east);
		}
		if(!west.isEmpty())
		{
			wd = getMinimumSize(west);
		}
		if(!north.isEmpty())
		{
			nd = getMinimumSize(north);
		}
		if(!south.isEmpty())
		{
			sd = getMinimumSize(south);
		}
		if(!center.isEmpty())
		{
			cd = getMinimumSize(center);
		}

		dim.width += ed.width
				+ Math.max(Math.max(nd.width, sd.width), cd.width) + wd.width;

		dim.height += nd.height
				+ Math.max(Math.max(ed.height, wd.height), cd.height)
				+ sd.height;

		Insets insets = target.getInsets();

		dim.width += insets.left + insets.right;
		dim.height += insets.top + insets.bottom;

		return dim;
	}

	/**
	 * Returns the preferred dimensions for this layout given the elements in
	 * the specified target BoundedComposite.
	 *
	 * @param target the element which needs to be laid out
	 * @see BoundedComposite
	 * @see #minimumLayoutSize
	 */
	public Dimension preferredLayoutSize(BoundedComposite target)
	{
		Dimension dim = new Dimension(0, 0);
		Dimension ed = new Dimension(0, 0), wd = new Dimension(0, 0), nd = new Dimension(
				0, 0), sd = new Dimension(0, 0), cd = new Dimension(0, 0);

		if(!east.isEmpty())
		{
			ed = getPreferredSize(east);
		}
		if(!west.isEmpty())
		{
			wd = getPreferredSize(west);
		}
		if(!north.isEmpty())
		{
			nd = getPreferredSize(north);
		}
		if(!south.isEmpty())
		{
			sd = getPreferredSize(south);
		}
		if(!center.isEmpty())
		{
			cd = getPreferredSize(center);
		}

		dim.width += ed.width
				+ Math.max(Math.max(nd.width, sd.width), cd.width) + wd.width;
		dim.height += nd.height
				+ Math.max(Math.max(ed.height, wd.height), cd.height)
				+ sd.height;

		Insets insets = target.getInsets();

		dim.width += insets.left + insets.right;
		dim.height += insets.top + insets.bottom;

		return dim;
	}

	/**
	 * Lays out the specified BoundedComposite. This method will actually
	 * reshape the elements in the specified target BoundedComposite in order to
	 * satisfy the constraints of the GEBorderLayout object.
	 *
	 * @param target the element being laid out
	 * @see BoundedComposite
	 */
	public void layoutContainer(BoundedComposite target)
	{

		Rectangle area = target.getInsetedBounds();

		Dimension pSize = this.preferredLayoutSize(target);
		Dimension aSize = target.getSize();
		boolean smallerDimension = (aSize.width < pSize.width || aSize.height < pSize.height);

		_scaleComponents = smallerDimension && _doScaleComponents;

		double widthScale = (1.0 * area.width) / pSize.width;
		double heightScale = (1.0 * area.height) / pSize.height;

		Dimension ed = new Dimension(0, 0), wd = new Dimension(0, 0), nd = new Dimension(
				0, 0), sd = new Dimension(0, 0), cd = new Dimension(0, 0);
		double edW = 0.0, edH = 0.0;
		double wdW = 0.0, wdH = 0.0;
		double ndW = 0.0, ndH = 0.0;
		double sdW = 0.0, sdH = 0.0;

		// if (! east.isEmpty()) {
		ed = getPreferredSize(east);
		if(_scaleComponents)
		{
			edW = widthScale * ed.width;
			edH = heightScale * ed.height;
		}
		else
		{
			edW = ed.width;
			edH = ed.height;
		}
		// }
		// if (! west.isEmpty()) {
		wd = getPreferredSize(west);
		if(_scaleComponents)
		{
			wdW = widthScale * wd.width;
			wdH = heightScale * wd.height;
		}
		else
		{
			wdW = wd.width;
			wdH = wd.height;
		}
		// }
		// if (! north.isEmpty()) {
		nd = getPreferredSize(north);
		if(_scaleComponents)
		{
			ndW = widthScale * nd.width;
			ndH = heightScale * nd.height;
		}
		else
		{
			ndW = nd.width;
			ndH = nd.height;
		}
		// }
		// if (! south.isEmpty()) {
		sd = getPreferredSize(south);
		if(_scaleComponents)
		{
			sdW = widthScale * sd.width;
			sdH = heightScale * sd.height;
		}
		else
		{
			sdW = sd.width;
			sdH = sd.height;
		}
		// }

		Rectangle nr = new Rectangle((int) Math.round(area.x + wdW), area.y,
				(int) Math.round(area.width - edW - wdW), (int) Math.round(ndH));
		if(target instanceof Plot && DEBUG)
		{
			System.out.println("ndW = " + ndW + " ndH= " + ndH);
			System.out.println("nr = " + nr);
		}
		if(!north.isEmpty())
		{
			setBounds(nr, north);
		}

		Rectangle sr = new Rectangle(nr.x, (int) Math.round(area.y
				+ area.height - sdH), nr.width, (int) Math.round(sdH));

		if(target instanceof Plot && DEBUG)
		{
			System.out.println("sdW = " + sdW + " sdH= " + sdH);
			System.out.println("sr = " + sr);
		}
		if(!south.isEmpty())
		{
			setBounds(sr, south);
		}

		Rectangle wr = new Rectangle(0, 0, 0, 0);

		wr.x = area.x;
		wr.y = area.y + nr.height;
		wr.width = (int) Math.round(wdW);
		wr.height = area.height - nr.height - sr.height;

		if(target instanceof Plot && DEBUG)
		{
			System.out.println("wdW = " + wdW + " wdH= " + wdH);
			System.out.println("wr = " + wr);
		}
		if(!west.isEmpty())
		{
			setBounds(wr, west);
		}

		Rectangle er = wr;
		er.x = nr.x + nr.width;
		er.width = (int) Math.round(edW);
		if(target instanceof Plot && DEBUG)
		{
			System.out.println("edW = " + edW + " edH= " + edH);
			System.out.println("er = " + er);
		}
		if(!east.isEmpty())
		{
			setBounds(er, east);
		}

		Rectangle cr = er;
		cr.x = (int) Math.round(area.x + wdW);
		cr.y = (int) Math.round(area.y + ndH);
		cr.width = (int) Math.round(area.width - edW - wdW);
		cr.height = (int) Math.round(area.height - ndH - sdH);

		if(target instanceof Plot && DEBUG)
		{
			System.out.println("cr = " + cr);
		}
		if(!center.isEmpty())
		{
			setBounds(cr, center);
		}
	}

	/**
	 * Returns the String representation of this GEBorderLayout's values.
	 */
	public String toString()
	{
		return getClass().getName();
	}

	/**
	 * returns the maximum of minimum required dimensions for all graphical
	 * elements in the ArrayList<Bounded>.
	 */
	private Dimension getMinimumSize(ArrayList<Bounded> array)
	{
		Dimension maxD = new Dimension(0, 0);
		for(Bounded ge : array)
		{
			Dimension d = ge.getMinimumSize();
			maxD.width = Math.max(maxD.width, d.width);
			maxD.height = Math.max(maxD.height, d.height);
		}
		return maxD;
	}

	/**
	 * returns the maximum of preferred required dimensions for all graphical
	 * elements in the array.
	 */
	protected Dimension getPreferredSize(ArrayList<Bounded> array)
	{
		Dimension maxD = new Dimension(0, 0);
		for(Bounded ge : array)
		{
			Dimension d = ge.getPreferredSize();
			maxD.width = Math.max(maxD.width, d.width);
			maxD.height = Math.max(maxD.height, d.height);
		}
		return maxD;
	}

	/**
	 * sets bounds to the rectangle for all graphical elements in array
	 */
	private void setBounds(Rectangle bounds, ArrayList<Bounded> array)
	{
		for(Bounded ge : array)
		{
			ge.setBounds(bounds);
		}
	}

	/**
	 * scales the component given its containers preferred and actual sizes and
	 * its own preferred size.
	 */
	private Dimension scaleToSize(Dimension componentPreferredSize,
								  double widthScale, double heightScale)
	{
		if(_scaleComponents)
		{
			componentPreferredSize.width = (int) Math
					.round(componentPreferredSize.width * widthScale);
			componentPreferredSize.height = (int) Math
					.round(componentPreferredSize.height * heightScale);
		}
		return componentPreferredSize;
	}

	/**
	 * sets the flag to switch between scaling and absolue scaling
	 */
	public void setScaleComponents(boolean b)
	{
		_doScaleComponents = b;
	}
}
