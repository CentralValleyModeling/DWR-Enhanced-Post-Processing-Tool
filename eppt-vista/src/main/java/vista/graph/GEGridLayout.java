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

/**
 * @author Nicky Sandhu
 * @version $Id: GEGridLayout.java,v 1.1 2003/10/02 20:48:57 redwood Exp $
 */
public class GEGridLayout implements GELayoutManager
{
	int hgap;
	int vgap;
	int rows;
	int cols;

	public GEGridLayout()
	{
		this(1, 0, 0, 0);
	}

	/**
	 * Creates a grid layout with the specified number of rows and columns. All
	 * components in the layout are given equal size.
	 * <p>
	 * One, but not both, of <code>rows</code> and <code>cols</code> can be
	 * zero, which means that any number of objects can be placed in a row or in
	 * a column.
	 *
	 * @param rows the rows, with the value zero meaning any number of rows.
	 * @param cols the columns, with the value zero meaning any number of
	 *             columns.
	 * @since JDK1.0
	 */
	public GEGridLayout(int rows, int cols)
	{
		this(rows, cols, 0, 0);
	}

	/**
	 * Creates a grid layout with the specified number of rows and columns. All
	 * components in the layout are given equal size.
	 * <p>
	 * In addition, the horizontal and vertical gaps are set to the specified
	 * values. Horizontal gaps are placed at the left and right edges, and
	 * between each of the columns. Vertical gaps are placed at the top and
	 * bottom edges, and between each of the rows.
	 * <p>
	 * One, but not both, of <code>rows</code> and <code>cols</code> can be
	 * zero, which means that any number of objects can be placed in a row or in
	 * a column.
	 *
	 * @param rows the rows, with the value zero meaning any number of rows.
	 * @param cols the columns, with the value zero meaning any number of
	 *             columns.
	 * @param hgap the horizontal gap.
	 * @param vgap the vertical gap.
	 * @throws IllegalArgumentException if the of <code>rows</code> or <code>cols</code> is
	 *                                  invalid.
	 * @since JDK1.0
	 */
	public GEGridLayout(int rows, int cols, int hgap, int vgap)
	{
		if((rows == 0) && (cols == 0))
		{
			throw new IllegalArgumentException(
					"rows and cols cannot both be zero");
		}
		this.rows = rows;
		this.cols = cols;
		this.hgap = hgap;
		this.vgap = vgap;
	}

	/**
	 * Gets the number of rows in this layout.
	 *
	 * @return the number of rows in this layout.
	 * @since JDK1.1
	 */
	public int getRows()
	{
		return rows;
	}

	/**
	 * Sets the number of rows in this layout to the specified value.
	 *
	 * @param rows the number of rows in this layout.
	 * @throws IllegalArgumentException if the value of both <code>rows</code> and
	 *                                  <code>cols</code> is set to zero.
	 * @since JDK1.1
	 */
	public void setRows(int rows)
	{
		if((rows == 0) && (this.cols == 0))
		{
			throw new IllegalArgumentException(
					"rows and cols cannot both be zero");
		}
		this.rows = rows;
	}

	/**
	 * Gets the number of columns in this layout.
	 *
	 * @return the number of columns in this layout.
	 * @since JDK1.1
	 */
	public int getColumns()
	{
		return cols;
	}

	/**
	 * Sets the number of columns in this layout to the specified value.
	 *
	 * @param cols the number of columns in this layout.
	 * @throws IllegalArgumentException if the value of both <code>rows</code> and
	 *                                  <code>cols</code> is set to zero.
	 * @since JDK1.1
	 */
	public void setColumns(int cols)
	{
		if((cols == 0) && (this.rows == 0))
		{
			throw new IllegalArgumentException(
					"rows and cols cannot both be zero");
		}
		this.cols = cols;
	}

	/**
	 * Gets the horizontal gap between components.
	 *
	 * @return the horizontal gap between components.
	 * @since JDK1.1
	 */
	public int getHgap()
	{
		return hgap;
	}

	/**
	 * Sets the horizontal gap between components to the specified value.
	 *
	 * @param hgap the horizontal gap between components.
	 * @since JDK1.1
	 */
	public void setHgap(int hgap)
	{
		this.hgap = hgap;
	}

	/**
	 * Gets the vertical gap between components.
	 *
	 * @return the vertical gap between components.
	 * @since JDK1.1
	 */
	public int getVgap()
	{
		return vgap;
	}

	/**
	 * Sets the vertical gap between components to the specified value.
	 *
	 * @param vgap the vertical gap between components.
	 * @since JDK1.1
	 */
	public void setVgap(int vgap)
	{
		this.vgap = vgap;
	}

	/**
	 * Adds the specified component with the specified name to the layout.
	 *
	 * @param name the name of the component.
	 * @param comp the component to be added.
	 * @since JDK1.0
	 */
	public void addLayoutElement(Object obj, Bounded comp)
	{
	}

	/**
	 * Removes the specified component from the layout.
	 *
	 * @param comp the component to be removed.
	 * @since JDK1.0
	 */
	public void removeLayoutElement(Bounded comp)
	{
	}

	/**
	 * Determines the preferred size of the container argument using this grid
	 * layout.
	 * <p>
	 * The preferred width of a grid layout is the largest preferred width of
	 * any of the widths in the container times the number of columns, plus the
	 * horizontal padding times the number of columns plus one, plus the left
	 * and right insets of the target container.
	 * <p>
	 * The preferred height of a grid layout is the largest preferred height of
	 * any of the widths in the container times the number of rows, plus the
	 * vertical padding times the number of rows plus one, plus the top and left
	 * insets of the target container.
	 *
	 * @param target the container in which to do the layout.
	 * @return the preferred dimensions to lay out the subcomponents of the
	 * specified container.
	 * @see java.awt.GridLayout#minimumLayoutSize
	 * @see java.awt.Container#getPreferredSize()
	 * @since JDK1.0
	 */
	public Dimension preferredLayoutSize(BoundedComposite parent)
	{
		Insets insets = parent.getInsets();
		int ncomponents = parent.getElementCount();
		int nrows = rows;
		int ncols = cols;

		if(nrows > 0)
		{
			ncols = (ncomponents + nrows - 1) / nrows;
		}
		else
		{
			nrows = (ncomponents + ncols - 1) / ncols;
		}
		int w = 0;
		int h = 0;
		for(int i = 0; i < ncomponents; i++)
		{
			Bounded comp = parent.getElement(i);
			Dimension d = comp.getPreferredSize();
			if(w < d.width)
			{
				w = d.width;
			}
			if(h < d.height)
			{
				h = d.height;
			}
		}
		return new Dimension(insets.left + insets.right + ncols * w
				+ (ncols - 1) * hgap, insets.top + insets.bottom + nrows * h
				+ (nrows - 1) * vgap);
	}

	/**
	 * Determines the minimum size of the container argument using this grid
	 * layout.
	 * <p>
	 * The minimum width of a grid layout is the largest minimum width of any of
	 * the widths in the container times the number of columns, plus the
	 * horizontal padding times the number of columns plus one, plus the left
	 * and right insets of the target container.
	 * <p>
	 * The minimum height of a grid layout is the largest minimum height of any
	 * of the widths in the container times the number of rows, plus the
	 * vertical padding times the number of rows plus one, plus the top and left
	 * insets of the target container.
	 *
	 * @param target the container in which to do the layout.
	 * @return the minimum dimensions needed to lay out the subcomponents of the
	 * specified container.
	 * @see java.awt.GridLayout#preferredLayoutSize
	 * @see java.awt.Container#doLayout
	 * @since JDK1.0
	 */
	public Dimension minimumLayoutSize(BoundedComposite parent)
	{
		Insets insets = parent.getInsets();
		int ncomponents = parent.getElementCount();
		int nrows = rows;
		int ncols = cols;

		if(nrows > 0)
		{
			ncols = (ncomponents + nrows - 1) / nrows;
		}
		else
		{
			nrows = (ncomponents + ncols - 1) / ncols;
		}
		int w = 0;
		int h = 0;
		for(int i = 0; i < ncomponents; i++)
		{
			Bounded comp = parent.getElement(i);
			Dimension d = comp.getMinimumSize();
			if(w < d.width)
			{
				w = d.width;
			}
			if(h < d.height)
			{
				h = d.height;
			}
		}
		return new Dimension(insets.left + insets.right + ncols * w
				+ (ncols - 1) * hgap, insets.top + insets.bottom + nrows * h
				+ (nrows - 1) * vgap);
	}

	/**
	 * Lays out the specified container using this layout.
	 * <p>
	 * This method reshapes the components in the specified target container in
	 * order to satisfy the constraints of the <code>GridLayout</code> object.
	 * <p>
	 * The grid layout manager determines the size of individual components by
	 * dividing the free space in the container into equal-sized portions
	 * according to the number of rows and columns in the layout. The
	 * container's free space equals the container's size minus any insets and
	 * any specified horizontal or vertical gap. All components in a grid layout
	 * are given the same size.
	 *
	 * @param target the container in which to do the layout.
	 * @see java.awt.Container
	 * @see java.awt.Container#doLayout
	 * @since JDK1.0
	 */
	public void layoutContainer(BoundedComposite parent)
	{
		Insets insets = parent.getInsets();
		Rectangle rect = parent.getBounds();
		int ncomponents = parent.getElementCount();
		int nrows = rows;
		int ncols = cols;

		if(ncomponents == 0)
		{
			return;
		}
		if(nrows > 0)
		{
			ncols = (ncomponents + nrows - 1) / nrows;
		}
		else
		{
			nrows = (ncomponents + ncols - 1) / ncols;
		}
		int w = rect.width - (insets.left + insets.right);
		int h = rect.height - (insets.top + insets.bottom);
		w = (w - (ncols - 1) * hgap) / ncols;
		h = (h - (nrows - 1) * vgap) / nrows;

		for(int c = 0, x = insets.left; c < ncols; c++, x += w + hgap)
		{
			for(int r = 0, y = insets.top; r < nrows; r++, y += h + vgap)
			{
				int i = r * ncols + c;
				if(i < ncomponents)
				{
					((Bounded) parent.getElement(i)).setBounds(new Rectangle(
							rect.x + x, rect.y + y, w, h));
				}
			}
		}
	}

	/**
	 * Returns the string representation of this grid layout's values.
	 *
	 * @return a string representation of this grid layout.
	 * @since JDK1.0
	 */
	public String toString()
	{
		return getClass().getName() + "[hgap=" + hgap + ",vgap=" + vgap
				+ ",rows=" + rows + ",cols=" + cols + "]";
	}
}
