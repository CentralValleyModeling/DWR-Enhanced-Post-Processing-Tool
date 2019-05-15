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
package calsim.wreslcoder.wresl;

import java.util.Vector;

/**
 * This defines a special Vector of Strings.  They're not case sensitive and the underlying
 * Vector is optimized for Wresl use.
 *
 * @author Armin Munevar
 * @version $Id: UniqueList.java,v 1.1.2.3 2001/07/12 02:00:10 amunevar Exp $
 */

public class UniqueList extends Vector
{

	/*
	  Stuff for the ID code handing feature
	*/
	private java.util.Hashtable idCode;

	/**
	 * Creates a new instance with default size and load factor.
	 */
	public UniqueList()
	{
		this(600, 300);
	}

	/**
	 * Creates a new instance with specified size and load factor.
	 *
	 * @param n size
	 * @param m amount to grow by when current size is exceeded
	 */
	public UniqueList(int n, int m)
	{
		super(n, m);
		idCode = new java.util.Hashtable(n);
	}

	public int getIndexOf(Object id)
	{
		return super.indexOf(id.toString().toUpperCase());
	}

	/**
	 * Adds an element to this instance.  Converts to upper case.
	 *
	 * @param the element to add
	 * @return true if element was successfully added, false
	 * if element was already in this instance.
	 */

	public boolean newItem(Object id)
	{
		String idUpper = id.toString().toUpperCase();
		if(super.indexOf(idUpper) < 0)
		{
			addElement(idUpper);
			idCode.put(idUpper, new Integer(super.indexOf(idUpper) + 1));
			// +1 for Fortran array compatability
			return true;
		}
		else
		{
			return false;
		}
	}

	public Integer getIdCode(Object id)
	{
		return (Integer) idCode.get(id.toString().toUpperCase());
	}

	/* for use by a sorting routine */
	public void swap(int m, int n)
	{
		String tmp;
		tmp = (String) elementAt(m);
		setElementAt(elementAt(n), m);
		setElementAt(tmp, n);
	}


}
