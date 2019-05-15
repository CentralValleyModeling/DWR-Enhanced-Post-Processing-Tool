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
package vista.set;

import java.io.FileReader;
import java.io.IOException;
import java.io.LineNumberReader;
import java.util.Vector;

/**
 * @author Nicky Sandhu
 * @version $Id: ListDirectedPredicate.java,v 1.1 2003/10/02 20:49:25 redwood
 * Exp $
 */
public class ListDirectedPredicate implements SortMechanism<DataReference>
{
	private static boolean DEBUG = false;
	/**
	 *
	 */
	private int _partId;
	/**
	 *
	 */
	private String[] _list;
	private int _sortOrder;

	/**
	 *
	 */
	public ListDirectedPredicate(String filename)
	{
		if(filename == null)
		{
			throw new IllegalArgumentException(
					"Invalid file list for list sort");
		}
		try
		{
			LineNumberReader reader = new LineNumberReader(new FileReader(
					filename));
			String line = null;
			Vector listV = new Vector();
			line = reader.readLine();
			int partId = 0;
			if(line != null)
			{
				line = line.toUpperCase();
				int pIndex = line.indexOf("PART");
				if(pIndex < 0)
				{
					throw new IllegalArgumentException(
							"filename has incorrect part specification"
									+ " @ line:" + line);
				}
				int eqIndex = line.indexOf("=");
				if(eqIndex < 0)
				{
					throw new IllegalArgumentException(
							"filename has incorrect part specification"
									+ " @ line:" + line);
				}
				line = line.substring(eqIndex + 1).trim();
				if(line.equals("A"))
				{
					_partId = Pathname.A_PART;
				}
				else if(line.equals("B"))
				{
					_partId = Pathname.B_PART;
				}
				else if(line.equals("C"))
				{
					_partId = Pathname.C_PART;
				}
				else if(line.equals("D"))
				{
					_partId = Pathname.D_PART;
				}
				else if(line.equals("E"))
				{
					_partId = Pathname.E_PART;
				}
				else if(line.equals("F"))
				{
					_partId = Pathname.F_PART;
				}
				else if(line.equals("P"))
				{
					_partId = Pathname.F_PART;
				}
				else
				{
					throw new IllegalArgumentException(
							"filename has incorrect path id: " + line);
				}
				//
			}
			// get order if available
			_sortOrder = INCREASING;
			line = reader.readLine();
			if(line.indexOf("order") >= 0)
			{
				if(line.indexOf("=") < 0)
				{
					throw new IllegalArgumentException(
							"Incorrect order specification: " + line);
				}
				line = line.substring(line.indexOf("=") + 1)
						   .trim();
				line = line.toUpperCase();
				if(line.indexOf("DECR") >= 0)
				{
					_sortOrder = DECREASING;
				}
				else
				{
					_sortOrder = INCREASING;
				}
			}
			do
			{
				// get list and up case everything
				listV.addElement(line.toUpperCase());
			} while((line = reader.readLine()) != null);
			_list = new String[listV.size()];
			listV.copyInto(_list);
		}
		catch(IOException ioe)
		{
			throw new IllegalArgumentException("IO Exception :"
					+ ioe.getMessage());
		}
		if(DEBUG)
		{
			System.out.println("Sorting on part = "
					+ Pathname.getPartName(_partId));
			System.out.println("order = " + _sortOrder);
			for(int i = 0; i < _list.length; i++)
			{
				System.out.println("list[" + i + "]=" + _list[i]);
			}
		}
	}

	/**
	 *
	 */
	public ListDirectedPredicate(int partId, String[] list)
	{
		this(partId, list, INCREASING);
	}

	/**
	 *
	 */
	public ListDirectedPredicate(int partId, String[] list, int sortOrder)
	{
		initializeAll(partId, list, sortOrder);
	}

	/**
	 *
	 */
	private void initializeAll(int partId, String[] list, int sortOrder)
	{
		_partId = partId;
		_list = list;
		_sortOrder = sortOrder;
	}

	/**
	 * method for collections Comapartor interface
	 */
	public int compare(DataReference first, DataReference second)
	{
		// check instanceof first and second
		Pathname fPath = first.getPathname();
		Pathname sPath = second.getPathname();
		int index1 = 0;
		int index2 = 0;
		if(_partId == -1)
		{
			index1 = getPositionInList(fPath.toString());
			index2 = getPositionInList(sPath.toString());
		}
		else
		{
			index1 = getPositionInList(fPath.getPart(_partId));
			index2 = getPositionInList(sPath.getPart(_partId));
		}
		int lexicalValue = 0;
		//
		if(_sortOrder == INCREASING)
		{
			lexicalValue = index2 - index1;
		}
		else if(_sortOrder == DECREASING)
		{
			lexicalValue = index1 - index2;
		}
		else
		{
			lexicalValue = 0;
		}
		//
		return lexicalValue;
	}

	/**
	 *
	 */
	protected int getPositionInList(String name)
	{
		int i = 0;
		for(i = 0; i < _list.length; i++)
		{
			if(name.indexOf(_list[i]) >= 0)
			{
				return i;
			}
		}
		i++;
		return i;
	}

	@Override
	public boolean isAscendingOrder()
	{
		return _sortOrder == INCREASING;
	}

	@Override
	public void setAscendingOrder(boolean ascending)
	{
		_sortOrder = INCREASING;
	}
}
