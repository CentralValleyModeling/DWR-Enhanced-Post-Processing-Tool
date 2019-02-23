/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */
package vista.set;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.TreeSet;

import org.python.core.Py;

/**
 * This class groups associate data references together.
 *
 * @author Nicky Sandhu
 * @version $Id: Group.java,v 1.1 2003/10/02 20:49:24 redwood Exp $
 */
public class Group implements Named, Serializable, Comparable<Group>
{
	/**
	 *
	 */
	// static final long serialVersionUID = -7794455500216432504L;
	/**
	 * The name of this group
	 */
	private String _name;
	/**
	 * The expandable array containing the reference list. This could be
	 * replaced by a linked list kind of implementation if insertion and
	 * deletion get excessive.
	 */
	private ArrayList<DataReference> _referenceList;

	/**
	 * private constructor.
	 */
	public Group()
	{
		_name = "";
		_referenceList = new ArrayList<DataReference>();
	}

	/**
	 * creates a group with a name..
	 */
	public Group(String name)
	{
		_name = name;
		_referenceList = new ArrayList<DataReference>();
	}

	/**
	 * creates an empty group with given name. This creates the shell of this
	 * group which will be later filled in when the first access to its data
	 * references is made...
	 */
	public static Group createGroup(String name)
	{
		Group g = new Group();
		g._name = name;
		g._referenceList = new ArrayList<DataReference>();
		return g;
	}

	/**
	 * creates a group with a name and reference list
	 */
	public static Group createGroup(String name, DataReference[] refs)
	{
		if(refs == null)
		{
			return null;
		}
		if(name == null)
		{
			name = "";
		}
		synchronized(refs)
		{
			Group g = new Group();
			g._name = name;
			g._referenceList = new ArrayList<DataReference>(Arrays.asList(refs));
			return g;
		}
	}

	/**
	 * creates a group with a name and reference list
	 */
	public static Group createGroup(String name, List refArray)
	{
		if(refArray == null)
		{
			return null;
		}
		if(name == null)
		{
			name = "";
		}
		synchronized(refArray)
		{
			Group g = new Group();
			g._name = name;
			g._referenceList = new ArrayList(refArray);
			return g;
		}
	}

	/**
	 * creates a group which is the copy of another group. This would mean
	 * copying the data references the other groups contain.
	 */
	public static Group createGroup(Group g)
	{
		Group gcopy = new Group();
		gcopy.setName(g.getName() + "(copy)");
		gcopy._referenceList = new ArrayList<DataReference>(g._referenceList);
		return gcopy;
	}

	/**
	 * gets group name
	 */
	public String getName()
	{
		return _name;
	}

	/**
	 * sets the group name
	 */
	public void setName(String name)
	{
		_name = name;
	}

	/**
	 * gets the number of data references
	 */
	public int getNumberOfDataReferences()
	{
		return _referenceList.size();
	}

	/**
	 * adds data reference if not already added at the specified index
	 */
	public void insertDataReferenceAt(int index, DataReference ref)
	{
		if(!_referenceList.contains(ref))
		{
			_referenceList.add(index, ref);
		}
	}

	/**
	 * adds data reference only if it not already present. This method takes a
	 * hit primarily because it searches for the occurence of a similar data
	 * reference before adding it. It is faster to create a group with an array
	 * of references.
	 */
	public synchronized void addDataReference(DataReference ref)
	{
		if(!_referenceList.contains(ref))
		{
			_referenceList.add(ref);
		}
	}

	/**
	 * removes data reference
	 */
	public void removeDataReference(DataReference ref)
	{
		_referenceList.remove(ref);
	}

	/**
	 * removes data reference in a certain continuous range
	 */
	public void removeDataReference(int index0, int index1)
	{
		if(index0 > index1)
		{
			int swap = index1;
			index1 = index0;
			index0 = swap;
		}
		for(int i = index1; i >= index0; i++)
		{
			_referenceList.remove(index0);
		}
	}

	/**
	 * gets data reference by index
	 */
	public DataReference getDataReference(int index)
	{
		if(index < _referenceList.size())
		{
			return _referenceList.get(index);
		}
		else
		{
			return null;
		}
	}

	/**
	 * gets data reference by name
	 */
	public DataReference getDataReference(String dataName)
	{
		for(DataReference r : _referenceList)
		{
			if(r.getName().equals(dataName))
			{
				return r;
			}
		}
		return null;
	}

	/**
	 * gets all the data references
	 */
	public DataReference[] getAllDataReferences()
	{
		DataReference[] refs = new DataReference[getNumberOfDataReferences()];
		return _referenceList.toArray(refs);
	}

	/**
	 * copies this group into given group.
	 */
	public void copyInto(Group group)
	{
		group._referenceList = new ArrayList<DataReference>(this._referenceList);
	}

	/**
	 * returns a copy of itself.
	 */
	public Object clone()
	{
		return createGroup(this);
	}

	/**
	 * Creates a new group which is the union of this group and the specified
	 * groupg
	 */
	public Group unionWith(Group group)
	{
		if(group == null)
		{
			return (Group) this.clone();
		}
		String name = this._name + " union " + group._name;
		// make copies of reference list and sort by hash code
		TreeSet<DataReference> refs = new TreeSet<DataReference>();
		refs.addAll(this._referenceList);
		refs.addAll(Arrays.asList(group.getAllDataReferences()));
		Group groupUnion = new Group();
		groupUnion._name = name;
		groupUnion._referenceList = new ArrayList<DataReference>(refs);
		return groupUnion;
	}

	/**
	 * Creates a new group which is the intersection of this group and the
	 * specified group. Returns null if intersection is the null set.
	 */
	public Group intersectionWith(Group group)
	{
		if(group == null)
		{
			return (Group) this.clone();
		}
		String name = this._name + " intersection " + group._name;
		// make copies of reference list and sort by hash code
		TreeSet<DataReference> refs = new TreeSet<DataReference>();
		refs.addAll(this._referenceList);
		refs.retainAll(group._referenceList);
		Group groupIntersection = new Group();
		groupIntersection._name = name;
		groupIntersection._referenceList = new ArrayList<DataReference>(refs);
		return groupIntersection;
	}

	/**
	 * Sorts group by calling upon the SortAlgorithm to return an array of data
	 * references by some criteria. The reference list is being directly
	 * manipulated by the sorting mechanism.
	 */
	public void sortBy(Comparator<DataReference> comparator)
	{
		Collections.sort(_referenceList, comparator);
	}

	/**
	 * filter on the pathname using regular expression. The current group is
	 * left to contain only those expressions that have the regular expression
	 * in them.
	 */
	public void filterBy(String regex)
	{
		filterBy(new PathnamePredicate(regex), true);
	}

	/**
	 * filters on this group using filter. This causes the reference list held
	 * by this group to be modified. This change is physically reflected by
	 * appending the filter expression string to the name of this group.
	 */
	public void filterBy(Predicate<DataReference> predicate, boolean selecting)
	{
		_referenceList = (ArrayList<DataReference>) CollectionUtils.filter(Collections.unmodifiableList(_referenceList),
				predicate, selecting);
		this._name = this._name + "<" + predicate + "[" + selecting + "]>";
	}

	/**
	 *
	 */
	public DataReference[] find(String[] pathParts)
	{
		// initialize group...
		getNumberOfDataReferences();
		ArrayList<DataReference> result = new ArrayList<DataReference>();
		PathPartsFilter predicate = new PathPartsFilter(pathParts);
		for(DataReference r : _referenceList)
		{
			if(predicate.apply(r))
			{
				result.add(r);
			}
		}
		DataReference[] refs = new DataReference[result.size()];
		return result.toArray(refs);
	}

	/**
	 *
	 */
	public void reload()
	{
	}

	/**
	 * name of this group.
	 */
	public String toString()
	{
		return _name;
	}

	/**
	 * for python script adding: allows g1+g2 kind of expressions
	 */
	public Group __add__(Group g)
	{
		return unionWith(g);
	}

	/**
	 *
	 */
	public Group __concat__(Group g)
	{
		return __add__(g);
	}

	/**
	 *
	 */
	public int __len__()
	{
		return getNumberOfDataReferences();
	}

	/**
	 *
	 */
	public DataReference __getitem__(int i)
	{
		DataReference ref = getDataReference(i);
		if(ref != null)
		{
			return getDataReference(i);
		}
		else
		{
			throw Py.IndexError("index out of range: " + i);
		}
	}

	/**
	 *
	 */
	public DataReference[] __getslice__(int i, int j)
	{
		int bi, ei;
		if(i < 0)
		{
			i = getNumberOfDataReferences() + i;
		}
		if(j < 0)
		{
			j = getNumberOfDataReferences() + j;
		}
		bi = Math.min(i, j);
		ei = Math.max(i, j);
		ei = Math.min(ei, getNumberOfDataReferences() - 1);
		DataReference[] refArray = new DataReference[ei - bi + 1];
		// initialize group
		getDataReference(0);
		//
		for(int k = 0; k < refArray.length; k++)
		{
			refArray[k] = _referenceList.get(k + bi);
		}
		return refArray;
	}

	/**
	 *
	 */
	public void __setslice__(int i, int j, DataReference[] array)
	{
		int bi, ei;
		if(i < 0)
		{
			i = getNumberOfDataReferences() + i;
		}
		if(j < 0)
		{
			j = getNumberOfDataReferences() + j;
		}
		bi = Math.min(i, j);
		ei = Math.max(i, j);
		// initialize group
		getDataReference(0);
		//
		for(int k = 0; k < array.length; k++)
		{
			_referenceList.add(k + bi, array[k]);
		}
	}

	/**
	 *
	 */
	public void __setitem__(int i, DataReference ref)
	{
		insertDataReferenceAt(i, ref);
	}

	/**
	 *
	 */
	public void __delitem__(int i)
	{
		removeDataReference(i, i);
	}

	/**
	 *
	 */
	public void __delslice__(int i, int j)
	{
		removeDataReference(i, j);
	}

	@Override
	public int compareTo(Group o)
	{
		return this.getName().compareTo(o.getName());
	}
}
