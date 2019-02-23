/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */
package vista.set;

import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.TreeSet;
import java.util.zip.GZIPInputStream;
import java.util.zip.GZIPOutputStream;

/**
 * A session is an aggregate of groups.
 */
@SuppressWarnings("serial")
public class Session implements GroupManager, Serializable
{
	/**
	 * The session name.
	 */
	private String _name;
	/**
	 * A list of groups in this session
	 */
	private List<Group> _groupList;
	/**
	 *
	 */
	private String _filename;

	/**
	 * creates an empty session with a blank name
	 */
	public Session()
	{
		_name = "";
		_groupList = new ArrayList<Group>();
	}

	/**
	 * creates a session with a name and list of groups
	 */
	public Session(String name, List<Group> groupList)
	{
		if(name != null)
		{
			_name = name;
		}
		if(groupList != null)
		{
			_groupList = groupList;
		}
	}

	/**
	 * Creates a saved session from the file. Not implemented yet...
	 */
	private static Session createSession(String sessionFile)
	{
		return new Session();
	}

	/**
	 * creates a session with a name and a group list
	 */
	public static Session createSession(String name, List<Group> groupList)
	{
		Session s = new Session();
		if(name != null)
		{
			s._name = name;
		}
		if(groupList != null)
		{
			s._groupList = groupList;
		}
		return s;
	}

	/**
	 * creates a clone of this session
	 */
	public static Session createSession(Session s)
	{
		return (Session) s.clone();
	}

	/**
	 *
	 */
	public static Session create(String filename)
	{
		Session s = null;
		try
		{
			//
			FileInputStream istream = new FileInputStream(filename);
			ObjectInputStream pin = new ObjectInputStream(new GZIPInputStream(
					istream));
			s = (Session) pin.readObject();
		}
		catch(FileNotFoundException fnfe)
		{
			System.err.println(fnfe.getMessage());
			throw new IllegalArgumentException("File " + filename
					+ " not found");
		}
		catch(IOException ioe)
		{
			System.err.println(ioe.getMessage());
			throw new IllegalArgumentException("IO Exception while reading "
					+ filename);
		}
		catch(ClassNotFoundException cnfe)
		{
			System.err.println(cnfe.getMessage());
		}
		return s;
	}

	/**
	 * copies the groups from this session into specified one.
	 */
	public void copyInto(Session s)
	{
		ArrayList<Group> groupList = new ArrayList<Group>();
		for(Group g : s.getAllGroups())
		{
			groupList.add(g);
		}
		s._groupList = groupList;
	}

	/**
	 * a shallow copy of the name and list of groups
	 */
	public Object clone()
	{
		Session s = new Session();
		synchronized(this)
		{
			s._name = _name;
			s._groupList = new ArrayList<Group>(s._groupList);
		}
		return s;
	}

	/**
	 * Creates a union of this session with given session. This is basically
	 * combining the group(s) in both sessions in one session and returning it.
	 */
	public Session createUnion(Session s)
	{
		if(s == null)
		{
			return (Session) this.clone();
		}
		String name;
		if(this._name.equals(s._name))
		{
			name = this._name;
		}
		else
		{
			name = this._name + " union " + s._name;
		}
		// make copies of reference list and sort by hash code
		TreeSet<Group> uniqueGroupList = new TreeSet<Group>();
		uniqueGroupList.addAll(_groupList);
		uniqueGroupList.addAll(s._groupList);
		//
		Session sessionUnion = new Session();
		sessionUnion._name = name;
		sessionUnion._groupList = new ArrayList<Group>(uniqueGroupList);
		return sessionUnion;
	}

	/**
	 * gets the name of this session
	 */
	public String getName()
	{
		return _name;
	}

	/**
	 * sets the name of this session
	 */
	public void setName(String str)
	{
		_name = str;
	}

	/**
	 * gets the Filename of this session
	 */
	public String getFilename()
	{
		return _filename;
	}

	/**
	 * sets the name of this session
	 */
	public void setFilename(String str)
	{
		_filename = str;
	}

	/**
	 * gets the number of groups in the list.
	 */
	public int getNumberOfGroups()
	{
		return _groupList.size();
	}

	/**
	 * adds group at specified index. Throws exception if such index is not
	 * available.
	 */
	public void insertGroupAt(int i, Group g)
	{
		if(!_groupList.contains(g))
		{
			_groupList.add(i, g);
		}
	}

	/**
	 * adds group if not already present
	 */
	public void addGroup(Group g)
	{
		if(!_groupList.contains(g))
		{
			_groupList.add(g);
		}
	}

	/**
	 * removes group from list.
	 */
	public void removeGroup(Group g)
	{
		_groupList.remove(g);
	}

	/**
	 * remove groups in the given range
	 */
	public void removeGroup(int index0, int index1)
	{
		if(index0 > index1)
		{
			int swap = index1;
			index1 = index0;
			index0 = swap;
		}
		for(int i = index1; i <= index0; i--)
		{
			_groupList.remove(i);
		}
	}

	/**
	 * gets the group by index
	 */
	public Group getGroup(int index)
	{
		if(index < _groupList.size())
		{
			return _groupList.get(index);
		}
		else
		{
			return null;
		}
	}

	/**
	 * gets group by name
	 */
	public Group getGroup(String groupName)
	{
		for(Group g : _groupList)
		{
			if(g.getName().equals(groupName))
			{
				return g;
			}
		}
		return null;
	}

	/**
	 * gets all the groups
	 */
	public Group[] getAllGroups()
	{
		Group[] groups = new Group[_groupList.size()];
		return _groupList.toArray(groups);
	}

	/**
	 * Sorts session by calling upon the SortAlgorithm to return an array of
	 * groups by some criteria. The group list is being directly manipulated by
	 * the sorting mechanism.
	 */
	public void sortBy(Comparator<Group> comparator)
	{
		Collections.sort(_groupList, comparator);
	}

	/**
	 * saves session to its file
	 */
	public void save()
	{
		if(_filename != null)
		{
			saveTo(_filename);
		}
	}

	/**
	 * saves to filename ands sets the filename for this session. All further
	 * saves will be to that file if filename is not specified.
	 */
	public void saveTo(String filename)
	{
		// save file
		try
		{
			setFilename(filename);
			FileOutputStream ostream = new FileOutputStream(filename);
			ObjectOutputStream pout = new ObjectOutputStream(
					new GZIPOutputStream(ostream));
			pout.writeObject(this);
			pout.flush();
			pout.close();
			ostream.close();
			_filename = filename;
		}
		catch(IOException ioe)
		{
			ioe.printStackTrace();
		}
	}
}
