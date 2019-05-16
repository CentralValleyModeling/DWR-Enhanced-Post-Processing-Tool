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
package vista.app.schematic;

/**
 *
 */
class Channel extends Link
{
	/**
	 *
	 */
	public static final int UPNODE_INDEX = 0;
	/**
	 *
	 */
	public static final int DOWNNODE_INDEX = 1;
	/**
	 *
	 */
	protected float _length;

	/**
	 *
	 */
	public Channel(int id, Node[] nodes, float length)
	{
		super(id, nodes);
		_length = length;
	}

	/**
	 *
	 */
	public String toString()
	{
		StringBuffer buf = new StringBuffer("Channel ");
		String eol = System.getProperty("line.separator");
		buf.append("Id: ").append(getId()).append(" Length: ").append(getLength()).append(eol);
		return buf.toString();
	}

	/**
	 *
	 */
	public float getLength()
	{
		return _length;
	}
}
