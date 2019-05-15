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
public class Node {
	/**
   *
   */
	public Node(int id, float x, float y) {
		_id = id;
		_x = x;
		_y = y;
	}

	/**
   *
   */
	public int getId() {
		return _id;
	}

	/**
   *
   */
	public float getX() {
		return _x;
	}

	/**
   *
   */
	public float getY() {
		return _y;
	}

	/**
   *
   */
	public String toString() {
		StringBuffer buf = new StringBuffer("Node #");
		String eol = System.getProperty("line.separator");
		buf.append(getId()).append(eol);
		return buf.toString();
	}

	/**
   *
   */
	protected int _id;
	/**
   *
   */
	protected float _x;
	/**
   *
   */
	protected float _y;
}
