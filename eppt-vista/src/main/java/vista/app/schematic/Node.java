/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
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
