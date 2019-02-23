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
public class Link {
	/**
   *
   */
	public Link(int id, Node[] nodes) {
		_id = id;
		_nodes = nodes;
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
	public int getNumberOfNodes() {
		return _nodes.length;
	}

	/**
   *
   */
	public Node getNode(int localIndex) {
		return _nodes[localIndex];
	}

	/**
   *
   */
	public String toString() {
		StringBuffer buf = new StringBuffer("Link #");
		String eol = System.getProperty("line.separator");
		buf.append(getId()).append(eol);
		return buf.toString();
	}

	/**
   *
   */
	protected Node[] _nodes;
	/**
   *
   */
	protected int _id;
}
