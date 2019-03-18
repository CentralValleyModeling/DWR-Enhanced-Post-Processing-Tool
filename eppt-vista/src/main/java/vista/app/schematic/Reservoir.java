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
class Reservoir extends Link {
	/**
   *
   */
	public Reservoir(int id, Node[] nodes) {
		super(id, nodes);
	}

	/**
   *
   */
	public String toString() {
		StringBuffer buf = new StringBuffer("Reservoir ");
		String eol = System.getProperty("line.separator");
		buf.append("Id: ").append(getId()).append(eol);
		return buf.toString();
	}
}
