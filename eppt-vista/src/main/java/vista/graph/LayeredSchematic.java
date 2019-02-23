/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */
package vista.graph;


public class LayeredSchematic extends GEContainer {
	/**
	 * creates a layered schematic with the first schematic in the lowest layer
	 * followed by the next one on top and so on
	 */
	public LayeredSchematic(Schematic[] schematics) {
		super(new GEAttr());
		this.setLayout(new GEOverlayLayout());
		for (int i = 0; i < schematics.length; i++)
			this.add(schematics[i]);
	}
}
