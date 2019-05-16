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
package vista.graph;


public class LayeredSchematic extends GEContainer
{
	/**
	 * creates a layered schematic with the first schematic in the lowest layer
	 * followed by the next one on top and so on
	 */
	public LayeredSchematic(Schematic[] schematics)
	{
		super(new GEAttr());
		this.setLayout(new GEOverlayLayout());
		for(int i = 0; i < schematics.length; i++)
		{
			this.add(schematics[i]);
		}
	}
}
