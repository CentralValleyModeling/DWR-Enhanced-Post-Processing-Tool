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

package vista.db.hdf5;

import vista.set.Group;
import vista.set.GroupProxy;

/**
 * Reads assuming a hydro tidefile structure.
 * 
 * @author psandhu
 * 
 */
@SuppressWarnings("serial")
public class HDF5Group extends GroupProxy {

	private String file;
	
	public HDF5Group(String file) {
		this.file = file;
		setName(file);
	}

	@SuppressWarnings("unchecked")
	@Override
	protected Group getInitializedGroup() {
		// try for hydro group first and then qual group
		HDF5HydroGroup hdf5HydroGroup = new HDF5HydroGroup(file);
		Group hydroGroup = hdf5HydroGroup.getInitializedGroup();
		if (hydroGroup == null){
			return new HDF5QualGroup(file).getInitializedGroup();
		} else {
			return hydroGroup;
		}
	}
}
