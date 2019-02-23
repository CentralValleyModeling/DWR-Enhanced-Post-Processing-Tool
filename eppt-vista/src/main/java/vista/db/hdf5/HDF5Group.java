/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
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
