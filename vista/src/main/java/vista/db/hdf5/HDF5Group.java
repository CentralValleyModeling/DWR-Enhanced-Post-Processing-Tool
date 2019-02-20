package vista.db.hdf5;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Vector;

import ncsa.hdf.object.Attribute;
import ncsa.hdf.object.CompoundDS;
import ncsa.hdf.object.HObject;
import ncsa.hdf.object.h5.H5File;
import vista.set.DataReference;
import vista.set.DataReferenceMath;
import vista.set.DataReferenceScalarMathProxy;
import vista.set.DataReferenceVectorMathProxy;
import vista.set.Group;
import vista.set.GroupProxy;
import vista.set.Pathname;
import vista.time.Time;
import vista.time.TimeFactory;
import vista.time.TimeInterval;
import vista.time.TimeWindow;

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
