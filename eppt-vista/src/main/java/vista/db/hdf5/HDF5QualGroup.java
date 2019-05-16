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

import java.util.ArrayList;
import java.util.List;
import java.util.Vector;

import ncsa.hdf.object.Attribute;
import ncsa.hdf.object.CompoundDS;
import ncsa.hdf.object.HObject;
import ncsa.hdf.object.h5.H5File;
import ncsa.hdf.object.h5.H5ScalarDS;
import vista.set.DataReference;
import vista.set.Group;
import vista.set.GroupProxy;
import vista.set.Pathname;
import vista.time.Time;
import vista.time.TimeFactory;
import vista.time.TimeInterval;
import vista.time.TimeWindow;

import hec.heclib.util.Heclib;

/**
 * Reads assuming a hydro tidefile structure.
 *
 * @author psandhu
 */
@SuppressWarnings("serial")
public class HDF5QualGroup extends GroupProxy
{

	private String file;

	public HDF5QualGroup(String file)
	{
		this.file = file;
		setName(file + "::");
	}

	@SuppressWarnings("unchecked")
	@Override
	protected Group getInitializedGroup()
	{
		H5File h5file = new H5File(file);
		ArrayList<DataReference> references = new ArrayList<DataReference>();
		try
		{
			h5file.open();
			readInput(h5file);

			// run meta data
			HObject hydroObject = h5file
					.get("/output/channel avg concentration");
			if(hydroObject == null)
			{
				return Group.createGroup(getName(), new DataReference[]{});
			}
			H5ScalarDS scalar = (H5ScalarDS) hydroObject;
			List metadata = hydroObject.getMetadata();
			int numberOfIntervals = (int) scalar.getDims()[0];
			Time startTime = null;
			TimeInterval timeInterval = null;
			String modelRun = "";
			for(Object meta : metadata)
			{
				Attribute attr = (Attribute) meta;
				if(attr.getName().equals("start_time"))
				{
					String timeStr = ((String[]) attr.getValue())[0];
					startTime = TimeFactory.getInstance().createTime(timeStr,
							"yyyy-MM-dd HH:mm:ss");
				}
				if(attr.getName().equals("interval"))
				{
					String tistr = ((String[]) attr.getValue())[0];
					//FIXME: workaround for bug in qual tidefile 
					if(tistr.toLowerCase().endsWith("m"))
					{
						tistr += "in";
					}

					timeInterval = TimeFactory.getInstance()
											  .createTimeInterval(tistr);
					int[] status = new int[]{0};
					int mins = (int) timeInterval.getIntervalInMinutes(null);
					String intervalAsString = Heclib.getEPartFromInterval(mins, status);
					timeInterval = TimeFactory.getInstance()
											  .createTimeInterval(intervalAsString);
				}
				if(attr.getName().equals("model"))
				{
					modelRun = ((String[]) attr.getValue())[0];
				}
			}
			//
			modelRun = getEnvar("DSM2MODIFIER", h5file);
			//
			if(startTime == null || timeInterval == null
					|| numberOfIntervals == 0)
			{
				throw new RuntimeException(
						"start time, time interval or number of intervals is not defined!");
			}
			Time endTime = startTime.create(startTime);
			endTime.incrementBy(timeInterval, numberOfIntervals - 1);
			TimeWindow timeWindow = TimeFactory.getInstance().createTimeWindow(
					startTime, endTime);
			// references for channel flow
			HObject hObject = h5file.get("/output/channel_number");
			if(hObject instanceof H5ScalarDS)
			{
				H5ScalarDS channelds = (H5ScalarDS) hObject;
				Object data = channelds.getData();
				if(data instanceof int[])
				{
					int[] channelArray = (int[]) data;
					for(int i = 0; i < channelArray.length; i++)
					{
						Pathname pathname = Pathname
								.createPathname(new String[]{"qual",
										channelArray[i] + "",
										"avg conc", timeWindow.toString(),
										timeInterval.toString(), modelRun});
						DataReference flow = new HDF5DataReference(
								file, "/output/channel avg concentration", i,
								0, timeWindow.create(), timeInterval
								.create(timeInterval), pathname);
						references.add(flow);
						// references for channel stage
						pathname = Pathname.createPathname(new String[]{
								"qual", channelArray[i] + "_upstream", "conc",
								timeWindow.toString(), timeInterval.toString(),
								modelRun});
						references.add(new HDF5DataReference(file,
								"output/channel concentration", i, 0,
								timeWindow.create(), timeInterval
								.create(timeInterval), pathname));
						pathname = Pathname.createPathname(new String[]{
								"qual", channelArray[i] + "_downstream",
								"conc", timeWindow.toString(),
								timeInterval.toString(), modelRun});
						references.add(new HDF5DataReference(file,
								"/output/channel concentration", i, 1,
								timeWindow.create(), timeInterval
								.create(timeInterval), pathname));
					}
				}
			}
			HObject reservoirObject = h5file.get("/output/reservoir_names");
			if(reservoirObject instanceof H5ScalarDS)
			{
				H5ScalarDS reservoirIds = (H5ScalarDS) reservoirObject;
				Object data = reservoirIds.getData();// for initializing
				// now add references for stage and flow
				String[] reservoirNames = (String[]) data;
				for(int i = 0; i < reservoirNames.length; i++)
				{
					// references for reservoir stage
					Pathname pathname = Pathname.createPathname(new String[]{
							"qual", reservoirNames[i], "conc",
							timeWindow.toString(), timeInterval.toString(),
							modelRun});
					references.add(new HDF5DataReference(file,
							"/output/reservoir concentration", i, 0,
							timeWindow, timeInterval, pathname));
				}
			}
		}
		catch(Exception e)
		{
			e.printStackTrace();
		}
		DataReference[] refs = new DataReference[references.size()];
		refs = references.toArray(refs);
		return Group.createGroup(this.file, refs);
	}

	private void readInput(H5File h5file)
	{
		// TODO Auto-generated method stub

	}

	private String getEnvar(String varName, H5File h5file) throws Exception
	{
		CompoundDS envarTable = (CompoundDS) h5file.get("/input/envvar");
		Vector columns = (Vector) envarTable.getData();
		String[] names = (String[]) columns.get(0);
		String[] values = (String[]) columns.get(1);
		for(int i = 0; i < names.length; i++)
		{
			if(varName.equals(names[i]))
			{
				return values[i];
			}
		}
		return "N.A.";
	}

	private int findIndexOfMemberName(String string, CompoundDS channelds)
	{
		String[] memberNames = channelds.getMemberNames();
		for(int i = 0; i < memberNames.length; i++)
		{
			if(string.equals(memberNames[i]))
			{
				return i;
			}
		}
		return -1;
	}

}
