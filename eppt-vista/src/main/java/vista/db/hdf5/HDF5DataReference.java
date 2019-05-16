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

import java.lang.ref.WeakReference;
import java.util.List;

import ncsa.hdf.hdf5lib.exceptions.HDF5Exception;
import ncsa.hdf.object.Attribute;
import ncsa.hdf.object.HObject;
import ncsa.hdf.object.h5.H5File;
import ncsa.hdf.object.h5.H5ScalarDS;
import vista.set.DataReference;
import vista.set.DataSet;
import vista.set.DataSetAttr;
import vista.set.DataType;
import vista.set.Pathname;
import vista.set.RegularTimeSeries;
import vista.time.Time;
import vista.time.TimeFactory;
import vista.time.TimeInterval;
import vista.time.TimeWindow;

import hec.heclib.util.Heclib;

/**
 * A data reference implementation to the hdf5 file format FIXME: Assumes
 * regular time series data for now
 *
 * @author Nicky Sandhu
 */
@SuppressWarnings("serial")
public class HDF5DataReference extends DataReference
{
	private String path;
	private transient WeakReference<DataSet> dataset;
	private int locationNumber;
	private int nodePosition;

	/**
	 * A reference to the location of the data
	 *
	 * @param file is the operating system file location
	 * @param path is the path to the data. This is of the form
	 *             /parent/child/dataset
	 */
	public HDF5DataReference(String file, String path)
	{
		this(file, path, 0, 0);
	}

	public HDF5DataReference(String file, String path, int locationNumber,
							 int nodePosition)
	{
		this(file, path, locationNumber, nodePosition, TimeFactory
				.getInstance().createTimeWindow(
						"01JAN1900 0000 - 01JAN2300 2400"), TimeFactory
				.getInstance().createTimeInterval("15MIN"), null);
	}

	public HDF5DataReference(String file, String path, int locationNumber,
							 int nodePosition, TimeWindow tw, TimeInterval ti, Pathname pathname)
	{
		setFilename(file);
		setServername("local");
		this.path = path;
		this.locationNumber = locationNumber;
		this.nodePosition = nodePosition;
		setTimeWindow(tw);
		setTimeInterval(ti);
		setPathname(pathname);
		setName(file + "::" + pathname);
	}

	@Override
	protected DataReference createClone()
	{
		return new HDF5DataReference(getFilename(), this.path,
				this.locationNumber, this.nodePosition, getTimeWindow(),
				getTimeInterval(), getPathname());
	}

	@SuppressWarnings("unchecked")
	@Override
	public DataSet getData()
	{
		if(dataset == null || dataset.get() == null)
		{
			String file = getFilename();
			H5File h5file = new H5File(file);
			try
			{
				h5file.open();
				HObject hObject = h5file.get(path);
				if(!(hObject instanceof H5ScalarDS))
				{
					throw new IllegalArgumentException("Path: " + path
							+ " in HDF5 file: " + file
							+ " is not a scalar dataset");
				}
				//
				H5ScalarDS ds = (H5ScalarDS) hObject;
				// initialize the dim arrays
				List<Attribute> attributes = ds.getMetadata();
				Time startTimeString = null;
				TimeInterval timeInterval = null;
				String modelName = "";
				String modelVersion = "";
				for(Attribute attribute : attributes)
				{
					String name = attribute.getName();
					Object value = attribute.getValue();
					if(name.equals("CLASS"))
					{
					}
					else if(name.equals("start_time"))
					{
						String tmstr = ((String[]) value)[0];
						startTimeString = TimeFactory.getInstance().createTime(
								tmstr, "yyyy-MM-dd HH:mm:ss");
					}
					else if(name.equals("interval"))
					{
						String intervalAsString = ((String[]) value)[0];
						//FIXME: workaround for bug in qual tidefile 
						if(intervalAsString.toLowerCase().endsWith("m"))
						{
							intervalAsString += "in";
						}
						int intervalInMinutes = (int) TimeFactory.getInstance().createTimeInterval(intervalAsString).getIntervalInMinutes(null);
						int[] status = new int[]{0};
						intervalAsString = Heclib.getEPartFromInterval(intervalInMinutes, status);
						timeInterval = TimeFactory.getInstance()
												  .createTimeInterval(intervalAsString);
					}
					else if(name.equals("model"))
					{
						modelName = ((String[]) value)[0];
					}
					else if(name.equals(""))
					{
						modelVersion = ((String[]) value)[0];
					}
				}
				Time dataStartTime = TimeFactory.getInstance().createTime(
						startTimeString);
				if(timeInterval == null)
				{
					setTimeInterval(timeInterval);
				}
				//
				long[] startDims = ds.getStartDims();
				long[] stride = ds.getStride();
				long[] selectedDims = ds.getSelectedDims();
				long[] dims = ds.getDims();
				// FIXME: startDims[0]=startTimeIndex
				if(getTimeWindow() != null)
				{
					Time startTime = getTimeWindow().getStartTime();
					startDims[0] = Math.max(0, dataStartTime
							.getExactNumberOfIntervalsTo(startTime,
									getTimeInterval()));
				}
				else
				{
					startDims[0] = 0;
				}
				if(modelName.equalsIgnoreCase("qual"))
				{

				}
				if(startDims.length == 4)
				{
					startDims[2] = locationNumber;
					startDims[3] = nodePosition;
				}
				else if(startDims.length == 3)
				{
					if(modelName.equalsIgnoreCase("qual"))
					{
						startDims[2] = locationNumber;
					}
					else
					{
						startDims[1] = locationNumber;
						startDims[2] = nodePosition;
					}
				}
				else
				{
					startDims[1] = locationNumber;
				}
				//
				stride[0] = 1;
				stride[1] = 1;
				if(stride.length > 2)
				{
					stride[2] = 1;
				}
				//
				// FIXME: set this to startIndex+numberof steps from desired
				// time window
				if(getTimeWindow() != null)
				{
					Time startTime = getTimeWindow().getStartTime();
					Time endTime = getTimeWindow().getEndTime();
					selectedDims[0] = Math.min(startTime
							.getExactNumberOfIntervalsTo(endTime,
									getTimeInterval()) + 1, dims[0]);
				}
				else
				{
					selectedDims[0] = dims[0];
				}
				selectedDims[1] = 1;
				if(selectedDims.length > 2)
				{
					selectedDims[2] = 1;
				}
				if(selectedDims.length > 3)
				{
					selectedDims[3] = 1;
				}
				// if channel get elevation at node position
				double bottomElevation = 0.0;
				if(path.contains("channel stage"))
				{
					bottomElevation = getBottomElevation(h5file,
							locationNumber, nodePosition);
				}
				//
				Object rawData = ds.read();
				if(!(rawData != null && rawData instanceof float[]))
				{
					throw new IllegalArgumentException("Path: " + path
							+ " in HDF5 file: " + file
							+ " is either null or not a floating point array");
				}
				// FIXME: data sets should be able to hold floats?
				float[] fData = (float[]) rawData;
				double[] dData = new double[fData.length];
				for(int i = 0; i < fData.length; i++)
				{
					dData[i] = fData[i] + bottomElevation;
				}

				if(getTimeWindow() == null)
				{
					Time endTime = dataStartTime.create(dataStartTime);
					endTime.incrementBy(getTimeInterval(), dData.length - 1);
					setTimeWindow(TimeFactory.getInstance().createTimeWindow(
							dataStartTime, endTime));
				}

				String[] pathParts = this.path.split("/");
				String nodePositionName = this.nodePosition == 1 ? "LENGTH"
						: "0";
				if(getPathname() == null)
				{
					String name = "/hydro/" + this.locationNumber + "_"
							+ nodePositionName + "/"
							+ pathParts[pathParts.length - 1] + "//"
							+ timeInterval.toString() + "/" + modelName + "-"
							+ modelVersion + "/";
					setPathname(Pathname.createPathname(name));
				}
				String yUnits = getUnits(getPathname().getPart(Pathname.C_PART));
				DataSetAttr attr = new DataSetAttr(
						DataType.REGULAR_TIME_SERIES, "TIME", yUnits, "",
						"INST-VAL");
				RegularTimeSeries rts = new RegularTimeSeries(getPathname()
						.toString(), getTimeWindow().getStartTime(),
						timeInterval, dData, null, attr);
				dataset = new WeakReference<DataSet>(rts);
			}
			catch(Exception e)
			{
				e.printStackTrace();
			}
			finally
			{
				try
				{
					h5file.close();
				}
				catch(HDF5Exception e)
				{
				}
			}
		}
		return dataset.get();
	}

	private double getBottomElevation(H5File h5file, int locationNumber2,
									  int nodePosition2) throws Exception
	{
		HObject hObject = h5file.get("/hydro/geometry/channel_bottom");
		if(!(hObject instanceof H5ScalarDS))
		{
			throw new IllegalArgumentException("Path: " + path
					+ " in HDF5 file: " + h5file.getPath()
					+ " is not a scalar dataset");
		}
		//
		H5ScalarDS ds = (H5ScalarDS) hObject;
		// initialize the dim arrays
		List<Attribute> attributes = ds.getMetadata();
		//
		long[] startDims = ds.getStartDims();
		long[] stride = ds.getStride();
		long[] selectedDims = ds.getSelectedDims();
		long[] dims = ds.getDims();
		//
		startDims[0] = nodePosition2;
		startDims[1] = locationNumber2;
		//
		stride[0] = 1;
		stride[1] = 1;
		//
		selectedDims[0] = 1;
		selectedDims[1] = 1;
		//
		Object rawData = ds.read();
		if(!(rawData != null && rawData instanceof float[]))
		{
			throw new IllegalArgumentException("Path: " + path
					+ " in HDF5 file: " + h5file.getPath()
					+ " is either null or not a floating point array");
		}
		// FIXME: data sets should be able to hold floats?
		float[] fData = (float[]) rawData;
		return fData[0];
	}

	private String getUnits(String part)
	{
		if(part.equalsIgnoreCase("FLOW"))
		{
			return "CFS";
		}
		else if(part.equalsIgnoreCase("STAGE"))
		{
			return "FT";
		}
		else if(part.equalsIgnoreCase("AREA")
				|| part.equalsIgnoreCase("AVG_AREA"))
		{
			return "FT^2";
		}
		else if(part.equalsIgnoreCase("VOLUME"))
		{
			return "FT^3";
		}
		else
		{
			return "";
		}
	}

	@Override
	public void reloadData()
	{
		if(dataset != null)
		{
			dataset.clear();
		}
		dataset = null;
	}

	public String toString()
	{
		return "HDF5::" + getFilename() + "::" + path + "::" + locationNumber
				+ "@" + nodePosition + "::" + getTimeWindow() + "::"
				+ getTimeInterval() + "::" + getPathname();
	}

}
