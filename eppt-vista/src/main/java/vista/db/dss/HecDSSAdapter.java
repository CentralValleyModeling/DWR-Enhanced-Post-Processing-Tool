/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */

package vista.db.dss;

import vista.set.DataReference;
import vista.set.DataSet;
import vista.set.IrregularTimeSeries;
import vista.set.Pathname;
import vista.set.RegularTimeSeries;
import vista.time.Time;
import vista.time.TimeFactory;

import hec.io.TimeSeriesContainer;

public class HecDSSAdapter {
	public static TimeSeriesContainer createContainerFrom(DataReference ref){
		DataSet dataSet = ref.getData();
		TimeSeriesContainer container = new TimeSeriesContainer();
		container.fileName=ref.getFilename();
		container.fullName=ref.getPathname().toString();
		container.location=ref.getPathname().getPart(Pathname.B_PART);
		container.parameter=ref.getPathname().getPart(Pathname.C_PART);
		container.units=dataSet.getAttributes().getYUnits();
		container.version=ref.getPathname().getPart(Pathname.F_PART);
		if (dataSet instanceof RegularTimeSeries){
			RegularTimeSeries rts = (RegularTimeSeries) dataSet;
			container.values = rts.getYArray();
			container.startTime=(int) rts.getStartTime().getTimeInMinutes();
			container.numberValues=container.values.length;
			container.endTime = (int) rts.getEndTime().getTimeInMinutes();
			container.interval=(int) rts.getTimeInterval().getIntervalInMinutes(TimeFactory.getInstance().createTime(0));
			double[] times = new double[container.numberValues];
			container.times = new int[times.length];
			Time startTime = rts.getStartTime().create(rts.getStartTime());
			for(int i=0; i < times.length; i++){
				container.times[i] = (int) startTime.getTimeInMinutes();
				startTime = startTime.__add__(rts.getTimeInterval());
			}
		} else if (dataSet instanceof IrregularTimeSeries){
			IrregularTimeSeries its = (IrregularTimeSeries) dataSet;
			container.values = its.getYArray();
			double[] times = its.getXArray();
			container.times = new int[times.length];
			for(int i=0; i < times.length; i++){
				container.times[i] = (int) times[i];
			}
			container.numberValues=times.length;
		} else {
			throw new IllegalArgumentException("Can only convert regular and irregular time series to HEC time series container");
		}
		
		return container;
	}
}
