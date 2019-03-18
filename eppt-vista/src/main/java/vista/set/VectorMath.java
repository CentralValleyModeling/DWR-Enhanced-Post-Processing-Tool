/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */
package vista.set;


/**
 * This class defines vector operations and vector - scalar operation on data
 * sets. Most data sets can have vector-scalar operations defined quite easily
 * however some assumptions have to be made when doing vector-vector operations.
 * One requirement is that for non-indexed data sets the values If value is
 * missing the sum has a missing value as well for that data set element
 * 
 */
public class VectorMath {
	/**
	 * does math operation defined by operation id on the two data sets d1 and
	 * d2;
	 */
	public static DataSet doMathOperation(DataSet d1, DataSet d2,
			int operationId) {
		check(d1, d2);
		RegularTimeSeries ts1 = (RegularTimeSeries) d1;
		RegularTimeSeries ts2 = (RegularTimeSeries) d2;
		switch (operationId) {
		case DataReferenceMath.ADD:
			return TimeSeriesMath.doBinaryOperation(ts1, ts2,
					TimeSeriesMath.ADD);
		case DataReferenceMath.MUL:
			return TimeSeriesMath.doBinaryOperation(ts1, ts2,
					TimeSeriesMath.MUL);
		case DataReferenceMath.SUB:
			return TimeSeriesMath.doBinaryOperation(ts1, ts2,
					TimeSeriesMath.SUB);
		case DataReferenceMath.DIV:
			return TimeSeriesMath.doBinaryOperation(ts1, ts2,
					TimeSeriesMath.DIV);
		default:
			return null;
		}
	}

	/**
	 * does a vector and scalar operation
	 */
	public static DataSet doMathOperation(DataSet d1, double scalar,
			int operationId, boolean reverseArgs) {
		check(d1);
		TimeSeries ts = (TimeSeries) d1;
		switch (operationId) {
		case DataReferenceMath.ADD:
			return TimeSeriesMath.doBinaryOperation(ts, scalar,
					TimeSeriesMath.ADD, reverseArgs);
		case DataReferenceMath.MUL:
			return TimeSeriesMath.doBinaryOperation(ts, scalar,
					TimeSeriesMath.MUL, reverseArgs);
		case DataReferenceMath.SUB:
			return TimeSeriesMath.doBinaryOperation(ts, scalar,
					TimeSeriesMath.SUB, reverseArgs);
		case DataReferenceMath.DIV:
			return TimeSeriesMath.doBinaryOperation(ts, scalar,
					TimeSeriesMath.DIV, reverseArgs);
		default:
			return null;
		}
	}

	/**
	 * check viability of operation on data sets
	 */
	public static void check(DataSet d1, DataSet d2) {
		check(d1);
		check(d2);
		if (!(d1 instanceof RegularTimeSeries)
				&& !(d2 instanceof RegularTimeSeries)) {
			throw new IllegalArgumentException(
					"Math defined only between regular time series");
		} else {
			return;
		}
	}
	
	public static void check(DataSet d){
		if (d == null) 
			throw new IllegalArgumentException("Data Set may be null");
		if (!(d instanceof TimeSeries)){
			throw new IllegalArgumentException("Data set needs to be a time series");
		}
	}
	
}
