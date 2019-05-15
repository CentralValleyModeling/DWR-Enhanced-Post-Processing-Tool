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
package vista.set;

import vista.time.TimeInterval;
import vista.time.TimeWindow;

/**
 * A proxy for vector math operations on data references. This proxy is formed
 * by vector math operations on data references. This can be thought of as a
 * reference to a math operation on those data references. Only when the data
 * for this reference is requested does it attempt to retrieve the data from its
 * data references and perform the operation.
 *
 * @author Nicky Sandhu
 * @version $Id: DataReferenceVectorMathProxy.java,v 1.1 2003/10/02 20:49:20
 * redwood Exp $
 */
public class DataReferenceVectorMathProxy extends DataReference
{
	/**
	 *
	 */
	private static final boolean DEBUG = false;
	/**
	 * the data set created by the math operation
	 */
	private transient DataSet _dataSet;
	/**
	 * reference 1
	 */
	private DataReference _ref1;
	/**
	 * reference 2
	 */
	private DataReference _ref2;
	/**
	 * the operation to be performed
	 */
	private int _operationId;
	/**
	 *
	 */
	private String _units;

	/**
	 *
	 */
	public DataReferenceVectorMathProxy(DataReference ref1, DataReference ref2,
										int operationId)
	{
		// check input
		checkInput(ref1, ref2, operationId);
		setOperationId(operationId);
		// choose max interval
		super.setTimeInterval(getProxyTimeInterval(ref1, ref2));
		// set state variables
		super.setName(getProxyName(ref1, ref2));
		super.setServername(getProxyServerName(ref1, ref2));
		super.setFilename(getProxyFileName(ref1, ref2));
		super.setPathname(getProxyPathname(ref1, ref2));
		super.setTimeWindow(getProxyTimeWindow(ref1, ref2));
		//
		setProxyReference(ref1, ref2);
	}

	/**
	 * returns the name of the proxy
	 */
	protected String getProxyName(DataReference ref1, DataReference ref2)
	{
		StringBuffer buf = new StringBuffer(400);
		buf.append(ref1.getName()).append(getOperationName()).append(
				ref2.getName());
		return buf.toString();
	}

	/**
	 * returns the proxy server name if any
	 */
	protected String getProxyServerName(DataReference ref1, DataReference ref2)
	{
		return "";
	}

	/**
	 *
	 */
	protected String getProxyFileName(DataReference ref1, DataReference ref2)
	{
		return "";
	}

	/**
	 *
	 */
	protected Pathname getProxyPathname(DataReference ref1, DataReference ref2)
	{
		Pathname p1 = ref1.getPathname();
		Pathname p2 = ref2.getPathname();
		String[] parts = new String[Pathname.MAX_PARTS];
		for(int i = 0; i < parts.length; i++)
		{
			String part1 = p1.getPart(i).trim();
			String part2 = p2.getPart(i).trim();
			if(part1.equals(part2) && i != Pathname.B_PART)
			{
				parts[i] = part1;
			}
			else
			{
				parts[i] = part1 + getOperationName() + part2;
				parts[i] = parts[i].substring(0, Math.min(64, parts[i].length()));
			}
		}
		parts[Pathname.E_PART] = getTimeInterval().getIntervalAsString();
		return Pathname.createPathname(parts);
	}

	/**
	 *
	 */
	protected TimeWindow getProxyTimeWindow(DataReference ref1,
											DataReference ref2)
	{
		return ref1.getTimeWindow().intersection(ref2.getTimeWindow());
	}

	/**
	 *
	 */
	protected String getOperationName()
	{
		switch(_operationId)
		{
			case DataReferenceMath.ADD:
				return " + ";
			case DataReferenceMath.MUL:
				return " * ";
			case DataReferenceMath.SUB:
				return " - ";
			case DataReferenceMath.DIV:
				return " div ";
			default:
				throw new IllegalArgumentException("Invalid operation id: "
						+ _operationId);
		}
	}

	/**
	 *
	 */
	protected TimeInterval getProxyTimeInterval(DataReference ref1,
												DataReference ref2)
	{
		TimeInterval ti = ref1.getTimeInterval();
		if(ti.compare(ref2.getTimeInterval()) < 0)
		{
			ti = ref2.getTimeInterval();
		}
		if(DEBUG)
		{
			System.out.println("Proxy time interval: " + ti);
		}
		return ti.create(ti);
	}

	/**
	 *
	 */
	protected void setProxyReference(DataReference ref1, DataReference ref2)
	{
		TimeWindow tw = getTimeWindow();
		TimeInterval ti = getTimeInterval();
		// time window
		if(tw.equals(ref1.getTimeWindow()))
		{
			_ref1 = ref1;
		}
		else
		{
			_ref1 = DataReference.create(ref1, tw);
		}
		if(tw.equals(ref2.getTimeWindow()))
		{
			_ref2 = ref2;
		}
		else
		{
			_ref2 = DataReference.create(ref2, tw);
		}
		// time interval
		if(ti.compare(_ref1.getTimeInterval()) != 0)
		{
			_ref1 = ProxyFactory.createPeriodOperationProxy(
					ProxyFactory.PERIOD_AVERAGE, _ref1, ti);
		}
		if(ti.compare(_ref2.getTimeInterval()) != 0)
		{
			_ref2 = ProxyFactory.createPeriodOperationProxy(
					ProxyFactory.PERIOD_AVERAGE, _ref2, ti);
		}
	}

	/**
	 *
	 */
	private void setOperationId(int operationId)
	{
		_operationId = operationId;
	}

	/**
	 *
	 */
	protected void checkInput(DataReference ref1, DataReference ref2,
							  int operationId)
	{
		if(ref1 == null || ref2 == null)
		{
			throw new IllegalArgumentException(
					" ? one of the references is null");
		}
		//
		switch(operationId)
		{
			case DataReferenceMath.ADD:
				break;
			case DataReferenceMath.MUL:
				break;
			case DataReferenceMath.SUB:
				break;
			case DataReferenceMath.DIV:
				break;
			default:
				throw new IllegalArgumentException("Invalid operation id");
		}
		if(ref1.getPathname().getPart(Pathname.E_PART).indexOf("IR-") >= 0)
		{
			throw new IllegalArgumentException(
					"Hmmm...one of the references for proxy is irregular");
		}
		if(ref2.getPathname().getPart(Pathname.E_PART).indexOf("IR-") >= 0)
		{
			throw new IllegalArgumentException(
					"Hmmm...one of the references for proxy is irregular");
		}
		// check time window
		if(ref1.getTimeWindow() == null)
		{
			throw new IllegalArgumentException("Hey! no time window on " + ref1);
		}
		if(ref2.getTimeWindow() == null)
		{
			throw new IllegalArgumentException("Hey! no time window on " + ref2);
		}
		// check time window intersection
		if(!(ref1.getTimeWindow().intersects(ref2.getTimeWindow())))
		{
			throw new IllegalArgumentException(
					"? No common time window on references");
		}
		// check time intervals
		if(ref1.getTimeInterval() == null || ref2.getTimeInterval() == null)
		{
			throw new IllegalArgumentException("? Time Interval is null");
		}
	}

	/**
	 *
	 */
	protected void setTimeWindow(TimeWindow tw)
	{
		super.setTimeWindow(tw);
		_ref1 = DataReference.create(_ref1, getTimeWindow());
		_ref2 = DataReference.create(_ref2, getTimeWindow());
	}

	public void setUnits(String units)
	{
		_units = units;
	}

	/**
	 * create a clone of itself
	 */
	public DataReference createClone()
	{
		return new DataReferenceVectorMathProxy(_ref1, _ref2, _operationId);
	}

	/**
	 *
	 */
	public void reloadData()
	{
		_dataSet = null;
		_ref1.reloadData();
		_ref2.reloadData();
	}

	/**
	 * returns data after initialation and operation...
	 */
	public DataSet getData() throws DataRetrievalException
	{
		if(_dataSet == null)
		{
			if(DEBUG)
			{
				System.out.println("Getting data for Reference 1: " + _ref1);
			}
			DataSet d1 = _ref1.getData();
			if(DEBUG)
			{
				System.out.println("Getting data for Reference 1: " + _ref2);
			}
			DataSet d2 = _ref2.getData();
			_dataSet = VectorMath.doMathOperation(d1, d2, _operationId);
			if(_dataSet != null && _units != null)
			{
				_dataSet.getAttributes().setYUnits(_units);
			}
		}
		// System.out.println(_dataSet);
		return _dataSet;
	}
}
