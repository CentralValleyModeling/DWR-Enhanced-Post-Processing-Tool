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
 * A proxy for scalar operations on data reference
 *
 * @author Nicky Sandhu
 * @version $Id: DataReferenceScalarMathProxy.java,v 1.1 2003/10/02 20:49:20
 * redwood Exp $
 */
public class DataReferenceScalarMathProxy extends UnaryOperationProxy
{
	/**
	 * scalar
	 */
	private double _scalar;
	/**
	 *
	 */
	private int _operationId;
	/**
	 *
	 */
	private int _precedence;
	/**
	 *
	 */
	private String _units;

	/**
	 *
	 */
	public DataReferenceScalarMathProxy(DataReference ref, double scalar,
										int operationId, int argumentPrecedence)
	{
		super.checkInput(ref);
		checkInput(scalar, operationId, argumentPrecedence);
		// set information for unary operation
		setOperationId(operationId);
		_scalar = scalar;
		_precedence = argumentPrecedence;
		super.initializeAll(ref);
	}

	/**
	 * returns the name of the proxy
	 */
	protected String getProxyName(DataReference ref)
	{
		StringBuffer buf = new StringBuffer(400);
		buf.append(ref.getName()).append(getOperationName()).append(_scalar);
		return buf.toString();
	}

	/**
	 * returns the proxy server name if any
	 */
	protected String getProxyServerName(DataReference ref)
	{
		return "";
	}

	/**
	 *
	 */
	protected String getProxyFileName(DataReference ref)
	{
		return "";
	}

	/**
	 *
	 */
	protected Pathname getProxyPathname(DataReference ref)
	{
		Pathname p1 = ref.getPathname();
		String[] parts = new String[Pathname.MAX_PARTS];
		for(int i = 0; i < parts.length; i++)
		{
			parts[i] = p1.getPart(i);
		}
		parts[Pathname.B_PART] = parts[Pathname.B_PART] + getOperationName()
				+ _scalar;
		Pathname pathname = Pathname.createPathname(parts);
		return pathname;
	}

	/**
	 *
	 */
	protected TimeWindow getProxyTimeWindow(DataReference ref)
	{
		return ref.getTimeWindow().create(ref.getTimeWindow().getStartTime(),
				ref.getTimeWindow().getEndTime());
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
	protected TimeInterval getProxyTimeInterval(DataReference ref)
	{
		return ref.getTimeInterval().create(ref.getTimeInterval());
	}

	/**
	 *
	 */
	private void setOperationId(int operationId)
	{
		_operationId = operationId;
	}

	public void setUnits(String units)
	{
		_units = units;
	}

	/**
	 *
	 */
	protected void checkInput(double scalar, int operationId,
							  int argumentPrecedence)
	{
		//
		switch(argumentPrecedence)
		{
			case DataReferenceMath.FIRST_FIRST:
				break;
			case DataReferenceMath.FIRST_LAST:
				break;
			default:
				throw new IllegalArgumentException("Invalid argument precedence id");
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
		if((operationId == DataReferenceMath.DIV)
				&& (argumentPrecedence == DataReferenceMath.FIRST_FIRST)
				&& (scalar == 0))
		{
			throw new IllegalArgumentException("? Scalar division by zero");
		}
	}

	/**
	 * create a clone of itself
	 */
	public DataReference createClone()
	{
		return new DataReferenceScalarMathProxy(getProxyReference(), _scalar,
				_operationId, _precedence);
	}

	/**
	 * does the scalar operation
	 */
	public DataSet doOperation(DataSet ds)
	{
		DataSet calc = null;
		if(_precedence == DataReferenceMath.FIRST_LAST)
		{
			calc = VectorMath.doMathOperation(ds, _scalar, _operationId, true);
		}
		else
		{
			calc = VectorMath.doMathOperation(ds, _scalar, _operationId, false);
		}
		if(calc != null && _units != null)
		{
			calc.getAttributes().setYUnits(_units);
		}
		if(this.getPathname() != null)
		{
			calc.setName(this.getPathname().getFullPath());
		}
		return calc;
	}
}
