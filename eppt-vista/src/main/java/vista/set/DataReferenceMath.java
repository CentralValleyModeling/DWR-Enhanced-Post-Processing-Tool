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

import vista.time.TimeWindow;

/**
 * This class defines the math operations on data references. These math
 * operations are proxies which are performed only as needed.
 * 
 * @author Nicky Sandhu
 * @version $Id: DataReferenceMath.java,v 1.1 2003/10/02 20:49:20 redwood Exp $
 */
public class DataReferenceMath {
	/**
	 * additon operation
	 */
	public static final int ADD = 100;
	/**
	 * muliplication operation
	 */
	public static final int MUL = ADD + 1;
	/**
	 * subtraction operation
	 */
	public static final int SUB = MUL + 1;
	/**
	 * division operation
	 */
	public static final int DIV = SUB + 1;
	/**
	 * first argument remains first argument in math operation
	 */
	public static final int FIRST_FIRST = 1000;
	/**
	 * first argument becomes second argument in math operation
	 */
	public static final int FIRST_LAST = FIRST_FIRST + 1;

	/**
	 * returns a proxy for the vector operation on the the given references and
	 * the opearation defined by the operation id.
	 */
	public static DataReference vectorOperation(DataReference ref1,
			DataReference ref2, int operationId)
			throws IllegalArgumentException {
		checkArgumentList(ref1, ref2, operationId);
		// create proxy and return its handle
		return new DataReferenceVectorMathProxy(ref1, ref2, operationId);
	}

	/**
	 * returns a proxy for the scalar operation on the given reference by the
	 * given scalar values for the operation given by operation id and the
	 * precedence defined by argumentPrecedence.
	 */
	public static DataReference scalarOperation(DataReference ref,
			double scalarValue, int opId) {
		return scalarOperation(ref, scalarValue, opId, FIRST_FIRST);
	}

	/**
	 * returns a proxy for the scalar operation on the given reference by the
	 * given scalar values for the operation given by operation id and the
	 * precedence defined by argumentPrecedence.
	 */
	public static DataReference scalarOperation(DataReference ref,
			double scalarValue, int operationId, int argumentPrecedence)
			throws IllegalArgumentException {
		checkArgumentList(ref, scalarValue, operationId, argumentPrecedence);
		// create proxy and return its handle
		return new DataReferenceScalarMathProxy(ref, scalarValue, operationId,
				argumentPrecedence);
	}

	/**
	 * check argument list for scalar operation
	 */
	private static void checkArgumentList(DataReference ref,
			double scalarValue, int operationId, int argumentPrecedence)
			throws IllegalArgumentException {
		//
		switch (argumentPrecedence) {
		case FIRST_FIRST:
			break;
		case FIRST_LAST:
			break;
		default:
			throw new IllegalArgumentException("Invalid argument precedence id");
		}
		//
		switch (operationId) {
		case ADD:
			break;
		case MUL:
			break;
		case SUB:
			break;
		case DIV:
			break;
		default:
			throw new IllegalArgumentException("Invalid operation id");
		}
		if (ref == null)
			throw new IllegalArgumentException("Reference may be null");
		TimeWindow tw = ref.getTimeWindow();
		if (tw == null)
			throw new IllegalArgumentException("Time Window may be null");
		if (operationId == DIV && argumentPrecedence == FIRST_FIRST
				&& scalarValue == 0)
			throw new IllegalArgumentException("Scalar division by zero");
	}

	/**
	 * returns a name for the given operation
	 */
	public static String getOperationName(int operationId) {
		switch (operationId) {
		case ADD:
			return " + ";
		case MUL:
			return " * ";
		case SUB:
			return " - ";
		case DIV:
			return " / ";
		default:
			return " invalid ";
		}
	}

	/**
	 * checks argument list for vector operation
	 * 
	 * @throws IllegalArgumentException
	 *             incase list is invalid.
	 */
	private static void checkArgumentList(DataReference ref1,
			DataReference ref2, int operationId)
			throws IllegalArgumentException {
		switch (operationId) {
		case ADD:
			break;
		case MUL:
			break;
		case SUB:
			break;
		case DIV:
			break;
		default:
			throw new IllegalArgumentException("Invalid operation id");
		}
		if (ref1 == null || ref2 == null)
			throw new IllegalArgumentException("Reference may be null");
		TimeWindow tw1 = ref1.getTimeWindow();
		TimeWindow tw2 = ref2.getTimeWindow();
		if (tw1 == null || tw2 == null)
			throw new IllegalArgumentException("Time Window may be null");
		if (!tw1.intersects(tw2))
			throw new IllegalArgumentException("Time Window: " + tw1 + " & "
					+ tw2 + " are incompatible");
		String xUnits1 = "";
		String xUnits2 = "";
		try {
			xUnits1 = ref1.getData().getAttributes().getXUnits();
			xUnits2 = ref2.getData().getAttributes().getXUnits();
		} catch (DataRetrievalException dre) {
			System.out.println("Data retrieval exception: " + dre.getMessage());
		}
		if (!xUnits1.equals(xUnits2)) {
			throw new IllegalArgumentException("X Units don't match: "
					+ " unit1 = " + xUnits1 + " unit2 = " + xUnits2);
		}

		String e1 = ref1.getPathname().getPart(Pathname.E_PART);
		String e2 = ref2.getPathname().getPart(Pathname.E_PART);
		if (!e1.equals(e2)) { // might need this to be e1.indexOf(e2) > 0 ?
			throw new IllegalArgumentException("E PARTS don't match: e1 = "
					+ e1 + ", e2 = " + e2);
		}
		// if ( _ref1.getYUnits() != _ref2.getYUnits()
		// if ( _ref1.getPathname().getPathPart(Pathname.E_PART)
	}

}
