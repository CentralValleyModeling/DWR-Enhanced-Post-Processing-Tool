/*
    Copyright (C) 1996, 1997, 1998 State of California, Department of 
    Water Resources.

    VISTA : A VISualization Tool and Analyzer. 
	Version 1.0beta
	by Nicky Sandhu
    California Dept. of Water Resources
    Division of Planning, Delta Modeling Section
    1416 Ninth Street
    Sacramento, CA 95814
    (916)-653-7552
    nsandhu@water.ca.gov

    Send bug reports to nsandhu@water.ca.gov

    This program is licensed to you under the terms of the GNU General
    Public License, version 2, as published by the Free Software
    Foundation.

    You should have received a copy of the GNU General Public License
    along with this program; if not, contact Dr. Francis Chung, below,
    or the Free Software Foundation, 675 Mass Ave, Cambridge, MA
    02139, USA.

    THIS SOFTWARE AND DOCUMENTATION ARE PROVIDED BY THE CALIFORNIA
    DEPARTMENT OF WATER RESOURCES AND CONTRIBUTORS "AS IS" AND ANY
    EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
    IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
    PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE CALIFORNIA
    DEPARTMENT OF WATER RESOURCES OR ITS CONTRIBUTORS BE LIABLE FOR
    ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
    CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT
    OR SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA OR PROFITS; OR
    BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
    LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
    (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE
    USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
    DAMAGE.

    For more information about VISTA, contact:

    Dr. Francis Chung
    California Dept. of Water Resources
    Division of Planning, Delta Modeling Section
    1416 Ninth Street
    Sacramento, CA  95814
    916-653-5601
    chung@water.ca.gov

    or see our home page: http://wwwdelmod.water.ca.gov/

    Send bug reports to nsandhu@water.ca.gov or call (916)-653-7552

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
