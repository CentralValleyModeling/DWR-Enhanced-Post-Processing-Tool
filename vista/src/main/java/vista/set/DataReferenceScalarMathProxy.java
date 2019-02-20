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

import vista.time.TimeInterval;
import vista.time.TimeWindow;

/**
 * A proxy for scalar operations on data reference
 * 
 * @author Nicky Sandhu
 * @version $Id: DataReferenceScalarMathProxy.java,v 1.1 2003/10/02 20:49:20
 *          redwood Exp $
 */
public class DataReferenceScalarMathProxy extends UnaryOperationProxy {
	/**
	 * returns the name of the proxy
	 */
	protected String getProxyName(DataReference ref) {
		StringBuffer buf = new StringBuffer(400);
		buf.append(ref.getName()).append(getOperationName()).append(_scalar);
		return buf.toString();
	}

	/**
	 * returns the proxy server name if any
	 */
	protected String getProxyServerName(DataReference ref) {
		return "";
	}

	/**
   *
   */
	protected String getProxyFileName(DataReference ref) {
		return "";
	}

	/**
   *
   */
	protected Pathname getProxyPathname(DataReference ref) {
		Pathname p1 = ref.getPathname();
		String[] parts = new String[Pathname.MAX_PARTS];
		for (int i = 0; i < parts.length; i++) {
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
	protected TimeWindow getProxyTimeWindow(DataReference ref) {
		return ref.getTimeWindow().create(ref.getTimeWindow().getStartTime(),
				ref.getTimeWindow().getEndTime());
	}

	/**
   *
   */
	protected String getOperationName() {
		switch (_operationId) {
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
	protected TimeInterval getProxyTimeInterval(DataReference ref) {
		return ref.getTimeInterval().create(ref.getTimeInterval());
	}

	/**
   *
   */
	private void setOperationId(int operationId) {
		_operationId = operationId;
	}
	
	public void setUnits(String units){
		_units = units;
	}

	/**
   *
   */
	protected void checkInput(double scalar, int operationId,
			int argumentPrecedence) {
		//
		switch (argumentPrecedence) {
		case DataReferenceMath.FIRST_FIRST:
			break;
		case DataReferenceMath.FIRST_LAST:
			break;
		default:
			throw new IllegalArgumentException("Invalid argument precedence id");
		}
		//
		switch (operationId) {
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
		if ((operationId == DataReferenceMath.DIV)
				&& (argumentPrecedence == DataReferenceMath.FIRST_FIRST)
				&& (scalar == 0))
			throw new IllegalArgumentException("? Scalar division by zero");
	}

	/**
   *
   */
	public DataReferenceScalarMathProxy(DataReference ref, double scalar,
			int operationId, int argumentPrecedence) {
		super.checkInput(ref);
		checkInput(scalar, operationId, argumentPrecedence);
		// set information for unary operation
		setOperationId(operationId);
		_scalar = scalar;
		_precedence = argumentPrecedence;
		super.initializeAll(ref);
	}

	/**
	 * create a clone of itself
	 */
	public DataReference createClone() {
		return new DataReferenceScalarMathProxy(getProxyReference(), _scalar,
				_operationId, _precedence);
	}

	/**
	 * does the scalar operation
	 */
	public DataSet doOperation(DataSet ds) {
		DataSet calc = null;
		if (_precedence == DataReferenceMath.FIRST_LAST)
			calc= VectorMath.doMathOperation(ds, _scalar, _operationId, true);
		else
			calc= VectorMath.doMathOperation(ds, _scalar, _operationId, false);
		if (calc != null && _units != null){
			calc.getAttributes().setYUnits(_units);
		}
		if (this.getPathname() != null){
			calc.setName(this.getPathname().getFullPath());
		}
		return calc;
	}

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
}
