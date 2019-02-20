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

/**
 * Encapsulates the attributes of the data set. These attributes are Type of
 * data: integer : DataType.REGULAR_TIME_SERIES | IRREGULAR_TIME_SERIES | PAIRED
 * | TEXT | UNDEFINED Units: String: X dimension and Y dimension Group Name :
 * String: A part Location Name: String: B part Type Name : String: C part
 * Source Name : String: F part The D and E parts are obtained from the DataSet
 * type
 * 
 * @author Nicky Sandhu
 * @version $Id: DataSetAttr.java,v 1.1 2003/10/02 20:49:21 redwood Exp $
 */
public class DataSetAttr implements java.io.Serializable {
	/**
	 * construct a new attribute object with unit units and type type
	 */
	public DataSetAttr(int type, String xUnits, String yUnits, String xType,
			String yType) {
		this(null, null, null, null, type, xUnits, yUnits, xType, yType);
	}

	/**
	 * construct a new attribute object with unit units and type type
	 */
	public DataSetAttr(String groupName, String locName, String typeName,
			String sourceName, int type, String xUnits, String yUnits,
			String xType, String yType) {
		setType(type);
		setXUnits(xUnits);
		setYUnits(yUnits);
		setXType(xType);
		setYType(yType);
		setGroupName(groupName);
		setLocationName(locName);
		setTypeName(typeName);
		setSourceName(sourceName);
	}

	/**
    *
    */
	public DataSetAttr createClone() {
		DataSetAttr attr = new DataSetAttr(_groupName, _locationName,
				_typeName, _sourceName, _type, _xUnits, _yUnits, _xType, _yType);
		return attr;
	}

	/**
	 * what is generally referred to as the A part
	 */
	public String getGroupName() {
		return _groupName;
	}

	/**
	 * the b part
	 */
	public String getLocationName() {
		return _locationName;
	}

	/**
	 * the c part
	 */
	public String getTypeName() {
		return _typeName;
	}

	/**
	 * the f part
	 */
	public String getSourceName() {
		return _sourceName;
	}

	/**
	 * get string representing the units
	 * 
	 * @see Units
	 */
	public String getXUnits() {
		return _xUnits;
	}

	/**
   *
   */
	public String getXType() {
		return _xType;
	}

	/**
   *
   */
	public String getYType() {
		return _yType;
	}

	/**
   *
   */
	public String getYUnits() {
		return _yUnits;
	}

	/**
	 * get integer representing type
	 * 
	 * @see DataType
	 */
	public int getType() {
		return _type;
	}

	/**
	 * what is generally referred to as the A part
	 */
	public void setGroupName(String name) {
		_groupName = name == null ? "" : name.toUpperCase();
	}

	/**
	 * the b part
	 */
	public void setLocationName(String name) {
		_locationName = name == null ? "" : name.toUpperCase();
	}

	/**
	 * the c part
	 */
	public void setTypeName(String name) {
		_typeName = name == null ? "" : name.toUpperCase();
	}

	/**
	 * the f part
	 */
	public void setSourceName(String name) {
		_sourceName = name == null ? "" : name.toUpperCase();
	}

	/**
	 * set string representing the units
	 * 
	 * @see Units
	 */
	public void setXUnits(String units) {
		_xUnits = units == null ? "" : units.toUpperCase();
	}

	/**
   *
   */
	public void setYUnits(String units) {
		_yUnits = units == null ? "" : units.toUpperCase();
	}

	/**
   *
   */
	public void setXType(String type) {
		_xType = type == null ? "" : type.toUpperCase();
	}

	/**
   *
   */
	public void setYType(String type) {
		_yType = type == null ? "" : type.toUpperCase();
	}

	/**
	 * set integer representing type
	 * 
	 * @see DataType
	 */
	public void setType(int type) {
		_type = type;
	}
	
	/**
	 * A string representation
	 */
	public String toString(){
		StringBuffer buffer = new StringBuffer();
		buffer.append(this.getTypeName()+ " @ " + this.getLocationName()+"\n");
		buffer.append("Source: "+ this.getSourceName()+"\n");
		buffer.append("Group: "+this.getGroupName()+"\n");
		buffer.append("Type: "+ this.getType()+"\n");
		buffer.append("X (type,units): "+ this.getXType()+","+this.getXUnits()+"\n");
		buffer.append("Y (type,units): "+ this.getYType()+","+this.getYUnits()+"\n");
		return buffer.toString();
	}

	/**
   *
   */
	private int _type;
	private String _xType, _yType;
	private String _xUnits, _yUnits;
	private String _groupName, _locationName, _typeName, _sourceName;
}
