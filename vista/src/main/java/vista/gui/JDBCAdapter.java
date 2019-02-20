/*
    Copyright (C) 1996-2000 State of California, Department of 
    Water Resources.

    VISTA : A VISualization Tool and Analyzer. 
	Version 1.0
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
/*
 * @(#)JDBCAdapter.java	1.10 99/04/23
 *
 * Copyright (c) 1997-1999 by Sun Microsystems, Inc. All Rights Reserved.
 * 
 * Sun grants you ("Licensee") a non-exclusive, royalty free, license to use,
 * modify and redistribute this software in source and binary code form,
 * provided that i) this copyright notice and license appear on all copies of
 * the software; and ii) Licensee does not utilize the software in a manner
 * which is disparaging to Sun.
 * 
 * This software is provided "AS IS," without a warranty of any kind. ALL
 * EXPRESS OR IMPLIED CONDITIONS, REPRESENTATIONS AND WARRANTIES, INCLUDING ANY
 * IMPLIED WARRANTY OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE OR
 * NON-INFRINGEMENT, ARE HEREBY EXCLUDED. SUN AND ITS LICENSORS SHALL NOT BE
 * LIABLE FOR ANY DAMAGES SUFFERED BY LICENSEE AS A RESULT OF USING, MODIFYING
 * OR DISTRIBUTING THE SOFTWARE OR ITS DERIVATIVES. IN NO EVENT WILL SUN OR ITS
 * LICENSORS BE LIABLE FOR ANY LOST REVENUE, PROFIT OR DATA, OR FOR DIRECT,
 * INDIRECT, SPECIAL, CONSEQUENTIAL, INCIDENTAL OR PUNITIVE DAMAGES, HOWEVER
 * CAUSED AND REGARDLESS OF THE THEORY OF LIABILITY, ARISING OUT OF THE USE OF
 * OR INABILITY TO USE SOFTWARE, EVEN IF SUN HAS BEEN ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGES.
 * 
 * This software is not designed or intended for use in on-line control of
 * aircraft, air traffic, aircraft navigation or aircraft communications; or in
 * the design, construction, operation or maintenance of any nuclear
 * facility. Licensee represents and warrants that it will not use or
 * redistribute the Software for such purposes.
 */

/**
 * An adaptor, transforming the JDBC interface to the TableModel interface.
 *
 * @version 1.20 09/25/97
 * @author Philip Milne
 */
package vista.gui;

import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.ResultSet;
import java.sql.ResultSetMetaData;
import java.sql.SQLException;
import java.sql.Statement;
import java.sql.Types;
import java.util.Vector;

import javax.swing.table.AbstractTableModel;

public class JDBCAdapter extends AbstractTableModel {
	Connection connection;
	Statement statement;
	ResultSet resultSet;
	String[] columnNames = {};
	Vector rows = new Vector();
	ResultSetMetaData metaData;

	public JDBCAdapter(String url, String driverName, String user, String passwd) {
		try {
			Class.forName(driverName);
			System.out.println("Opening db connection");

			connection = DriverManager.getConnection(url, user, passwd);
			statement = connection.createStatement();
		} catch (ClassNotFoundException ex) {
			System.err.println("Cannot find the database driver classes.");
			System.err.println(ex);
		} catch (SQLException ex) {
			System.err.println("Cannot connect to this database.");
			System.err.println(ex);
		}
	}

	public String executeQuery(String query) {
		if (connection == null || statement == null) {
			return "There is no database to execute the query.";
		}
		try {
			resultSet = statement.executeQuery(query);
			metaData = resultSet.getMetaData();

			int numberOfColumns = metaData.getColumnCount();
			columnNames = new String[numberOfColumns];
			// Get the column names and cache them.
			// Then we can close the connection.
			for (int column = 0; column < numberOfColumns; column++) {
				columnNames[column] = metaData.getColumnLabel(column + 1);
			}

			// Get all rows.
			rows = new Vector();
			while (resultSet.next()) {
				Vector newRow = new Vector();
				for (int i = 1; i <= getColumnCount(); i++) {
					newRow.addElement(resultSet.getObject(i));
				}
				rows.addElement(newRow);
			}
			// close(); Need to copy the metaData, bug in jdbc:odbc driver.
			fireTableChanged(null); // Tell the listeners a new table has
									// arrived.
		} catch (SQLException ex) {
			return ex.getMessage();
		}
		return "Done.";
	}

	public void close() throws SQLException {
		System.out.println("Closing db connection");
		resultSet.close();
		statement.close();
		connection.close();
	}

	protected void finalize() throws Throwable {
		close();
		super.finalize();
	}

	// ////////////////////////////////////////////////////////////////////////
	//
	// Implementation of the TableModel Interface
	//
	// ////////////////////////////////////////////////////////////////////////

	// MetaData

	public String getColumnName(int column) {
		if (columnNames[column] != null) {
			return columnNames[column];
		} else {
			return "";
		}
	}

	public Class getColumnClass(int column) {
		int type;
		try {
			type = metaData.getColumnType(column + 1);
		} catch (SQLException e) {
			return super.getColumnClass(column);
		}

		switch (type) {
		case Types.CHAR:
		case Types.VARCHAR:
		case Types.LONGVARCHAR:
			return String.class;

		case Types.BIT:
			return Boolean.class;

		case Types.TINYINT:
		case Types.SMALLINT:
		case Types.INTEGER:
			return Integer.class;

		case Types.BIGINT:
			return Long.class;

		case Types.FLOAT:
		case Types.DOUBLE:
			return Double.class;

		case Types.DATE:
			return java.sql.Date.class;

		default:
			return Object.class;
		}
	}

	public boolean isCellEditable(int row, int column) {
		try {
			return metaData.isWritable(column + 1);
		} catch (SQLException e) {
			return false;
		}
	}

	public int getColumnCount() {
		return columnNames.length;
	}

	// Data methods

	public int getRowCount() {
		return rows.size();
	}

	public Object getValueAt(int aRow, int aColumn) {
		Vector row = (Vector) rows.elementAt(aRow);
		return row.elementAt(aColumn);
	}

	public String dbRepresentation(int column, Object value) {
		int type;

		if (value == null) {
			return "null";
		}

		try {
			type = metaData.getColumnType(column + 1);
		} catch (SQLException e) {
			return value.toString();
		}

		switch (type) {
		case Types.INTEGER:
		case Types.DOUBLE:
		case Types.FLOAT:
			return value.toString();
		case Types.BIT:
			return ((Boolean) value).booleanValue() ? "1" : "0";
		case Types.DATE:
			return value.toString(); // This will need some conversion.
		default:
			return "\"" + value.toString() + "\"";
		}

	}

	public void setValueAt(Object value, int row, int column) {
		try {
			String tableName = metaData.getTableName(column + 1);
			// Some of the drivers seem buggy, tableName should not be null.
			if (tableName == null) {
				System.out.println("Table name returned null.");
			}
			String columnName = getColumnName(column);
			String query = "update " + tableName + " set " + columnName + " = "
					+ dbRepresentation(column, value) + " where ";
			// We don't have a model of the schema so we don't know the
			// primary keys or which columns to lock on. To demonstrate
			// that editing is possible, we'll just lock on everything.
			for (int col = 0; col < getColumnCount(); col++) {
				String colName = getColumnName(col);
				if (colName.equals("")) {
					continue;
				}
				if (col != 0) {
					query = query + " and ";
				}
				query = query + colName + " = "
						+ dbRepresentation(col, getValueAt(row, col));
			}
			System.out.println(query);
			System.out.println("Not sending update to database");
			// statement.executeQuery(query);
		} catch (SQLException e) {
			// e.printStackTrace();
			System.err.println("Update failed");
		}
		Vector dataRow = (Vector) rows.elementAt(row);
		dataRow.setElementAt(value, column);

	}
}
