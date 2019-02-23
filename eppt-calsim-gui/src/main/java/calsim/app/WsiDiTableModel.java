/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */

package calsim.app;
//import javax.swing.*;

import javax.swing.table.DefaultTableModel;

/**
 * A table model for wsi di table
 *
 * @author Armin Munevar
 * @version $Id: WsiDiTableModel.java,v 1.1.2.4 2001/07/12 01:58:35 amunevar Exp $
 */
public class WsiDiTableModel extends DefaultTableModel
{
	Object[] columnNames = {"Name", "WSI Variable", "DI Variable", "Max WSI Value"};
	Object[][] initData = {
			{"SWP", "WSI_ACTUAL_SWP", "DI_ACTUAL_SWP", "20000.0"},
			//		{"CVP_S", "WSI_ACT_CVP_S", "DI_ACT_CVP_S", "20000.0"},
			{"CVP_SYS", "WSI_ACT_CVP_SYS", "DI_ACT_CVP_SYS", "20000.0"},
	};
	Object[] defaultData = {"SWP", "WSI_ACTUAL_SWP", "DI_ACTUAL_SWP", "5000.0"};
	/**
	 *
	 */
	//  private DerivedTimeSeries _dts;
	private boolean _quitmode = false;

	/**
	 *
	 */
	public WsiDiTableModel()
	{
		super();
		setDataVector(initData, columnNames);
	}

	/**
	 * Get boolean for continue or quit state
	 */
	public boolean getQuitMode()
	{
		return _quitmode;
	}

	/**
	 * Set boolean for continue or quit state
	 */
	public void setQuitMode(boolean mode)
	{
		_quitmode = mode;
	}
	/**
	 * Number of rows in table
	 */
/*  public int getRowCount(){
    return 3;
  }*/
	/**
	 * Number of columns in the table
	 */
/*  public int getColumnCount(){
    return 5;
  }*/
	/**
	 * Returns the name of the column at <i>columnIndex</i>.
	 * @param    columnIndex    the index of column
	 * @return the name of the column
	 */
	//  public String getColumnName(int columnIndex){
	//    return tableHeaders[columnIndex];
	//  }
	/**
	 * Returns true if the cell at <I>rowIndex</I> and <I>columnIndex</I>
	 * is editable.  Otherwise, setValueAt() on the cell will not change
	 * the value of that cell.
	 *
	 * @param    rowIndex    the row whose value is to be looked up
	 * @param    columnIndex    the column whose value is to be looked up
	 * @return true if the cell is editable.
	 * @see #setValueAt
	 */
/*  public boolean isCellEditable(int rowIndex, int columnIndex){
    String dtsname = _dts.getDTSNameAt(rowIndex);
    if ( columnIndex > 1 ){
      if ( dtsname == null ){
	return true;
      }else {
	if ( dtsname.equals("") )
	  return true;
	else
	  return false;
      }
    } else {
      return true;
    }
  }
*/  /**
 * Returns an attribute value for the cell at <I>columnIndex</I>
 * and <I>rowIndex</I>.
 *
 * @param    rowIndex    the row whose value is to be looked up
 * @param    columnIndex    the column whose value is to be looked up
 * @return the value Object at the specified cell
 */
/*  public Object getValueAt(int rowIndex, int columnIndex){
		return new String("hello");
    if ( columnIndex == 0 ){
      return AppUtils.getOperationName(_dts.getOperationIdAt(rowIndex));
    } else if ( columnIndex == 1 ){
      String dtsname = _dts.getDTSNameAt(rowIndex);
      if ( dtsname == null )
	return "";
      else
	return dtsname;
    } else if ( columnIndex == 2 ){
      String dtsname = _dts.getDTSNameAt(rowIndex);
      if ( dtsname != null &&
	   ! dtsname.equals("") )
	return "";
      String varType = _dts.getVarTypeAt(rowIndex);
      if ( varType == null ){
	return "";
      }else{
	return varType;
      }
    } else if ( columnIndex == 3 ){
      String dtsname = _dts.getDTSNameAt(rowIndex);
      if ( dtsname != null &&
	   ! dtsname.equals("") )
	return "";
      String bpart = _dts.getBPartAt(rowIndex);
      if ( bpart == null ){
	return "";
      }else{
	return bpart;
      }
    } else if ( columnIndex == 4 ){
      String dtsname = _dts.getDTSNameAt(rowIndex);
      if ( dtsname != null &&
	   ! dtsname.equals("") )
	return "";
      String cpart = _dts.getCPartAt(rowIndex);
      if ( cpart == null ){
	return "";
      }else{
	return cpart;
      }
    } else{
      throw new InternalError("Invalid column access");
    }
  }*/
	/**
	 * Sets an attribute value for the record in the cell at
	 * @param    aValue         the new value
	 * @param    rowIndex     the row whose value is to be changed
	 * @param    columnIndex     the column whose value is to be changed
	 * @see #getValueAt
	 * @see #isCellEditable
	 */
/*  public void setValueAt(Object aValue, int rowIndex, int columnIndex){
    if ( columnIndex == 0 ){
      int opId = AppUtils.getOperationId((String)aValue);
      _dts.setOperationIdAt(rowIndex, opId);
    } else if ( columnIndex == 1 ){
      String dtsname = (String) aValue;
      if ( dtsname == null ) return; // this shouldn't be
      if ( dtsname.length() > 0 )
	_dts.setDTSNameAt(rowIndex, dtsname);
    } else if ( columnIndex == 2 ){
      String varType = (String) aValue;
      _dts.setVarTypeAt(rowIndex,varType);
    } else if ( columnIndex == 3 ){
      String bpart = (String) aValue;
      _dts.setBPartAt(rowIndex, bpart);
    } else if ( columnIndex == 4 ){
      String cpart = (String) aValue;
      _dts.setCPartAt(rowIndex, cpart);
    } else{
      throw new InternalError("Invalid column access");
    }
  }
*/

	/**
	 * Get default data for new rows
	 */
	public Object[] getDefaultData()
	{
		return defaultData;
	}
}
