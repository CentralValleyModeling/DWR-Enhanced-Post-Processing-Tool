package vista.report;import vista.set.DataSetAttr;/** *  * A set of data attributes that contains original units field. *  *  *  *  */public class TSDataAttr extends DataSetAttr {	/**
   *
   */	public TSDataAttr(DataSetAttr attr) {		super(attr.getGroupName(),		attr.getLocationName(),		attr.getTypeName(),		attr.getSourceName(),		attr.getType(),		attr.getXUnits(),		attr.getYUnits(),		attr.getXType(),		attr.getYType());		setOriginalUnits(attr.getYUnits());	}	/**
   *
   */	public String getOriginalUnits() {		return _originalUnits;	}	/**
   *
   */	private void setOriginalUnits(String units) {		_originalUnits = units;	}	private String _originalUnits;}