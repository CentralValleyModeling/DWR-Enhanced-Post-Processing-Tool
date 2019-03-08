/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */

package gov.ca.water.bo;
//! Representation of GUI_Links4 (Hydroclimate dashboard logic)

/**
 * This is used to hold the information of GUI_Link4.table in memory. Please
 * refer GUI_Link4.table for more information.
 *
 * @author Mohan
 */
public class GUILinks4BO
{
	/**
	 * The RunBasis_ID from GUI_Link4.table. Please refer GUI_Link4.table for
	 * more information.
	 */
	private String _runBasisID;
	/**
	 * The LOD_ID from GUI_Link4.table. Please refer GUI_Link4.table for more
	 * information.
	 */
	private String _lodId;
	/**
	 * The CCProject_ID from GUI_Link4.table. Please refer GUI_Link4.table for
	 * more information.
	 */
	private String _ccprojectId;
	/**
	 * The CCModel_ID from GUI_Link4.table. Please refer GUI_Link4.table for
	 * more information.
	 */
	private String _ccmodelId;
	/**
	 * The SV_File from GUI_Link4.table. Please refer GUI_Link4.table for more
	 * information.
	 */
	private String _svFile;
	/**
	 * The F_PartSV from GUI_Link4.table. Please refer GUI_Link4.table for more
	 * information.
	 */
	private String _fPartSV1;
	/**
	 * The Init_File from GUI_Link4.table. Please refer GUI_Link4.table for more
	 * information.
	 */
	private String _initFile;
	/**
	 * The F_PartSV from GUI_Link4.table. Please refer GUI_Link4.table for more
	 * information.
	 */
	private String _fPartSV2;
	/**
	 * The CVP_WSI_DI from GUI_Link4.table. Please refer GUI_Link4.table for
	 * more information.
	 */
	private String _cvpWsiDi;
	/**
	 * The SWP_WSI_DI from GUI_Link4.table. Please refer GUI_Link4.table for
	 * more information.
	 */
	private String _swpWsiDi;
	/**
	 * The Lookup from GUI_Link4.table. Please refer GUI_Link4.table for more
	 * information.
	 */
	private String _lookup;

	public GUILinks4BO(String runBasisID, String lodId, String ccprojectId, String ccmodelId, String svFile,
					   String fPartSV1, String initFile, String fPartSV2, String cvpWsiDi, String swpWsiDi,
					   String lookup)
	{
		super();
		this._runBasisID = runBasisID;
		this._lodId = lodId;
		this._ccprojectId = ccprojectId;
		this._ccmodelId = ccmodelId;
		this._svFile = svFile;
		this._fPartSV1 = fPartSV1;
		this._initFile = initFile;
		this._fPartSV2 = fPartSV2;
		this._cvpWsiDi = cvpWsiDi;
		this._swpWsiDi = swpWsiDi;
		this._lookup = lookup;
	}

	public String getRunBasisID()
	{
		return _runBasisID;
	}

	public String getLodId()
	{
		return _lodId;
	}

	public String getCcprojectId()
	{
		return _ccprojectId;
	}

	public String getCcmodelId()
	{
		return _ccmodelId;
	}

	public String getSvFile()
	{
		return _svFile;
	}

	public String getfPartSV1()
	{
		return _fPartSV1;
	}

	public String getInitFile()
	{
		return _initFile;
	}

	public String getfPartSV2()
	{
		return _fPartSV2;
	}

	public String getCvpWsiDi()
	{
		return _cvpWsiDi;
	}

	public String getSwpWsiDi()
	{
		return _swpWsiDi;
	}

	public String getLookup()
	{
		return _lookup;
	}
}
