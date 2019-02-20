/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */

package gov.ca.water.calgui.bo;
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
	private String runBasisID;
	/**
	 * The LOD_ID from GUI_Link4.table. Please refer GUI_Link4.table for more
	 * information.
	 */
	private String lodId;
	/**
	 * The CCProject_ID from GUI_Link4.table. Please refer GUI_Link4.table for
	 * more information.
	 */
	private String ccprojectId;
	/**
	 * The CCModel_ID from GUI_Link4.table. Please refer GUI_Link4.table for
	 * more information.
	 */
	private String ccmodelId;
	/**
	 * The SV_File from GUI_Link4.table. Please refer GUI_Link4.table for more
	 * information.
	 */
	private String svFile;
	/**
	 * The F_PartSV from GUI_Link4.table. Please refer GUI_Link4.table for more
	 * information.
	 */
	private String fPartSV1;
	/**
	 * The Init_File from GUI_Link4.table. Please refer GUI_Link4.table for more
	 * information.
	 */
	private String initFile;
	/**
	 * The F_PartSV from GUI_Link4.table. Please refer GUI_Link4.table for more
	 * information.
	 */
	private String fPartSV2;
	/**
	 * The CVP_WSI_DI from GUI_Link4.table. Please refer GUI_Link4.table for
	 * more information.
	 */
	private String cvpWsiDi;
	/**
	 * The SWP_WSI_DI from GUI_Link4.table. Please refer GUI_Link4.table for
	 * more information.
	 */
	private String swpWsiDi;
	/**
	 * The Lookup from GUI_Link4.table. Please refer GUI_Link4.table for more
	 * information.
	 */
	private String lookup;

	public GUILinks4BO(String runBasisID, String lodId, String ccprojectId, String ccmodelId, String svFile,
					   String fPartSV1, String initFile, String fPartSV2, String cvpWsiDi, String swpWsiDi,
					   String lookup)
	{
		super();
		this.runBasisID = runBasisID;
		this.lodId = lodId;
		this.ccprojectId = ccprojectId;
		this.ccmodelId = ccmodelId;
		this.svFile = svFile;
		this.fPartSV1 = fPartSV1;
		this.initFile = initFile;
		this.fPartSV2 = fPartSV2;
		this.cvpWsiDi = cvpWsiDi;
		this.swpWsiDi = swpWsiDi;
		this.lookup = lookup;
	}

	public String getRunBasisID()
	{
		return runBasisID;
	}

	public String getLodId()
	{
		return lodId;
	}

	public String getCcprojectId()
	{
		return ccprojectId;
	}

	public String getCcmodelId()
	{
		return ccmodelId;
	}

	public String getSvFile()
	{
		return svFile;
	}

	public String getfPartSV1()
	{
		return fPartSV1;
	}

	public String getInitFile()
	{
		return initFile;
	}

	public String getfPartSV2()
	{
		return fPartSV2;
	}

	public String getCvpWsiDi()
	{
		return cvpWsiDi;
	}

	public String getSwpWsiDi()
	{
		return swpWsiDi;
	}

	public String getLookup()
	{
		return lookup;
	}
}