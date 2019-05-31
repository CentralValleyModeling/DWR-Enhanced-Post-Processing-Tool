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

package gov.ca.water.calgui.constant;


/**
 * This class is used to hold all the constant values required for the
 * application.
 *
 * @author Mohan
 */
public final class Constant
{
	public static final String EPPT_EXT = "eppt";
	public static final String CLS_EXT = ".CLS";
	public static final String TABLE_EXT = ".table";
	public static final String CSV_EXT = ".csv";
	public static final String DSS_EXT = ".dss";
	public static final String TXT_EXT = ".txt";
	public static final String DV_NAME = "_DV";
	public static final String CONFIG_DIR = System.getProperty("user.dir") + "\\dwr_eppt\\config\\";
	public static final String DOCS_DIR = System.getProperty("user.dir") + "\\dwr_eppt\\docs\\";
	public static final String WRESL_DIR = System.getProperty("user.dir") + "\\dwr_eppt\\wresl\\";
	public static final String WRIMS_DIR = System.getProperty("user.dir") + "\\dwr_eppt\\wresl\\wrims";
	public static final String WY_TYPES_TABLE = WRESL_DIR + "/run/lookup/wytypes" + TABLE_EXT;
	public static final String WY_TYPES_NAME_LOOKUP = WRESL_DIR + "/WYTypesLookup" + CSV_EXT;
	public static final String JASPER_DIR = System.getProperty("user.dir") + "\\dwr_eppt\\jasper\\";
	public static final String MODEL_W2_DIR = System.getProperty("user.dir") + "//Model_w2//";
	public static final String RUN_DETAILS_DIR = "Run_Details";
	public static final String MODEL_W2_DSS_DIR = MODEL_W2_DIR + "//DSS_Files//";
	public static final String MODEL_W2_WRESL_DIR = MODEL_W2_DIR + "wresl//";
	public static final String LOOKUP_DIR = "//Lookup//";
	public static final String MODEL_W2_WRESL_LOOKUP_DIR = MODEL_W2_WRESL_DIR + LOOKUP_DIR;
	public static final String GENERATED_DIR = "//Generated//";
	public static final String RUN_DIR = "Run";
	// File Names.
	public static final String GUI_XML_FILENAME = System.getProperty("user.dir") + "//Config//GUI.xml";

	public static final String DYNAMIC_CONTROL_FOR_STARTUP_FILENAME = System.getProperty("user.dir")
			+ "//Config//DynamicControlForStartUp" + CSV_EXT;
	public static final String TRIGGER_ENABLE_DISABLE_FILENAME = System.getProperty("user.dir")
			+ "//Config//TriggerForDynamicDisplay" + CSV_EXT;
	public static final String TRIGGER_CHECK_UNCHECK_FILENAME = System.getProperty("user.dir")
			+ "//Config//TriggerForDynamicSelection" + CSV_EXT;
	public static final String IMAGE_PATH = System.getProperty("user.dir") + "//images//CalLiteIcon.png";
	public static final String SAVE_FILE = "save";
	public static final String BATCH_RUN = "Batch Run";
	public static final String BATCH_RUN_WSIDI = "Batch Run wsidi";
	public static final String SAVE = "save";
	public static final String DEFAULT = "DEFAULT";
	public static final String CURRENT_SCENARIO = "Current_Scenario";
	public static final String DELIMITER = ",";
	public static final String TAB_SPACE = "\t";
	public static final String OLD_DELIMITER = TAB_SPACE;
	public static final String TAB_OR_SPACE_DELIMITER = "\t| ";
	public static final String N_A = "n/a";
	public static final String UNDER_SCORE = "_";
	public static final String PIPELINE = "|";
	public static final String PIPELINE_DELIMITER = "[|]";
	public static final String EXCLAMATION = "!";
	public static final String HEADERS = "headers";
	public static final String SPACE = " ";
	public static final String DASH = "-";
	public static final String D1641 = "D1641";
	public static final String D1485 = "D1485";
	public static final String BDCP = "BDCP";
	public static final String D1641BO = "1641BO";
	public static final String USER_DEFINED = "UD";
	public static final String SEMICOLON = ";";
	public static final String NEW_LINE = "\n";
	public static final String COMMA = ",";
	public static final String EMPTY = "Empty";
	public static final String MAIN_PANEL_NAME = "tabbedPane1";
	public static final String MAIN_FRAME_NAME = "desktop";
	public static final String SWP = "SWP";
	public static final String CVP = "CVP";
	public static final String UNEDITED_FORLABEL = " (Unedited)";
	public static final String EDITED_FORLABEL = " (Edited)";
	public static final String SWP_START_FILENAME = "wsi_di_swp";
	public static final String CVP_START_FILENAME = "wsi_di_cvp_sys";
	public static final String SCHEMATIC_PREFIX = "SchVW";

	public static final String HYDROCLIMATE_TABNAME = "Hydroclimate";
	public static final String REGULATIONS_TABNAME = "Regulations";

	// Radio button
	public static final String QUICK_SELECT_RB_D1641 = "rdbRegQS_D1641";
	public static final String QUICK_SELECT_RB_D1485 = "rdbRegQS_D1485";
	public static final String QUICK_SELECT_RB_D1641_BO = "rdbRegQS_1641BO";

	public static final String PANEL_RB_D1641 = "btnRegD1641";
	public static final String PANEL_RB_D1485 = "btnRegD1485";
	public static final String PANEL_RB_USER_DEFIND = "btnRegUD";

	// Numbers
	public static final int ZERO = 0;
	public static final int MIN_YEAR = 1921;
	public static final int MAX_YEAR = 2003;

	// Status window
	public static final String STATUS_BTN_TEXT_CLOSE = "Close this status window";
	public static final String STATUS_BTN_TEXT_STOP = "Stop all runs";

	// VAMP text strings
	public static final String CKB_REG_VAMP = "ckbReg_VAMP";
	public static final String VAMP_SELECTED_TEXT = "If D1485 is selected, take VAMP D1641 hydrology with a D1485 run.";
	public static final String VAMP_NOT_SELECTED_TEXT = "Access regulation table (where applicable) by selecting or right-clicking on a regulation name.";

	public static final String SCENARIOS_DIR = "Scenarios";
	static final String MODEL_DIR = "Model_w2";
	static final String MODEL_DSS_DIR = "DSS_Files";
	static final String REPORTS_DIR = "Reports";

	private Constant()
	{
		throw new AssertionError("Utility class");
	}
}
