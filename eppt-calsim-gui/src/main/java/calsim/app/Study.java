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

package calsim.app;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.PrintWriter;
import java.util.Date;
import java.util.Vector;

import com.sun.xml.tree.XmlDocument;
import org.w3c.dom.Element;

//import javax.swing.JOptionPane;
//import com.sun.xml.tree.TreeWalker;

/**
 * Contains all necessary properties required to run a study
 *
 * @author Armin Munevar
 * @version $Id: Study.java,v 1.1.2.13.2.2 2002/06/20 19:12:46 adraper Exp $
 */

public class Study
{
	public static boolean DEBUG = false;
	// Study properties
	private String _name;
	private String _author;
	private Date _date;
	private String _desc;
	private String _hydrology;
	private String _studyDir;
	private String _filename;
	private String _wreslFile;
	private String _svFile;
	private String _dvFile;
	private String _initFile;
	private String _initFileFPart;
	private String _timeStep; //DJE
	private String _numberSteps, _startDay, _stopDay; //DJE
	private boolean _newStudyObject = true;
	private String _startMonth;
	private Integer _startYear;
	private String _stopMonth;
	private Integer _stopYear;
	private String _simOption;
	private Integer _numberSequences;
	private Boolean _solverReport;
	private String _solverList;
	private Boolean _slackReport;
	private Boolean _slackSave;
	private String _addXaOptions;
	private Boolean _svReport;
	private Boolean _svSave;
	private Boolean _dssDebug;
	private Boolean _dssSave;
	private Boolean _genWsiDi;
	private Boolean _posAnalysis;
	private Boolean _useRestart;
	private Boolean _genRestart;
	private boolean _pricingRHSSensitivity; //CB added
	private boolean _activitySensitivity; //CB added
	private Boolean _dialogWindow;
	private Boolean _posstartyear;
	private String _commonPath;
	private boolean _modified;

	public Study()
	{
		_name = "";
		_author = "";
		_date = new Date();
		_desc = "";
		_hydrology = "hydrology version";//remove text when working
		_studyDir = "";
		_commonPath = "";
		_filename = "";
		_wreslFile = "";
		_svFile = "";
		_dvFile = "";
		_initFile = "";
		_initFileFPart = "";
		//DJE*************************
		_timeStep = "";
		_numberSteps = "";
		_startDay = "";
		//****************************
		_startMonth = "";
		_startYear = new Integer(1921);
		_stopDay = "";   //DJE*****
		_stopMonth = "";
		_stopYear = new Integer(1994);
		_simOption = "SLP";
		_numberSequences = new Integer(1);
		_solverReport = new Boolean(false);
		_solverList = "NONE";
		_slackReport = new Boolean(false);
		_slackSave = new Boolean(false);
		_addXaOptions = "";
		_svReport = new Boolean(false);
		_svSave = new Boolean(false);
		_dssDebug = new Boolean(false);
		_dssSave = new Boolean(false);
		_genWsiDi = new Boolean(
				false); //CB set to true for Jianzhong's version that runs many,many WSI-DI runs in a row w/o needing check selection or having popup windows
		//    _pricingRHSSensitivity = false; //CB added
		//	_activitySensitivity = false; //CB added
		_posAnalysis = new Boolean(false);
		_useRestart = new Boolean(false);
		_genRestart = new Boolean(false);
		_dialogWindow = new Boolean(true);
		_posstartyear = new Boolean(false);
	}

	/**
	 * Get methods for each property
	 */
	public String getCommonPath()
	{
		return _commonPath;
	}

	/**
	 * Set methods for each property
	 */
	public void setCommonPath(String common)
	{
		_commonPath = common;
	}

	public String getName()
	{
		return _name;
	}

	public void setName(String name)
	{
		_name = name;
	}

	public String getAuthor()
	{
		return _author;
	}

	public void setAuthor(String author)
	{
		_author = author;
	}

	public Date getDate()
	{
		return _date;
	}

	public void setDate(Date date)
	{
		_date = date;
	}

	public String getDescription()
	{
		return _desc;
	}

	public void setDescription(String desc)
	{
		_desc = desc;
	}

	public String getHydrologyVersion()
	{
		return _hydrology;
	}

	public void setHydrologyVersion(String hydro)
	{
		_hydrology = hydro;
	}

	public String getStudyDir()
	{
		return _studyDir;
	}

	public void setStudyDir(String dir)
	{
		_studyDir = dir;
	}

	public String getFileName()
	{
		return _filename;
	}

	public void setFileName(String fname)
	{
		_filename = fname;
	}

	public String getWreslFile()
	{
		return _wreslFile;
	}

	public void setWreslFile(String fname)
	{
		_wreslFile = fname;
		setStudyDir(new File(_wreslFile).getParent());
	}

	public String getSvFile()
	{
		return _svFile;
	}

	public void setSvFile(String fname)
	{
		_svFile = fname;
	}

	public String getDvFile()
	{
		return _dvFile;
	}

	public void setDvFile(String fname)
	{
		_dvFile = fname;
	}

	public String getInitFile()
	{
		return _initFile;
	}

	public void setInitFile(String fname)
	{
		_initFile = fname;
	}

	public String getInitFileFPart()
	{
		return _initFileFPart;
	}

	public void setInitFileFPart(String fname)
	{
		_initFileFPart = fname;
	}

	//DJE*******************************************************
	public String getTimeStep()
	{
		return _timeStep;
	}

	//DJE*********************************************
	public void setTimeStep(String ts)
	{
		_timeStep = ts;
	}

	public String getNumberSteps()
	{
		return _numberSteps;
	}

	public void setNumberSteps(String ns)
	{
		_numberSteps = ns;
	}

	public String getStartDay()
	{
		return _startDay;
	}

	public void setStartDay(String day)
	{
		_startDay = day;
	}

	public String getStopDay()
	{
		return _stopDay;
	}

	public void setStopDay(String day)
	{
		_stopDay = day;
	}

	//*********************************************************
	public String getStartMonth()
	{
		return _startMonth;
	}

	//***********************************************
	public void setStartMonth(String month)
	{
		_startMonth = month;
	}

	public Integer getStartYear()
	{
		return _startYear;
	}

	public void setStartYear(Integer year)
	{
		_startYear = year;
	}

	public String getStopMonth()
	{
		return _stopMonth;
	}

	public void setStopMonth(String month)
	{
		_stopMonth = month;
	}

	public Integer getStopYear()
	{
		return _stopYear;
	}

	public void setStopYear(Integer year)
	{
		_stopYear = year;
	}

	public String getSimOption()
	{
		return _simOption;
	}

	public void setSimOption(String option)
	{
		_simOption = option;
	}

	public Integer getNumberSequences()
	{
		return _numberSequences;
	}

	public void setNumberSequences(Integer number)
	{
		_numberSequences = number;
	}

	public Boolean getSolverReportOption()
	{
		return _solverReport;
	}

	public void setSolverReportOption(Boolean b)
	{
		_solverReport = b;
	}

	public String getSolverReportList()
	{
		return _solverList;
	}

	public void setSolverReportList(String s)
	{
		_solverList = s;
	}

	public Boolean getSlackReportOption()
	{
		return _slackReport;
	}

	public void setSlackReportOption(Boolean b)
	{
		_slackReport = b;
	}

	public Boolean getSlackReportSaveOption()
	{
		return _slackSave;
	}

	public void setSlackReportSaveOption(Boolean b)
	{
		_slackSave = b;
	}

	public String getAddXaOptions()
	{
		return _addXaOptions;
	}

	public void setAddXaOptions(String options)
	{
		_addXaOptions = options;
	}

	public Boolean getSvReportOption()
	{
		return _svReport;
	}

	public void setSvReportOption(Boolean b)
	{
		_svReport = b;
	}

	public Boolean getSvReportSaveOption()
	{
		return _svSave;
	}

	public void setSvReportSaveOption(Boolean b)
	{
		_svSave = b;
	}

	public Boolean getDssDebugOption()
	{
		return _dssDebug;
	}

	public void setDssDebugOption(Boolean b)
	{
		_dssDebug = b;
	}

	public Boolean getDssSaveOption()
	{
		return _dssSave;
	}

	public void setDssSaveOption(Boolean b)
	{
		_dssSave = b;
	}

	public Boolean getWsiDiOption()
	{
		return _genWsiDi;
	}

	public void setWsiDiOption(Boolean b)
	{
		_genWsiDi = b;
	}

	public Boolean getPosAnalysisOption()
	{
		return _posAnalysis;
	}

	public void setPosAnalysisOption(Boolean b)
	{
		_posAnalysis = b;
	}

	public Boolean getDialogWindowOption()
	{
		return _dialogWindow;
	}

	public void setDialogWindowOption(Boolean b)
	{
		_dialogWindow = b;
	}

	public Boolean getUseRestartOption()
	{
		return _useRestart;
	}

	public void setUseRestartOption(Boolean b)
	{
		_useRestart = b;
	}

	public Boolean getGenerateRestartOption()
	{
		return _genRestart;
	}

	public void setGenerateRestartOption(Boolean b)
	{
		_genRestart = b;
	}

	//CB added
	public boolean getPricingAndRHSSensitivityOption()
	{
		return _pricingRHSSensitivity;
	}

	//CB added
	public void setPricingAndRHSSensitivityOption(boolean b)
	{
		_pricingRHSSensitivity = b;
	}

	//CB added
	public boolean getActivitySensitivityOption()
	{
		return _activitySensitivity;
	}

	//CB added
	public void setActivitySensitivityOption(boolean b)
	{
		_activitySensitivity = b;
	}

	public Boolean getPositionStartYear()
	{
		return _posstartyear;
	}

	public void setPositionStartYear(Boolean b)
	{
		_posstartyear = b;
	}

	public Vector getAllProperties()
	{
		Vector v = new Vector();
		v.addElement(_name);
		v.addElement(_author);
		v.addElement(_date);
		v.addElement(_desc);
		v.addElement(_hydrology);
		v.addElement(_studyDir);
		v.addElement(_filename);
		v.addElement(_wreslFile);
		v.addElement(_svFile);
		v.addElement(_dvFile);
		v.addElement(_initFile);
		//DJE********************************************************
		v.addElement(_timeStep);

		if(!AppUtils.POSITION)
		{
			v.addElement(_numberSteps);
		}
		else
		{
			v.addElement(Integer.toString(AppUtils.nperiods));
		}
		v.addElement(_startDay);
		//******************************************
		v.addElement(_startMonth);
		v.addElement(_startYear);
		//DJE removed stop date parameters
		v.addElement(_simOption);
		v.addElement(_numberSequences);
		v.addElement(_solverReport);
		v.addElement(_solverList);
		v.addElement(_slackReport);
		v.addElement(_slackSave);
		v.addElement(_addXaOptions);
		v.addElement(_svReport);
		v.addElement(_svSave);
		v.addElement(_dssDebug);
		v.addElement(_dssSave);
		v.addElement(_genWsiDi);
		v.addElement(_useRestart);
		v.addElement(_genRestart);
		//CB not yet   v.addElement(_pricingRHSSensitivity);
		//CB not yet   v.addElement(_activitySensitivity);
		v.addElement(_initFileFPart);
		v.addElement(_posAnalysis);
		v.addElement(_dialogWindow);
		v.addElement(_posstartyear);
		return v;
	}

	/**
	 * saves all the study data in a binary format
	 */
	public void save(String saveFile) throws IOException
	{
		XmlDocument doc = new XmlDocument();
		_filename = saveFile;
		this.toXml(doc);
		PrintWriter pw = new PrintWriter(new FileOutputStream(saveFile));
		doc.write(pw);
		pw.close();
		_modified = false;
	}

	/**
	 * reads all the study data from a file in binary format
	 */
	public void load(String loadFile) throws IOException
	{
		Study sty = this;
		try
		{
			XmlDocument doc = XmlDocument.createXmlDocument(new FileInputStream(loadFile), false);
			sty.fromXml(doc.getDocumentElement());
		}
		catch(Exception e)
		{
			e.printStackTrace(System.err);
			throw new IOException("Invalid study file: " + loadFile);
		}
		setFileName(loadFile);
	}

	/**
	 * @return true if the study has been changed since a save
	 * had been executed
	 */
	public boolean isModified()
	{
		return _modified;
	}

	public boolean isUpdatedStudyObject()
	{
		return _newStudyObject;
	}

	public void updateStudyObject()
	{
		_newStudyObject = true;
	}

	/**
	 *
	 */
	public void fromXml(Element se)
	{
		//    TreeWalker tw = new TreeWalker(se);
		setName(se.getAttribute("name"));
		setAuthor(se.getAttribute("author"));
		//    setDate(se.getAttribute("date"));
		setDescription(se.getAttribute("desc"));
		setHydrologyVersion(se.getAttribute("hydrology"));
		setStudyDir(se.getAttribute("dir"));
		setFileName(se.getAttribute("filename"));
		setWreslFile(se.getAttribute("wreslfile"));
		setSvFile(se.getAttribute("svfile"));
		setDvFile(se.getAttribute("dvfile"));
		setInitFile(se.getAttribute("initfile"));
		setInitFileFPart(se.getAttribute("initfilefpart"));
		//DJE*************************************************
		if(se.getAttribute("ts").equals(""))
		{
			_newStudyObject = false;
		}
		setTimeStep(se.getAttribute("ts"));
		setStartDay(se.getAttribute("startday"));
		setStopDay(se.getAttribute("stopday"));
		//****************************************************
		setStartMonth(se.getAttribute("startmo"));
		setStartYear(new Integer(se.getAttribute("startyr")));
		setStopMonth(se.getAttribute("stopmo"));
		setStopYear(new Integer(se.getAttribute("stopyr")));
		setSimOption(se.getAttribute("simopt"));
		setNumberSequences(new Integer(se.getAttribute("numseq")));
		setCommonPath(se.getAttribute("common"));
	}

	/**
	 * Returns a element of an xml document
	 */
	public void toXml(XmlDocument doc)
	{
		Element styElement = doc.createElement("study");
		styElement.appendChild(doc.createComment("study xml format"));
		styElement.setAttribute("name", getName());
		styElement.setAttribute("author", getAuthor());
		styElement.setAttribute("date", getDate().toString());
		styElement.setAttribute("desc", getDescription());
		styElement.setAttribute("hydrology", getHydrologyVersion());
		styElement.setAttribute("dir", getStudyDir());
		styElement.setAttribute("filename", getFileName());
		styElement.setAttribute("wreslfile", getWreslFile());
		styElement.setAttribute("svfile", getSvFile());
		styElement.setAttribute("dvfile", getDvFile());
		styElement.setAttribute("initfile", getInitFile());
		styElement.setAttribute("initfilefpart", getInitFileFPart());
		//DJE********************************************************
		styElement.setAttribute("ts", getTimeStep());
		styElement.setAttribute("startday", getStartDay());
		styElement.setAttribute("stopday", getStopDay());
		//***********************************************************
		styElement.setAttribute("startmo", getStartMonth());
		styElement.setAttribute("startyr", getStartYear().toString());
		styElement.setAttribute("stopmo", getStopMonth());
		styElement.setAttribute("stopyr", getStopYear().toString());
		styElement.setAttribute("simopt", getSimOption());
		styElement.setAttribute("numseq", getNumberSequences().toString());
		styElement.setAttribute("common", getCommonPath());
		doc.appendChild(styElement);
	}

}

