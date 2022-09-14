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
import java.io.IOException;
import java.io.PrintWriter;
import java.io.Serializable;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.Enumeration;
import java.util.Hashtable;
import java.util.Vector;
import javax.swing.JOptionPane;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;

import calsim.gui.DtsTreeModel;
import calsim.gui.DtsTreePanel;
import calsim.gym.Network;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import vista.set.Group;
import vista.set.Pathname;
import vista.time.TimeWindow;

/**
 * A project contains a user defined list of DerivedTimeSeries (DTS) and
 * and MultipleTimeSeries (MTS).
 * This project is a container for user defined lists. All globally defined
 * lists (DTS and MTS) are not part of this project and exist independently
 * of this project. This list is a set in that none of the elements are
 * repeated.
 * A project has a study consisting of decision and state variable files from
 * which it gets the data. In the comparative mode there may further be an
 * alternative study against which the study is compared, usually by taking
 * differences.
 * <p>
 * <p>
 * The current project can always be accessed using AppUtils.getCurrentProject()
 *
 * @author Nicky Sandhu
 * @version $Id: Project.java,v 1.1.4.25 2001/10/23 16:28:23 jfenolio Exp $
 * @see AppUtils#getCurrentProject, DerivedTimeSeries, MultipleTimeSeries
 */
public class Project implements Serializable
{
	public static boolean DEBUG = false;
	/**
	 *
	 */
	// non - transient saved
	public Hashtable _dtsList, _mtsList, _dvList, _svList;
	boolean _dtsmod;
	String _fname, _fdir;
	DtsTreeModel _dtm = DtsTreePanel.getCurrentModel();
	String[] _array;
	String[] _bsv, _bdv, _bparts = null;
	Enumeration _e;
	String _loadFile = "";
	// private class variables
	// transient ( not saved )
	private transient Group _svg, _dvg, _sv2g, _dv2g, _sv3g, _dv3g, _sv4g, _dv4g;
	private String _svf, _dvf, _sv2f, _dv2f, _sv3f, _dv3f, _sv4f, _dv4f;
	private boolean _modified;
	private TimeWindow _tw;
	private String _studyName, _study1Name, _study2Name, _study3Name;
	private Network _network;
	private String _gifFile, _filename, _name;

	/**
	 * initializes all lists and creates an empty project
	 */
	public Project()
	{
		_dtsList = new Hashtable();
		_mtsList = new Hashtable();
		_svList = new Hashtable();
		_dvList = new Hashtable();
		setTimeWindow(AppUtils.DEFAULT_TIME_WINDOW);
		_modified = false;
		_dtsmod = true;
	}

	//Sets the b and c parts in the SV file to a Hashtable
	public void setSVHashtable()
	{
		Group g = Group.createGroup(getSVGroup());
		int num = g.getNumberOfDataReferences();
		_svList = new Hashtable(num);
		String b, c;
		_bsv = new String[num - 3];
		for(int i = 0; i < num - 3; i++)
		{
			Pathname p = g.getDataReference(i).getPathname();
			b = p.getPart(Pathname.B_PART);
			c = p.getPart(Pathname.C_PART);
			if(b == null || c == null)
			{
				return;
			}
			_bsv[i] = b;
			_svList.put(b, c);
		}
	}

	//Sets the b and c parts in the DV file to a Hashtable
	public void setDVHashtable()
	{
		Group g = Group.createGroup(getDVGroup());
		int num = g.getNumberOfDataReferences();
		_dvList = new Hashtable(num);
		String b, c;
		_bdv = new String[num];
		for(int i = 0; i < num; i++)
		{
			Pathname p = g.getDataReference(i).getPathname();
			b = p.getPart(Pathname.B_PART);
			c = p.getPart(Pathname.C_PART);
			if(b == null || c == null)
			{
				return;
			}
			_bdv[i] = b;
			_dvList.put(b, c);
		}
	}

	public void setBParts()
	{
		_bparts = new String[_bsv.length + _bdv.length];
		System.arraycopy(_bsv, 0, _bparts, 0, _bsv.length);
		System.arraycopy(_bdv, 0, _bparts, _bsv.length, _bdv.length);
		for(int i = 0; i < _bparts.length; i++)
		{
			System.out.println(_bparts[i]);
		}
	}

	//Returns a Hashtable with the b & c parts of the SV table in it
	public Hashtable getSVHashtable()
	{
		return _svList;
	}

	//Returns a Hashtable with the b & c parts of the DV table in it
	public Hashtable getDVHashtable()
	{
		return _dvList;
	}

	public String[] getBParts()
	{
		return _bparts;
	}

	/**
	 *
	 */
	public String getName()
	{
		return _name == null ? "" : _name;
	}

	/**
	 *
	 */
	public void setName(String name)
	{
		_name = name;
	}

	/**
	 *
	 */
	public String getFilename()
	{
		return _filename == null ? "" : _filename;
	}

	/**
	 *
	 */
	void setFilename(String filename)
	{
		_filename = filename;
	}

	/**
	 * adds a derived time series to this project only if
	 * it is not already there.
	 */
	public void add(DerivedTimeSeries dts)
	{
		if(!_dtsList.containsKey(dts.getName()))
		{
			_dtsList.put(dts.getName(), dts);
			_modified = true;
			_dtsmod = true;
		}
	}

	/**
	 * removes the DTS if it is in the project
	 */
	public void remove(DerivedTimeSeries dts)
	{
		if(_dtsList.contains(dts))
		{
			_dtsList.remove(dts.getName());
			_modified = true;
		}
	}

	/**
	 * removes a MTS or DTS by the given name from the project.
	 * else prints out an error
	 */
	public void remove(String name)
	{
		if(name == null)
		{
			return;
		}
		name = name.toUpperCase().trim();
		if(_dtsList.containsKey(name))
		{
			_dtsList.remove(name);
			_modified = true;
		}
		else if(_mtsList.containsKey(name))
		{
			_mtsList.remove(name);
			_modified = true;
		}
		else
		{
			System.err.println("No matching name : " + name + " found in mts or dts list");
		}
	}

	public boolean isInLists(String name)
	{
		if(name == null)
		{
			return false;
		}
		name = name.toUpperCase().trim();
		if(_dtsList.containsKey(name))
		{
			return true;
		}
		else
		{
			return _mtsList.containsKey(name);
		}
	}

	/**
	 * adds a MTS if it does not already present
	 */
	public void add(MultipleTimeSeries mts)
	{
		if(!_mtsList.containsKey(mts.getName()))
		{
			_mtsList.put(mts.getName(), mts);
			_modified = true;
			_dtsmod = true;
		}
	}

	/**
	 * removes the given MTS if present
	 */
	public void remove(MultipleTimeSeries mts)
	{
		if(_mtsList.contains(mts))
		{
			_mtsList.remove(mts.getName());
		}
	}

	public Hashtable getHashtable()
	{
		return _dtsList;
	}

	public void setHashtable(Hashtable h)
	{
		_dtsList = h;
	}

	/**
	 * @return the list of DTS as an array or null if none present
	 */
	public DerivedTimeSeries[] getDTSList()
	{
		if(_dtsList == null)
		{
			return null;
		}
		DerivedTimeSeries[] array = new DerivedTimeSeries[_dtsList.size()];
		if(array == null)
		{
			System.out.println(true);
		}
		if(_dtsList.size() == 0)
		{
			return null;
		}
		int count = 0;
		for(Enumeration e = _dtsList.elements(); e.hasMoreElements(); )
		{
			array[count] = (DerivedTimeSeries) e.nextElement();
			count++;
		}
		return array;
	}

	/**
	 * @clear the list of DTS array
	 */
	public void clearDTSList()
	{
		if(_dtsList == null)
		{
			return;
		}
		_dtsList.clear();
		_dtsList = new Hashtable();
	}

	/**
	 * @clear the list of DTS array
	 */
	public void clearMTSList()
	{
		if(_mtsList == null)
		{
			return;
		}
		_mtsList.clear();
		_mtsList = new Hashtable();
	}

	/**
	 * @return the names of all the DTS in this project
	 */
	public String[] getDTSNames()
	{
		if(_dtsList.size() == 0)
		{
			return null;
		}
		String[] array = new String[_dtsList.size() + 1];
		array[0] = " ";
		int count = 1;
		for(Enumeration e = _dtsList.elements(); e.hasMoreElements(); )
		{
			array[count] = ((DerivedTimeSeries) e.nextElement()).getName();
			count++;
		}
		return array;
	}

	/**
	 * @return the list of MTS as an array or null if none present
	 */
	public MultipleTimeSeries[] getMTSList()
	{
		if(_mtsList.size() == 0)
		{
			return null;
		}
		MultipleTimeSeries[] array = new MultipleTimeSeries[_mtsList.size()];
		int count = 0;
		for(Enumeration e = _mtsList.elements(); e.hasMoreElements(); )
		{
			array[count] = (MultipleTimeSeries) e.nextElement();
			count++;
		}
		return array;
	}

	/**
	 * @return the names of all the MTS in this project
	 */
	public String[] getMTSNames()
	{
		if(_mtsList.size() == 0)
		{
			return null;
		}
		String[] array = new String[_mtsList.size()];
		int count = 0;
		for(Enumeration e = _mtsList.elements(); e.hasMoreElements(); )
		{
			array[count] = ((MultipleTimeSeries) e.nextElement()).getName();
			count++;
		}
		return array;
	}

	/**
	 * @return the DTS with the given name or null if not found
	 */
	public DerivedTimeSeries getDTS(String name)
	{
		DerivedTimeSeries dts;
		if(_dtsList != null)
		{
			dts = (DerivedTimeSeries) _dtsList.get(name);
		}
		else
		{
			dts = null;
		}
		return dts;
	}

	/**
	 * @return the number of dts of the given name
	 */
	public int getNumberOfDTS()
	{
		return _dtsList.size();
	}

	/**
	 * @return the number of mts of the given name
	 */
	public int getNumberOfMTS()
	{
		return _mtsList.size();
	}

	/**
	 * @return the MTS with the given name or null if not found
	 */
	public MultipleTimeSeries getMTS(String name)
	{
		MultipleTimeSeries mts = (MultipleTimeSeries) _mtsList.get(name);
		return mts;
	}

	/**
	 * adds the given array to the project list, again adding
	 * only those that are not already present
	 */
	public void addDTSList(DerivedTimeSeries[] dts)
	{
		if(dts == null || dts.length == 0)
		{
			return;
		}
		for(int i = 0; i < dts.length; i++)
		{
			add(dts[i]);
		}
	}

	/**
	 * adds the given array to the project list, again adding
	 * only those that are not already present
	 */
	public void addMTSList(MultipleTimeSeries[] mts)
	{
		if(mts == null || mts.length == 0)
		{
			return;
		}
		for(int i = 0; i < mts.length; i++)
		{
			add(mts[i]);
		}
	}

	/**
	 * copies the lists from another project and includes them
	 * in its own list. Equivalent to merging the two projects
	 * This merging does not change this projects other non-mergable
	 * items such as study files, time windows, etcetra. Such an
	 * effect may be achieved by carefully thinking which object is
	 * being copied from
	 */
	public void copyFrom(Project p)
	{
		this.addDTSList(p.getDTSList());
		this.addMTSList(p.getMTSList());
	}

	/**
	 * creates a copy of this project
	 *
	 * @return a copy of itself
	 */
	public Project createCopy()
	{
		Project p = new Project();
		p.copyFrom(this);
		return p;
	}

	/**
	 *
	 */
	public String getDVFile()
	{
		return _dvf == null ? "" : _dvf;
	}

	/**
	 * sets the decision variable file for the study
	 */
	public void setDVFile(String file)
	{
		_dvf = file;
		_dvg = null;
		_modified = true;
	}

	/**
	 *
	 */
	public String getSVFile()
	{
		return _svf == null ? "" : _svf;
	}

	/**
	 * sets the state variable file for the study
	 */
	public void setSVFile(String file)
	{
		_svf = file;
		_svg = null;
		_modified = true;
	}

	/**
	 *
	 */
	public String getDV2File()
	{
		return _dv2f == null ? "" : _dv2f;
	}

	/**
	 * sets the decision variable file for the alternative study
	 */
	public void setDV2File(String file)
	{
		_dv2f = file;
		_dv2g = null;
		_modified = true;
	}

	/**
	 *
	 */
	public String getSV2File()
	{
		return _sv2f == null ? "" : _sv2f;
	}

	/**
	 * sets the state variable file for the alternative study
	 */
	public void setSV2File(String file)
	{
		_sv2f = file;
		_sv2g = null;
		_modified = true;
	}

	/**
	 *
	 */
	public String getDV3File()
	{
		return _dv3f == null ? "" : _dv3f;
	}

	/**
	 * sets the decision variable file for the alternative study
	 */
	public void setDV3File(String file)
	{
		_dv3f = file;
		_dv3g = null;
		_modified = true;
	}

	/**
	 *
	 */
	public String getSV3File()
	{
		return _sv3f == null ? "" : _sv3f;
	}

	/**
	 * sets the state variable file for the alternative study
	 */
	public void setSV3File(String file)
	{
		_sv3f = file;
		_sv3g = null;
		_modified = true;
	}

	/**
	 *
	 */
	public String getDV4File()
	{
		return _dv4f == null ? "" : _dv4f;
	}

	/**
	 * sets the decision variable file for the alternative study
	 */
	public void setDV4File(String file)
	{
		_dv4f = file;
		_dv4g = null;
		_modified = true;
	}

	/**
	 *
	 */
	public String getSV4File()
	{
		return _sv4f == null ? "" : _sv4f;
	}

	/**
	 * sets the state variable file for the alternative study
	 */
	public void setSV4File(String file)
	{
		_sv4f = file;
		_sv4g = null;
		_modified = true;
	}

	/**
	 * @return a group of all the data references (containing data sets)
	 * for the decision variables in the study
	 * @see vista.set.Group
	 */
	public Group getDVGroup()
	{
		if(_dvg == null || AppUtils.needsRecataloging(_dvf))
		{
			_dvg = AppUtils.openDSSFile(_dvf);
		}
		return _dvg;
	}

	/**
	 * @return a group of all the data references (containing data sets)
	 * for the state variables in the study
	 * @see vista.set.Group
	 */
	public Group getSVGroup()
	{
		if(_svg == null || AppUtils.needsRecataloging(_svf))
		{
			_svg = AppUtils.openDSSFile(_svf);
		}
		return _svg;
	}

	/**
	 * @return a group of all the data references (containing data sets)
	 * for the decision variables in the alternative study
	 * @see vista.set.Group
	 */
	public Group getDV2Group()
	{
		if(_dv2g == null || AppUtils.needsRecataloging(_dv2f))
		{
			_dv2g = AppUtils.openDSSFile(_dv2f);
		}
		//if ( _dv2g == null ){
		//  throw new RuntimeException("No file loaded for alternate study decision variables!");
		//}
		return _dv2g;
	}

	/**
	 * @return a group of all the data references (containing data sets)
	 * for the state variables in the alternative study
	 * @see vista.set.Group
	 */
	public Group getSV2Group()
	{
		if(_sv2g == null || AppUtils.needsRecataloging(_sv2f))
		{
			_sv2g = AppUtils.openDSSFile(_sv2f);
		}
		//if ( _sv2g == null ){
		//  throw new RuntimeException("No file loaded for alternate study state variables!");
		//}
		return _sv2g;
	}

	/**
	 * @return a group of all the data references (containing data sets)
	 * for the decision variables in the alternative study
	 * @see vista.set.Group
	 */
	public Group getDV3Group()
	{
		if(_dv3g == null || AppUtils.needsRecataloging(_dv3f))
		{
			_dv3g = AppUtils.openDSSFile(_dv3f);
		}
		//if ( _dv2g == null ){
		//  throw new RuntimeException("No file loaded for alternate study decision variables!");
		//}
		return _dv3g;
	}

	/**
	 * @return a group of all the data references (containing data sets)
	 * for the state variables in the alternative study
	 * @see vista.set.Group
	 */
	public Group getSV3Group()
	{
		if(_sv3g == null || AppUtils.needsRecataloging(_sv3f))
		{
			_sv3g = AppUtils.openDSSFile(_sv3f);
		}
		//if ( _sv3g == null ){
		//  throw new RuntimeException("No file loaded for alternate study state variables!");
		//}
		return _sv3g;
	}

	/**
	 * @return a group of all the data references (containing data sets)
	 * for the decision variables in the alternative study
	 * @see vista.set.Group
	 */
	public Group getDV4Group()
	{
		if(_dv4g == null || AppUtils.needsRecataloging(_dv4f))
		{
			_dv4g = AppUtils.openDSSFile(_dv4f);
		}
		//if ( _dv4g == null ){
		//  throw new RuntimeException("No file loaded for alternate study decision variables!");
		//}
		return _dv4g;
	}

	/**
	 * @return a group of all the data references (containing data sets)
	 * for the state variables in the alternative study
	 * @see vista.set.Group
	 */
	public Group getSV4Group()
	{
		if(_sv4g == null || AppUtils.needsRecataloging(_sv4f))
		{
			_sv4g = AppUtils.openDSSFile(_sv4f);
		}
		//if ( _sv4g == null ){
		//  throw new RuntimeException("No file loaded for alternate study state variables!");
		//}
		return _sv4g;
	}

	/**
	 * sets the time period of interest in this study to the
	 * default of what exists in the database
	 */
	public void setToDefaultTimeWindow()
	{
		_tw = AppUtils.guessTimeWindowFromGroup(getSVGroup());
	}

	/**
	 * @return the currently valid time window
	 * @see vista.time.TimeWindow
	 */
	public TimeWindow getTimeWindow()
	{
		return _tw;
	}

	/**
	 * sets the time window to the given time window string
	 *
	 * @param twstr is a time window string representation in
	 *              the format MMMyyyy - MMMyyyy, e.g.
	 *              JAN1990 - NOV1993
	 * @see vista.time.TimeWindow
	 */
	public void setTimeWindow(String twstr)
	{
		_tw = AppUtils.createTimeWindowFromString(twstr);
		_modified = true;
	}

	/**
	 * gets the study name by using the F part of the data base
	 * to guess it
	 *
	 * @return the study name
	 */
	public String getBaseName()
	{
		if(_studyName == null)
		{
			return AppUtils.guessStudyNameFromGroup(getDVGroup());
		}
		else
		{
			return _studyName;
		}
	}

	/**
	 * sets the study name explicitly, this will now be the study name
	 * irrespective of what is in the data base
	 */
	public void setBaseName(String studyName)
	{
		_studyName = studyName;
		_modified = true;
	}

	/**
	 * gets the alternative study name by using the F part of the data base
	 * to guess it
	 *
	 * @return the alternative study name
	 */
	public String getComp1Name()
	{
		if(_study1Name == null)
		{
			return AppUtils.guessStudyNameFromGroup(getDV2Group());
		}
		else
		{
			return _study1Name;
		}
	}

	/**
	 * sets the alternative name explicitly, this will now be the alternative study name
	 * irrespective of what is in the data base
	 */
	public void setComp1Name(String studyName)
	{
		_study1Name = studyName;
		_modified = true;
	}

	/**
	 * gets the alternative study name by using the F part of the data base
	 * to guess it
	 *
	 * @return the alternative study name
	 */
	public String getComp2Name()
	{
		if(_study2Name == null)
		{
			return AppUtils.guessStudyNameFromGroup(getDV3Group());
		}
		else
		{
			return _study2Name;
		}
	}

	/**
	 * sets the alternative name explicitly, this will now be the alternative study name
	 * irrespective of what is in the data base
	 */
	public void setComp2Name(String studyName)
	{
		_study2Name = studyName;
		_modified = true;
	}

	/**
	 * gets the alternative study name by using the F part of the data base
	 * to guess it
	 *
	 * @return the alternative study name
	 */
	public String getComp3Name()
	{
		if(_study3Name == null)
		{
			return AppUtils.guessStudyNameFromGroup(getDV4Group());
		}
		else
		{
			return _study3Name;
		}
	}

	/**
	 * sets the alternative name explicitly, this will now be the alternative study name
	 * irrespective of what is in the data base
	 */
	public void setComp3Name(String studyName)
	{
		_study3Name = studyName;
		_modified = true;
	}

	/**
	 * @returns the network associated with this project or null if none available
	 */
	public Network getNetwork()
	{
		return _network;
	}

	/**
	 * sets the network associated with this project
	 *
	 * @param connectivityFile the file containing the node - arc connectivity
	 */
	public void setNetwork(String connectivityFile)
	{
		try
		{
			_network = Network.read(connectivityFile);
			_modified = true;
		}
		catch(IOException ioe)
		{
			_network = null; // uncache the old one, is this wize ??
			System.err.println("Error initializing network from " + connectivityFile);
			System.err.println("Error: " + ioe.getMessage());
			throw new RuntimeException(ioe.getMessage());
		}
	}

	/**
	 * @return the file for loading the schematic gif or
	 * null if none available
	 */
	public String getSchematicGif()
	{
		return _gifFile;
	}

	/**
	 * sets the gif file associated with this project
	 */
	public void setSchematicGif(String gifFile)
	{
		_gifFile = gifFile;
		_modified = true;
	}

	/**
	 * saves all the state variables in a binary format
	 */
	public void save(String saveFile) throws IOException
	{
		_filename = saveFile;
		try(PrintWriter pw = new PrintWriter(Files.newOutputStream(Paths.get(saveFile)));
			PrintWriter pw1 = new PrintWriter(Files.newOutputStream(Paths.get(_fname))))
		{
			DocumentBuilderFactory docFactory = DocumentBuilderFactory.newInstance();
			DocumentBuilder docBuilder = docFactory.newDocumentBuilder();
			Document prjdoc = docBuilder.newDocument();
			Document dtsdoc = docBuilder.newDocument();
			Element masterdts = dtsdoc.createElement("dts_master");
			dtsdoc.appendChild(masterdts);
			this.toXml(prjdoc);
			DtsTreePanel.getCurrentModel().saveData(dtsdoc, masterdts);
			DtsTreePanel.getCurrentModel().saveDts(dtsdoc, masterdts);
			DtsTreePanel.getCurrentModel().saveMts(dtsdoc, masterdts);

			TransformerFactory transformerFactory = TransformerFactory.newInstance();
			Transformer transformer = transformerFactory.newTransformer();
			DOMSource source = new DOMSource(prjdoc);
			StreamResult result = new StreamResult(pw);
			transformer.transform(source, result);
			pw.close();

			source = new DOMSource(dtsdoc);
			result = new StreamResult(pw1);
			transformer.transform(source, result);
			pw1.close();
			_modified = false;
		}
		catch(ParserConfigurationException | TransformerException e)
		{
			throw new IOException(e);
		}
	}

	/**
	 * reads all the state variables from a file in binary format,
	 * usually from a previously saved project file and merges it
	 * with this one, except that it only overwrites the study, alternative study
	 * time window and other such parameters which cannot be merged, when
	 * these are null. In other words if this project had not previously defined
	 * these parameters they may be loaded from the saved project
	 */
	public void load(String loadFile) throws IOException
	{
		Project prj = this;
		_dtsList.clear();
		_loadFile = loadFile;
		try
		{
			DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
			DocumentBuilder db = dbf.newDocumentBuilder();
			Document doc = db.parse(new File(loadFile));
			prj.fromXml(doc.getDocumentElement());
		}
		catch(Exception e)
		{
			e.printStackTrace(System.err);
			throw new IOException("Invalid project file: " + loadFile);
		}
		setFilename(loadFile);
		//if (oldprj) _dtm.createTreeFromPrj(getDTSNames(),getMTSNames());
	}

	/**
	 * @return true if the project has been changed since a save
	 * had been executed
	 */
	public boolean isModified()
	{
		return _modified;
	}

	/**
	 *
	 */
	public void fromXml(Element pe) throws IOException
	{
		_filename = pe.getAttribute("name");
		//
		Node tw = pe.getElementsByTagName("tw").item(0);
		_tw = AppUtils.createTimeWindowFromString(tw.getFirstChild().getNodeValue());
		// study items
		NodeList studyList = pe.getElementsByTagName("study");
		for(int i = 0; i < studyList.getLength(); i++)
		{
			Element se = (Element) studyList.item(i);
			if(se == null)
			{
				break;
			}
			int si = new Integer(se.getAttribute("number")).intValue();
			if(si == 1)
			{
				setSVFile(se.getAttribute("svf"));
				setDVFile(se.getAttribute("dvf"));
			}
			else if(si == 2)
			{
				setSV2File(se.getAttribute("svf"));
				setDV2File(se.getAttribute("dvf"));
			}
			else if(si == 3)
			{
				setSV3File(se.getAttribute("svf"));
				setDV3File(se.getAttribute("dvf"));
			}
			else if(si == 4)
			{
				setSV4File(se.getAttribute("svf"));
				setDV4File(se.getAttribute("dvf"));
			}
		}
		NodeList elements = pe.getElementsByTagName("dts_master");
		//Dts Master File
		if(elements.getLength() > 0)
		{
			Element me = (Element) elements.item(0);
			String filepath = me.getAttribute("file");
			File file = new File(filepath);
			String filedir = file.getParent();
			setDtsFile(filepath);
			setDtsDir(filedir);
			if(file.exists())
			{
				_dtm = DtsTreePanel.getCurrentModel();
				try
				{
					_dtm.getFile(filepath, filedir);
				}
				catch(Exception e)
				{
					throw new IOException("Dts master file");
				}
				Vector dtsvector = DtsTreeModel.getPrjDts();
				if(dtsvector.size() > 0)
				{
					for(int i = 0; i < dtsvector.capacity(); i++)
					{
						DerivedTimeSeries dts = (DerivedTimeSeries) dtsvector.elementAt(i);
						add(dts);
					}
				}
				Vector mtsvector = DtsTreeModel.getPrjMts();
				if(mtsvector.size() > 0)
				{
					for(int i = 0; i < mtsvector.capacity(); i++)
					{
						MultipleTimeSeries mts = (MultipleTimeSeries) mtsvector.elementAt(i);
						add(mts);
					}
				}
			}
			else
			{
				JOptionPane.showMessageDialog(null, "Cannot find Tree File", "Warning", JOptionPane.WARNING_MESSAGE);
			}
		}
		else
		{
			NodeList dtsElements = pe.getElementsByTagName("DTS");
			for(int i = 0; i < dtsElements.getLength(); i++)
			{
				Element de = (Element) dtsElements.item(i);
				if(de == null)
				{
					break;
				}
				DerivedTimeSeries dts = new DerivedTimeSeries();
				dts.fromXml(de);
				DerivedTimeSeries olddts = getDTS(dts.getName());
				if(olddts != null)
				{
					remove(olddts);
				}
				add(dts);
			}
			NodeList mtsElements = pe.getElementsByTagName("DTS");
			for(int i = 0; i < mtsElements.getLength(); i++)
			{
				Element de = (Element) mtsElements.item(i);
				if(de == null)
				{
					break;
				}
				MultipleTimeSeries mts = new MultipleTimeSeries();
				mts.fromXml(de);
				MultipleTimeSeries oldmts = getMTS(mts.getName());
				if(oldmts != null)
				{
					remove(oldmts);
				}
				add(mts);
			}
			int end = _loadFile.lastIndexOf("\\");
			String treefile2 = _loadFile.substring(0, end) + "\\new-file.tree";
			setDtsFile(treefile2);
			setDtsDir(treefile2);
			_dtm.createTreeFromPrj(getDTSNames(), getMTSNames(), treefile2);
		}
	}

	/**
	 * Returns a element of an xml document
	 */
	public void toXml(Document doc)
	{
		Element prjElement = doc.createElement("project");
		prjElement.appendChild(doc.createComment("project xml format"));
		prjElement.setAttribute("name", _filename);
		// time window
		Element twe = doc.createElement("tw");
		twe.appendChild(doc.createTextNode
				(_tw == null ? " "
						: _tw.getStartTime().toString().substring(2, 9) + " - " +
						_tw.getEndTime().toString().substring(2, 9)
				)
		);
		prjElement.appendChild(twe);
		// sv/dv files
		Element s1 = doc.createElement("study");
		s1.setAttribute("number", "1");
		s1.setAttribute("svf", getSVFile());
		s1.setAttribute("dvf", getDVFile());
		Element s2 = doc.createElement("study");
		s2.setAttribute("number", "2");
		s2.setAttribute("svf", getSV2File());
		s2.setAttribute("dvf", getDV2File());
		Element s3 = doc.createElement("study");
		s3.setAttribute("number", "3");
		s3.setAttribute("svf", getSV3File());
		s3.setAttribute("dvf", getDV3File());
		Element s4 = doc.createElement("study");
		s4.setAttribute("number", "4");
		s4.setAttribute("svf", getSV4File());
		s4.setAttribute("dvf", getDV4File());
		prjElement.appendChild(s1);
		prjElement.appendChild(s2);
		prjElement.appendChild(s3);
		prjElement.appendChild(s4);
		Element s5 = doc.createElement("dts_master");
		s5.setAttribute("file", _fname);
		prjElement.appendChild(s5);
		// create elements for dts files
 /*
    DerivedTimeSeries [] dtsList = getDTSList();
    if ( dtsList != null ) {
      for(int i=0; i < dtsList.length; i++) dtsList[i].toXml(doc, prjElement);
    }

    // create elements for mts files
    MultipleTimeSeries [] mtsList = getMTSList();
    if ( mtsList != null ) {
      for(int i=0; i < mtsList.length; i++) mtsList[i].toXml(doc, prjElement);
    }
   */
		//
		doc.appendChild(prjElement);
	}

	/**
	 * Sets the current location and file for the DTS/MTS Tree
	 */
	public void setDtsFile(String fname)
	{
		_fname = fname;
	}

	/**
	 * Gets the current location and file for the DTS/MTS Tree
	 */
	public String getDtsPath()
	{
		return _fname == null ? "" : _fname;
	}

	/**
	 * Gets the current directory for the DTS/MTS Tree
	 */
	public String getDtsDir()
	{
		return _fdir == null ? "" : _fdir;
	}

	/**
	 * Sets the current directory for the DTS/MTS Tree
	 */
	public void setDtsDir(String fdir)
	{
		_fdir = fdir;
	}

	/**
	 * Gets the boolean the says if the current dts list has been modified
	 */
	public boolean getDTSMod()
	{
		return _dtsmod;
	}

	/**
	 * Updates the boolean the says if the current dts list has been modified
	 */
	public void setDTSMod(boolean dtsmod)
	{
		_dtsmod = dtsmod;
	}

}
