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
package vista.db.dss;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.LineNumberReader;
import java.io.PrintWriter;
import java.rmi.RemoteException;
import java.rmi.server.RemoteServer;
import java.rmi.server.ServerNotActiveException;
import java.rmi.server.UnicastRemoteObject;
import java.util.ArrayList;
import java.util.Date;

import vista.set.DataReference;
import vista.set.DataSet;
import vista.set.DataSetAttr;
import vista.set.DataType;
import vista.set.DefaultDataSet;
import vista.set.IrregularTimeSeries;
import vista.set.Pathname;
import vista.set.RegularTimeSeries;
import vista.time.Time;
import vista.time.TimeInterval;

/**
 * Implements the server side of the RMI call
 * 
 * @author Nicky Sandhu
 * @version $Id: DSSRemoteClientImpl.java,v 1.1 2003/10/02 20:48:45 redwood Exp
 *          $
 */
class DSSRemoteClientImpl extends UnicastRemoteObject implements
		DSSRemoteClient {
	public static String logFile = "vista.rmilog";
	public PrintWriter _writer;
	public static boolean VERBOSE = true;

	/**
   *
   */
	public DSSRemoteClientImpl() throws RemoteException {
		super();
		try {
			String logFile = (String) ServerProperties.props.get("logfile");
			if (logFile.equals("")) {
				VERBOSE = false;
				_writer = null;
			} else
				_writer = new PrintWriter(new FileWriter(logFile));
		} catch (IOException ioe) {
			ioe.printStackTrace(System.err);
			throw new RemoteException("Log file exception ", ioe);
		}
	}

	/**
	 * sends request for listing of dss files on server and returns with an
	 * array of string of that list
	 */
	public String[] getListing(String directory) throws RemoteException {
		long stm = System.currentTimeMillis();
		String[] list = null;
		try {
			File currentDir = new File(directory);
			if (currentDir.isDirectory() && currentDir.canRead()) {
				list = currentDir.list(new SubscriptFilenameFilter(
						DSSUtil.DSS_EXTENSION));
			} else {
				if (!currentDir.isDirectory()) {
					throw new RemoteException(directory
							+ "is not a directory? "
							+ "Please enter a valid directory name");
				} else if (!currentDir.canRead()) {
					throw new RemoteException(directory + " cannot be read");
				} else {
					throw new RemoteException(
							"Unknown error occured accessing directory");
				}
			}
		} catch (SecurityException se) {
			throw new RemoteException("A security violation " + se.getMessage()
					+ "occured when accessing " + directory);
		}
		if (VERBOSE) {
			try {
				_writer.println("Request from: " + RemoteServer.getClientHost()
						+ " @ " + new Date());
			} catch (ServerNotActiveException snae) {
			}
			_writer.println("Send listing for " + directory);
			_writer.println("Time taken: " + (System.currentTimeMillis() - stm)
					+ " ms");
			_writer.flush();
		}
		return list;
	}

	/**
	 * returns an array of string where each element is a line in the catalog
	 * file
	 */
	public String[] getCatalog(String dssFile, boolean doFreshCatalog)
			throws RemoteException {
		long stm = System.currentTimeMillis();
		String catalogFile = DSSUtil.getCatalogFilename(dssFile);
		long timeDifference = new File(catalogFile).lastModified()
				- new File(dssFile).lastModified();
		if (doFreshCatalog
				|| ((!new File(catalogFile).exists()) || timeDifference < -10)) {
			try {
				_dataReader.generateCatalog(dssFile);
				// as dss calls zclose on dss file last, the file gets
				// recataloged every time
				// to avoid that, lets touch the catalog file once again.
				new File(catalogFile).setLastModified(new Date().getTime());
			} catch (Exception ie) {
				throw new RemoteException(ie.toString()
						+ "Exception while creating catalog " + catalogFile);
			} finally {
				_dataReader.close();
			}
		}
		LineNumberReader reader = null;
		try {
			reader = new LineNumberReader(new BufferedReader(
					new FileReader(catalogFile)));
			ArrayList<String> catalog = new ArrayList<String>();
			String line = null;
			while ((line = reader.readLine()) != null) {
				catalog.add(line);
			}
			String[] catalogListing = new String[catalog.size()];
			catalogListing = catalog.toArray(catalogListing);
			if (VERBOSE) {
				try {
					_writer
							.println("Request from: "
									+ RemoteServer.getClientHost() + " @ "
									+ new Date());
				} catch (ServerNotActiveException snae) {
				}
				_writer.println("Send catalog for " + dssFile);
				_writer.println("Time taken: "
						+ (System.currentTimeMillis() - stm) + " ms");
				_writer.flush();
			}
			return catalogListing;
		} catch (Exception e) {
			throw new RemoteException(e.toString()
					+ " exception while reading catalog " + catalogFile);
		} finally {
			if (reader !=null) {
				try {
					reader.close();
				} catch (IOException e) {
					e.printStackTrace();
				}
			}
		}
	}

	/**
   *
   */
	private void checkReference(DataReference ref) throws RemoteException {
		if (ref == null)
			throw new RemoteException(
					"No/Null reference specifed for data retreival");
		String filename = ref.getFilename();
		String pathname = ref.getPathname().toString();
		if (filename == null)
			throw new RemoteException("No filename specified in reference");
		try {
			File f = new File(filename);
			if (!f.exists())
				throw new RemoteException(filename + " is unavailable");
		} catch (SecurityException se) {
			throw new RemoteException("Security exception while accessing "
					+ filename);
		}
		// check record type
		int recordType = _dataReader.recordType(filename, pathname);
		if (recordType != DSSUtil.REGULAR_TIME_SERIES
				&& recordType != DSSUtil.IRREGULAR_TIME_SERIES
				&& recordType != DSSUtil.PAIRED
				&& recordType != DSSUtil.REGULAR_TIME_SERIES + 5
				&& recordType != DSSUtil.IRREGULAR_TIME_SERIES + 5)
			throw new RemoteException("Data " + filename + ":" + pathname
					+ " is of unrecognized type: " + recordType);
	}

	/**
	 * returns a data set for the given data reference
	 */
	public DataSet getData(DataReference ref, boolean retrieveFlags)
			throws RemoteException {
		try {
			long stm = System.currentTimeMillis();
			checkReference(ref);
			String filename = ref.getFilename();
			String pathname = ref.getPathname().toString();
			Pathname path = ref.getPathname();
			String dataName = "/" + path.getPart(Pathname.A_PART) + "/"
					+ path.getPart(Pathname.B_PART) + "/"
					+ path.getPart(Pathname.C_PART) + "/" + "/" + "/"
					+ path.getPart(Pathname.F_PART) + "/";
			if (VERBOSE) {
				try {
					_writer
							.println("Request from: "
									+ RemoteServer.getClientHost() + " @ "
									+ new Date());
				} catch (ServerNotActiveException snae) {
				}
				_writer.println("File: " + filename);
				_writer.println("pathname: " + pathname);
				_writer.flush();
			}
			int recordType = _dataReader.recordType(filename, pathname);
			int startTime = 0;
			int endTime = 0;
			if (recordType == DSSUtil.REGULAR_TIME_SERIES
					|| recordType == DSSUtil.REGULAR_TIME_SERIES + 5) {
				startTime = (int) ref.getTimeWindow().getStartTime()
						.getTimeInMinutes();
				endTime = (int) ref.getTimeWindow().getEndTime()
						.getTimeInMinutes();
				//
				DSSData data = _dataReader.getData(filename, pathname,
						startTime, endTime, retrieveFlags);
				if (data._offset > 0) {
					System.out.println("Path: " + pathname + " has offset "
							+ data._offset);
					Time stime = ref.getTimeWindow().getStartTime();
					stime.incrementBy(ref.getTimeInterval(), -1);
					startTime = (int) (stime.getTimeInMinutes() + data._offset);
				}
				if (data == null)
					throw new RemoteException("Data " + filename + "::"
							+ pathname + " is empty?");
				// write out number read
				int numberRead = (data == null) ? 0 : data._numberRead;
				if (numberRead <= 0)
					throw new RemoteException("Data " + filename + "::"
							+ pathname + " is empty ?");
				DataSetAttr attr = new DataSetAttr(path
						.getPart(Pathname.A_PART), path
						.getPart(Pathname.B_PART), path
						.getPart(Pathname.C_PART), path
						.getPart(Pathname.F_PART),
						DataType.REGULAR_TIME_SERIES, "TIME", data._yUnits, "",
						data._yType);
				//
				TimeInterval ti = ref.getTimeInterval();
				if (VERBOSE) {
					try {
						_writer.println("Request from: "
								+ RemoteServer.getClientHost() + " @ "
								+ new Date());
					} catch (ServerNotActiveException snae) {
					}
					_writer.println("Time taken: "
							+ (System.currentTimeMillis() - stm) + " ms");
					_writer.flush();
				}
				Time stime = DSSUtil.getTimeFactory().createTime(startTime);
				return new RegularTimeSeries(path.toString(), stime, ti,
						data._yValues, data._flags, attr);
			} else if (recordType == DSSUtil.PAIRED) { // for paired data
				//
				DSSData data = _dataReader.getData(filename, pathname, 0, 0,
						false);
				if (data == null)
					throw new RemoteException("Data " + filename + "::"
							+ pathname + " is empty?");
				// write out number of data points read
				int numberRead = data._numberRead;
				if (DEBUG)
					System.out.println("Paired data count: " + numberRead);
				if (numberRead <= 0)
					throw new RemoteException("Data " + filename + "::"
							+ pathname + " is empty ?");
				// write out units of data
				DataSetAttr attr = new DataSetAttr(path
						.getPart(Pathname.A_PART), path
						.getPart(Pathname.B_PART), path
						.getPart(Pathname.C_PART), path
						.getPart(Pathname.F_PART), DataType.PAIRED,
						data._xUnits, data._yUnits, data._xType, data._yType);

				if (VERBOSE) {
					try {
						_writer.println("Request from: "
								+ RemoteServer.getClientHost() + " @ "
								+ new Date());
					} catch (ServerNotActiveException snae) {
					}
					_writer.println("Time taken: "
							+ (System.currentTimeMillis() - stm) + " ms");
					_writer.flush();
				}
				return new DefaultDataSet(dataName, data._xValues,
						data._yValues, data._flags, attr);
			} else if (recordType == DSSUtil.IRREGULAR_TIME_SERIES
					|| recordType == DSSUtil.IRREGULAR_TIME_SERIES + 5) { // irregular
				// time
				startTime = (int) ref.getTimeWindow().getStartTime()
						.getTimeInMinutes();
				endTime = (int) ref.getTimeWindow().getEndTime()
						.getTimeInMinutes();
				//
				DSSData data = _dataReader.getData(filename, pathname,
						startTime, endTime, retrieveFlags);
				// write out number read
				if (data == null || data._numberRead == 0)
					throw new RemoteException("Data " + filename + "::"
							+ pathname + " is empty?");
				DataSetAttr attr = new DataSetAttr(path
						.getPart(Pathname.A_PART), path
						.getPart(Pathname.B_PART), path
						.getPart(Pathname.C_PART), path
						.getPart(Pathname.F_PART),
						DataType.IRREGULAR_TIME_SERIES, "TIME", data._yUnits,
						"", data._yType);
				if (VERBOSE) {
					try {
						_writer.println("Request from: "
								+ RemoteServer.getClientHost() + " @ "
								+ new Date());
					} catch (ServerNotActiveException snae) {
					}
					_writer.println("Time taken: "
							+ (System.currentTimeMillis() - stm) + " ms");
					_writer.flush();
				}
				return new IrregularTimeSeries(path.toString(), data._xValues,
						data._yValues, data._flags, attr);
			} else {
				throw new RemoteException("Data " + filename + "::" + pathname
						+ " not recognized");
			}
		} finally {
			_dataReader.close();
		}
	}

	/**
   *
   */
	private void checkUserId(String id) throws RemoteException {
		if (DSSUtil.isAuthorizedUser(id)) {
		} else {
			throw new RemoteException("User cannot write to data base");
		}
	}

	/**
   *
   */
	public void checkPassword(String passwd) throws RemoteException {
		if (passwd == null)
			throw new PasswordException("Password not specified");
		String ap = ServerProperties.getProperty("password");
		if (!passwd.equals(ap))
			throw new PasswordException("Invalid or incorrect password");
	}

	/**
	 * stores a data set and its attributes
	 */
	public void storeData(DataSet ds, String filename, String path,
			long startJulmin, long endJulmin, boolean storeFlags, String id,
			String passwd) throws RemoteException {
		checkPassword(passwd);
		if (ds == null)
			throw new RemoteException("Nothing to store");
		checkUserId(id);
		DSSDataWriter writer = new DSSDataWriter(filename);
		try {
			writer.openDSSFile();
			writer.storeData(path, startJulmin, endJulmin, ds,
					storeFlags);
		} finally {
			writer.closeDSSFile();
		}
	}

	/**
   *
   */
	private DSSDataReader _dataReader = new DSSDataReader();
	private static final boolean DEBUG = false;
}
