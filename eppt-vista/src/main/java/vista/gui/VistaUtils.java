/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */
package vista.gui;

import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.FileDialog;
import java.awt.Frame;
import java.awt.Rectangle;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.InputStream;
import java.util.Properties;
import java.util.prefs.Preferences;
import javax.swing.*;
import javax.swing.filechooser.FileNameExtensionFilter;

/**
 * Encapsulates commonly needed functionality.
 *
 * @author Nicky Sandhu
 * @version $Id: VistaUtils.java,v 1.1 2003/10/02 20:49:16 redwood Exp $
 */
public class VistaUtils
{
	/**
	 *
	 */
	private static final String copyrightNotice = "    Copyright (C) 1996-2009 State of California, Department of Water\n"
			+ "    Resources.\n"
			+ "\n"
			+ "    VISTA : A VISualization Tool and Analyzer.\n"
			+ "              Version "
			+ getVersionId()
			+ "\n"
			+ "	      "
			+ "       California Dept. of Water Resources\n"
			+ "       Bay Delta Office, Delta Modeling Section\n"
			+ "       1416 Ninth Street\n"
			+ "       Sacramento, CA  95814\n"
			+ "\n"
			+ "    Send bug reports to delmod@water.ca.gov\n"
			+ "\n"
			+ "\n"
			+ "    This program is licensed to you under the terms of the GNU General\n"
			+ "    Public License, version 2, as published by the Free Software\n"
			+ "    Foundation.\n"
			+ "\n"
			+ "    You should have received a copy of the GNU General Public License\n"
			+ "    along with this program; if not, contact Delta Modeling Section, below,\n"
			+ "    or the Free Software Foundation, 675 Mass Ave, Cambridge, MA\n"
			+ "    02139, USA.\n"
			+ "\n"
			+ "    THIS SOFTWARE AND DOCUMENTATION ARE PROVIDED BY THE CALIFORNIA\n"
			+ "    DEPARTMENT OF WATER RESOURCES AND CONTRIBUTORS \"AS IS\" AND ANY\n"
			+ "    EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE\n"
			+ "    IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR\n"
			+ "    PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE CALIFORNIA\n"
			+ "    DEPARTMENT OF WATER RESOURCES OR ITS CONTRIBUTORS BE LIABLE FOR\n"
			+ "    ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR\n"
			+ "    CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT\n"
			+ "    OR SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA OR PROFITS; OR\n"
			+ "    BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF\n"
			+ "    LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT\n"
			+ "    (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE\n"
			+ "    USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH\n"
			+ "    DAMAGE.\n"
			+ "\n"
			+ "    For more information about VISTA, contact:\n"
			+ "\n"
			+ "    California Dept. of Water Resources\n"
			+ "    Bay Delta Office, Delta Modeling Section\n"
			+ "    1416 Ninth Street\n"
			+ "    Sacramento, CA  95814\n"
			+ "\n"
			+ "    or see our home page: http://wwwdelmod.water.ca.gov/\n";
	/**
	 *
	 */
	public static boolean AWT_FILE_DIALOG = false;
	/**
	 *
	 */
	private static String _lastDirectory = null;
	private static FramelessIcon startUpIcon;

	static
	{
		try
		{
			_lastDirectory = Preferences.userRoot().get("last.dir", System.getProperty("user.dir"));
		}
		catch(Exception e)
		{
			System.err.println("Could not determine user directory");
		}
	}

	/**
	 *
	 */
	static
	{

		try
		{
			// javax.swing.plaf.metal.
			// MetalLookAndFeel.setCurrentTheme(new VistaMetalTheme());
		}
		catch(Exception e)
		{
			System.err.println("Could not set gui l&f to vista metal theme");
			System.err.println(e.getMessage());
		}
	}

	/**
	 *
	 */
	public static String getVersionId()
	{
		Properties props = new Properties();
		String version = "1.0-v02/03/2015";
		try
		{
			props.load(VistaUtils.getResourceAsStream("/vista/version"));
			version = props.getProperty("version");
		}
		catch(Exception e)
		{
		}
		return version;
	}

	/**
	 * returns a description of the environment of the Java VM
	 */
	public static String getLocaleDescription()
	{
		String osname = System.getProperty("os.name");
		String osversion = System.getProperty("os.version");
		String archName = System.getProperty("os.arch");
		String javaVendor = System.getProperty("java.vendor");
		String javaVersion = System.getProperty("java.version");
		return "Running java " + javaVersion + " from " + javaVendor + " on "
				+ osname + " " + osversion + " " + archName + " machine";
	}

	/**
	 * binds a method in object to a key stroke in component in a specific mode
	 *
	 * @param comp            The component registering the key stroke
	 * @param key             The key to be registered
	 * @param keyMode         one of Event.SHIFT_MASK, Event.CTRL_MASK, Event.META_MASK,
	 *                        Event.ALT_MASK
	 * @param methodContainer The object having the public void method
	 * @param the             public void method(ActionEvent evt);
	 */
	public static void addKeyListener(JComponent comp, int key, int keyMode,
									  Object methodContainer, String method)
	{
		try
		{
			ActionListener listener = new GenericActionAdapter(methodContainer,
					method);
			KeyStroke keyS = KeyStroke.getKeyStroke(key, keyMode, true);
			int condition = JComponent.WHEN_IN_FOCUSED_WINDOW;
			comp.registerKeyboardAction(listener, keyS, condition);
		}
		catch(InstantiationException ie)
		{
			ie.printStackTrace(System.err);
			System.err.println("Could not add " + key + ": method : " + method);
		}
	}

	/**
	 * @return null or input stream to resource
	 */
	public static final InputStream getResourceAsStream(String res)
	{
		return Object.class.getResourceAsStream(res);
	}

	/**
	 *
	 */
	public static final byte[] getImageAsBytes(String res)
	{
		try
		{
			InputStream is = getResourceAsStream(res);
			if(is == null)
			{
				return null;
			}
			ByteArrayOutputStream baos = new ByteArrayOutputStream();
			while(is.available() != 0)
			{
				byte[] array = new byte[is.available()];
				is.read(array);
				baos.write(array);
			}
			return baos.toByteArray();
		}
		catch(Exception e)
		{
			e.printStackTrace();
			return null;
		}
	}

	/**
	 * Searches user directory and then user home directory for given file.
	 *
	 * @return null or input stream to property file.
	 */
	public static final InputStream getPropertyFileAsStream(String pFile)
	{
		InputStream is = null;
		try
		{
			String fs = System.getProperty("file.separator");
			// user directory first
			is = getFileAsStream(System.getProperty("user.dir") + fs + pFile);
			// home directory second
			if(is == null)
			{
				is = getFileAsStream(System.getProperty("user.home") + fs
						+ pFile);
			}
		}
		catch(Exception e)
		{
			return null;
		}
		return is;
	}

	/**
	 * @return input stream to file or null
	 */
	public static final InputStream getFileAsStream(String file)
	{
		try
		{
			return new FileInputStream(file);
		}
		catch(Exception e)
		{
			return null;
		}
	}

	/**
	 *
	 */
	public static String getPasswordFromDialog()
	{
		JDialog dialog = new JDialog(new JFrame(), "Enter Password");
		JPasswordField jp;
		JButton btn;
		dialog.getContentPane().setLayout(new BorderLayout());
		dialog.getContentPane().add(new JLabel("Enter Password"),
				BorderLayout.NORTH);
		dialog.getContentPane().add(jp = new JPasswordField("", 12),
				BorderLayout.CENTER);
		dialog.getContentPane()
			  .add(btn = new JButton("OK"), BorderLayout.SOUTH);
		class PasswordListener implements ActionListener
		{
			private JDialog dialog;

			PasswordListener(JDialog dialog)
			{
				this.dialog = dialog;
			}

			public void actionPerformed(ActionEvent evt)
			{
				if(dialog.isVisible())
				{
					dialog.setModal(false);
					dialog.setVisible(false);
					dialog.dispose();
				}
			}
		}
		btn.addActionListener(new PasswordListener(dialog));
		dialog.setModal(true);
		dialog.pack();
		dialog.show();
		return new String(jp.getPassword());
	}

	/**
	 *
	 */
	public static final void displayException(Component comp, Exception e)
	{
		e.printStackTrace(System.err);
		JOptionPane.showMessageDialog(comp, e, "Exception!",
				JOptionPane.ERROR_MESSAGE);
	}

	/**
	 * gets the filename to load from or save to
	 */
	public static String getDirectoryNameFromDialog(Component comp, int type)
	{
		return getDirOrFileFromDialog(comp, type, "*.*", "all", true);
	}

	/**
	 * gets the filename to load from or save to
	 */
	public static String getFilenameFromDialog(Component comp, int type,
											   String extension, String description)
	{
		return getDirOrFileFromDialog(comp, type, extension, description, false);
	}

	/**
	 *
	 */
	public static String getDirOrFileFromDialog(Component comp, int type,
												String extension, String description, boolean dirOnly)
	{
		if(comp == null)
		{
			Frame[] frames = Frame.getFrames();
			if(frames != null && frames.length > 0)
			{
				comp = frames[0];
			}
		}
		Frame frame = JOptionPane.getFrameForComponent(comp);
		String filename = null;
		String title = "Save File to...";
		if(type == FileDialog.LOAD)
		{
			title = "Load File from...";
		}
		if(AWT_FILE_DIALOG)
		{
			JFileChooser fileChooser = new JFileChooser();

			fileChooser.setSelectedFile(new File("*" + extension));
			if(dirOnly)
			{
				fileChooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
			}
			else
			{
				fileChooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
			}
			fileChooser.setFileFilter(new FileNameExtensionFilter(description, extension));
			fileChooser.showSaveDialog(frame);
			if(fileChooser.getSelectedFile() == null)
			{
				return null;
			}
			if(fileChooser.getSelectedFile().toString().equals("*" + extension))
			{
				return null;
			}
			if(fileChooser.getCurrentDirectory() == null)
			{
				return null;
			}
			if(!dirOnly)
			{
				filename = fileChooser.getSelectedFile().toString();
			}
			else
			{
				filename = fileChooser.getCurrentDirectory().toString();
			}
			_lastDirectory = fileChooser.getCurrentDirectory().toString();
		}
		else
		{
			JFileChooser chooser = new JFileChooser();
			if(dirOnly)
			{
				chooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
			}
			chooser.setCurrentDirectory(new File(_lastDirectory));
			if(!dirOnly)
			{
				javax.swing.filechooser.FileFilter filter = new ExtensionFilter(
						extension, description);
				chooser.addChoosableFileFilter(filter);
				chooser.setFileFilter(filter);
			}
			int rv = 0;
			if(type == FileDialog.LOAD)
			{
				rv = chooser.showOpenDialog(frame);
			}
			else
			{
				rv = chooser.showSaveDialog(frame);
			}
			if(rv == JFileChooser.APPROVE_OPTION)
			{
				if(chooser.getSelectedFile() == null)
				{
					filename = null;
				}
				else
				{
					filename = chooser.getSelectedFile().getPath();
				}
			}
			if(filename != null)
			{
				_lastDirectory = chooser.getSelectedFile().getParent() == null ? _lastDirectory
						: chooser.getSelectedFile().getParent();
				Preferences.userRoot().put("last.dir", _lastDirectory);
			}
		}
		return filename;
	}

	/**
	 *
	 */
	public static String setExtension(String filename, String extension)
	{
		if(filename == null)
		{
			return null;
		}
		int dotIndex = filename.indexOf(".");
		if(dotIndex > 0)
		{
			filename = filename.substring(0, dotIndex) + "." + extension;
		}
		else if(dotIndex == 0)
		{
			throw new RuntimeException("Invalid filename chosen");
		}
		else
		{
			filename = filename + "." + extension;
		}
		return filename;
	}

	/**
	 *
	 */
	public static Frame getFrameForComponent(Component comp)
	{
		while(comp instanceof JMenuItem)
		{
			if(comp.getParent() instanceof JPopupMenu)
			{
				comp = ((JPopupMenu) comp.getParent()).getInvoker();
			}
			else
			{
				break;
			}
		}
		return JOptionPane.getFrameForComponent(comp);
	}

	/**
	 * sets look and feel on given component
	 */
	public static final void setLookAndFeel(Component comp, String str)
	{
		String swingPackage = "javax.swing.plaf.";
		boolean propAccessException = false;
		java.util.Properties props = null;
		String osname = null;
		try
		{
			props = System.getProperties();
			osname = props.getProperty("os.name");
			if(str.equals("windows"))
			{
				props.put("os.name", "Windows");
			}
			else if(str.equals("mac"))
			{
				props.put("os.name", "Macintosh");
			}
		}
		catch(Exception e)
		{
			propAccessException = true;
		}
		try
		{
			if(str.equals("system"))
			{
				UIManager.setLookAndFeel(UIManager
						.getSystemLookAndFeelClassName());
				SwingUtilities.updateComponentTreeUI(comp);
			}
			else if(str.equals("xplaf"))
			{
				UIManager.setLookAndFeel(UIManager
						.getCrossPlatformLookAndFeelClassName());
				SwingUtilities.updateComponentTreeUI(comp);
			}
			else if(str.equals("motif"))
			{
				UIManager
						.setLookAndFeel("com.sun.java.swing.plaf.motif.MotifLookAndFeel");
				SwingUtilities.updateComponentTreeUI(comp);
			}
			else if(str.equals("windows"))
			{
				UIManager
						.setLookAndFeel("com.sun.java.swing.plaf.windows.WindowsLookAndFeel");
				SwingUtilities.updateComponentTreeUI(comp);
			}
			else if(str.equals("mac"))
			{
				UIManager
						.setLookAndFeel("com.sun.java.swing.plaf.mac.MacLookAndFeel");
				SwingUtilities.updateComponentTreeUI(comp);
			}
			else
			{
				UIManager.setLookAndFeel(UIManager
						.getCrossPlatformLookAndFeelClassName());
				SwingUtilities.updateComponentTreeUI(comp);
			}
		}
		catch(Exception e)
		{
			e.printStackTrace(System.err);
		}
		finally
		{
			if(!propAccessException && props != null)
			{
				props.put("os.name", osname);
			}
		}
	}

	/**
	 *
	 */
	public static void showStartUpIcon()
	{
		startUpIcon = new FramelessIcon(VistaUtils
				.getImageAsBytes("/vista/dwrbig.gif"));
	}

	/**
	 *
	 */
	public static void disposeStartUpIcon()
	{
		if(startUpIcon != null)
		{
			startUpIcon.setVisible(false);
			startUpIcon.dispose();
		}
	}

	/**
	 * returns true if JVM is jdk2 or more.
	 */
	public static boolean isJDK2()
	{
		return (System.getProperty("java.version").compareTo("1.1z") > 0);
	}

	/**
	 *
	 */
	public static final void setLookAndFeel(Component comp, LookAndFeel lf)
	{
		try
		{
			UIManager.setLookAndFeel(lf);
			SwingUtilities.updateComponentTreeUI(comp);
		}
		catch(Exception e)
		{
			e.printStackTrace(System.err);
		}
	}

	/**
	 *
	 */
	public static final void positionComponent(Component comp,
											   Component parent, int position)
	{
		// ignore position for now. just center the component
		Rectangle bounds = comp.getBounds();
		Rectangle parentBounds = parent.getBounds();
		Rectangle r = new Rectangle(bounds);
		r.x = parentBounds.x + (parentBounds.width - bounds.width) / 2;
		r.y = parentBounds.y + (parentBounds.height - bounds.height) / 2;
		comp.setBounds(r);
		// position it @ the north/south/east/west
		// + left/right/center
		// + overlapping/non-overlapping
	}

	/**
	 * @returns a string containing the copyright notice
	 */
	public static final String getCopyrightNotice()
	{
		return copyrightNotice;
	}

	/**
	 * creates an internalized frame with the same title, size, menu bars and
	 * content pane
	 */
	public static JInternalFrame internalizeFrame(JFrame frame)
	{
		JInternalFrame ifr = new JInternalFrame(frame.getTitle(), true, true,
				true, true);
		ifr.setSize(frame.getSize());
		ifr.setContentPane(frame.getContentPane());
		ifr.setJMenuBar(frame.getJMenuBar());
		return ifr;
	}
}
