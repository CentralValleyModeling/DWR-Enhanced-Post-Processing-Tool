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

package calsim.gui;

import java.awt.Component;
import java.awt.FileDialog;
import java.awt.event.ActionListener;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.LineNumberReader;
import java.io.PrintWriter;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.Vector;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import javax.swing.JPopupMenu;
import javax.swing.JTree;
import javax.swing.event.TreeModelEvent;
import javax.swing.event.TreeModelListener;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.TreeNode;
import javax.swing.tree.TreePath;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;

import calsim.app.AppUtils;
import calsim.app.DerivedTimeSeries;
import calsim.app.MultipleTimeSeries;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;
import org.xml.sax.SAXException;
import vista.gui.VistaUtils;

/**
 * Tree Model that specifically handles DTS and MTS organization
 *
 * @author Joel Fenolio
 * @version $Id: DtsTreeModel.java,v 1.1.2.5 2001/10/23 16:28:36 jfenolio Exp $
 */

public class DtsTreeModel extends GeneralTreeModel
{

	private static final MainPanel MAIN_PANEL = GuiUtils.getMainPanel();
	private static DefaultMutableTreeNode newnode;
	private static Vector prjdts = new Vector(1, 1);
	private static Vector prjmts = new Vector(1, 1);
	String _name = null;
	String[] _tags = null;
	DerivedTimeSeries _dts;
	MultipleTimeSeries _mts;
	int newfile = 0;
	String oldname = " ", oldpaste = " ";
	boolean isMerge = false;
	private JPopupMenu _nodepopup = new JPopupMenu();
	private JMenuItem _open = new JMenuItem("Open");
	private JMenuItem _rename = new JMenuItem("Rename");
	private JMenuItem _adddts = new JMenuItem("Add DTS");
	private JMenuItem _addmts = new JMenuItem("Add MTS");
	private JMenuItem _deletenode = new JMenuItem("Delete");
	private JMenuItem _addfolder = new JMenuItem("Add Folder");
	private JMenuItem _cutnode = new JMenuItem("Cut");
	private JMenuItem _copynode = new JMenuItem("Copy");
	private JMenuItem _pastenode = new JMenuItem("Paste");
	private JMenuItem _dtsimport = new JMenuItem("Import DTS");
	private JMenuItem _mtsimport = new JMenuItem("Import MTS");
	private JMenuItem _merge = new JMenuItem("Merge");
	private DtsTreePanel _dtp;
	private CalsimTree _tree;
	ActionListener _listener = e -> {
		JMenuItem mi = (JMenuItem) (e.getSource());
		if(mi.getText() == "Add DTS")
		{
			addDTS();
		}
		if(mi.getText() == "Add MTS")
		{
			addMTS();
		}
		if(mi.getText() == "Delete")
		{
			removeNode(true);
		}
		if(mi.getText() == "Edit")
		{
			editNode();
		}
		if(mi.getText() == "Rename")
		{
			renameNode();
		}
		if(mi.getText() == "Properties")
		{
			properties();
		}
		if(mi.getText() == "Add Folder")
		{
			addFolder();
		}
		if(mi.getText() == "Cut")
		{
			cutNode();
		}
		if(mi.getText() == "Copy")
		{
			copyNode();
		}
		if(mi.getText() == "Paste")
		{
			pasteNode();
		}
	};

	public DtsTreeModel(TreeNode dumbyRoot, String[] tags, String[] pics, DtsTreePanel dtp)
	{
		super(dumbyRoot, tags, pics);
		dumbyRoot = null;
		setRoot(readData());
		_dtp = dtp;
		addTreeModelListener(new TreeModelListener()
		{
			public void treeNodesChanged(TreeModelEvent e)
			{
				DefaultMutableTreeNode node = (DefaultMutableTreeNode)
						(e.getTreePath().getLastPathComponent());
				try
				{
					int index = e.getChildIndices()[0];
					node = (DefaultMutableTreeNode) (node.getChildAt(index));
				}
				catch(NullPointerException exc)
				{
				}
				System.out.println("The user has finished editing the node.");
				System.out.println("New value: " + node.getUserObject());
				String name = (String) node.getUserObject();
				if(checkExtension(".dts", oldname) && !checkExtension(".dts", name))
				{
					if(checkExtension(".mts", name))
					{
						cannotChange(2);
						name = oldname;
						node.setUserObject(name);
					}
					else
					{
						cannotChange(3);
						name += ".DTS";
						node.setUserObject(name);
					}
				}
				else if(checkExtension(".mts", oldname) && !checkExtension(".mts", name))
				{
					if(checkExtension(".dts", name))
					{
						cannotChange(1);
						name = oldname;
						node.setUserObject(name);
					}
					else
					{
						cannotChange(3);
						name += ".MTS";
						node.setUserObject(name);
					}
				}
				if(checkExtension(".dts", name))
				{
					changeDTSName(name);
				}
				else if(checkExtension(".mts", name))
				{
					changeMTSName(name);
				}
			}

			public void treeNodesInserted(TreeModelEvent e)
			{
			}

			public void treeNodesRemoved(TreeModelEvent e)
			{
			}

			public void treeStructureChanged(TreeModelEvent e)
			{
			}
		});
		getPopupMenuItems();
	}

	public static DefaultMutableTreeNode getNewNode()
	{
		return newnode;
	}

	public static void setNewNodeName(String name)
	{
		newnode.setUserObject(name);
		newnode = null;
	}

	public static Vector getPrjDts()
	{
		return prjdts;
	}

	public static Vector getPrjMts()
	{
		return prjmts;
	}

	public static void clearVectors()
	{
		prjdts.removeAllElements();
		prjmts.removeAllElements();
		prjdts = new Vector(1, 1);
		prjmts = new Vector(1, 1);
	}

	@Override
	public void getNodePopup(Component jtree, int x, int y)
	{
		tree = (JTree) jtree;
		path = tree.getSelectionPath();
		_nodepopup.show(jtree, x, y);
	}

	@Override
	public boolean checkExtension(String extension, String name)
	{
		int end = name.length();
		int period = name.lastIndexOf(".");
		if(period == -1)
		{
			return false;
		}
		String name1 = name.substring(period, end);
		return extension.equalsIgnoreCase(name1);
	}

	@Override
	public void getPopupMenuItems()
	{
		_open.addActionListener(new GuiTaskListener("Retrieving...")
		{
			public void doWork()
			{
				GuiUtils.getCLGPanel().getOpenDtsButton().doClick();
			}
		});
		_nodepopup.add(_open);
		_nodepopup.addSeparator();
		_adddts.addActionListener(_listener);
		_nodepopup.add(_adddts);
		_addmts.addActionListener(_listener);
		_nodepopup.add(_addmts);
		_addfolder.addActionListener(_listener);
		_nodepopup.add(_addfolder);
		_nodepopup.addSeparator();
		_merge.addActionListener(new GuiTaskListener("Merging with another tree...")
		{
			public void doWork()
			{
				try
				{
					mergeWith();
				}
				catch(IOException ioe)
				{
				}
			}
		});
		_dtsimport.addActionListener(new GuiTaskListener("Importing DTS...")
		{
			public void doWork()
			{
				DTSFileOpen();
			}
		});
		_nodepopup.add(_dtsimport);
		_mtsimport.addActionListener(new GuiTaskListener("Importing MTS...")
		{
			public void doWork()
			{
				MTSFileOpen();
			}
		});
		_nodepopup.add(_mtsimport);
		save.addActionListener(new GuiTaskListener("Saving...")
		{
			public void doWork()
			{
				try
				{
					save();
				}
				catch(IOException ioe)
				{
				}
			}
		});
		_nodepopup.add(save);
		_nodepopup.addSeparator();
		_rename.addActionListener(_listener);
		_nodepopup.add(_rename);
		_deletenode.addActionListener(_listener);
		_nodepopup.add(_deletenode);
		_nodepopup.addSeparator();
		_cutnode.addActionListener(_listener);
		_nodepopup.add(_cutnode);
		_copynode.addActionListener(_listener);
		_nodepopup.add(_copynode);
		_pastenode.addActionListener(_listener);
		_nodepopup.add(_pastenode);
		_nodepopup.addSeparator();
	}

	public boolean nameCheck(String newname)
	{
		if(checkExtension(".dts", newname) && checkExtension(".mts", oldname))
		{
			cannotChange(1);
			return true;
		}
		else if(checkExtension(".dts", newname) && checkExtension(".mts", oldname))
		{
			cannotChange(2);
			return true;
		}
		else
		{
			return false;
		}
	}

	public void changeDTSName(String name)
	{
		if(_dts != null)
		{
			DefaultMutableTreeNode node = (DefaultMutableTreeNode) (path.getLastPathComponent());
			if(checkForDuplicates(name, (DefaultMutableTreeNode) getRoot()) > 1 && !name.toUpperCase().equals(
					oldname.toUpperCase()))
			{
				JOptionPane.showMessageDialog(null, "Cannot rename node. A node by this name already exists.",
						"Cannot Perform", JOptionPane.WARNING_MESSAGE);
				node.setUserObject(oldname);
				return;
			}
			_dts.setName(name.toUpperCase());
			_dtp.setDTSTable(_dts, null);
			if(checkForDuplicates(oldname, (DefaultMutableTreeNode) getRoot()) > 0)
			{
				DerivedTimeSeries dts1 = new DerivedTimeSeries(oldname.toUpperCase());
				dts1.setName(oldname.toUpperCase());
				int num = _dts.getNumberOfDataReferences();
				for(int k = 0; k < num; k++)
				{
					dts1.setVarTypeAt(k, _dts.getVarTypeAt(k));
					dts1.setBPartAt(k, _dts.getBPartAt(k));
					dts1.setCPartAt(k, _dts.getCPartAt(k));
					dts1.setOperationIdAt(k, _dts.getOperationIdAt(k));
					dts1.setDTSNameAt(k, _dts.getDTSNameAt(k));
				}
				AppUtils.getCurrentProject().add(dts1);
			}
		}
	}

	private void changeMTSName(String name)
	{
		if(_mts != null)
		{
			DefaultMutableTreeNode node = (DefaultMutableTreeNode) (path.getLastPathComponent());
			if(checkForDuplicates(name, (DefaultMutableTreeNode) getRoot()) > 1 && !name.equalsIgnoreCase(
					oldname))
			{
				JOptionPane.showMessageDialog(null, "Cannot rename node. A node by this name already exists.",
						"Cannot Perform", JOptionPane.WARNING_MESSAGE);
				node.setUserObject(oldname);
				return;
			}
			_mts.setName(name.toUpperCase());
			_dtp.setDTSTable(null, _mts);
			if(checkForDuplicates(oldname, (DefaultMutableTreeNode) getRoot()) > 0)
			{
				MultipleTimeSeries mts1 = new MultipleTimeSeries(oldname.toUpperCase());
				int num = _mts.getNumberOfDataReferences();
				for(int k = 0; k < num; k++)
				{
					mts1.setVarTypeAt(k, _mts.getVarTypeAt(k));
					mts1.setBPartAt(k, _mts.getBPartAt(k));
					mts1.setCPartAt(k, _mts.getCPartAt(k));
					mts1.setDTSNameAt(k, _mts.getDTSNameAt(k));
				}
				AppUtils.getCurrentProject().add(mts1);
			}
		}
	}

	public void setTree(CalsimTree tree)
	{
		_tree = tree;
		_tree.addTreeSelectionListener(e ->
		{
			path = _tree.getSelectionPath();
			if(path != null)
			{
				DefaultMutableTreeNode node = (DefaultMutableTreeNode) (path.getLastPathComponent());
				String name = (String) node.getUserObject();
				if(!node.getAllowsChildren() && checkExtension(".dts", name))
				{
					_dts = AppUtils.getCurrentProject().getDTS(name.toUpperCase());
					DTSOpen();
					AppUtils.getCurrentProject().setDTSMod(false);
				}
				else if(!node.getAllowsChildren() && checkExtension(".mts", name))
				{
					_mts = AppUtils.findMTS(name.toUpperCase());
					MTSOpen();
					AppUtils.getCurrentProject().setDTSMod(false);
				}
				else if(node.getAllowsChildren())
				{
					_dts = new DerivedTimeSeries(" ");
					_dtp.setDTSTable(_dts, null);
				}
			}
		});
	}

	private boolean copiedNodeChanged(String newname)
	{
		return !oldpaste.toUpperCase().equals(newname.toUpperCase());
	}

	@Override
	public void copyNode()
	{
		copiednode = (DefaultMutableTreeNode)
				(path.getLastPathComponent());
		iscopied = true;
	}

	@Override
	public void cutNode()
	{
		DefaultMutableTreeNode node = (DefaultMutableTreeNode)
				(path.getLastPathComponent());
		copiednode = node;
		iscopied = true;
		removeNode(node);
	}

	private DefaultMutableTreeNode copyAllChildren(DefaultMutableTreeNode parent)
	{
		DefaultMutableTreeNode newparent = (DefaultMutableTreeNode) parent.clone();
		if(parent.getChildCount() == 0)
		{
			return newparent;
		}
		for(int i = 0; i < parent.getChildCount(); i++)
		{
			DefaultMutableTreeNode node = (DefaultMutableTreeNode) parent.getChildAt(i);
			DefaultMutableTreeNode child;
			if(node.getChildCount() > 0)
			{
				child = copyAllChildren(node);
				newparent.add(child);
			}
			else
			{
				child = (DefaultMutableTreeNode) node.clone();
				String name = (String) child.getUserObject();
				if(checkExtension(".dts", name) || checkExtension(".mts", name))
				{
					child.setAllowsChildren(false);
				}
				newparent.add(child);
			}
		}
		return newparent;
	}

	@Override
	public void pasteNode()
	{
		DefaultMutableTreeNode parent = null;
		TreePath path = tree.getSelectionPath();
		DefaultMutableTreeNode child;
		if(copiednode.getChildCount() > 0)
		{
			child = copyAllChildren(copiednode);
		}
		else
		{
			child = (DefaultMutableTreeNode) copiednode.clone();
		}
		String newname = (String) copiednode.getUserObject();
		if(copiednode == null)
		{
			iscopied = false;
		}
		String name, name1;
		name1 = (String) child.getUserObject();
		if(iscopied)
		{
			if(path == null)
			{
				parent = (DefaultMutableTreeNode) getRoot();
			}
			else
			{
				parent = (DefaultMutableTreeNode) (path.getLastPathComponent());
			}
			int children = parent.getChildCount();
			if(children > 0)
			{
				DefaultMutableTreeNode node;
				for(int i = 0; i < children; i++)
				{
					node = (DefaultMutableTreeNode) parent.getChildAt(i);
					name = (String) node.getUserObject();
					if(name.equals(child.getUserObject()))
					{
						if(copiedNodeChanged(newname))
						{
							paste = 0;
						}
						Integer j = paste;
						name1 = "Copy (" + j.toString() + ") of " + " " + name;
						child.setUserObject(name1);
						if(checkExtension(".dts", name))
						{
							child.setAllowsChildren(false);
							DerivedTimeSeries dts = AppUtils.getCurrentProject().getDTS(name.toUpperCase());
							DerivedTimeSeries dts1 = new DerivedTimeSeries(name1);
							dts1.setName(name1.toUpperCase());
							int num = dts.getNumberOfDataReferences();
							for(int k = 0; k < num; k++)
							{
								dts1.setVarTypeAt(k, dts.getVarTypeAt(k));
								dts1.setBPartAt(k, dts.getBPartAt(k));
								dts1.setCPartAt(k, dts.getCPartAt(k));
								dts1.setOperationIdAt(k, dts.getOperationIdAt(k));
								dts1.setDTSNameAt(k, dts.getDTSNameAt(k));
							}
							AppUtils.getCurrentProject().add(dts1);
						}
						else if(checkExtension(".mts", name))
						{
							child.setAllowsChildren(false);
							MultipleTimeSeries mts = AppUtils.findMTS(name.toUpperCase());
							MultipleTimeSeries mts1 = new MultipleTimeSeries(name1.toUpperCase());
							int num = mts.getNumberOfDataReferences();
							for(int k = 0; k < num; k++)
							{
								mts1.setVarTypeAt(k, mts.getVarTypeAt(k));
								mts1.setBPartAt(k, mts.getBPartAt(k));
								mts1.setCPartAt(k, mts.getCPartAt(k));
								mts1.setDTSNameAt(k, mts.getDTSNameAt(k));
							}
							AppUtils.getCurrentProject().add(mts1);
						}
						paste++;
						oldpaste = (String) copiednode.getUserObject();
					}
				}
			}
			if(parent.getAllowsChildren())
			{
				insertNodeInto(child, parent, parent.getChildCount());
				tree.scrollPathToVisible(new TreePath(child.getPath()));
			}
			else
			{
				_nodepopup.setVisible(false);
				cannotPerform();
			}
		}
	}

	@Override
	public void renameNode()
	{
		DefaultMutableTreeNode node = (DefaultMutableTreeNode) (path.getLastPathComponent());
		String name = (String) node.getUserObject();
		if(!node.getAllowsChildren() && checkExtension(".dts", name))
		{
			_dts = AppUtils.getCurrentProject().getDTS(name.toUpperCase());
		}
		else if(!node.getAllowsChildren() && checkExtension(".mts", name))
		{
			_mts = AppUtils.findMTS(name);
		}
		tree.startEditingAtPath(path);
	}

	@Override
	public void setClassTS()
	{
		DefaultMutableTreeNode node = (DefaultMutableTreeNode) (path.getLastPathComponent());
		String name = (String) node.getUserObject();
		oldname = name.toUpperCase();
		if(!node.getAllowsChildren() && checkExtension(".dts", name))
		{
			_dts = AppUtils.getCurrentProject().getDTS(name.toUpperCase());
		}
		else if(!node.getAllowsChildren() && checkExtension(".mts", name))
		{
			_mts = AppUtils.findMTS(name);
		}
	}

	private String getDTSLeafName()
	{
		Integer i = newfile;
		String name = "NEW-FILE" + i.toString() + ".DTS";
		newfile++;
		return name;
	}

	private String getMTSLeafName()
	{
		Integer i = newfile;
		String name = "NEW-FILE" + i.toString() + ".MTS";
		newfile++;
		return name;
	}

	public void getFile(String fname, String dirname) throws IOException
	{
		try
		{
			readData(fname, dirname);
		}
		catch(Exception e)
		{
			e.printStackTrace(System.err);
			throw new IOException("File Not Found");
		}
	}

	@Override
	public void removeNode(boolean warn)
	{
		if(warn)
		{
			if(permissionToRemove())
			{
				_nodepopup.setVisible(false);
				return;
			}
		}
		if(path != null)
		{
			DefaultMutableTreeNode node = (DefaultMutableTreeNode) (path.getLastPathComponent());
			DefaultMutableTreeNode parent = (DefaultMutableTreeNode) (node.getParent());
			if(node.getAllowsChildren())
			{
				deleteAllChildren(node);
			}
			removeNode(node);
			String name = (String) node.getUserObject();
			if(parent != null)
			{
				int copies = 0;
				copies = checkForDuplicates(name, (DefaultMutableTreeNode) getRoot());
				System.out.println(copies);
				if(!node.getAllowsChildren() && copies == 0)
				{
					TreePath path1 = new TreePath(parent.getPath());
					_tree.setSelectionPath(path1);
					DerivedTimeSeries dts1 = new DerivedTimeSeries(" ");
					_dtp.setDTSTable(dts1, null);
					if(warn)
					{
						if(AppUtils.getCurrentProject().isInLists(name))
						{
							AppUtils.getCurrentProject().remove(name);
							AppUtils.getCurrentProject().setDTSMod(true);
						}
					}
				}
			}
		}
	}

	public int checkForDuplicates(String name, DefaultMutableTreeNode startnode)
	{
		int number = 0;
		int children = startnode.getChildCount();
		if(children == 0 || name == null)
		{
			return number;
		}
		DefaultMutableTreeNode node;
		String name1;
		name = name.toUpperCase();
		for(int i = 0; i < children; i++)
		{
			node = (DefaultMutableTreeNode) startnode.getChildAt(i);
			name1 = (String) node.getUserObject();
			name1 = name1.toUpperCase();
			if(node.getAllowsChildren())
			{
				number += checkForDuplicates(name, node);
			}
			else if(name.equals(name1))
			{
				number++;
			}
		}
		return number;
	}

	public void deleteAllChildren(DefaultMutableTreeNode parent)
	{
		if(parent.getChildCount() == 0)
		{
			return;
		}
		int i = 0;
		DefaultMutableTreeNode node;
		String name;
		while(i < parent.getChildCount())
		{
			node = (DefaultMutableTreeNode) parent.getChildAt(i);
			if(node.getAllowsChildren())
			{
				deleteAllChildren(node);
			}
			else
			{
				name = (String) node.getUserObject();
				AppUtils.getCurrentProject().remove(name);
			}
			i++;
		}
	}

	public DefaultMutableTreeNode addDTS()
	{
		DefaultMutableTreeNode parent = null;
		String name = getDTSLeafName();
		DefaultMutableTreeNode child = new DefaultMutableTreeNode(name);
		if(path == null)
		{
			parent = (DefaultMutableTreeNode) child.getRoot();
		}
		else
		{
			parent = (DefaultMutableTreeNode) (path.getLastPathComponent());
		}
		child.setAllowsChildren(false);
		if(parent.getAllowsChildren())
		{
			insertNodetoModel(parent, child, parent.getChildCount());
			tree.scrollPathToVisible(new TreePath(child.getPath()));
		}
		else
		{
			_nodepopup.setVisible(false);
			cannotPerform();
			return child;
		}
		newnode = child;
		DTSNew(name);
		return child;
	}

	public DefaultMutableTreeNode addMTS()
	{
		DefaultMutableTreeNode parent = null;
		String name = getMTSLeafName();
		DefaultMutableTreeNode child = new DefaultMutableTreeNode(name);
		if(path == null)
		{
			parent = (DefaultMutableTreeNode) child.getRoot();
		}
		else
		{
			parent = (DefaultMutableTreeNode) (path.getLastPathComponent());
		}
		child.setAllowsChildren(false);
		if(parent.getAllowsChildren())
		{
			insertNodetoModel(parent, child, parent.getChildCount());
			tree.scrollPathToVisible(new TreePath(child.getPath()));
		}
		else
		{
			_nodepopup.setVisible(false);
			cannotPerform();
			return child;
		}
		newnode = child;
		MTSNew(name);
		return child;
	}

	@Override
	public void open(TreePath path, Component jtree)
	{
		tree = (JTree) jtree;
		DefaultMutableTreeNode node = (DefaultMutableTreeNode) path.getLastPathComponent();
		if(node.isLeaf())
		{
			String name = (String) node.getUserObject();
			name = name.toUpperCase();
			if(name == null)
			{
				return;
			}
			try
			{
				if(checkExtension(".dts", name))
				{
					DerivedTimeSeries dts = AppUtils.getCurrentProject().getDTS(name);
					DTSTable dt = new DTSTable(dts);
					dt.retrieve();
				}
				else if(checkExtension(".mts", name))
				{
					MultipleTimeSeries mts = AppUtils.findMTS(name);
					MTSTable mt = new MTSTable(mts);
					mt.retrieve();
				}
			}
			catch(Exception e)
			{
				VistaUtils.displayException(MAIN_PANEL, e);
			}
		}
		else
		{
			tree.expandPath(path);
		}
	}

	@Override
	public void open()
	{
		DefaultMutableTreeNode node = (DefaultMutableTreeNode) path.getLastPathComponent();
		if(!node.getAllowsChildren())
		{
			String name = (String) node.getUserObject();
			name = name.toUpperCase();
			if(name == null)
			{
				return;
			}
			try
			{
				if(checkExtension(".dts", name))
				{
					DerivedTimeSeries dts = AppUtils.getCurrentProject().getDTS(name);
					DTSTable dt = new DTSTable(dts);
					dt.retrieve();
				}
				else if(checkExtension(".mts", name))
				{
					MultipleTimeSeries mts = AppUtils.findMTS(name);
					MTSTable mt = new MTSTable(mts);
					mt.retrieve();
				}
			}
			catch(Exception e)
			{
				VistaUtils.displayException(MAIN_PANEL, e);
			}
		}
		else
		{
			tree.expandPath(path);
		}
	}

	@Override
	public void editNode()
	{
		DTSOpen();
	}

	/**
	 * Display a blank table with retrieve, save/as to a text file functions,
	 * and have a default name
	 * "new.dts".
	 * Change the status bar and cursor.
	 */
	void DTSNew(String name)
	{
		DerivedTimeSeries dts = new DerivedTimeSeries(name);
		GuiUtils.checkAndAddToProject(MAIN_PANEL, dts);
		_dtp.setDTSTable(dts, null);
	}

	/**
	 * 1. Display a blank table with retrieve, save/as to a text file functions, and have a default
	 * name "new.mts".
	 * 2. Change the status bar and cursor.
	 * 3. If the Monthly view type is chosen, a warning message will be displayed.
	 */
	void MTSNew(String name)
	{
		MultipleTimeSeries mts = new MultipleTimeSeries(name);
		GuiUtils.checkAndAddToProject(MAIN_PANEL, mts);
	}

	public void setNodeName(String newname)
	{
		path = _tree.getSelectionPath();
		DefaultMutableTreeNode node = (DefaultMutableTreeNode) (path.getLastPathComponent());
		node.setUserObject(newname);
	}

	/**
	 * 1. Display a file dialog box, let user choose a DTS file, then send to package app.
	 * 2. Display the contents of the file on a table with retrieve, save/as to a text file
	 * functions.
	 * 3. If the file is not of the appropriate type, then an error message will be displayed.
	 * 4. Change the status bar and cursor.
	 */
	@Override
	public void DTSOpen()
	{
		DefaultMutableTreeNode node = (DefaultMutableTreeNode) path.getLastPathComponent();
		String name = (String) node.getUserObject();
		name = name.toUpperCase();
		if(name == null)
		{
			return;
		}
		try
		{
			DerivedTimeSeries dts = AppUtils.getCurrentProject().getDTS(name);
			if(dts == null)
			{
				dts = new DerivedTimeSeries(" ");
				JOptionPane.showMessageDialog(null, "DTS not found in project", "DTS not found!!!",
						JOptionPane.ERROR_MESSAGE);
			}
			_dtp.setDTSTable(dts, null);
		}
		catch(Exception e)
		{
			e.printStackTrace();
			VistaUtils.displayException(MAIN_PANEL, e);
		}
	}

	/**
	 * 1. Display a file dialog box, let user choose a MTS file, then send to package app.
	 * 2. Display the contents of the file on a table with retrieve, save/as to a text file
	 * functions.
	 * 3. If the file is not of the appropriate type, then an error message will be displayed.
	 * 4. Change the status bar and cursor.
	 */
	public void MTSOpen()
	{
		DefaultMutableTreeNode node = (DefaultMutableTreeNode) path.getLastPathComponent();
		String name = (String) node.getUserObject();
		name = name.toUpperCase();
		if(name == null)
		{
			return;
		}
		try
		{
			MultipleTimeSeries mts = AppUtils.findMTS(name);
			if(mts == null)
			{
				mts = new MultipleTimeSeries(" ");
				JOptionPane.showMessageDialog(null, "MTS not found in project", "MTS not found!!!",
						JOptionPane.ERROR_MESSAGE);
			}
			_dtp.setDTSTable(null, mts);
		}
		catch(Exception e)
		{
			VistaUtils.displayException(MAIN_PANEL, e);
		}
	}

	/**
	 * 1. Display a file dialog box, let user choose a DTS file, then send to package app.
	 * 2. Display the contents of the file on a table with retrieve, save/as to a text file
	 * functions.
	 * 3. If the file is not of the appropriate type, then an error message will be displayed.
	 * 4. Change the status bar and cursor.
	 */
	void DTSFileOpen()
	{
		try
		{
			DefaultMutableTreeNode parent = null;
			DefaultMutableTreeNode child = new DefaultMutableTreeNode();
			if(path == null)
			{
				parent = (DefaultMutableTreeNode) child.getRoot();
			}
			else
			{
				parent = (DefaultMutableTreeNode) (path.getLastPathComponent());
			}
			if(parent.getAllowsChildren())
			{
				String filename = VistaUtils.getFilenameFromDialog(MAIN_PANEL, FileDialog.LOAD, "dts", "DTS Files");
				if(filename == null)
				{
					return;
				}
				DerivedTimeSeries dts;
				try(InputStream is = Files.newInputStream(Paths.get(filename));
					LineNumberReader reader = new LineNumberReader(new InputStreamReader(is)))
				{

					String check = reader.readLine();
					if(!check.startsWith("name"))
					{
						DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
						DocumentBuilder db = dbf.newDocumentBuilder();
						Document doc = db.parse(new File(filename));
						Element top = doc.getDocumentElement();
						NodeList elements = top.getElementsByTagName("DTS");
						if(elements == null || elements.getLength() == 0)
						{
							return;
						}
						Element de = (Element) elements.item(0);
						if(de == null)
						{
							return;
						}
						dts = new DerivedTimeSeries();
						dts.fromXml(de);
					}
					else
					{
						dts = DerivedTimeSeries.load(filename);
					}
				}
				GuiUtils.checkAndAddToProject(MAIN_PANEL, dts);
				child.setUserObject(dts.getName());
				child.setAllowsChildren(false);
				insertNodetoModel(parent, child, parent.getChildCount());
				TreePath path = new TreePath(child.getPath());
				tree.setSelectionPath(path);
			}
			else
			{
				_nodepopup.setVisible(false);
				cannotPerform();
			}
			newnode = child;
		}
		catch(Exception e)
		{
			e.printStackTrace();
			VistaUtils.displayException(MAIN_PANEL, e);
		}
	}

	/**
	 * 1. Display a file dialog box, let user choose a MTS file, then send to package app.
	 * 2. Display the contents of the file on a table with retrieve, save/as to a text file
	 * functions.
	 * 3. If the file is not of the appropriate type, then an error message will be displayed.
	 * 4. Change the status bar and cursor.
	 */
	void MTSFileOpen()
	{
		try
		{
			DefaultMutableTreeNode parent = null;
			DefaultMutableTreeNode child = new DefaultMutableTreeNode();
			if(path == null)
			{
				parent = (DefaultMutableTreeNode) child.getRoot();
			}
			else
			{
				parent = (DefaultMutableTreeNode) (path.getLastPathComponent());
			}
			if(parent.getAllowsChildren())
			{
				String filename = VistaUtils.getFilenameFromDialog(MAIN_PANEL, FileDialog.LOAD, "mts", "MTS Files");
				if(filename == null)
				{
					return;
				}
				MultipleTimeSeries mts = null;
				try(InputStream is = new FileInputStream(filename);
					LineNumberReader reader = new LineNumberReader(new InputStreamReader(is)))
				{
					String check = reader.readLine();
					if(!check.startsWith("name"))
					{
						DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
						DocumentBuilder db = dbf.newDocumentBuilder();
						Document doc = db.parse(new File(filename));
						Element top = doc.getDocumentElement();
						NodeList elements = top.getElementsByTagName("MTS");
						if(elements == null || elements.getLength() == 0)
						{
							return;
						}
						Element de = (Element) elements.item(0);
						if(de == null)
						{
							return;
						}
						mts = new MultipleTimeSeries();
						mts.fromXml(de);
					}
					else
					{
						mts = MultipleTimeSeries.load(filename);
					}
				}
				GuiUtils.checkAndAddToProject(MAIN_PANEL, mts);
				child.setUserObject(mts.getName());
				child.setAllowsChildren(false);
				insertNodetoModel(parent, child, parent.getChildCount());
				TreePath path = new TreePath(child.getPath());
				tree.setSelectionPath(path);
			}
			else
			{
				_nodepopup.setVisible(false);
				cannotPerform();
			}
			newnode = child;
		}
		catch(Exception e)
		{
			e.printStackTrace();
			VistaUtils.displayException(MAIN_PANEL, e);
		}
	}

	public void save() throws IOException
	{
		DefaultMutableTreeNode node = (DefaultMutableTreeNode) (path.getLastPathComponent());
		String name = (String) node.getUserObject();
		try
		{
			DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
			DocumentBuilder db = dbf.newDocumentBuilder();
			Document dtsdoc = db.newDocument();
			Element masterdts;
			String dtsfile = "default.dts";
			if(checkExtension(".dts", name))
			{
				dtsfile = VistaUtils.getFilenameFromDialog(MAIN_PANEL, FileDialog.SAVE,
						"dts", "dts File");
				masterdts = dtsdoc.createElement("DTS_File");
				if(dtsfile == null)
				{
					return;
				}
				if(dtsfile.indexOf((int) '.') == -1)  //no extension
				{
					dtsfile += ".dts";  //set default extension
				}
				dtsdoc.appendChild(masterdts);
				_dts.toXml(dtsdoc, masterdts);
			}
			else if(checkExtension(".mts", name))
			{
				dtsfile = VistaUtils.getFilenameFromDialog(MAIN_PANEL, FileDialog.SAVE,
						"mts", "MTS File");
				masterdts = dtsdoc.createElement("MTS_File");
				if(dtsfile == null)
				{
					return;
				}
				if(dtsfile.indexOf((int) '.') == -1)  //no extension
				{
					dtsfile += ".mts";  //set default extension
				}
				dtsdoc.appendChild(masterdts);
				_mts.toXml(dtsdoc, masterdts);
			}
			try(PrintWriter pw1 = new PrintWriter(Files.newOutputStream(Paths.get(dtsfile))))
			{
				TransformerFactory transformerFactory = TransformerFactory.newInstance();
				Transformer transformer = transformerFactory.newTransformer();
				DOMSource source = new DOMSource(dtsdoc);
				StreamResult result = new StreamResult(pw1);

				transformer.transform(source, result);
			}
			catch(IOException | TransformerException ioe)
			{
			}
		}
		catch(ParserConfigurationException e)
		{
			throw new IOException(e);
		}
	}

	public void createTreeFromPrj(String[] dtsarray, String[] mtsarray, String treefile)
	{
		DefaultMutableTreeNode root = (DefaultMutableTreeNode) getRoot();
		root.removeAllChildren();
		setRoot(root);
		DefaultMutableTreeNode dtsnode = new DefaultMutableTreeNode("DTS");
		DefaultMutableTreeNode mtsnode = new DefaultMutableTreeNode("MTS");
		root.add(dtsnode);
		root.add(mtsnode);
		if(dtsarray != null)
		{
			for(int i = 0; i < dtsarray.length; i++)
			{
				DefaultMutableTreeNode node = new DefaultMutableTreeNode(dtsarray[i]);
				node.setAllowsChildren(false);
				dtsnode.add(node);
			}
		}
		if(mtsarray != null)
		{
			for(int i = 0; i < mtsarray.length; i++)
			{
				DefaultMutableTreeNode node = new DefaultMutableTreeNode(mtsarray[i]);
				node.setAllowsChildren(false);
				mtsnode.add(node);
			}
		}
		_tree.expandRow(0);
		_tree.setSelectionRow(0);
		MessagePanel.setDtsMasterMessage(treefile);
	}

	@Override
	public void saveFile(String fname) throws IOException
	{
		try
		{
			DocumentBuilderFactory docFactory = DocumentBuilderFactory.newInstance();
			DocumentBuilder docBuilder = docFactory.newDocumentBuilder();

			// root elements
			Document dtsdoc = docBuilder.newDocument();
			Element masterdts = dtsdoc.createElement("dts_master");
			dtsdoc.appendChild(masterdts);
			saveData(dtsdoc, masterdts);
			saveDts(dtsdoc, masterdts);
			saveMts(dtsdoc, masterdts);
			try(PrintWriter pw1 = new PrintWriter(Files.newOutputStream(Paths.get(fname)));)
			{
				TransformerFactory transformerFactory = TransformerFactory.newInstance();
				Transformer transformer = transformerFactory.newTransformer();
				DOMSource source = new DOMSource(dtsdoc);
				StreamResult result = new StreamResult(pw1);

				transformer.transform(source, result);
			}
		}
		catch(Exception ioe)
		{
		}
	}

	public void saveDts(Document doc, Element master)
	{
		DerivedTimeSeries[] dtsList = AppUtils.getCurrentProject().getDTSList();
		if(dtsList != null)
		{
			for(int i = 0; i < dtsList.length; i++)
			{
				dtsList[i].toXml(doc, master);
			}
		}
	}

	public void saveMts(Document doc, Element master)
	{
		MultipleTimeSeries[] mtsList = AppUtils.getCurrentProject().getMTSList();
		if(mtsList != null)
		{
			for(int i = 0; i < mtsList.length; i++)
			{
				mtsList[i].toXml(doc, master);
			}
		}
	}

	public void saveData(Document doc, Element master)
	{
		String rootname = (String) _root.getUserObject();
		String name;
		Element parentel, childel;
		DefaultMutableTreeNode parentnode, childnode;
		Integer level;
		int rootchild = _root.getChildCount();
		int rootdepth = _root.getDepth();
		Element rootel = doc.createElement("node");
		master.appendChild(rootel);
		DefaultMutableTreeNode[] nodes = new DefaultMutableTreeNode[100];
		Element[] elements = new Element[100];
		rootel.setAttribute("level", "0");
		rootel.setAttribute("name", rootname);
		int rootcounter = 1;
		int lvlcount = 1;
		if(_root.getChildCount() == 0)
		{
			return;
		}
		parentnode = (DefaultMutableTreeNode) _root.getChildAt(0);
		name = (String) parentnode.getUserObject();
		parentel = doc.createElement("node");
		level = lvlcount;
		parentel.setAttribute("level", level.toString());
		parentel.setAttribute("name", name);
		nodes[1] = parentnode;
		elements[1] = parentel;
		int parentchildren = parentnode.getChildCount();
		int depth = 1;
		rootel.appendChild(parentel);
		while(rootchild >= rootcounter)
		{
			if(parentchildren > 0)
			{
				lvlcount++;
				depth++;
				for(int i = 0; i <= parentchildren - 1; i++)
				{
					childnode = (DefaultMutableTreeNode) parentnode.getChildAt(i);
					name = (String) childnode.getUserObject();
					childel = doc.createElement("node");
					level = lvlcount;
					childel.setAttribute("level", level.toString());
					childel.setAttribute("name", name);
					nodes[lvlcount] = childnode;
					elements[lvlcount] = childel;
					parentel.appendChild(childel);
					if(childnode.getChildCount() > 0)
					{
						lvlcount++;
						depth++;
						parentnode = childnode;
						parentel = childel;
						int[] childnum = new int[100];
						childnum[0] = parentnode.getChildCount();
						int[] childindex = new int[100];
						childnum[lvlcount] = parentnode.getChildCount();
						boolean exit = true;
						int i1 = 0;
						childindex[i1] = 0;
						while(exit)
						{
							if(childindex[i1] > parentnode.getChildCount() - 1)
							{
								break;
							}
							childnode = (DefaultMutableTreeNode) parentnode.getChildAt(childindex[i1]);
							name = (String) childnode.getUserObject();
							childel = doc.createElement("node");
							level = lvlcount;
							childel.setAttribute("level", level.toString());
							childel.setAttribute("name", name);
							nodes[lvlcount] = childnode;
							elements[lvlcount] = childel;
							parentel.appendChild(childel);
							if(childnode.getChildCount() > 0)
							{
								parentnode = childnode;
								parentel = childel;
								lvlcount++;
								depth++;
								i1++;
								childnum[lvlcount] = parentnode.getChildCount();
							}
							else if(childnode.getChildCount() == 0 && childnum[lvlcount] - 1 == childindex[i1])
							{
								boolean check = false;
								i1--;
								if(i1 < 0)
								{
									i1 = 0;
								}
								lvlcount--;
								depth--;
								childindex[i1]++;
								childindex[i1 + 1] = 0;
								if(lvlcount <= 2)
								{
									break;
								}
								parentnode = nodes[lvlcount - 1];
								parentel = elements[lvlcount - 1];
								if(childindex[i1] > parentnode.getChildCount() - 1)
								{
									check = true;
								}
								while(check)
								{
									i1--;
									if(i1 < 0)
									{
										i1 = 0;
									}
									lvlcount--;
									depth--;
									childindex[i1]++;
									childindex[i1 + 1] = 0;
									parentnode = nodes[lvlcount - 1];
									parentel = elements[lvlcount - 1];
									if(childindex[i1] > parentnode.getChildCount() - 1)
									{
										check = false;
									}
								}
							}
							else if(childnode.getChildCount() == 0 && childnum[lvlcount] - 1 > childindex[i1])
							{
								childindex[i1]++;
							}
							else if(childnum[lvlcount - 1] - 1 == childindex[i1] && depth == rootdepth)
							{
								exit = false;
							}
							if(lvlcount - 1 == 2 && childindex[i1] > parentnode.getChildCount() - 1)
							{
								break;
							}
						}
					}
					lvlcount = 2;
					depth = 2;
					parentnode = nodes[lvlcount - 1];
					parentel = elements[lvlcount - 1];
				}
			}
			if(rootchild == rootcounter)
			{
				break;
			}
			parentnode = (DefaultMutableTreeNode) _root.getChildAt(rootcounter);
			parentchildren = parentnode.getChildCount();
			name = (String) parentnode.getUserObject();
			rootcounter++;
			parentel = doc.createElement("node");
			lvlcount = 1;
			depth = 1;
			parentel.setAttribute("level", Integer.toString(lvlcount));
			parentel.setAttribute("name", name);
			rootel.appendChild(parentel);
			nodes[lvlcount] = parentnode;
			elements[lvlcount] = parentel;
		}
	}

	public void mergeWith() throws IOException
	{
		String filepath = VistaUtils.getFilenameFromDialog(MAIN_PANEL, FileDialog.LOAD,
				"tree", "Tree files (*.tree)");
		if(filepath == null)
		{
			return;
		}
		File file = new File(filepath);
		String filedir = file.getParent();
		try
		{
			isMerge = true;
			getFile(filepath, filedir);
			Vector dtsvector = getPrjDts();
			if(dtsvector.elementAt(0) != null)
			{
				for(int i = 0; i < dtsvector.capacity(); i++)
				{
					DerivedTimeSeries dts = (DerivedTimeSeries) dtsvector.elementAt(i);
					AppUtils.getCurrentProject().add(dts);
				}
			}
			Vector mtsvector = getPrjMts();
			if(mtsvector.elementAt(0) != null)
			{
				for(int i = 0; i < mtsvector.capacity(); i++)
				{
					MultipleTimeSeries mts = (MultipleTimeSeries) mtsvector.elementAt(i);
					AppUtils.getCurrentProject().add(mts);
				}
			}
		}
		catch(IOException ex)
		{
			throw new IOException("No File by that name");
		}
	}

	public TreeNode readData(String fname, String dirname) throws IOException, SAXException
	{
		DefaultMutableTreeNode parentnode, prvnode, curnode;
		Element prvel, curel;
		Integer lvl;
		int prvlvl, curlvl;
		String name;
		DefaultMutableTreeNode[] nodes = new DefaultMutableTreeNode[100];
		try
		{
			DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
			// parse XML file
			DocumentBuilder db = dbf.newDocumentBuilder();

			Document doc = db.parse(new File(fname));
			Element top = doc.getDocumentElement();
			NodeList nodeElements = top.getElementsByTagName("node");
			Element parentel = (Element) nodeElements.item(0);
			parentnode = new DefaultMutableTreeNode(parentel.getAttribute("name"));
			if(!isMerge)
			{
				_root = parentnode;
				nodes[0] = _root;
				curel = (Element) nodeElements.item(1);
				name = curel.getAttribute("name");
				curnode = new DefaultMutableTreeNode(name);
				curnode.setAllowsChildren(getAllowChildren(name));
				lvl = new Integer(curel.getAttribute("level"));
				curlvl = lvl;
				nodes[curlvl] = curnode;
				parentnode.add(curnode);
				prvnode = curnode;
				prvel = curel;
				prvlvl = curlvl;
			}
			else
			{
				_root = (DefaultMutableTreeNode) readData();
				nodes[0] = _root;
				curel = (Element) nodeElements.item(1);
				name = curel.getAttribute("name");
				curnode = new DefaultMutableTreeNode(name);
				curnode.setAllowsChildren(getAllowChildren(name));
				lvl = new Integer(curel.getAttribute("level"));
				curlvl = lvl.intValue();
				nodes[curlvl] = curnode;
				_root.add(curnode);
				prvnode = curnode;
				prvel = curel;
				prvlvl = curlvl;
			}
			isMerge = false;
			for(int i = 2; i < nodeElements.getLength(); i++)
			{
				curel = (Element) nodeElements.item(i);
				if(curel == null)
				{
					break;
				}
				name = curel.getAttribute("name");
				curnode = new DefaultMutableTreeNode(name);
				if(!getAllowChildren(name))
				{
					curnode.setAllowsChildren(false);
				}
				else
				{
					curnode.setAllowsChildren(true);
				}
				lvl = new Integer(curel.getAttribute("level"));
				curlvl = lvl.intValue();
				if(curlvl == prvlvl)
				{
					parentnode.add(curnode);
				}
				else if(curlvl > prvlvl)
				{
					parentel = prvel;
					parentnode = prvnode;
					parentnode.add(curnode);
					nodes[prvlvl] = parentnode;
				}
				else if(curlvl < prvlvl)
				{
					parentnode = nodes[curlvl - 1];
					parentnode.add(curnode);
				}
				prvlvl = curlvl;
				prvel = curel;
				prvnode = curnode;
			}
			reload();
			setRoot(_root);
			NodeList dtsElements = top.getElementsByTagName("DTS");
			for(int i = 0; i < dtsElements.getLength(); i++)
			{
				Element de = (Element) dtsElements.item(i);
				if(de == null)
				{
					break;
				}
				DerivedTimeSeries dts = new DerivedTimeSeries();
				dts.fromXml(de);
				prjdts.addElement(dts);
			}
			NodeList mtsElements = top.getElementsByTagName("MTS");
			for(int i = 0; i < mtsElements.getLength(); i++)
			{
				Element de = (Element) mtsElements.item(i);
				if(de == null)
				{
					break;
				}
				MultipleTimeSeries mts = new MultipleTimeSeries();
				mts.fromXml(de);
				prjmts.addElement(mts);
			}
		}
		catch(SAXException se)
		{
			throw new SAXException("Error trying to read Xml File", se);
		}
		catch(Exception e)
		{
			e.printStackTrace(System.err);
			throw new IOException("File Not Found", e);
		}
		return _root;
	}

	private void cannotChange(int flag)
	{
		if(flag == 1)
		{
			JOptionPane.showMessageDialog(null, "Cannot Change a MTS to DTS");
		}
		else if(flag == 2)
		{
			JOptionPane.showMessageDialog(null, "Cannot Change a DTS to MTS");
		}
		else if(flag == 3)
		{
			JOptionPane.showMessageDialog(null, "Node has to end with DTS or MTS");
		}
	}

}
