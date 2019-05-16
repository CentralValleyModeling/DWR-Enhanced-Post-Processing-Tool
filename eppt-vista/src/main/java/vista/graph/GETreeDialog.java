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
package vista.graph;

import java.awt.BorderLayout;
import java.awt.Container;
import java.awt.Frame;
import java.awt.Rectangle;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import javax.swing.*;
import javax.swing.event.TreeSelectionEvent;
import javax.swing.event.TreeSelectionListener;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.TreePath;

import vista.gui.Changeable;
import vista.gui.DialogButtonPanel;

/**
 * Initializes a tree dialog for a GEContainer and redraws the containing canvas
 * on apply/done button press.
 *
 * @author Nicky Sandhu
 * @version $Id: GETreeDialog.java,v 1.1 2003/10/02 20:48:59 redwood Exp $
 */
public class GETreeDialog extends JDialog implements Changeable
{
	/**
	 *
	 */
	public static final boolean DEBUG = false;
	private GECanvas _gC;
	private JTree tree;
	private JSplitPane _splitPane;
	private GEDialogPanel _currentPanel;

	/**
	 * initializes a tree structure of components contained in a GEContainer
	 * from the given graph frame.
	 */
	public GETreeDialog(Frame frame, GECanvas canvas)
	{
		super(frame);
		_splitPane = new JSplitPane(JSplitPane.VERTICAL_SPLIT);
		// make left pane tree
		_gC = canvas;
		GraphicElement ge = _gC.getGraphicElement();
		GETree treePanel = null;
		if(ge instanceof GEContainer)
		{
			treePanel = new GETree((GEContainer) ge);
			tree = treePanel.getTree();
			_splitPane.setLeftComponent(treePanel);
			setCurrentPanel(ge.createDialogPanel());
		}
		// main layout
		Container cPane = getContentPane();
		cPane.setLayout(new BorderLayout());
		cPane.add(_splitPane, BorderLayout.CENTER);
		cPane.add(new DialogButtonPanel(this), BorderLayout.SOUTH);
		// add listeners & open frame @ location
		addMouseListenerToTree();
		pack();
		Rectangle r = frame.getBounds();
		setLocation(r.x - getBounds().width, r.y);
		_splitPane.setDividerLocation(0.35);
		show();
	}

	/**
	 *
	 */
	public void setCurrentPanel(GEDialogPanel panel)
	{
		_currentPanel = panel;
		_splitPane.setRightComponent(_currentPanel);
		_splitPane.setDividerLocation(0.35);
	}

	/**
	 * apply changes done
	 */
	public void applyChanges()
	{
		_currentPanel.applyChanges();
		_gC.redoNextPaint();
		_gC.update(_gC.getGraphics());
	}

	/**
	 * done with changes, dispose of dialog
	 */
	public void doneChanges()
	{
		dispose();
	}

	/**
	 * path single clicked
	 */
	public void pathSingleClicked(int selRow, TreePath selPath)
	{
		pathSelected(selPath);
	}

	/**
	 *
	 */
	public void pathSelected(TreePath selPath)
	{
		DefaultMutableTreeNode node = (DefaultMutableTreeNode) selPath
				.getLastPathComponent();
		GraphicElement ge = (GraphicElement) node.getUserObject();
		setCurrentPanel(ge.createDialogPanel());
	}

	/**
	 * path double clicked
	 */
	public void pathDoubleClicked(int selRow, TreePath selPath)
	{
		pathSelected(selPath);
	}

	/**
	 *
	 */
	private void addMouseListenerToTree()
	{
		MouseListener ml = new MouseAdapter()
		{
			public void mouseClicked(MouseEvent e)
			{
				int selRow = tree.getRowForLocation(e.getX(), e.getY());
				TreePath selPath = tree.getPathForLocation(e.getX(), e.getY());
				if(selRow != -1)
				{
					if(e.getClickCount() == 1)
					{
						pathSingleClicked(selRow, selPath);
					}
					else if(e.getClickCount() == 2)
					{
						pathDoubleClicked(selRow, selPath);
					}
				}
			}
		};
		tree.addMouseListener(ml);
	}

	/**
	 *
	 */
	private class GETreeListener implements TreeSelectionListener
	{
		public void valueChanged(TreeSelectionEvent evt)
		{
			pathSelected(evt.getPath());
		}
	}
}
