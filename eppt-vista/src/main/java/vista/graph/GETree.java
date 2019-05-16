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

import java.util.Enumeration;
import javax.swing.*;
import javax.swing.tree.DefaultMutableTreeNode;

/**
 * Constructs a tree view for the the GEContainer. Embeds the tree in a buffered
 * pane and a scrollpane and returns the pane.
 *
 * @author Nicky Sandhu
 * @version $Id: GETree.java,v 1.1 2003/10/02 20:48:59 redwood Exp $
 */
public class GETree extends JPanel
{
	/**
	 * the tree for this component
	 */
	private JTree tree;

	/**
	 * creates a tree for the container and its components
	 */
	public GETree(GEContainer comp)
	{
		super(true); // true = please double buffer
		// Create the nodes.
		DefaultMutableTreeNode compNode = new DefaultMutableTreeNode(comp);
		// Add all the components of this comp
		addChildren(compNode, comp);
		// Make all child-free nodes leaves.
		makeLeaves(compNode);

		tree = new JTree(compNode);
		// Create the scroll pane and add the tree to it.
		JScrollPane scrollPane = new JScrollPane();
		scrollPane.getViewport().add(tree);

		// Add the scroll pane to this panel.
		setLayout(new java.awt.GridLayout(1, 0));
		add(scrollPane);
	}

	/**
	 * @return the tree for this component.
	 */
	public JTree getTree()
	{
		return tree;
	}

	/**
	 * Adds the children of the component to it and recursively calls this
	 * function till all its tree structure has been added.
	 */
	private void addChildren(DefaultMutableTreeNode node, GEContainer comp)
	{
		for(Enumeration e = comp.getIterator(); e.hasMoreElements(); )
		{
			GraphicElement ge = (GraphicElement) e.nextElement();
			DefaultMutableTreeNode geNode = new DefaultMutableTreeNode(ge);
			node.add(geNode);
			if(ge instanceof GEContainer)
			{
				addChildren(geNode, (GEContainer) ge);
			}
		}
	}

	// This method will be added to the DefaultMutableTreeNode API in a future
	// release.
	private void makeLeaves(DefaultMutableTreeNode compNode)
	{
		Enumeration childEnum = compNode.preorderEnumeration();
		DefaultMutableTreeNode descendant;

		while(childEnum.hasMoreElements())
		{
			descendant = (DefaultMutableTreeNode) childEnum.nextElement();
			if(descendant.getChildCount() == 0)
			{
				descendant.setAllowsChildren(false);
			}
		}
	}
}
