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
package vista.app;

import java.util.Enumeration;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.TreeNode;

import vista.set.Group;
import vista.set.Pathname;

/**
 * This class generates a tree which is a view onto the Group. As the TreeModel
 * is not efficient to implement in terms of a Group's linear structure the
 * DefaultMutableTreeNode objects are used to construct the tree using the parts
 * of the pathname.
 * <p>
 * Displays a group as a tree in which parts of the pathname are nodes. For
 * example the pathname /A/B/C/D/E/F would be found by default at root A then
 * node B, then C and so on. This default path display can be shuffled so that
 * other parts could be the root or at a different node level. This class only
 * constructs the tree .The display of the tree is done by JTree.
 *
 * @author Nicky Sandhu
 * @version $Id: GroupTree.java,v 1.1 2003/10/02 20:48:32 redwood Exp $
 */
public class GroupTree
{
	/**
	 * the tree representing the group
	 */
	private TreeNode _root;
	/**
	 * The group being represented as a tree
	 */
	private Group _group;
	/**
	 * The part order initialized to the default.
	 */
	private int[] _partOrder = {Pathname.A_PART, Pathname.B_PART,
			Pathname.C_PART, Pathname.D_PART, Pathname.E_PART, Pathname.F_PART};

	/**
	 * constructor
	 */
	public GroupTree(Group group)
	{
		_group = group;
		createTree(_group);
	}

	/**
	 * An array of indices from 0..5 shuffled to create a tree with different
	 * part addition scenarios. This shuffles the level of the node within the
	 * tree.
	 */
	public void setPartAdditionOrder(int[] partOrder)
	{
		if(partOrder.length != 6)
		{
			return;
		}
		else
		{
			_partOrder = partOrder;
		}
		createTree(_group);
	}

	/**
	 * creates a tree with the given group and part order.
	 */
	private void createTree(Group group)
	{
		_root = new DefaultMutableTreeNode(group.getName());
		addGroup(group, _root);
	}

	/**
	 * returns the updated tree.
	 */
	public TreeNode getRoot()
	{
		return _root;
	}

	/**
	 * adds a group to the node
	 */
	private void addGroup(Group group, TreeNode node)
	{
		int nrefs = group.getNumberOfDataReferences();
		for(int i = 0; i < nrefs; i++)
		{
			Pathname p = group.getDataReference(i).getPathname();
			addPathname(p, node);
		}
	}

	/**
	 * adds a pathname to the node
	 */
	private void addPathname(Pathname p, TreeNode node)
	{
		DefaultMutableTreeNode currentNode = (DefaultMutableTreeNode) node;
		for(int i = 0; i < Pathname.MAX_PARTS; i++)
		{
			String part = p.getPart(_partOrder[i]);
			DefaultMutableTreeNode partChildNode = getPartChildNode(part,
					currentNode);
			if(partChildNode == null)
			{
				DefaultMutableTreeNode newNode = new DefaultMutableTreeNode(
						part);
				currentNode.add(newNode);
				currentNode = newNode;
			}
			else
			{
				currentNode = partChildNode;
			}
		}
	}

	/**
	 * adds a part of the pathname to the node
	 */
	private DefaultMutableTreeNode getPartChildNode(String part,
													DefaultMutableTreeNode node)
	{
		for(Enumeration e = node.children(); e.hasMoreElements(); )
		{
			DefaultMutableTreeNode child = (DefaultMutableTreeNode) e
					.nextElement();
			String name = (String) child.getUserObject();
			if(name.equals(part))
			{
				return child;
			}
		}
		return null;
	}
}
