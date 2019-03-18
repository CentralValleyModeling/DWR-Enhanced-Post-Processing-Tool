/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */
package calsim.schematic;

import java.awt.Component;
import java.awt.event.MouseEvent;
import javax.swing.*;

import calsim.gui.GuiUtils;
import vista.gui.CursorChangeListener;
import vista.gui.VistaUtils;

/**
 * An interface defining the call back functions when the schematic
 * recives meaninful input
 *
 * @author Nicky Sandhu
 * @version $Id: PopupMenuHandler.java,v 1.1.2.7 2000/04/14 16:36:05 amunevar Exp $
 */
public class PopupMenuHandler implements RequestHandler
{
	private JPopupMenu _flowMenu, _storageMenu, _nodeMenu;
	private String _bpart = ""; //, _cpart = "";
	private JComponent _comp;

	/**
	 *
	 */
	public PopupMenuHandler()
	{
		JMenuItem mi = null;
		//
		_flowMenu = new JPopupMenu("Channel:");
		mi = new JMenuItem("Flow");
		mi.addActionListener(new PopupListener("FLOW-CHANNEL"));
		_flowMenu.add(mi);
		//
		_storageMenu = new JPopupMenu("Storage:");
		_storageMenu.add(mi = new JMenuItem("Storage EOP"));
		mi.addActionListener(new PopupListener("STORAGE"));
		_storageMenu.add(mi = new JMenuItem("Evaporation"));
		mi.addActionListener(new PopupListener("EVAPORATION"));
		_storageMenu.add(mi = new JMenuItem("Surface Area"));
		mi.addActionListener(new PopupListener("SURFACE-AREA"));
		_storageMenu.add(mi = new JMenuItem("Spill"));
		mi.addActionListener(new PopupListener("FLOW-SPILL-NON-RECOV"));
		_storageMenu.add(mi = new JMenuItem("Mass Balance"));
		mi.addActionListener(new PopupListener("MASS-BALANCE"));
		//
		_nodeMenu = new JPopupMenu("Junction");
		_nodeMenu.add(mi = new JMenuItem("Mass Balance"));
		mi.addActionListener(new PopupListener("MASS-BALANCE"));
	}

	/**
	 * called when a symbol with reference object obj is clicked on
	 */
	public void clickedOn(Object obj, MouseEvent e)
	{
		if(obj == null)
		{
			return;
		}
		if(!SwingUtilities.isRightMouseButton(e))
		{
			return;
		}
		JComponent comp = (JComponent) e.getSource();
		JPopupMenu menu = null;
		if(obj instanceof String)
		{
			String str = (String) obj;
			if(str.indexOf("storage") >= 0)
			{
				menu = _storageMenu;
				int len = "storage".length();
				_bpart = str.substring(str.indexOf("storage") + 1 + len).trim();
			}
			else if(str.indexOf("junction") >= 0)
			{
				menu = _nodeMenu;
				int len = "junction".length();
				_bpart = str.substring(str.indexOf("junction") + 1 + len).trim();
				//	_cpart = "NODE";
			}
			else if(str.indexOf("demand") >= 0)
			{
				menu = _flowMenu;
				int len = "demand".length();
				_bpart = str.substring(str.indexOf("demand") + 1 + len).trim();
				//	_cpart = "DEMAND";
			}
			else if(str.indexOf("inflow") >= 0)
			{
				menu = _flowMenu;
				int len = "inflow".length();
				_bpart = str.substring(str.indexOf("inflow") + 1 + len).trim();
			}
			else if(str.indexOf("flow") >= 0)
			{
				menu = _flowMenu;
				int len = "flow".length();
				_bpart = str.substring(str.indexOf("flow") + 1 + len).trim();
				//	_cpart = "FLOW";
			}
			else if(str.indexOf("return") >= 0)
			{
				menu = _flowMenu;
				int len = "return".length();
				_bpart = str.substring(str.indexOf("return") + 1 + len).trim();
			}
			else
			{
				throw new RuntimeException("Unknown element type: " + str);
			}
		}
		else
		{
			return;
		}
		// System.out.println("B part: " + _bpart);
		// System.out.println("C part: " + _cpart);
		menu.show(comp, e.getX(), e.getY());
	}

	/**
	 * called when a symbol with reference object obj is double clicked on
	 */
	public void doubleClickedOn(Object obj, MouseEvent e)
	{
	}

	/**
	 * called when a symbol with reference object obj is double clicked on
	 */
	public void pressedOn(Object obj, MouseEvent e)
	{
	}

	/**
	 * called when a symbol with reference object obj is double clicked on
	 */
	public void releasedOn(Object obj, MouseEvent e)
	{
	}

	/**
	 * called when a symbol with reference object obj is moved over
	 */
	public void movedOver(Object obj, MouseEvent e)
	{
		if(obj == null)
		{
			if(_comp != null)
			{
				_comp.setToolTipText(null);
			}
			return;
		}
		if(_comp == null)
		{
			_comp = (JComponent) e.getSource();
			ToolTipManager.sharedInstance().setDismissDelay(100000);
			ToolTipManager.sharedInstance().setInitialDelay(1000);
			ToolTipManager.sharedInstance().setReshowDelay(2000);
			ToolTipManager.sharedInstance().registerComponent(_comp);
			ToolTipManager.sharedInstance().setEnabled(true);
		}
		_comp.setToolTipText(obj.toString());
	}

	/**
	 * called when a symbol is dragged
	 */
	public void draggedTo(Object obj, MouseEvent e)
	{
	}

	/**
	 * @author Nicky Sandhu
	 * @version $Id: PopupMenuHandler.java,v 1.1.2.7 2000/04/14 16:36:05 amunevar Exp $
	 */
	class PopupListener extends CursorChangeListener
	{
		private String _type;

		public PopupListener(String str)
		{
			_type = str;
		}

		public void doWork()
		{
			String bpart = _bpart.trim();
			String cpart = _type.trim();
			if(_type.equals("STORAGE"))
			{
				bpart = "S" + bpart;
			}
			else if(_type.equals("EVAPORATION"))
			{
				bpart = "E" + bpart;
			}
			else if(_type.equals("SURFACE-AREA"))
			{
				bpart = "A" + bpart;
			}
			else if(_type.equals("FLOW-SPILL-NON-RECOV"))
			{
				bpart = "F" + bpart;
			}
			else if(_type.equals("MASS-BALANCE"))
			{
				//
			}
			else
			{
			}
			//
			if(bpart.indexOf("I") == 0)
			{
				cpart = "FLOW-INFLOW";
			}
			else if(bpart.indexOf("C") == 0)
			{
				cpart = "FLOW-CHANNEL";
			}
			else if(bpart.indexOf("R") == 0)
			{
				cpart = "FLOW-RETURN";
			}
			else if(bpart.indexOf("D") == 0)
			{
				cpart = "FLOW-DELIVERY";
			}
			//
			System.out.println("Retrieve: " + " bpart = " + bpart + " & cpart = " + cpart);
			Component comp = super.getComponent();
			Component fr = VistaUtils.getFrameForComponent(comp);
			if(cpart.equals("MASS-BALANCE"))
			{
				GuiUtils.displayMassBalanceData(fr, new Integer(bpart).intValue());
			}
			else
			{
				GuiUtils.displayData(fr, bpart, cpart);
			}
		}
	}
}
