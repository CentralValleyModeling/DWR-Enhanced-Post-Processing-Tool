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

/* (swing1.1.1beta2) */
package vista.gui;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import javax.swing.*;
import javax.swing.border.EtchedBorder;

/**
 * @version 1.0 05/10/99
 */
public class ComboMenuBar extends JMenuBar {

	JMenu menu;
	Dimension preferredSize;

	public ComboMenuBar(JMenu menu) {
		this.menu = menu;

		Color color = UIManager.getColor("Menu.selectionBackground");
		UIManager.put("Menu.selectionBackground", UIManager
				.getColor("Menu.background"));
		menu.updateUI();
		UIManager.put("Menu.selectionBackground", color);

		MenuItemListener listener = new MenuItemListener();
		setListener(menu, listener);

		add(menu);
	}

	class MenuItemListener implements ActionListener {
		public void actionPerformed(ActionEvent e) {
			JMenuItem item = (JMenuItem) e.getSource();
			menu.setText(item.getText());
			menu.requestFocus();
		}
	}

	private void setListener(JMenuItem item, ActionListener listener) {
		if (item instanceof JMenu) {
			JMenu menu = (JMenu) item;
			int n = menu.getItemCount();
			for (int i = 0; i < n; i++) {
				setListener(menu.getItem(i), listener);
			}
		} else if (item != null) { // null means separator
			item.addActionListener(listener);
		}
	}

	public String getSelectedItem() {
		return menu.getText();
	}

	public void setPreferredSize(Dimension size) {
		preferredSize = size;
	}

	public Dimension getPreferredSize() {
		if (preferredSize == null) {
			Dimension sd = super.getPreferredSize();
			Dimension menuD = getItemSize(menu);
			Insets margin = menu.getMargin();
			Dimension retD = new Dimension(menuD.width, margin.top
					+ margin.bottom + menuD.height);
			menu.setPreferredSize(retD);
			preferredSize = retD;
		}
		return preferredSize;
	}

	private Dimension getItemSize(JMenu menu) {
		Dimension d = new Dimension(0, 0);
		int n = menu.getItemCount();
		for (int i = 0; i < n; i++) {
			Dimension itemD;
			JMenuItem item = menu.getItem(i);
			if (item instanceof JMenu) {
				itemD = getItemSize((JMenu) item);
			} else if (item != null) {
				itemD = item.getPreferredSize();
			} else {
				itemD = new Dimension(0, 0); // separator
			}
			d.width = Math.max(d.width, itemD.width);
			d.height = Math.max(d.height, itemD.height);
		}
		return d;
	}

	public static class ComboMenu extends JMenu {
		ArrowIcon iconRenderer;

		public ComboMenu(String label) {
			super(label);
			iconRenderer = new ArrowIcon(SwingConstants.SOUTH, true);
			setBorder(new EtchedBorder());
			setIcon(new BlankIcon(null, 11));
			setHorizontalTextPosition(JButton.LEFT);
			setFocusPainted(true);
		}

		public void paintComponent(Graphics g) {
			super.paintComponent(g);
			Dimension d = this.getPreferredSize();
			int x = Math.max(0, d.width - iconRenderer.getIconWidth() - 3);
			int y = Math.max(0,
					(d.height - iconRenderer.getIconHeight()) / 2 - 2);
			iconRenderer.paintIcon(this, g, x, y);
		}
	}

	public static JMenu createMenu(String label) {
		return new ComboMenu(label);
	}

}
