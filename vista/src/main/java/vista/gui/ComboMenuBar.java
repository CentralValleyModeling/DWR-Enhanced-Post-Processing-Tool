/*
    Copyright (C) 1996-2000 State of California, Department of 
    Water Resources.

    VISTA : A VISualization Tool and Analyzer. 
	Version 1.0
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

/* (swing1.1.1beta2) */
package vista.gui;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JButton;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import javax.swing.SwingConstants;
import javax.swing.UIManager;
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
