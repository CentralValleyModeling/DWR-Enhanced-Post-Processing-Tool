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
package vista.graph;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.Properties;

import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import javax.swing.JPanel;

/**
 * This class constructs a frame and provides the context with which to interact
 * with the Graph object. The Graph object itself is contained with the
 * GraphCanvas object
 * 
 * @see Graph
 * @see HEC.DSS.GraphCanvas
 * @author Nicky Sandhu
 * @version $Id: AnimatorFrame.java,v 1.4 2001/03/05 21:46:19 eli2 Exp $
 */
public class AnimatorFrame extends JFrame {
	/**
	 * for debuggin'
	 */
	public boolean DEBUG = false;
	/**
	 * The component on which the graph is drawn.
	 */
	public AnimatorCanvas _gC = null;

	/**
	 * Constructor
	 */
	public AnimatorFrame(AnimatorCanvas gC, String frameTitle) {
		super(frameTitle);

		_gC = gC;

		// add graph canvas to frame and set its listeners
		getContentPane().setLayout(new BorderLayout());
		getContentPane().add("Center", _gC);

		// ZoomInteractor zi = new ZoomInteractor(_gC);
		// Graph graph = (Graph) _gC.getGraphicElement();
		// if (graph.getAttributes()._backgroundColor == Color.black)
		// zi.setZoomRectangleColor(Color.white);
		// _gC.addMouseListener(zi);
		// _gC.addMouseMotionListener(zi);
		// ResizeInteractor ri = new FontResizeInteractor(_gC);
		// _gC.addComponentListener(ri);

		JMenuBar mb = new JMenuBar();
		JMenu mainMenu = new JMenu("Main Menu");
		JMenuItem quitItem = new JMenuItem("Quit Window");
		quitItem.addActionListener(new QuitListener());

		mainMenu.addSeparator();
		mainMenu.add(quitItem);

		mb.add(mainMenu);
		this.setJMenuBar(mb);

		ActionListener bl = new ButtonListener(this);
		JPanel buttonPanel = new JPanel();
		buttonPanel.setLayout(new FlowLayout());

		JButton pauseButton = new JButton("Pause");
		pauseButton.addActionListener(bl);
		buttonPanel.add(pauseButton);

		JButton speedUpButton = new JButton("Speed Up");
		speedUpButton.addActionListener(bl);
		buttonPanel.add(speedUpButton);

		JButton slowDownButton = new JButton("Slow Down");
		slowDownButton.addActionListener(bl);
		buttonPanel.add(slowDownButton);

		JButton stepButton = new JButton("Step Forward");
		stepButton.addActionListener(bl);
		buttonPanel.add(stepButton);

		getContentPane().add("South", buttonPanel);

		this.pack();
		Toolkit tk = getToolkit();
		Dimension screenSize = tk.getScreenSize();
		Dimension frameSize = getSize();
		this.setLocation(screenSize.width - frameSize.width, screenSize.height
				- frameSize.height);
		// this.show();
		setVisible(true);
		_animator = _gC.getAnimator();
	}

	/**
	 * gets the reference to the graph canvas
	 */
	public AnimatorCanvas getCanvas() {
		return _gC;
	}

	/**
   * 
   */
	Animator _animator;
	/**
   * 
   */
	Properties props = new Properties();

	/**
   *
   */
	protected class QuitListener implements ActionListener {
		/**
   *
   */
		public void actionPerformed(ActionEvent e) {
			// System.exit(0);
			dispose();
		}
	}

	/**
   * 
   */
	void toggleAnimation() {
		// System.out.println("Toggling animation");
		if (_animator.isAnimationRunning())
			_animator.stopAnimation();
		else
			_animator.startAnimation();
	}

	/**
   * 
   */
	void speedUpAnimation() {
		_animator.setInterval(_animator.getInterval() - 250);
	}

	/**
   * 
   */
	void slowDownAnimation() {
		_animator.setInterval(_animator.getInterval() + 250);
	}

	/**
   * 
   */
	void stepAnimation() {
		_animator.animate();
		_animator.notifyDisplays();
	}

	/**
   *
   */
	protected class ButtonListener implements ActionListener {
		/**
   *
   */
		protected AnimatorFrame _tg;

		/**
   *
   */
		public ButtonListener(AnimatorFrame tg) {
			_tg = tg;
		}

		/**
		 * Handles button events for
		 */
		public void actionPerformed(ActionEvent e) {
			if (e.getSource() instanceof JMenuItem) {
				JMenuItem mItem = (JMenuItem) e.getSource();
				String label = mItem.getText();
				if (label.equals("Pause")) {
					_tg.toggleAnimation();
				} else if (label.equals("Quit Window")) {
					System.exit(0);
				} else if (label.equals("Speed Up")) {
					_tg.speedUpAnimation();
				} else if (label.equals("Slow Down")) {
					_tg.slowDownAnimation();
				} else if (label.equals("Step Forward")) {
					_tg.stepAnimation();
				}
			} else if (e.getSource() instanceof JButton) {
				JButton button = (JButton) e.getSource();
				String label = button.getText();
				if (label.equals("Pause")) {
					_tg.toggleAnimation();
				} else if (label.equals("Quit Window")) {
					System.exit(0);
				} else if (label.equals("Speed Up")) {
					_tg.speedUpAnimation();
				} else if (label.equals("Slow Down")) {
					_tg.slowDownAnimation();
				} else if (label.equals("Step Forward")) {
					_tg.stepAnimation();
				}
			}

		}
	}
}
