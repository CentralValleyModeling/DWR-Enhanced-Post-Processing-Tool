/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */
package vista.graph;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.Properties;
import javax.swing.*;

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
