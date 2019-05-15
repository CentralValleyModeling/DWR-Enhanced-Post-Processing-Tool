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
package vista.app.schematic;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.io.EOFException;
import java.io.IOException;

import vista.graph.AnimateElement;
import vista.graph.AnimationObservable;
import vista.graph.GEAttr;
import vista.graph.Scale;

/**
 * A class representing particles in an animation. This element needs a
 * DSMGridElement to draw the particles as squares on the gridded channels.
 * 
 * @author Nicky Sandhu
 * @version $Id: ParticleElement.java,v 1.7 2000/01/04 22:01:13 miller Exp $
 */
public class ParticleElement extends AnimateElement {
	/**
   *
   */
	public ParticleElement(DSMGridElement grid, String particleFile) {
		super(new GEAttr());
		_grid = grid;
		_particleInput = new ParticleDataInput();
		_particleFile = particleFile;
		try {
			_particleInput.initializeData(particleFile);
			_particleData = new ParticleData(_particleInput
					.getInitialNParticles());
		} catch (IOException ioe) {
			System.out.println("Error reading particle data: " + particleFile);
			System.out.println(ioe);
		}
	}

	/**
   *
   */
	public void rewind() {
		try {
			_particleInput.initializeData(_particleFile);
			setFileStatus(false);
		} catch (IOException ioe) {
			System.out.println("Error reading particle data: " + _particleFile);
			System.out.println(ioe);
		}
	}

	/**
   *
   */
	protected void Draw() {

	}

	/**
   * 
   */
	public void animateNext() {
		// System.out.println(" animating next frame in particle element");
		Graphics gc = getGraphics();
		Color previousColor = gc.getColor();
		gc.setColor(_particleColor);
		Particle[] particles = _particleData.getParticles();
		Network net = _grid.getNetwork();
		Scale xS = _grid.getGridXScale();
		Scale yS = _grid.getGridYScale();
		for (int i = 0; i < particles.length; i++) {
			Particle particle = particles[i];
			if (particle == null)
				continue;
			int wbId = particles[i].getWaterbodyId();
			if (wbId < 0)
				continue;
			Link link = net.getLink(particles[i].getWaterbodyId());
			if (link == null)
				continue;
			if (link instanceof Channel) {
				Channel channel = (Channel) link;
				Node upNode = channel.getNode(Channel.UPNODE_INDEX);
				Node downNode = channel.getNode(Channel.DOWNNODE_INDEX);
				float dist = particle.getDistanceFromUpNode();
				float px = dist / 100.0f * (downNode.getX() - upNode.getX())
						+ upNode.getX();
				float py = dist / 100.0f * (downNode.getY() - upNode.getY())
						+ upNode.getY();
				((Graphics2D) gc).setTransform(_grid.getTransform());
				gc.fillOval(xS.scaleToUC(px) - channel_width / 2, yS
						.scaleToUC(py)
						- channel_width / 2, channel_width, channel_width);
			}
		}

		gc.setColor(previousColor);
	}

	/**
	 * The update method is called when the animation frame needs to be updated.
	 * Each element is responsible for drawing itself within the bounds given.
	 */
	public void update(AnimationObservable o, Object arg) {
		updateParticles();
		// draw();
	}

	/**
   *
   */
	public void updateParticles() {
		try {
			_particleInput.updateParticles(_particleData);
		} catch (EOFException eof) {
			setFileStatus(_particleInput.isAtEndOfFile());
		} catch (IOException ioe) {
			System.out.println("Exception:" + ioe.getMessage()
					+ " reading animation file:");
			// throw new InterruptedException("Ending animation due to : " +
			// ioe);
		}

	}

	/**
   *
   */
	public Dimension getPreferredSize() {
		return new Dimension(4, 4);
	}

	/**
   *
   */
	public Dimension getMinimumSize() {
		return getPreferredSize();
	}

	/**
   * 
   */
	public void setColor(Color c) {
		_particleColor = c;
	}

	/**
   * 
   */
	public PTMTimeData getTimeData() {
		return new PTMTimeData(_particleData);
	}

	public void setFileStatus(boolean fileStatus) {
		this.fileStatus = fileStatus;
	}

	public boolean isFileStatus() {
		return fileStatus;
	}

	/**
   *
   */
	private boolean fileStatus;
	/**
   *
   */
	protected DSMGridElement _grid;
	/**
   *
   */
	protected ParticleDataInput _particleInput;
	/**
   *
   */
	protected ParticleData _particleData;
	/**
   *
   */
	private int channel_width = 4;
	/**
   *
   */
	private static final int NPARTICLES = 100;
	/**
   *
   */
	private Color _particleColor = Color.yellow;
	private String _particleFile;
}
