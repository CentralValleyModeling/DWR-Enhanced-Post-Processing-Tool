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
package vista.app.schematic;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Insets;
import java.awt.Rectangle;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.event.MouseMotionListener;
import java.awt.event.MouseWheelEvent;
import java.awt.event.MouseWheelListener;
import java.awt.geom.AffineTransform;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.IOException;

import javax.imageio.ImageIO;
import javax.swing.JDialog;
import javax.swing.JPanel;
import javax.swing.JTextField;

import vista.graph.GEAttr;
import vista.graph.GraphicElement;
import vista.graph.Scale;

/**
 * The base class for all objects that need to be animated. This class has its
 * update method called by the Animator class. This class implements setBounds
 * and draw methods to render itself on the graphics context
 */
public class DSMGridElement extends GraphicElement {
	private BufferedImage backgroundImage;
	private AffineTransform transform;

	/**
   * 
   */
	public DSMGridElement(Network net, String backgroundImageFile) {
		super(new GEAttr());

		_net = net;
		double xMin = Double.MAX_VALUE;
		double xMax = Double.MIN_VALUE;
		double yMin = Double.MAX_VALUE;
		double yMax = Double.MIN_VALUE;

		for (int i = 0; i < _net.getNumberOfNodes(); i++) {
			Node n = _net.getNode(i);
			if (n != null) {
				double x = n.getX();
				double y = n.getY();
				xMax = Math.max(xMax, x);
				xMin = Math.min(xMin, x);
				yMax = Math.max(yMax, y);
				yMin = Math.min(yMin, y);
			}
		}

		xS = new Scale(xMin, xMax, 0, 10);
		yS = new Scale(yMin, yMax, 0, 10);
		grid_xS = new Scale(xMin, xMax, 0, 10);
		grid_yS = new Scale(yMin, yMax, 0, 10);
		setInsets(new Insets(15, 15, 15, 15));

		try {
			this.backgroundImage = ImageIO.read(new File(backgroundImageFile));
		} catch (IOException ex) {

		}
		transform = new AffineTransform();
		transform.translate(-340, -340);
		//transform.rotate(0.5, -340, -340);
		transform.translate(20, -30);
		transform.scale(2.35, 2.5);
		// showTransformDialog();
	}

	/**
   * 
   */
	public void setBounds(Rectangle r) {
		super.setBounds(r);
		Rectangle rb = getInsetedBounds();
		xS.setUCRange(rb.x, rb.width + rb.x);
		yS.setUCRange(rb.height + rb.y, rb.y);
		setZoomReference(rb);
	}

	public void setZoomReference(Rectangle rb) {
		if (zoomfactor != 1) {
			grid_xS.setUCRange(xpos, (int) (rb.width * zoomfactor) + xpos);
			grid_yS.setUCRange((int) (rb.height * zoomfactor) + ypos, ypos);
		} else {
			grid_xS.setUCRange(rb.x, rb.width + rb.x);
			grid_yS.setUCRange(rb.height + rb.y, rb.y);
		}
	}

	public void setZoom(float zf, int xp, int yp) {
		zoomfactor = zf;
		xpos = xp;
		ypos = yp;
	}

	/**
	 * Draws the DSM grid map
	 */
	protected void Draw() {
		/*
		AffineTransform tx = new AffineTransform();
		tx.translate(translateX, translateY);
		tx.scale(scale, scale);
		transform = tx;
*/
		Graphics gc = getGraphics();
		Color previousColor = gc.getColor();

		Rectangle r = getInsetedBounds();

		if (gc instanceof Graphics2D) {
			Graphics2D g2d = (Graphics2D) gc;
			g2d.setTransform(transform);
			g2d.setClip(r.x, r.y, r.width, r.height);
			// FIXME: finally set transform back to identity
		}
		// gc.setColor(getAttributes()._backgroundColor);

		// gc.fillRect(r.x, r.y, r.width, r.height);
		if (backgroundImage != null) {
			if (gc instanceof Graphics2D) {
				Graphics2D g2d = (Graphics2D) gc;
				AffineTransform tr = new AffineTransform();
				/*
				 * For delta_map.png tr.translate(-5,-24); tr.scale(1.085,
				 * 1.025); tr.rotate(-0.011, 15, 100);
				 */
				g2d.setTransform(tr);
				g2d.drawImage(backgroundImage, r.x, r.y, r.width, r.height,
						null);
				g2d.setTransform(transform);
			} else {
				gc.drawImage(backgroundImage, r.x - 20, r.y - 20,
						r.width + 110, r.height + 15, null);
			}
		}
		gc.setColor(channelColor);

		for (int i = 0; i < _net.getNumberOfLinks(); i++) {
			Link link = _net.getLink(i);
			if (link != null) {
				if (link instanceof Channel) {
					drawChannel(gc, (Channel) link);
				} else if (link instanceof Reservoir) {
					drawReservoir(gc, (Reservoir) link);
				}
			}
		}
		gc.setColor(previousColor);

	}

	protected void showTransformDialog() {
		JPanel p = new JPanel();
		final JTextField zoomField = new JTextField("1.5");
		zoomField.addActionListener(new ActionListener() {

			@Override
			public void actionPerformed(ActionEvent e) {
				double scale = Double.parseDouble(zoomField.getText());
				transform.scale(scale, scale);
			}

		});
		p.add(zoomField);
		JDialog d = new JDialog();
		d.add(p);
		d.pack();
		d.setModal(false);
		d.setVisible(true);
	}

	/**
   * 
   */
	protected void drawChannel(Graphics g, Channel channel) {
		if (channel != null) {
			Node upnode, downnode;
			upnode = channel.getNode(Channel.UPNODE_INDEX);
			downnode = channel.getNode(Channel.DOWNNODE_INDEX);

			try {
				drawThickLine(g, grid_xS.scaleToUC(upnode.getX()), grid_yS
						.scaleToUC(upnode.getY()), grid_xS.scaleToUC(downnode
						.getX()), grid_yS.scaleToUC(downnode.getY()),
						channel_width);
			} catch (Exception e) {
				System.out.println("Problem with channel " + channel);
			}
		}
	}

	/**
   * 
   */
	protected void drawReservoir(Graphics g, Reservoir res) {
		if (res != null) {
			int numberOfNodes = res.getNumberOfNodes();
			int[] xc = new int[numberOfNodes];
			int[] yc = new int[numberOfNodes];

			for (int i = 0; i < numberOfNodes; i++) {
				Node node = res.getNode(i);
				xc[i] = grid_xS.scaleToUC(node.getX());
				yc[i] = grid_yS.scaleToUC(node.getY());
			}

			g.fillPolygon(xc, yc, numberOfNodes);
		}
	}

	// /**
	// * The update method is called when the animation frame needs to be
	// updated.
	// * Each element is responsible for drawing itself within the bounds given.
	// */
	// public void update(Observable o, Object arg){
	// draw();
	// }
	/**
	 * Sets the channel color
	 */
	public void setChannelColor(Color c) {
		channelColor = c;
	}

	/**
	 * gets the channel color
	 */
	public Color getChannelColor() {
		return channelColor;
	}

	private static int[] xtl = new int[4], ytl = new int[4];

	/**
	 * Simulates drawing of different thickness lines by using filled polygon.
	 * 
	 * @param g
	 *            Graphics on which to draw
	 * @param x1
	 *            The starting x co-ordinate of line
	 * @param y1
	 *            The starting y co-ordinate of line
	 * @param x2
	 *            The ending x co-ordinate of line
	 * @param y2
	 *            The ending y co-ordinate of line
	 * @param t
	 *            The thickness of the line in pixels
	 */
	public final void drawThickLine(Graphics g, int x1, int y1, int x2, int y2,
			double t) {
		double theta;
		if (Math.abs(x2 - x1) > 0.01)
			theta = Math.atan((y2 - y1) / (x2 - x1));
		else
			theta = Math.PI / 2;
		double ct = Math.cos(theta), st = Math.sin(theta);
		// Polygon filledPolygon = new Polygon();
		// filledPolygon.addPoint((int)(x1-t/2*st),(int) (y1+t/2*ct));
		// filledPolygon.addPoint((int)(x1+t/2*st),(int)(y1-t/2*ct));
		// filledPolygon.addPoint((int)(x2+t/2*st),(int)(y2-t/2*ct));
		// filledPolygon.addPoint((int)(x2-t/2*st),(int)(y2+t/2*ct));
		// g.fillPolygon(filledPolygon);
		xtl[0] = (int) (x1 - t / 2 * st);
		ytl[0] = (int) (y1 + t / 2 * ct);
		xtl[1] = (int) (x1 + t / 2 * st);
		ytl[1] = (int) (y1 - t / 2 * ct);
		xtl[2] = (int) (x2 + t / 2 * st);
		ytl[2] = (int) (y2 - t / 2 * ct);
		xtl[3] = (int) (x2 - t / 2 * st);
		ytl[3] = (int) (y2 + t / 2 * ct);

		g.fillPolygon(xtl, ytl, 4);
	}

	/**
   *
   */
	public Dimension getPreferredSize() {
		// return new Dimension( 400, 600);
		return new Dimension(333, 500);
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
	public Network getNetwork() {
		return _net;
	}

	/**
   *
   */
	public Scale getXScale() {
		return xS;
	}

	/**
   *
   */
	public Scale getYScale() {
		return yS;
	}

	/**
   *
   */
	public Scale getGridXScale() {
		return grid_xS;
	}

	/**
   *
   */
	public Scale getGridYScale() {
		return grid_yS;
	}

	/**
   *
   */
	protected Network _net;
	/**
	 * channel color
	 */
	protected Color channelColor = Color.blue;
	/**
	 * x scaling
	 */
	protected Scale xS;
	/**
	 * y scaling
	 */
	protected Scale yS;

	protected Scale grid_xS;
	protected Scale grid_yS;

	protected float zoomfactor;
	protected int xpos;
	protected int ypos;

	/**
	 * The width of the channel in pixels
	 */
	private int channel_width = 4;

	public double translateX = 0;
	public double translateY = 0;
	public double scale = 1;

	public AffineTransform getTransform() {
		return transform;
	}

}
