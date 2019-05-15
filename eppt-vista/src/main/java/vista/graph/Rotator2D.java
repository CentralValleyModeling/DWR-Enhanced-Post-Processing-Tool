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

import java.awt.Graphics;
import java.awt.Graphics2D;

/**
 * This class
 * 
 */
public class Rotator2D {
	public static boolean DEBUG = false;

	/**
    *
    */
	public static void rotateVertical(Graphics gr, int x, int y) {
		if (gr instanceof Graphics2D) {
			Graphics2D g2d = (Graphics2D) gr;
			if (DEBUG)
				System.out.println("rotation -90 for x: " + x + " y: " + y);
			g2d.rotate(Math.toRadians(-90), x, y);
		} else {
			System.err.println("Attempt to rotate non jdk2 item");
			throw new RuntimeException("Attempt to rotate non jdk2 item");
		}
	}

	/**
    *
    */
	public static void rotateHorizontal(Graphics gr, int x, int y) {
		if (gr instanceof Graphics2D) {
			Graphics2D g2d = (Graphics2D) gr;
			if (DEBUG)
				System.out.println("rotation 90 for x: " + x + " y: " + y);
			g2d.rotate(Math.toRadians(90), x, y);
		}
	}
}
