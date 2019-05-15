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

/**
   *
   */
public class ParticleData {
	/**
   *
   */
	public ParticleData(int nparticles) {
		if (particles == null) {
			particles = new Particle[nparticles];
			for (int i = 0; i < particles.length; i++)
				particles[i] = new Particle(0, 0, -1.0f, -1.0f, -1.0f);
		}
	}

	/**
   *
   */
	public Particle[] getParticles() {
		return particles;
	}

	/**
   *
   */
	public void setDate(String d) {
		date = d;
	}

	/**
   * 
   */
	public String getDate() {
		return date;
	}

	/**
   *
   */
	Particle[] particles;
	/**
   *
   */
	String date;
	/**
   *
   */
}
