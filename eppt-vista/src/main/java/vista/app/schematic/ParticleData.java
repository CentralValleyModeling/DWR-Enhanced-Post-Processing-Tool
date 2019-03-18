/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
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
