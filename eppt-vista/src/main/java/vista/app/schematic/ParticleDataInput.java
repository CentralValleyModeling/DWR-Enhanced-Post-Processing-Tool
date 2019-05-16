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

import java.io.EOFException;
import java.io.IOException;

import vista.app.AsciiFileInput;
import vista.app.BinaryFileInput;
import vista.app.F77BinaryFileInput;
import vista.app.GenericFileInput;

/**
 * This class reads the particle animation data from a URL into an array of
 * Particle.
 *
 * @author Nicky Sandhu
 * @version $Revision: 1.4 $,$Date: 2000/01/04 22:00:56 $
 * @see Particle
 */
public class ParticleDataInput
{
	/**
	 *
	 */
	private static ParticleDataStructure[] dataS = null;
	/**
	 *
	 */
	private static String endOfFile = "EOF";
	/**
	 *
	 */
	protected GenericFileInput input;
	/**
	 *
	 */
	private boolean atEndOfFile;
	/**
	 *
	 */
	private boolean dateRead;
	/**
	 *
	 */
	private boolean partRead;
	/**
	 *
	 */
	private String date;
	/**
	 *
	 */
	private int nparticles;

	/**
	 *
	 */
	public void initializeData(String filename) throws IOException
	{
		if(filename.endsWith(".javabin"))
		{
			input = new BinaryFileInput();
		}
		else if(filename.endsWith(".bin"))
		{
			input = new BinaryFileInput();// input = new F77BinaryFileInput();
		}
		else
		{
			input = new AsciiFileInput();
		}
		input.initializeInput(filename);
		partRead = false;
		dateRead = false;
	}

	public int getInitialNParticles() throws IOException
	{
		if(!dateRead)
		{
			date = readDate();
		}
		if(!partRead)
		{
			nparticles = readNParticles();
		}
		return nparticles;
	}

	/**
	 *
	 */
	public void updateParticles(ParticleData data) throws IOException
	{

		if(input instanceof F77BinaryFileInput)
		{
			date = input.readString();
			nparticles = input.readInt();

			data.setDate(date);
			Particle[] particles = data.getParticles();

			if(nparticles != particles.length)
			{
				throw new IOException(" # of particles incorrect ");
			}

			if(dataS == null)
			{
				dataS = new ParticleDataStructure[nparticles];
				for(int i = 0; i < dataS.length; i++)
				{
					dataS[i] = new ParticleDataStructure();
				}
			}

			dataS = (ParticleDataStructure[]) input.readObject(dataS);

			for(int i = 0; i < particles.length; i++)
			{
				particles[i].setData((int) dataS[i].p, (int) dataS[i].c,
						(float) dataS[i].x, (float) dataS[i].y,
						(float) dataS[i].z);
			}

		}
		else
		{
			// if (input instanceof AsciiFileInput){
			// date = input.readString();
			// if (date.equals(endOfFile)){
			// fileStatus = true;
			// throw new EOFException(" End of Ascii File ");
			// }
			// nparticles = (int) input.readShort();
			// }
			// else {
			// date = input.readString();
			// if (date.equals(endOfFile)) {
			// fileStatus = true;
			// throw new EOFException(" End of Binary File ");
			// }
			// date = date + " " + input.readShort();
			// nparticles = (int) input.readShort();
			// }
			if(!dateRead)
			{
				date = readDate();
			}

			if(date.equals(endOfFile))
			{
				atEndOfFile = true;
				throw new EOFException(" End of File ");
			}
			if(!partRead)
			{
				nparticles = readNParticles();
			}
			data.setDate(date);
			Particle[] particles = data.getParticles();
			if(nparticles != particles.length)
			{
				throw new IOException(" # of particles incorrect ");
			}

			for(int i = 0; i < particles.length; i++)
			{
				particles[i].setData((int) input.readShort(), (int) input
						.readShort(), (float) input.readShort(), (float) input
						.readShort(), (float) input.readShort());
				input.readShort();
			}
			partRead = false;
			dateRead = false;
		}
	}

	protected int readNParticles() throws IOException
	{
		partRead = true;
		return (int) input.readShort();
	}

	protected String readDate() throws IOException
	{
		String date;
		if(input instanceof AsciiFileInput)
		{
			date = input.readString();
		}
		else
		{
			date = input.readString();
			if(date.equals(endOfFile))
			{
			}
			else
			{
				date = date + " " + input.readShort();
			}
		}
		dateRead = true;
		return date;
	}

	/**
	 *
	 */
	public boolean isAtEndOfFile()
	{
		return atEndOfFile;
	}
}
