/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */

package vista.graph;

import java.awt.Image;
import java.awt.image.ColorModel;
import java.awt.image.ImageConsumer;
import java.awt.image.ImageProducer;
import java.io.IOException;
import java.io.OutputStream;
import java.util.Hashtable;

/// Abstract class for writing out an image.
// <P>
// A framework for classes that encode and write out an image in
// a particular file format.
// <P>
// This provides a simplified rendition of the ImageConsumer interface.
// It always delivers the pixels as ints in the RGBdefault color model.
// If you want more flexibility you can always implement ImageProducer
// directly.
// <P>
// <A HREF="/resources/classes/Acme/JPM/Encoders/ImageEncoder.java">Fetch the software.</A><BR>
// <A HREF="/resources/classes/Acme.tar.Z">Fetch the entire Acme package.</A>
// <P>
// @see GifEncoder
// @see PpmEncoder
// @see Acme.JPM.Decoders.ImageDecoder

public abstract class ImageEncoder implements ImageConsumer {

	protected OutputStream out;

	private ImageProducer producer;
	private int width = -1;
	private int height = -1;
	private int hintflags = 0;
	private boolean started = false;
	private boolean encoding;
	private IOException iox;
	private static final ColorModel rgbModel = ColorModel.getRGBdefault();
	private Hashtable props = null;

	// / Constructor.
	// @param img The image to encode.
	// @param out The stream to write the bytes to.
	public ImageEncoder(Image img, OutputStream out) throws IOException {
		this(img.getSource(), out);
	}

	// / Constructor.
	// @param producer The ImageProducer to encode.
	// @param out The stream to write the bytes to.
	public ImageEncoder(ImageProducer producer, OutputStream out)
			throws IOException {
		this.producer = producer;
		this.out = out;
	}

	// Methods that subclasses implement.

	// / Subclasses implement this to initialize an encoding.
	abstract void encodeStart(int w, int h) throws IOException;

	// / Subclasses implement this to actually write out some bits. They
	// are guaranteed to be delivered in top-down-left-right order.
	// One int per pixel, index is row * scansize + off + col,
	// RGBdefault (AARRGGBB) color model.
	abstract void encodePixels(int x, int y, int w, int h, int[] rgbPixels,
			int off, int scansize) throws IOException;

	// / Subclasses implement this to finish an encoding.
	abstract void encodeDone() throws IOException;

	// Our own methods.

	// / Call this after initialization to get things going.
	public synchronized void encode() throws IOException {
		encoding = true;
		iox = null;
		producer.startProduction(this);
		while (encoding)
			try {
				wait();
			} catch (InterruptedException e) {
			}
		if (iox != null)
			throw iox;
	}

	private void encodePixelsWrapper(int x, int y, int w, int h,
			int[] rgbPixels, int off, int scansize) throws IOException {
		if (!started) {
			started = true;
			encodeStart(width, height);
			if ((hintflags & TOPDOWNLEFTRIGHT) == 0) {
				producer.requestTopDownLeftRightResend(this);
				return;
			}
		}
		encodePixels(x, y, w, h, rgbPixels, off, scansize);
	}

	private synchronized void done() {
		encoding = false;
		notifyAll();
	}

	// Methods from ImageConsumer.

	public void setDimensions(int width, int height) {
		this.width = width;
		this.height = height;
	}

	public void setProperties(Hashtable props) {
		this.props = props;
	}

	public void setColorModel(ColorModel model) {
		// Ignore.
	}

	public void setHints(int hintflags) {
		this.hintflags = hintflags;
	}

	public void setPixels(int x, int y, int w, int h, ColorModel model,
			byte[] pixels, int off, int scansize) {
		int[] rgbPixels = new int[w];
		for (int row = 0; row < h; ++row) {
			int rowOff = off + row * scansize;
			for (int col = 0; col < w; ++col)
				rgbPixels[col] = model.getRGB(pixels[rowOff + col] & 0xff);
			try {
				encodePixelsWrapper(x, y + row, w, 1, rgbPixels, 0, w);
			} catch (IOException e) {
				iox = e;
				done();
				return;
			}
		}
	}

	public void setPixels(int x, int y, int w, int h, ColorModel model,
			int[] pixels, int off, int scansize) {
		if (model == rgbModel) {
			try {
				encodePixelsWrapper(x, y, w, h, pixels, off, scansize);
			} catch (IOException e) {
				iox = e;
				done();
				return;
			}
		} else {
			int[] rgbPixels = new int[w];
			for (int row = 0; row < h; ++row) {
				int rowOff = off + row * scansize;
				for (int col = 0; col < w; ++col)
					rgbPixels[col] = model.getRGB(pixels[rowOff + col]);
				try {
					encodePixelsWrapper(x, y + row, w, 1, rgbPixels, 0, w);
				} catch (IOException e) {
					iox = e;
					done();
					return;
				}
			}
		}
	}

	public void imageComplete(int status) {
		producer.removeConsumer(this);
		if (status == ImageConsumer.IMAGEABORTED)
			iox = new IOException("image aborted");
		else {
			try {
				encodeDone();
			} catch (IOException e) {
				iox = e;
			}
		}
		done();
	}

}
