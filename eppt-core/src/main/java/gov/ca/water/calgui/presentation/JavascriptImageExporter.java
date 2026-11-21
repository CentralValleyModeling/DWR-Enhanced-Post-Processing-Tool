/*
 * Enhanced Post Processing Tool (EPPT) Copyright (c) 2020.
 *
 * EPPT is copyrighted by the State of California, Department of Water Resources. It is licensed
 * under the GNU General Public License, version 2. This means it can be
 * copied, distributed, and modified freely, but you may not restrict others
 * in their ability to copy, distribute, and modify it. See the license below
 * for more details.
 *
 * GNU General Public License
 */

package gov.ca.water.calgui.presentation;

import java.awt.Desktop;
import java.awt.Frame;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Base64;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.swing.*;

import gov.ca.water.calgui.bo.SimpleFileFilter;
import gov.ca.water.calgui.constant.EpptPreferences;
import gov.ca.water.calgui.techservice.impl.DialogSvcImpl;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 03-13-2020
 */
public class JavascriptImageExporter
{
	private static final Logger LOGGER = Logger.getLogger(JavascriptImageExporter.class.getName());

	/**
	 * Receives the page's console output, see {@code connectJava} in eppt_plotly.js.
	 */
	public void log(String level, String message)
	{
		Level logLevel = "error".equals(level) ? Level.SEVERE : "warn".equals(level) ? Level.WARNING : Level.FINE;
		LOGGER.log(logLevel, "Plotly Output: {0}", message);
	}

	/**
	 * Saves a file the page tried to download, see the download hook in eppt_plotly.js.
	 */
	public void saveDownload(String filename, String dataUrl)
	{
		int comma = dataUrl.indexOf(',');
		if(!dataUrl.startsWith("data:") || comma < 0 || !dataUrl.substring(0, comma).endsWith(";base64"))
		{
			LOGGER.log(Level.SEVERE, "Unable to save download {0}, unexpected data url", filename);
			return;
		}
		byte[] bytes = Base64.getDecoder().decode(dataUrl.substring(comma + 1));
		SwingUtilities.invokeLater(() -> promptAndSave(filename, bytes));
	}

	private void promptAndSave(String filename, byte[] bytes)
	{
		Path defaultPath = EpptPreferences.getLastProjectConfiguration().getParent().resolve(filename);
		JFileChooser fileChooser = new JFileChooser(defaultPath.getParent().toFile());
		int dot = filename.lastIndexOf('.');
		String extension = dot >= 0 ? filename.substring(dot) : "";
		if(!extension.isEmpty())
		{
			fileChooser.setFileFilter(new SimpleFileFilter(extension.substring(1), "Export " + extension.substring(1) + " file"));
		}
		fileChooser.setMultiSelectionEnabled(false);
		fileChooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
		fileChooser.setSelectedFile(defaultPath.toFile());
		if(JFileChooser.APPROVE_OPTION == fileChooser.showSaveDialog(Frame.getFrames()[0]))
		{
			Path outputPath = fileChooser.getSelectedFile().toPath();
			if(!outputPath.getFileName().toString().toLowerCase().endsWith(extension.toLowerCase()))
			{
				outputPath = outputPath.resolveSibling(outputPath.getFileName() + extension);
			}
			try
			{
				Files.write(outputPath, bytes);
				Desktop.getDesktop().open(outputPath.toFile());
			}
			catch(IOException | RuntimeException e)
			{
				LOGGER.log(Level.SEVERE, "Error saving: " + outputPath, e);
				DialogSvcImpl.getDialogSvcInstance().getOK("Error saving: " + outputPath + "\n" + e.getMessage(),
						JOptionPane.WARNING_MESSAGE);
			}
		}
	}
}
