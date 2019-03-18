/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */

package gov.ca.water.calgui.tech_service.impl;

import java.io.BufferedWriter;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.List;
import java.util.function.Predicate;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;

import gov.ca.water.calgui.bo.CalLiteGUIException;
import gov.ca.water.calgui.bo.CalLiteGUIExceptionFatal;
import gov.ca.water.calgui.constant.Constant;
import gov.ca.water.calgui.tech_service.IFileSystemSvc;
import org.apache.commons.io.FilenameUtils;
import org.apache.log4j.Logger;
import org.w3c.dom.Document;

/**
 * This is the class for File Handling like reading and saving etc.
 *
 * @author Mohan
 */
public class FileSystemSvcImpl implements IFileSystemSvc
{
	private static final Logger LOG = Logger.getLogger(FileSystemSvcImpl.class.getName());

	@Override
	public List<String> getFileData(Path fileName, boolean isRequired) throws CalLiteGUIException
	{
		Path p = Paths.get(System.getProperty("user.dir")).resolve(fileName);
		List<String> list = null;
		if(Files.isExecutable(p))
		{
			try(Stream<String> stream = Files.lines(p))
			{
				list = stream.collect(Collectors.toList());
			}
			catch(IOException ex)
			{
				LOG.error(ex.getMessage(), ex);
				if(isRequired)
				{
					throw new CalLiteGUIExceptionFatal("File data is corrupted : " + fileName, ex);
				}
				else
				{
					throw new CalLiteGUIException("File data is corrupted : " + fileName, ex);
				}
			}
		}
		else
		{
			LOG.error("File missing : " + fileName);
			FileNotFoundException ex = new FileNotFoundException("File missing : " + fileName);
			if(isRequired)
			{
				throw new CalLiteGUIExceptionFatal("File missing : " + fileName, ex);
			}
			else
			{
				throw new CalLiteGUIException("File missing : " + fileName, ex);

			}
		}
		return list;
	}

	@Override
	public List<String> getFileData(Path fileName, boolean isRequired, Predicate<String> selector)
			throws CalLiteGUIException
	{
		return this.getFileData(fileName, isRequired).stream().filter(selector).collect(Collectors.toList());
	}

	@Override
	public List<String> getFileDataForTables(Path fileName) throws CalLiteGUIException
	{
		List<String> list = this.getFileData(fileName, false);
		return list.stream().filter((obj) -> {
			return (!obj.startsWith(Constant.EXCLAMATION))
					|| obj.startsWith(Constant.EXCLAMATION + "	" + Constant.HEADERS);
		}).map((obj) -> {
			if(obj.startsWith(Constant.EXCLAMATION))
			{
				return obj.substring(2);
			}
			return obj;
		}).collect(Collectors.toList());
	}

	@Override
	public Document getXMLDocument() throws CalLiteGUIException
	{
		DocumentBuilder dBuilder;
		try
		{
			dBuilder = DocumentBuilderFactory.newInstance().newDocumentBuilder();
			InputStream fi = new FileInputStream(Constant.GUI_XML_FILENAME);
			return dBuilder.parse(fi);
		}
		catch(Exception ex)
		{
			LOG.error("The SwiXml is not build." + Constant.NEW_LINE + ex.getMessage(), ex);
			throw new CalLiteGUIExceptionFatal("The SwiXml is not build." + Constant.NEW_LINE + ex.getMessage(), ex);
		}
	}

	@Override
	public void saveDataToFile(Path fileName, String data) throws CalLiteGUIException
	{
		if(!fileName.toString().isEmpty())
		{
			try(BufferedWriter writer = Files.newBufferedWriter(fileName))
			{
				writer.write(data);
			}
			catch(IOException ex)
			{
				throw new CalLiteGUIException("Can't save the data to file  : " + fileName, ex);
			}
		}
	}

	@Override
	public String getTheLookupFromTheFullFileName(String fullName)
	{
		String fileName = Paths.get(fullName).getFileName().toString();
		return getLookupFromTheFileName(Paths.get(fileName));
	}

	@Override
	public String getLookupFromTheFileName(Path fileName)
	{
		String[] arr = FilenameUtils.removeExtension(fileName.getFileName().toString()).split(Constant.UNDER_SCORE);
		String lookupValue = arr[arr.length - 1];
		if(lookupValue.equalsIgnoreCase("swp") || lookupValue.equalsIgnoreCase("sys"))
		{
			return Constant.USER_DEFINED;
		}
		return lookupValue;
	}
}
