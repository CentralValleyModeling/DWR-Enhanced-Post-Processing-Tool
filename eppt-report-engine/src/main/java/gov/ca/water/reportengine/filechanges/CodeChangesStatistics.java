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

package gov.ca.water.reportengine.filechanges;

import java.nio.file.Path;
import java.util.Set;

public class CodeChangesStatistics
{
	private final Set<Path> _filesDeletedFromBase;
	private final Set<Path> _filesAddedToAlt;
	private final Set<Path> _codeChangesModifiedFiles;

	CodeChangesStatistics(Set<Path> filesDeletedFromBase, Set<Path> filesAddedToAlt, Set<Path> codeChangesFilesModified)
	{


		_filesDeletedFromBase = filesDeletedFromBase;
		_filesAddedToAlt = filesAddedToAlt;
		_codeChangesModifiedFiles = codeChangesFilesModified;
	}


	public Set<Path> getCodeChangesModifiedFiles()
	{
		return _codeChangesModifiedFiles;
	}

	public Set<Path> getFilesDeletedFromBase()
	{
		return _filesDeletedFromBase;
	}

	public Set<Path> getFilesAddedToAlt()
	{
		return _filesAddedToAlt;
	}

}
