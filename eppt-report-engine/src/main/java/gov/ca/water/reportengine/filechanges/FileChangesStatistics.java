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
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

public class FileChangesStatistics
{

    private final Set<Path> _filesDeletedFromBase;
    private final Set<Path> _filesAddedToAlt;
    private final Set<Path> _codeChangesModifiedFiles;

    private final Set<String> _recordsOnlyInAlt;
    private final Set<String> _recordsOnlyInBase;
    private final Set<String> _changedFiles;

    private FileChangesStatistics(Set<String> recordsOnlyInBase, Set<String> recordsOnlyInAlt, Set<String> filesUpdates,
                                  Set<Path> filesDeletedFromBase, Set<Path> filesAddedToAlt, Set<Path> codeChangesFilesModified)
    {
        _recordsOnlyInAlt = recordsOnlyInAlt;
        _recordsOnlyInBase = recordsOnlyInBase;
        _changedFiles = filesUpdates;

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


    public Set<String> getChangedFiles()
    {
        return _changedFiles;
    }

    public Set<String> getRecordsOnlyInAlt()
    {
        return _recordsOnlyInAlt;
    }

    public Set<String> getRecordsOnlyInBase()
    {
        return _recordsOnlyInBase;
    }





    public static class Builder
    {
        //assumption changes
        private final Set<String> _recordsOnlyInAlt;
        private final Set<String> _recordsOnlyInBase;
        private final Set<String> _changes;

        //for code changes section of eppt report
        private final Set<Path> _filesAddedToAlt;
        private final Set<Path> _filesDeletedFromBase;
        private final Set<Path> _codeChangesModifiedFiles;


        public Builder()
        {
            _recordsOnlyInAlt = new HashSet<>();
            _recordsOnlyInBase = new HashSet<>();
            _changes = new HashSet<>();

            _filesDeletedFromBase = new HashSet<>();
            _filesAddedToAlt = new HashSet<>();
            _codeChangesModifiedFiles = new HashSet<>();
        }

        public FileChangesStatistics build()
        {
            return new FileChangesStatistics(_recordsOnlyInBase, _recordsOnlyInAlt, _changes, _filesDeletedFromBase, _filesAddedToAlt, _codeChangesModifiedFiles);
        }

        public Builder withCodeChangesModifiedFiles(Set<Path> modifiedPaths)
        {
            _codeChangesModifiedFiles.addAll(modifiedPaths);
            return this;
        }

        public Builder withFilesDeletedFromBase(Set<Path> deletedFromBase)
        {
            _filesDeletedFromBase.addAll(deletedFromBase);
            return this;
        }

        public Builder withFilesAddedToAlt(Set<Path> addedToAlt)
        {
            _filesAddedToAlt.addAll(addedToAlt);
            return this;
        }



        public Builder withRecordsOnlyInAlt(Set<String> onlyInAlt)
        {
            _recordsOnlyInAlt.addAll(onlyInAlt);
            return this;
        }

        public Builder withRecordsOnlyInBase(Set<String> onlyInBase)
        {
            _recordsOnlyInBase.addAll(onlyInBase);
            return this;
        }

        public Builder withFileChanges(Set<String> fileChanges)
        {
            _changes.addAll(fileChanges);
            return this;
        }
    }

}
