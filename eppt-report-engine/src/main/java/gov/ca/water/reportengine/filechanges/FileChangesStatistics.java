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

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

public class FileChangesStatistics
{

    private final Set<String> _filesDeletedFromBase;
    private final Set<String> _recordsOnlyInAlt;
    private final Set<String> _recordsOnlyInBase;
    private final Set<String> _changedFiles;
    private final Map<CodeChangesSubType, Set<String>> _codeChangesSubtypeToModifiedFiles;

    private FileChangesStatistics(Set<String> recordsOnlyInBase, Set<String> recordsOnlyInAlt, Set<String> filesUpdates, Set<String> filesDeletedFromBase,
                                  Map<CodeChangesSubType, Set<String>> codeChangesSubtypeToModifiedFiles)
    {
        _recordsOnlyInAlt = recordsOnlyInAlt;
        _recordsOnlyInBase = recordsOnlyInBase;
        _changedFiles = filesUpdates;
        _filesDeletedFromBase = filesDeletedFromBase;
        _codeChangesSubtypeToModifiedFiles = codeChangesSubtypeToModifiedFiles;
    }

    public Set<String> getChangedFiles()
    {
        return _changedFiles;
    }
    public Set<String> getFilesDeletedFromBase()
    {
        return _filesDeletedFromBase;
    }
    public Set<String> getRecordsOnlyInAlt()
    {
        return _recordsOnlyInAlt;
    }

    public Set<String> getRecordsOnlyInBase()
    {
        return _recordsOnlyInBase;
    }

    public Set<String> getModifiedFilesForSubtype(CodeChangesSubType subtype)
    {
        Set<String> retval = new HashSet<>();
        if(_codeChangesSubtypeToModifiedFiles.containsKey(subtype))
        {
            retval = _codeChangesSubtypeToModifiedFiles.get(subtype);
        }
        return retval;
    }

    public static class Builder
    {
        private Set<String> _recordsOnlyInAlt;
        private Set<String> _recordsOnlyInBase;
        private Set<String> _changes;
        private Set<String> _filesDeletedFromBase;
        private Map<CodeChangesSubType, Set<String>> _codeChangesSubtypeToModifiedFiles;


        public Builder()
        {
            _recordsOnlyInAlt = new HashSet<>();
            _recordsOnlyInBase = new HashSet<>();
            _changes = new HashSet<>();
            _filesDeletedFromBase = new HashSet<>();
            _codeChangesSubtypeToModifiedFiles = new HashMap<>();
        }

        public FileChangesStatistics build()
        {
            return new FileChangesStatistics(_recordsOnlyInBase, _recordsOnlyInAlt, _changes, _filesDeletedFromBase, _codeChangesSubtypeToModifiedFiles);
        }

        public Builder withSubtypeModifiedFiles(Map<CodeChangesSubType, Set<String>> map)
        {
            _codeChangesSubtypeToModifiedFiles.putAll(map);
            return this;
        }

        public Builder withFilesDeletedFromBase(Set<String> deletedFromBase)
        {
            _filesDeletedFromBase = deletedFromBase;
            return this;
        }

        public Builder withRecordsOnlyInBase(Set<String> recsOnlyInBase)
        {
            _recordsOnlyInBase = recsOnlyInBase;
            return this;
        }

        public Builder withRecordsOnlyInAlt(Set<String> recsOnlyInAlt)
        {
            _recordsOnlyInAlt = recsOnlyInAlt;
            return this;
        }

        public Builder withFileChanges(Set<String> fileChanges)
        {
            _changes = fileChanges;
            return this;
        }
    }

}
