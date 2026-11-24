package gov.ca.water.calgui.constant;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.prefs.Preferences;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

class EpptPreferencesTest
{
	private static final String KEY = "last_scenario_configuration";
	private final Preferences _node = Preferences.userRoot().node("DWR").node("EPPT").node("eppt_home");
	private String _original;

	@BeforeEach
	void saveOriginal()
	{
		_original = _node.get(KEY, null);
	}

	@AfterEach
	void restoreOriginal()
	{
		if(_original == null)
		{
			_node.remove(KEY);
		}
		else
		{
			_node.put(KEY, _original);
		}
	}

	@Test
	void noProjectWhenPreferenceUnset()
	{
		_node.remove(KEY);
		assertFalse(EpptPreferences.hasProjectConfiguration());
		assertThrows(IllegalStateException.class, EpptPreferences::getLastProjectDirectory);
	}

	@Test
	void noProjectWhenFileMissing(@TempDir Path dir)
	{
		EpptPreferences.setLastProjectConfiguration(dir.resolve("missing.eppt"));
		assertFalse(EpptPreferences.hasProjectConfiguration());
		assertThrows(IllegalStateException.class, EpptPreferences::getLastProjectDirectory);
	}

	@Test
	void projectDirectoryIsParentOfExistingFile(@TempDir Path dir) throws IOException
	{
		Path project = Files.createFile(dir.resolve("test.eppt"));
		EpptPreferences.setLastProjectConfiguration(project);
		assertTrue(EpptPreferences.hasProjectConfiguration());
		assertEquals(project.toAbsolutePath().getParent(), EpptPreferences.getLastProjectDirectory());
	}
}
