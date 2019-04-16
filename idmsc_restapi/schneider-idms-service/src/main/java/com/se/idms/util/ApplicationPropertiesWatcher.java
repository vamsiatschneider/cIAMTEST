package com.se.idms.util;

import static java.nio.file.StandardWatchEventKinds.ENTRY_CREATE;
import static java.nio.file.StandardWatchEventKinds.ENTRY_DELETE;
import static java.nio.file.StandardWatchEventKinds.ENTRY_MODIFY;
import static java.nio.file.StandardWatchEventKinds.OVERFLOW;

import java.io.IOException;
import java.nio.file.FileSystems;
import java.nio.file.FileVisitResult;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.SimpleFileVisitor;
import java.nio.file.StandardWatchEventKinds;
import java.nio.file.WatchEvent;
import java.nio.file.WatchKey;
import java.nio.file.WatchService;
import java.nio.file.attribute.BasicFileAttributes;
import java.util.HashMap;
import java.util.Map;
import java.util.Properties;

import javax.inject.Inject;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Service;

import com.se.idms.cache.api.CacheBuilder;
import com.se.idms.cache.api.CacheManagerProvider;
import com.se.idms.cache.api.CacheManagerProviderImpl;

@Service("applicationPropertiesWatcher")
public class ApplicationPropertiesWatcher implements Runnable {

	private String configFileName = null;
	private final WatchService watcher;
	private final Map<WatchKey, Path> keys;
	private boolean trace = false;

	@Inject
	PropertyFileAutoRefresh propertyFileAutoRefresh;

	/**
	 * Logger instance.
	 */
	private static final Logger LOGGER = LoggerFactory.getLogger(ApplicationPropertiesWatcher.class);

	@SuppressWarnings("unchecked")
	static <T> WatchEvent<T> cast(WatchEvent<?> event) {
		return (WatchEvent<T>) event;
	}

	public ApplicationPropertiesWatcher(final String filePath) throws IOException {
		LOGGER.info("ApplicationPropertiesWatcher constr called!!"+ filePath);
		Path dir = Paths.get(filePath);// full absolute file path
		this.watcher = FileSystems.getDefault().newWatchService();
		this.keys = new HashMap<WatchKey, Path>();
		registerAll(dir);
		Runtime.getRuntime().addShutdownHook(new Thread() {
			public void run() {
				try {
					watcher.close();
					LOGGER.info("shutdown hook called!!!!");
				} catch (IOException e) {
					e.printStackTrace();
				}
			}
		});
		// enable trace after initial registration
		this.trace = true;
	}

	public void run() {
		try {
			// register(this.fullFilePath);
			processEvents();
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	/**
	 * Process all events for keys queued to the watcher
	 */
	@SuppressWarnings("rawtypes")
	void processEvents() {
		for (;;) {
			// wait for key to be signalled
			WatchKey key;
			try {
				key = watcher.take();
			} catch (InterruptedException x) {
				return;
			}
			// key=watcher.poll();
			Path dir = keys.get(key);
			if (dir == null) {
				LOGGER.error("WatchKey not recognized!!");
				continue;
			}

			for (WatchEvent<?> event : key.pollEvents()) {
				WatchEvent.Kind kind = event.kind();
				// TBD - provide example of how OVERFLOW event is handled
				if (kind == OVERFLOW) {
					continue;
				}
				// Context for directory entry event is the file name of entry
				WatchEvent<Path> ev = cast(event);
				Path name = ev.context(); // file name
				LOGGER.info("Process events file name:"+name);
				Path child = dir.resolve(name);// absolute path
				LOGGER.info("Process events toAbsolutePath():"+child.toAbsolutePath().toString());
				try{
				if(name.toString().startsWith("IDMS") & name.toString().endsWith(".properties")){
					LOGGER.info("picklist IDMS data modified !!:"+ event.kind().name()+"::"+ child);
					CacheManagerProvider cacheManagerProvider = new CacheManagerProviderImpl();
					CacheBuilder cacheBuilder = new CacheBuilder(cacheManagerProvider);
					cacheBuilder.refreshCache(child.toAbsolutePath().toString());
				}
				else if(name.toString().endsWith(".properties")) {
					// print out event
					//System.out.printf("%s: %s\n", event.kind().name()+ child);
					LOGGER.info("Application properties path and file name:"+ event.kind().name()+"::"+ child);
					configurationChanged(child.toAbsolutePath().toString());
				}
				}
				catch(Exception exception){
					LOGGER.error("Executing while processing events :: -> " + exception.getMessage(),exception);
				}

			}
			// reset key and remove from set if directory no longer accessible
			boolean valid = key.reset();
			if (!valid) {
				keys.remove(key);

				// all directories are inaccessible
				if (keys.isEmpty()) {
					break;
				}
			}
		}
	}

	@SuppressWarnings("unused")
	private void register(final String file) throws IOException {
		final int lastIndex = file.lastIndexOf("\\");
		String dirPath = file.substring(0, lastIndex + 1);
		String fileName = file.substring(lastIndex + 1, file.length());
		this.configFileName = fileName;
		configurationChanged(file);
		startWatcher(dirPath, fileName);
	}

	/**
	 * Register the given directory with the WatchService
	 */
	private void register(Path dir) throws IOException {
		//WatchKey key = dir.register(watcher, ENTRY_CREATE, ENTRY_DELETE, ENTRY_MODIFY);
		WatchKey key = dir.register(watcher, ENTRY_MODIFY);
		if (trace) {
			Path prev = keys.get(key);
			if (prev == null) {
				System.out.format("register: %s\n", dir);
			} else {
				if (!dir.equals(prev)) {
					LOGGER.info("update: %s -> %s\n", prev, dir);
				}
			}
		}
		keys.put(key, dir);
	}

	/**
	 * Register the given directory, and all its sub-directories, with the
	 * WatchService.
	 */
	private void registerAll(final Path start) throws IOException {
		// register directory and sub-directories
		Files.walkFileTree(start, new SimpleFileVisitor<Path>() {
			@Override
			public FileVisitResult preVisitDirectory(Path dir, BasicFileAttributes attrs) throws IOException {
				register(dir);
				return FileVisitResult.CONTINUE;
			}
		});
	}

	private void startWatcher(String dirPath, String file) throws IOException {
		final WatchService watchService = FileSystems.getDefault().newWatchService();
		Path path = Paths.get(dirPath);
		path.register(watchService, StandardWatchEventKinds.ENTRY_CREATE, StandardWatchEventKinds.ENTRY_DELETE,
				StandardWatchEventKinds.ENTRY_MODIFY);
		Runtime.getRuntime().addShutdownHook(new Thread() {
			public void run() {
				try {
					watchService.close();
				} catch (IOException e) {
					e.printStackTrace();
				}
			}
		});

		WatchKey key = null;
		while (true) {
			try {
				key = watchService.take();
				for (WatchEvent<?> event : key.pollEvents()) {
					if (event.context().toString().equals(configFileName)) {
						configurationChanged(dirPath + file);
					}
				}
				boolean reset = key.reset();
				if (!reset) {
					LOGGER.info("Could not reset the watch key.");
					break;
				}
			} catch (Exception e) {
				LOGGER.error("InterruptedException: " + e.getMessage(),e);
			}
		}
	}

	public void configurationChanged(final String file) {
		LOGGER.info("Refreshing the configuration.");
		// PropertyFileAutoRefresh.getInstance().initilize(file);
		propertyFileAutoRefresh.initilize(file);
	}

}
