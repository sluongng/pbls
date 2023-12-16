use std::sync::OnceLock;

static LOGGER: OnceLock<Logger> = OnceLock::new();

struct Logger {
    level: log::Level,
}

pub fn init(level: log::Level) {
    if let Ok(_) = LOGGER.set(Logger { level }) {
        log::set_logger(LOGGER.get().unwrap()).unwrap();
        log::set_max_level(level.to_level_filter());
    }
}

impl log::Log for Logger {
    fn enabled(&self, metadata: &log::Metadata) -> bool {
        metadata.level() <= self.level
    }

    fn log(&self, record: &log::Record) {
        if self.enabled(record.metadata()) {
            eprintln!("{}: {}", record.level(), record.args());
        }
    }

    fn flush(&self) {}
}
