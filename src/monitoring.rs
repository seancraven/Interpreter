use tracing::Level;
use tracing_subscriber::fmt::format::FmtSpan;

pub fn init_tracing() {
    tracing_subscriber::fmt()
        .with_span_events(FmtSpan::ENTER)
        .with_max_level(Level::DEBUG)
        .init();
}
