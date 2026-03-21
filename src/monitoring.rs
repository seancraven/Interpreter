use opentelemetry::trace::TracerProvider;
use tracing_flame::FlameLayer;
use tracing_subscriber::{layer::SubscriberExt, util::SubscriberInitExt};

pub fn init_tracing() {
    // tracer provider
    let provider = opentelemetry_sdk::trace::SdkTracerProvider::builder()
        .with_simple_exporter(opentelemetry_stdout::SpanExporter::default())
        .build();

    let tracer = provider.tracer("tracing");
    let otel_layer = tracing_opentelemetry::layer().with_tracer(tracer);
    let (flame, _gard) = FlameLayer::with_file("./flamegraph").unwrap();

    tracing_subscriber::registry()
        .with(otel_layer)
        .with(flame)
        .with(tracing_subscriber::fmt::layer())
        .init();
}
