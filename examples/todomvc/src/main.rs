use concoct::{use_signal, Html, View};

fn app() -> impl View {
    let label = use_signal(|| String::new());
    let value = label.read();

   
    Html::input()
        .attr("value", value.clone())
        .on_input(move |event| {
            event.prevent_default();
            *label.write() = event.target().unwrap().value();
        })
}

fn main() {
    console_error_panic_hook::set_once();
    dioxus_logger::init(log::LevelFilter::Info).expect("failed to init logger");
    concoct::run(app);
}