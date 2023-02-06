use concoct::{
    composable::{column, material::button, row, state, text},
    modify::{
        container::{ContainerModifier, Gap},
        TextModifier,
    },
    render::run,
    DevicePixels, Modifier,
};
use taffy::style::{AlignItems, Dimension, JustifyContent};

fn app() {
    column(
        Modifier
            .align_items(AlignItems::Center)
            .justify_content(JustifyContent::Center)
            .flex_grow(1.)
            .gap(Gap::default().height(Dimension::Points(20.dp()))),
        || {
            let count = state(|| 0);

            text(
                Modifier.font_size(80.dp()),
                count.get().cloned().to_string(),
            );

            row(
                Modifier.gap(Gap::default().width(Dimension::Points(20.dp()))),
                move || {
                    button(Modifier, "More", move || *count.get().as_mut() += 1);

                    button(Modifier, "Less", move || *count.get().as_mut() -= 1);
                },
            )
        },
    )
}

fn main() {
    run(app)
}