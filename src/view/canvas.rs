use super::{Id, LayoutContext, View};
use skia_safe::Rect;
use std::any::Any;
use taffy::{
    prelude::{Layout, Node},
    style::{Dimension, Style},
    Taffy,
};

/// Canvas element.
/// This lets you draw directly to the skia canvas.
pub struct Canvas<F> {
    layout_key: Option<Node>,
    draw: F,
    style: Style,
}

impl<F> Canvas<F>
where
    F: FnMut(&Layout, &mut skia_safe::Canvas),
{
    /// Create a new canvas element that will draw its content with the given function.
    pub fn new(draw: F) -> Self {
        Self {
            draw,
            layout_key: None,
            style: Style::default(),
        }
    }

    pub fn size(mut self, size: taffy::prelude::Size<Dimension>) -> Self {
        self.style.size = size;
        self
    }
}

impl<T, A, F> View<T, A> for Canvas<F>
where
    F: FnMut(&Layout, &mut skia_safe::Canvas),
{
    fn build(&mut self, cx: &mut super::BuildContext) -> Id {
        cx.id()
    }

    fn rebuild(&mut self, _cx: &mut super::BuildContext, old: &mut Self) {
        self.layout_key = old.layout_key;
    }

    fn message(&mut self, _state: &mut T, _id_path: &[Id], _message: &dyn Any) -> Option<A> {
        None
    }

    fn layout(&mut self, cx: &mut LayoutContext, id: Id) {
        if let Some(key) = self.layout_key {
            cx.taffy.set_style(key, self.style.clone()).unwrap();
        } else {
            let layout_key = cx.insert(id, self.style.clone());
            self.layout_key = Some(layout_key);
        }
    }

    fn paint(&mut self, taffy: &Taffy, canvas: &mut skia_safe::Canvas) {
        let layout = taffy.layout(self.layout_key.unwrap()).unwrap();
        canvas.save();
        canvas.clip_rect(
            Rect::new(
                layout.location.x,
                layout.location.y,
                layout.location.x + layout.size.width,
                layout.location.y + layout.size.height,
            ),
            None,
            None,
        );
        canvas.translate((layout.location.x, layout.location.y));

        (self.draw)(layout, canvas);

        canvas.restore();
    }
}
