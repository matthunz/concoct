use crate::{semantics::LayoutNode, DevicePixels, Modify, Semantics, Widget};
use accesskit::{Node, NodeId, Role};
use skia_safe::{
    textlayout::{FontCollection, Paragraph, ParagraphBuilder, ParagraphStyle, TextStyle},
    Canvas, FontMgr, Typeface, RGB,
};
use std::{
    any,
    sync::{Arc, Mutex},
};
use taffy::{
    node::MeasureFunc,
    prelude::{AvailableSpace, Size},
    style::Style,
};

use super::widget;

pub mod modifier;
pub use modifier::TextModifier;

pub struct TextConfig {
    pub typeface: Typeface,
    pub style: Style,
    pub font_size: f32,
}

impl AsMut<Style> for TextConfig {
    fn as_mut(&mut self) -> &mut Style {
        &mut self.style
    }
}

#[track_caller]
pub fn text(mut modifier: impl Modify<TextConfig> + 'static, string: impl Into<String>) {
    let mut text_modifier = TextConfig {
        typeface: Typeface::new("serif", Default::default()).unwrap(),
        style: Style::default(),
        font_size: 14.dp(),
    };
    modifier.modify(&mut text_modifier);

    widget(
        string.into(),
        |text| TextWidget {
            text: text,
            node_id: None,
            layout_id: None,
            paragraph: None,
            modify: Box::new(modifier),
            modifier: text_modifier,
        },
        |text, node| {
            let widget: &mut TextWidget = node.as_mut();
            widget.text = text;
        },
    )
}

pub struct TextWidget {
    text: String,
    node_id: Option<NodeId>,
    layout_id: Option<LayoutNode>,
    paragraph: Option<Arc<Mutex<Paragraph>>>,
    modify: Box<dyn Modify<TextConfig>>,
    modifier: TextConfig,
}

impl Widget for TextWidget {
    fn layout(&mut self, semantics: &mut Semantics) {
        let font_mgr = FontMgr::new();
        let mut font_collection = FontCollection::new();
        font_collection.set_default_font_manager(font_mgr, None);

        let paragraph_style = ParagraphStyle::new();
        let mut paragraph_builder = ParagraphBuilder::new(&paragraph_style, font_collection);

        let mut text_style = TextStyle::new();
        text_style.set_color(RGB::from((0, 0, 0)));
        text_style.set_font_size(self.modifier.font_size);
        text_style.set_typeface(self.modifier.typeface.clone());
        paragraph_builder.push_style(&text_style);

        paragraph_builder.add_text(&self.text);
        paragraph_builder.pop();

        let paragraph = Arc::new(Mutex::new(paragraph_builder.build()));
        self.paragraph = Some(paragraph.clone());
        let measure = move |_known_dimensions, available_space: Size<AvailableSpace>| {
            let mut paragraph = paragraph.lock().unwrap();
            let max_width = match available_space.width {
                AvailableSpace::Definite(px) => px,
                AvailableSpace::MaxContent => f32::MAX,
                AvailableSpace::MinContent => f32::MIN,
            };
            paragraph.layout(max_width);

            Size {
                width: paragraph.longest_line(),
                height: paragraph.height(),
            }
        };

        if let Some(layout_id) = self.layout_id {
            semantics
                .taffy
                .set_style(layout_id, self.modifier.style)
                .unwrap();
            semantics
                .taffy
                .set_measure(layout_id, Some(MeasureFunc::Boxed(Box::new(measure))))
                .unwrap();

            semantics
                .layout_children
                .last_mut()
                .unwrap()
                .push(layout_id);
        } else {
            let layout_id = semantics.insert_layout_with_measure(self.modifier.style, measure);
            self.layout_id = Some(layout_id);
        }
    }

    fn semantics(&mut self, semantics: &mut Semantics) {
        let node = Node {
            role: Role::StaticText,
            value: Some(self.text.clone().into_boxed_str()),
            ..Node::default()
        };

        let node_id = if let Some(node_id) = self.node_id {
            semantics.update(node_id, node);
            node_id
        } else {
            let node_id = semantics.insert(node);
            self.node_id = Some(node_id);
            node_id
        };

        self.modify.semantics(node_id, semantics);
    }

    fn paint(&mut self, semantics: &Semantics, canvas: &mut Canvas) {
        let layout = semantics.layout(self.layout_id.unwrap());

        self.modify.paint(&layout, canvas);
        self.paragraph
            .as_ref()
            .unwrap()
            .lock()
            .unwrap()
            .paint(canvas, (layout.location.x, layout.location.y))
    }

    fn remove(&mut self, semantics: &mut Semantics) {
        if let Some(node_id) = self.node_id {
            semantics.remove(node_id);

            self.modify.remove(node_id, semantics)
        }
    }

    fn any(&self) -> &dyn any::Any {
        self
    }

    fn any_mut(&mut self) -> &mut dyn any::Any {
        self
    }
}