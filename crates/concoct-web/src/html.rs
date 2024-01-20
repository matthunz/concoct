use super::WebContext;
use concoct::{
    hook::{use_context, use_on_drop, use_provider, use_ref},
    view::Child,
    Tree, View, ViewBuilder,
};
use std::{borrow::Cow, cell::RefCell, rc::Rc};
use web_sys::{
    wasm_bindgen::{closure::Closure, JsCast},
    Element, Event,
};

macro_rules! make_tag_fns {
    ($($name:tt),*) => {
        $(
            pub fn $name(child: impl View) -> Html<impl Tree> {
                html(stringify!($name), child)
            }
        )*
    };
}

make_tag_fns!(
    a, abbr, address, area, article, aside, audio, b, base, bdi, bdo, blockquote, body, br, button,
    canvas, caption, cite, code, col, colgroup, data, datalist, dd, del, details, dfn, dialog, div,
    dl, dt, em, embed, fieldset, figcaption, figure, footer, form, h1, h2, h3, h4, h5, h6, head,
    header, hr, i, iframe, img, input, ins, kbd, label, legend, li, link, main, map, mark, meta,
    meter, nav, noscript, object, ol, optgroup, option, output, p, param, picture, pre, progress,
    q, rp, rt, ruby, s, samp, script, section, select, small, source, span, strong, sub, summary,
    sup, svg, table, tbody, td, template, textarea, tfoot, th, thead, time, title, tr, track, u,
    ul, var, video, wbr
);

#[derive(Default)]
struct Data {
    element: Option<Element>,
    callbacks: Vec<(
        Closure<dyn FnMut(Event)>,
        Rc<RefCell<Rc<RefCell<dyn FnMut(Event)>>>>,
    )>,
}

pub struct Html<C> {
    tag: Cow<'static, str>,
    attrs: Vec<(Cow<'static, str>, Cow<'static, str>)>,
    handlers: Vec<(Cow<'static, str>, Rc<RefCell<dyn FnMut(Event)>>)>,
    child: Child<C>,
}

macro_rules! impl_attr_methods {
    ($($fn_name: tt: $name: tt),*) => {
        $(
            pub fn $fn_name(self, value: impl Into<Cow<'static, str>>,) -> Self {
                self.attr($name, value)
            }
        )*
    };
}

macro_rules! impl_handler_methods {
    ($($fn_name: tt: $name: tt),*) => {
        $(
            pub fn $fn_name(self, handler: impl FnMut(Event) + 'static) -> Self {
                self.handler($name, handler)
            }
        )*
    };
}

pub fn html(tag: impl Into<Cow<'static, str>>, child: impl View) -> Html<impl Tree> {
    Html {
        tag: tag.into(),
        attrs: Vec::new(),
        handlers: Vec::new(),
        child: concoct::view::child(child),
    }
}

impl<C> Html<C> {
    pub fn attr(
        mut self,
        name: impl Into<Cow<'static, str>>,
        value: impl Into<Cow<'static, str>>,
    ) -> Self {
        self.attrs.push((name.into(), value.into()));
        self
    }

    pub fn handler(
        mut self,
        name: impl Into<Cow<'static, str>>,
        handler: impl FnMut(Event) + 'static,
    ) -> Self {
        self.handlers
            .push((name.into(), Rc::new(RefCell::new(handler))));
        self
    }

    impl_attr_methods!(
        class: "class",
        kind: "type"
    );

    impl_handler_methods!(
        on_click: "click",
        on_input: "input",
        on_submit: "submit"
    );
}

impl<C: Tree> ViewBuilder for Html<C> {
    fn build(&self) -> impl View {
        let data = use_ref(|| RefCell::new(Data::default()));
        let mut data_ref = data.borrow_mut();

        let web_cx = use_context::<WebContext>().unwrap();
        let data_clone = data.clone();

        use_on_drop(move || {
            if let Some(element) = &data_clone.borrow_mut().element {
                element.remove();
            }
        });

        if data_ref.element.is_none() {
            let elem = web_cx.document.create_element(&self.tag).unwrap();
            web_cx.parent.append_child(&elem).unwrap();

            for (name, value) in &self.attrs {
                elem.set_attribute(name, value).unwrap();
            }

            for (name, handler) in &self.handlers {
                let handler_cell = Rc::new(RefCell::new(handler.clone()));
                let handler_cell_clone = handler_cell.clone();

                let callback: Closure<dyn FnMut(Event)> = Closure::wrap(Box::new(move |event| {
                    handler_cell.borrow().borrow_mut()(event)
                }));
                elem.add_event_listener_with_callback(&name, callback.as_ref().unchecked_ref())
                    .unwrap();

                data_ref.callbacks.push((callback, handler_cell_clone));
            }

            data_ref.element = Some(elem);
        } else {
            for ((_name, handler), (_callback, cell)) in
                self.handlers.iter().zip(&data_ref.callbacks)
            {
                *cell.borrow_mut() = handler.clone();
            }
        }

        use_provider(WebContext {
            window: web_cx.window.clone(),
            document: web_cx.document.clone(),
            parent: data_ref.element.as_ref().unwrap().clone().into(),
        });

        self.child.clone()
    }
}
