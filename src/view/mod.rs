use crate::Context;
use impl_trait_for_tuples::impl_for_tuples;
use web_sys::Text;

pub mod html;
pub use html::Html;

mod lazy;
pub use lazy::{lazy, Lazy};

pub trait View<E> {
    type State;

    fn build(self, cx: &mut Context<E>) -> Self::State;

    fn rebuild(self, cx: &mut Context<E>, state: &mut Self::State);

    fn remove(cx: &mut Context<E>, state: &mut Self::State);
}

impl<E, V: View<E>> View<E> for Option<V> {
    type State = Option<V::State>;

    fn build(self, cx: &mut Context<E>) -> Self::State {
        self.map(|view| view.build(cx))
    }

    fn rebuild(self, cx: &mut Context<E>, state: &mut Self::State) {
        if let Some(view) = self {
            if let Some(state) = state {
                view.rebuild(cx, state)
            } else {
                *state = Some(view.build(cx))
            }
        } else if let Some(s) = state {
            V::remove(cx, s);
            *state = None;
            cx.skip();
        }
    }

    fn remove(cx: &mut Context<E>, state: &mut Self::State) {
        if let Some(state) = state {
            V::remove(cx, state);
            cx.skip()
        }
    }
}

impl<E> View<E> for &'_ str {
    type State = (Self, Text);

    fn build(self, cx: &mut Context<E>) -> Self::State {
        let elem = cx.document.create_text_node(&self);
        cx.insert(&elem);

        (self, elem)
    }

    fn rebuild(self, cx: &mut Context<E>, (prev, text): &mut Self::State) {
        if &self != &*prev {
            text.set_text_content(Some(&self))
        }
        cx.skip()
    }

    fn remove(_cx: &mut Context<E>, state: &mut Self::State) {
        state.1.remove();
    }
}

impl<E> View<E> for String {
    type State = (String, Text);

    fn build(self, cx: &mut Context<E>) -> Self::State {
        let elem = cx.document.create_text_node(&self);
        cx.insert(&elem);
        (self, elem)
    }

    fn rebuild(self, cx: &mut Context<E>, (prev, text): &mut Self::State) {
        if &self != &*prev {
            text.set_text_content(Some(&self))
        }
        cx.skip()
    }

    fn remove(_cx: &mut Context<E>, state: &mut Self::State) {
        state.1.remove();
    }
}

#[impl_for_tuples(16)]
impl<E> View<E> for Tuple {
    for_tuples!( type State = ( #( Tuple::State ),* ); );

    fn build(self, cx: &mut Context<E>) -> Self::State {
        for_tuples!( (#( self.Tuple.build(cx) ),*) )
    }

    fn rebuild(self, cx: &mut Context<E>, state: &mut Self::State) {
        for_tuples!( #( self.Tuple.rebuild(cx, &mut state.Tuple); )* )
    }

    fn remove(cx: &mut Context<E>, state: &mut Self::State) {
        for_tuples!( #( Tuple::remove(cx, &mut state.Tuple); )* )
    }
}

impl<E, K, V> View<E> for Vec<(K, V)>
where
    K: PartialEq,
    V: View<E>,
{
    type State = Vec<(K, V::State)>;

    fn build(self, cx: &mut Context<E>) -> Self::State {
        self.into_iter()
            .map(|(key, view)| {
                let state = view.build(cx);
                (key, state)
            })
            .collect()
    }

    fn rebuild(self, cx: &mut Context<E>, state: &mut Self::State) {
        // Build new views and rebuild old views
        let new_state = self
            .into_iter()
            .map(|(key, view)| {
                let view_state = if let Some(pos) = state
                    .iter_mut()
                    .position(|(state_key, _)| &key == state_key)
                {
                    let (_, mut view_state) = state.remove(pos);
                    view.rebuild(cx, &mut view_state);
                    view_state
                } else {
                    view.build(cx)
                };
                (key, view_state)
            })
            .collect();

        // Remove trailing views
        for (_, view_state) in &mut state[..] {
            V::remove(cx, view_state);
        }

        *state = new_state;
    }

    fn remove(_cx: &mut Context<E>, _state: &mut Self::State) {
        todo!()
    }
}
