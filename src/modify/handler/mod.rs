use super::{Chain, ModifyExt};
use crate::{
    composable::{interaction_source::InteractionSource, state::State},
    semantics::Handler,
    Modify, Semantics,
};
use accesskit::{NodeId, Role};

pub mod clickable;
use clickable::ClickHandler;
use tokio::sync::broadcast;

pub mod keyboard_input;
use self::{
    clickable::ClickInteration,
    keyboard_input::{KeyboardHandler, KeyboardInputHandler},
};

pub trait HandlerModifier<T>: Modify<T> {
    fn handler<H>(self, handler: H) -> Chain<T, Self, ModifierHandler<H>>
    where
        Self: Sized,
        H: Handler + 'static,
    {
        self.chain(ModifierHandler {
            handler: Some(handler),
        })
    }

    fn clickable<F>(
        self,
        role: Role,
        on_click: F,
    ) -> Chain<T, Chain<T, Self, Role>, ModifierHandler<ClickHandler<(), F>>>
    where
        Self: Sized,
        T: AsMut<Role>,
        F: FnMut() + 'static,
    {
        self.chain(role).handler(ClickHandler::new((), on_click))
    }

    fn clickable_interaction<F, I>(
        self,
        role: Role,
        on_click: F,
        interaction_source: I,
    ) -> Chain<T, Chain<T, Self, Role>, ModifierHandler<ClickHandler<I, F>>>
    where
        Self: Sized,
        T: AsMut<Role>,
        F: FnMut() + 'static,
        I: InteractionSource<ClickInteration> + 'static,
    {
        self.chain(role)
            .handler(ClickHandler::new(interaction_source, on_click))
    }

    fn keyboard_handler<H>(
        self,
        handler: H,
    ) -> Chain<T, Self, ModifierHandler<KeyboardInputHandler<H>>>
    where
        Self: Sized,
        H: KeyboardHandler + 'static,
    {
        self.handler(KeyboardInputHandler::new(handler))
    }
}

impl<T, M> HandlerModifier<T> for M where M: Modify<T> {}

pub struct ModifierHandler<H> {
    handler: Option<H>,
}

impl<T, H> Modify<T> for ModifierHandler<H>
where
    H: Handler + 'static,
{
    fn modify(&mut self, _value: &mut T) {}

    fn semantics(&mut self, node_id: NodeId, semantics: &mut Semantics) {
        if let Some(handler) = self.handler.take() {
            semantics.handlers.insert(node_id, Box::new(handler));
        }
    }

    fn remove(&mut self, node_id: NodeId, semantics: &mut Semantics) {
        semantics.handlers.remove(&node_id);
    }
}