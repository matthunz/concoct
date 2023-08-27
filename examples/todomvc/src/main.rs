use concoct::attr::{attr, class, event_key_code, event_target_value, on, value};
use concoct::view::html::{button, div, footer, h1, header, input, label, li, p, section, ul};
use concoct::view::{lazy, View};
use concoct::Attribute;
use std::mem;

enum Event {
    None,
    UpdateInput(String),
    Add,
    Remove(u32),
    Check(u32),
    Edit { id: u32, is_editing: bool },
    Update { id: u32, content: String },
}

#[derive(Clone, PartialEq)]
struct Todo {
    id: u32,
    content: String,
    is_editing: bool,
    is_completed: bool,
}

#[derive(Default)]
struct State {
    input: String,
    next_id: u32,
    unused_ids: Vec<u32>,
    todos: Vec<Todo>,
}

fn view(state: &State) -> impl View<Event> {
    (div().modify(attr("class", "todomvc-wrapper")).then((
        section().modify(attr("class", "todoapp")).then((
            lazy(state.input.clone(), view_input(state)),
            lazy(state.todos.clone(), view_entries(state)),
        )),
        lazy((), view_footer()),
    )),)
}

fn view_input(state: &State) -> impl View<Event> {
    header().modify(attr("class", "header")).then((
        h1().then("Todos"),
        input().modify((
            attr("class", "new-todo"),
            attr("placeholder", "What needs to be done?"),
            attr("autofocus", "True"),
            attr("name", "newTodo"),
            value(state.input.clone()),
            on("input", |event| {
                event.prevent_default();
                Event::UpdateInput(event_target_value(&event))
            }),
            on_enter(|| Event::Add),
        )),
    ))
}

fn view_entries(state: &State) -> impl View<Event> {
    ul().modify(class("todo-list")).then(
        state
            .todos
            .iter()
            .map(|todo| (todo.id, view_entry(&todo)))
            .collect::<Vec<_>>(),
    )
}

fn view_entry(todo: &Todo) -> impl View<Event> {
    let id = todo.id;
    let class_list = if todo.is_completed {
        if todo.is_editing {
            "completed editing"
        } else {
            "completed"
        }
    } else if todo.is_editing {
        "editing"
    } else {
        ""
    };

    li().modify(class(class_list)).then((
        div().modify(class("view")).then((
            input().modify((
                class("toggle"),
                attr("type", "checkbox"),
                attr("checked", todo.is_completed.to_string()),
                on("click", move |_| Event::Check(id)),
            )),
            label()
                .modify(on("click", move |_| Event::Edit {
                    id,
                    is_editing: true,
                }))
                .then(todo.content.clone()),
            button().modify((class("destroy"), on("click", move |_| Event::Remove(id)))),
        )),
        input().modify((
            class("edit"),
            value(todo.content.clone()),
            attr("name", "content"),
            on("input", move |event| {
                event.prevent_default();
                Event::Update {
                    id,
                    content: (event_target_value(&event)),
                }
            }),
            on("blur", move |_| Event::Edit {
                id,
                is_editing: false,
            }),
            on_enter(move || Event::Edit {
                id,
                is_editing: false,
            }),
        )),
    ))
}

fn view_footer() -> impl View<Event> {
    footer()
        .modify(class("info"))
        .then(p().then("Click to edit a todo"))
}

fn on_enter(f: impl Fn() -> Event + 'static) -> impl Attribute<Event> {
    on("keydown", move |event| {
        if event_key_code(&event) == 13 {
            f()
        } else {
            Event::None
        }
    })
}

fn main() {
    concoct::run(
        State::default(),
        |state, event| match event {
            Event::None => {}
            Event::UpdateInput(value) => {
                state.input = value;
            }
            Event::Add => {
                let content = mem::take(&mut state.input);
                let id = state.unused_ids.pop().unwrap_or_else(|| {
                    let id = state.next_id;
                    state.next_id += 1;
                    id
                });
                state.todos.push(Todo {
                    id,
                    content,
                    is_editing: false,
                    is_completed: false,
                });
            }
            Event::Check(id) => {
                if let Some(todo) = state.todos.iter_mut().find(|todo| todo.id == id) {
                    todo.is_completed = !todo.is_completed;
                }
            }
            Event::Edit { id, is_editing } => {
                if let Some(todo) = state.todos.iter_mut().find(|todo| todo.id == id) {
                    todo.is_editing = is_editing;
                }
            }
            Event::Update { id, content } => {
                if let Some(todo) = state.todos.iter_mut().find(|todo| todo.id == id) {
                    todo.content = content;
                }
            }
            Event::Remove(id) => {
                if let Some(idx) = state.todos.iter().position(|todo| todo.id == id) {
                    state.todos.remove(idx);
                    state.unused_ids.push(id);
                }
            }
        },
        view,
    )
}
