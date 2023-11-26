use crate::View;
use crate::{use_ref, BUILD_CONTEXT};

pub trait IntoView: 'static {
    fn into_view(self) -> impl View;
}

impl<C: View> IntoView for C {
    fn into_view(self) -> impl View {
        self
    }
}

macro_rules! impl_view_for_tuple {
    ($($a:ident: $b:tt),*) => {
        impl<$($a: IntoView),*> IntoView for ($($a),*) {
            fn into_view(self) -> impl View {
                let mut views = Some(self);
                let keys = *use_ref(|| {
                    BUILD_CONTEXT
                        .try_with(|cx| {
                            let mut g = cx.borrow_mut();
                            let mut cx = g.as_mut().unwrap().borrow_mut();
                            let views = views.take().unwrap();

                            ($({
                                let mut a = Some(views.$b);
                                cx.insert(Box::new(move || Box::new(a.take().unwrap().into_view())))
                            }),*)
                        })
                        .unwrap()
                })
                .get();

                if let Some(views) = views.take() {
                    BUILD_CONTEXT
                        .try_with(|cx| {
                            let mut g = cx.borrow_mut();
                            let cx = g.as_mut().unwrap().borrow_mut();

                            ($({
                                let mut a = Some(views.$b);
                                cx.nodes[keys.$b].borrow_mut().make_view =
                                    Box::new(move || Box::new(a.take().unwrap().into_view()));
                            }),*)
                        })
                        .unwrap();
                }
            }
        }
    };
}

macro_rules! impl_view_for_tuples {
    ($( ( $( $a:tt: $b:tt ),* ) ), * ) => {
        $(impl_view_for_tuple!($($a: $b),*);)*
    };
}

impl_view_for_tuples!(
    (A: 0, B: 1),
    (A: 0, B: 1, C: 2),
    (A: 0, B: 1, C: 2, D: 3),
    (A: 0, B: 1, C: 2, D: 3, E: 4),
    (A: 0, B: 1, C: 2, D: 3, E: 4, F: 5),
    (A: 0, B: 1, C: 2, D: 3, E: 4, F: 5, G: 6),
    (A: 0, B: 1, C: 2, D: 3, E: 4, F: 5, G: 6, H: 7),
    (A: 0, B: 1, C: 2, D: 3, E: 4, F: 5, G: 6, H: 7, I: 8),
    (A: 0, B: 1, C: 2, D: 3, E: 4, F: 5, G: 6, H: 7, I: 8, J: 9),
    (A: 0, B: 1, C: 2, D: 3, E: 4, F: 5, G: 6, H: 7, I: 8, J: 9, K: 10),
    (A: 0, B: 1, C: 2, D: 3, E: 4, F: 5, G: 6, H: 7, I: 8, J: 9, K: 10, L: 11),
    (A: 0, B: 1, C: 2, D: 3, E: 4, F: 5, G: 6, H: 7, I: 8, J: 9, K: 10, L: 11, M: 12),
    (A: 0, B: 1, C: 2, D: 3, E: 4, F: 5, G: 6, H: 7, I: 8, J: 9, K: 10, L: 11, M: 12, N: 13),
    (A: 0, B: 1, C: 2, D: 3, E: 4, F: 5, G: 6, H: 7, I: 8, J: 9, K: 10, L: 11, M: 12, N: 13, O: 14),
    (A: 0, B: 1, C: 2, D: 3, E: 4, F: 5, G: 6, H: 7, I: 8, J: 9, K: 10, L: 11, M: 12, N: 13, O: 14, P: 15)
);
