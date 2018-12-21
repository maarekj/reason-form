module type ContextConfig = {
  let debugName: string;
  type t;
  let value: t;
};

module type ConsumerConfig = {type t;};

module type Consumer = {
  type context;
  type t;
  type action =
    | ChangeState(context);
  let make:
    (~pure: bool=?, ~selector: (context, option(t)) => t, ~render: t => ReasonReact.reactElement, _) =>
    ReasonReact.component(t, t => ReasonReact.reactElement, action);
};

module type Context = {
  type context;
  module Provider: {
    let make:
      (~value: context=?, 'a) =>
      ReasonReact.component(ReasonReact.stateless, ReasonReact.noRetainedProps, ReasonReact.actionless);
  };

  module CreateConsumer:
    (ConsumerConfig: ConsumerConfig) => Consumer with type context := context and type t := ConsumerConfig.t;
};

module CreateContext = (C: ContextConfig) : (Context with type context = C.t) => {
  type context = C.t;
  let passThrough = [%bs.raw {| props => props.children |}];
  let state = ref(C.value);
  let subscriptions = ref([||]);
  let addSubscription = f => {
    subscriptions := Js.Array.concat([|f|], subscriptions^);
    () => subscriptions := Js.Array.filter(sub => sub !== f, subscriptions^);
  };
  let updateState = newStateOpt => {
    let newState =
      switch (newStateOpt) {
      | None => C.value
      | Some(newValue) => newValue
      };
    state := newState;
    Js.Array.forEach(f => f(newState), subscriptions^);
  };

  module Provider = {
    let component = ReasonReact.statelessComponent(C.debugName ++ "ContextProvider");
    let make = (~value=?, children) => {
      ...component,
      willReceiveProps: _self => updateState(value),
      didMount: _self => {
        updateState(value);
        ();
      },
      render: _self =>
        ReasonReact.element(ReasonReact.wrapJsForReason(~reactClass=passThrough, ~props=Js.Obj.empty(), children)),
    };
  };

  module CreateConsumer =
         (ConsumerConfig: ConsumerConfig)
         : (Consumer with type t := ConsumerConfig.t and type context := C.t) => {
    type action =
      | ChangeState(C.t);
    let component = ReasonReact.reducerComponentWithRetainedProps(C.debugName ++ "ContextConsumer");
    let make =
        (
          ~pure=false,
          ~selector: (C.t, option(ConsumerConfig.t)) => ConsumerConfig.t,
          ~render: ConsumerConfig.t => ReasonReact.reactElement,
          _children,
        ) => {
      ...component,
      retainedProps: render,
      initialState: () => selector(state^, None),
      shouldUpdate: ({oldSelf, newSelf}) =>
        pure == true ?
          oldSelf.state !== newSelf.state :
          oldSelf.retainedProps !== newSelf.retainedProps || oldSelf.state !== newSelf.state,
      reducer: (action, oldSelected) =>
        switch (action) {
        | ChangeState(newState) => ReasonReact.Update(selector(newState, Some(oldSelected)))
        },
      didMount: ({send, onUnmount}) => (newState => send(ChangeState(newState))) |> addSubscription |> onUnmount,
      render: ({state}) => render(state),
    };
  };
};
