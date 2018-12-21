module Address = Example_address_fields;
module User = Example_user_fields;

let initialForm = User.initializeForm();

module ReactForm = {
  include React.Make({
    type values = User.Value.t;
    let initialForm = initialForm;
  });
};

module RenderForm = Example_user_render_form.Make(ReactForm);

let delay = ms => {
  let unit = ();
  Js.Promise.make((~resolve, ~reject as _) => ignore(Js.Global.setTimeout(() => resolve(. unit), ms)));
};

let reactForm: module React.S = (module ReactForm);

type state = int;
type action =
  | Inc;

let component = ReasonReact.reducerComponent("UserForm");
let make = _children => {
  ...component,
  initialState: () => 0,
  reducer: (action: action, state: state) =>
    switch (action) {
    | Inc => Update(state + 1)
    },
  render: ({state, send}) => {
    Js.log("root render");
    <div>
      <p> {ReasonReact.string(string_of_int(state))} </p>
      <button
        onClick={
          event => {
            event->ReactEvent.Synthetic.preventDefault;
            send(Inc);
          }
        }>
        {ReasonReact.string("increment")}
      </button>
      <ReactForm.WithFormContainer
        initialForm
        onSubmit={
          (~dispatch as _, ~form) => {
            let values = Form.getValues(form);
            Js.log(values);
            delay(3000)
            |> Js.Promise.then_(() => {
                 Js.log("after 3000ms");
                 Js.Promise.resolve(Belt.Result.Error("Submit error"));
               });
          }
        }
        render={(~dispatch, ~handleSubmit) => <RenderForm state initialForm dispatch handleSubmit />}
      />
    </div>;
  },
};
